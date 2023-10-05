/**
 * @file DataFlow.hpp
 * Generalized Dataflow framework for LLVM.
 * Very similar in implementation to my generalized dataflow framework for
 * BRIL written in Rust
 * [here](https://github.com/stephenverderame/cs6120-bril/blob/main/cfg/src/analysis/mod.rs)
 */
#pragma once
#include <bits/iterator_concepts.h>
#include <llvm-17/llvm/IR/BasicBlock.h>
#include <llvm-17/llvm/IR/CFG.h>
#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/Instruction.h>
#include <llvm-17/llvm/IR/Instructions.h>

#include <concepts>
#include <deque>
#include <iterator>
#include <map>
#include <type_traits>
#include <variant>

/// Bare bones concept for checking if a type is iterable.
template <typename T>
concept Iterable = requires(T t) {
    {
        t.begin()
    };
    {
        t.end()
    };
};

/**
 * @brief Dataflow analysis direction.
 * A Direction must have a static `successors` function which returns an
 * iterable of basic blocks, a `begin` function which returns an iterator to
 * the first instruction in the basic block, and an `end` function which
 * returns a sentinel for the end of the basic block.
 */
template <typename T>
concept Direction = requires(T) {
    /// Get the successors of a basic block with respect to the direction.
    {
        T::successors(std::declval<const llvm::BasicBlock&>())
    } -> Iterable;

    /// Begin iteratoring over instruction of the basic block in the direction.
    {
        T::begin(std::declval<const llvm::BasicBlock&>())
    } -> std::incrementable;

    /// Sentinel for the end of the basic block iteration in the direction.
    {
        T::end(std::declval<const llvm::BasicBlock&>())
    } -> std::incrementable;
};

/**
 * @brief The return type of a transfer function for a Fact `T`
 * @tparam T The fact type
 */
template <typename T>
using TransferRetType = std::variant<std::map<const llvm::BasicBlock*, T>, T>;

/**
 * @brief Mapping from instructions to facts
 *
 * @tparam F
 */
template <typename F>
struct DataFlowFacts {
    /// @brief The incoming facts for each instruction.
    std::map<const llvm::Instruction*, F> InstructionInFacts;
    std::map<const llvm::BasicBlock*, TransferRetType<F>> BlockOutFacts;

    bool operator==(const DataFlowFacts& Other) const = default;
};

/**
 * @brief Dataflow analysis fact.
 * A Fact must have a static `meet` function, a `transfer` method which takes
 * an instruction and returns a new fact, and a `Dir` type which is a
 * Direction. It must also be equality comparable and copyable.
 */
template <typename T>
concept Fact = requires(const T& t) {
    /// Greatest lower bound of two facts
    {
        T::meet(t, t)
    } -> std::convertible_to<T>;

    /**
     * @brief Transfer function for a fact.
     * @param I The instruction to transfer
     * @param Facts The current set of facts
     * @return The outgoing fact for I
     */
    {
        t.transfer(std::declval<const llvm::Instruction&>(),
                   std::declval<const DataFlowFacts<T>&>())
    } -> std::convertible_to<TransferRetType<T>>;

    requires std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>;

    {
        t == t
    } -> std::same_as<bool>;

    Direction<typename T::Dir>;
};

/**
 * @brief Forwards dataflow analysis direction
 */
struct Forwards {
    [[nodiscard]] static auto successors(const llvm::BasicBlock& BB)
    {
        return llvm::successors(&BB);
    }

    [[nodiscard]] static auto begin(const llvm::BasicBlock& BB)
    {
        return BB.begin();
    }

    [[nodiscard]] static auto end(const llvm::BasicBlock& BB)
    {
        return BB.end();
    }
};

static_assert(Direction<Forwards>);

/**
 * @brief Backwards dataflow analysis direction
 *
 */
struct Backwards {
    [[nodiscard]] static auto successors(const llvm::BasicBlock& BB)
    {
        return llvm::predecessors(&BB);
    }

    [[nodiscard]] static auto begin(const llvm::BasicBlock& BB)
    {
        return BB.rbegin();
    }

    [[nodiscard]] static auto end(const llvm::BasicBlock& BB)
    {
        return BB.rend();
    }
};

static_assert(Direction<Backwards>);

/**
 * @brief Meets each output fact with the existing input fact for each
 * successor of the basic block.
 *
 * If `Out` is a single fact, applies that fact to all successors. Otherwise
 * `Out` should be a map from basic blocks to facts. In which case we apply
 * the corresponding fact to each successor.
 *
 * @tparam F The fact type
 * @param BB The basic block
 * @param Out The output fact
 * @param Facts The current set of facts
 * @return The new set of facts
 */
template <Fact F>
DataFlowFacts<F> broadcastOutFacts(const llvm::BasicBlock& BB,
                                   const TransferRetType<F>& Out,
                                   DataFlowFacts<F> Facts)
{
    using Dir = typename F::Dir;
    Facts.BlockOutFacts[&BB] = Out;
    if (std::holds_alternative<F>(Out)) {
        for (const auto& Succ : Dir::successors(BB)) {
            const auto& SuccFirstInst = *Dir::begin(*Succ);
            Facts.InstructionInFacts.at(&SuccFirstInst) = F::meet(
                Facts.InstructionInFacts.at(&SuccFirstInst), std::get<F>(Out));
        }
    } else {
        const auto& OutMap = std::get<0>(Out);
        for (const auto& Succ : Dir::successors(BB)) {
            const auto& SuccBB = *Succ;
            const auto& SuccFirstInst = *Dir::begin(SuccBB);
            Facts.InstructionInFacts.at(&SuccFirstInst) =
                F::meet(Facts.InstructionInFacts.at(&SuccFirstInst),
                        OutMap.at(&SuccBB));
        }
    }
    return Facts;
}

/**
 * @brief Performs a dataflow analysis on a function.
 *
 * @tparam F
 * @param Func The function to analyze.
 * @param Top The top element of the lattice.
 * @return DataFlowFacts<F>
 */
template <Fact F>
DataFlowFacts<F> analyze(const llvm::Function& Func, F Top)
{
    using Dir = typename F::Dir;
    DataFlowFacts<F> Facts;
    std::deque<std::reference_wrapper<const llvm::BasicBlock>> Worklist;
    for (const auto& BB : Func) {
        for (const auto& I : BB) {
            Facts.InstructionInFacts.emplace(&I, Top);
        }
        Worklist.emplace_back(BB);
    }

    while (!Worklist.empty()) {
        const auto& BB = Worklist.front();
        Worklist.pop_front();

        TransferRetType<F> LastOut =
            Dir::begin(BB) != Dir::end(BB)
                ? Facts.InstructionInFacts.at(&*Dir::begin(BB))
                : Top;
        for (auto I = Dir::begin(BB); I != Dir::end(BB); ++I) {
            const auto& Inst = *I;
            assert(std::holds_alternative<F>(LastOut));
            Facts.InstructionInFacts.at(&Inst) = std::get<F>(LastOut);
            LastOut = std::get<F>(LastOut).transfer(Inst, Facts);
        }
        const auto NewFacts = broadcastOutFacts(BB, LastOut, Facts);
        if (NewFacts != Facts) {
            Facts = NewFacts;
            for (const auto& Succ : Dir::successors(BB)) {
                Worklist.emplace_back(*Succ);
            }
        }
    }

    return Facts;
}

/**
 * @brief An element in a complete lattice.
 *
 * @tparam T
 */
template <typename T>
class LatticeElem
{
    enum Kind { Top, Inhabited, Bottom };

    std::optional<T> Fact_;
    Kind K_;

    explicit LatticeElem(Kind K) : K_(K) {}

  public:
    explicit LatticeElem(const T& Fact) : Fact_(Fact), K_(Kind::Inhabited) {}
    explicit LatticeElem(T&& Fact) : Fact_(std::move(Fact)), K_(Kind::Inhabited)
    {
    }
    /// @brief Create a top fact
    LatticeElem() : K_(Kind::Top) {}

    static LatticeElem makeTop() { return LatticeElem(); }
    static LatticeElem makeBottom() { return LatticeElem(Kind::Bottom); }

    bool isTop() const { return K_ == Kind::Top; }
    bool isBottom() const { return K_ == Kind::Bottom; }
    bool hasValue() const { return K_ == Kind::Inhabited; }
    T& value() { return Fact_.value(); }
    const T& value() const { return Fact_.value(); }

    bool operator==(const LatticeElem& Other) const = default;

    /// @brief Makes this fact bottom
    void bottomOut()
    {
        K_ = Kind::Bottom;
        Fact_ = std::nullopt;
    }

    /**
     * @brief Meets two individual lattice element (greatest lower bound).
     * If either is bottom, returns bottom. If either is top, returns the other.
     * Otherwise, returns the function applied to the two facts.
     *
     * @tparam Meet callable object to perform the meet of two inhabited facts
     * @param A The first fact
     * @param B The second fact
     * @param M The meet function, which must be callable with two facts and
     * return a new fact
     * @return a new lattice element that represents the meet of the two lattice
     * elements
     */
    template <typename Meet>
    requires std::is_invocable_v<Meet, const T&, const T&>
    static LatticeElem meet(const LatticeElem& A, const LatticeElem& B,
                            Meet&& M)
    {
        if (A.isBottom() || B.isBottom()) {
            return makeBottom();
        }
        if (A.isTop()) {
            return B;
        } else if (B.isTop()) {
            return A;
        }
        return LatticeElem(M(A.value(), B.value()));
    }

    /**
     * @brief Joins two individual lattice element (least upper bound).
     * If either is top, returns top. If either is bottom, returns the other.
     * Otherwise, returns the function applied to the two facts.
     *
     * @tparam Meet callable object to perform the meet of two inhabited facts
     * @param A The first fact
     * @param B The second fact
     * @param M The meet function, which must be callable with two facts and
     * return a new fact
     * @return a new lattice element that represents the meet of the two lattice
     * elements
     */
    template <typename Join>
    requires std::is_invocable_v<Join, const T&, const T&>
    static LatticeElem join(const LatticeElem& A, const LatticeElem& B,
                            Join&& M)
    {
        if (A.isTop() || B.isTop()) {
            return makeTop();
        }
        if (A.isBottom()) {
            return B;
        } else if (B.isBottom()) {
            return A;
        }
        return LatticeElem(M(A.value(), B.value()));
    }

    /**
     * @brief Constructs a new lattice element by applying a function to the
     * existing fact, if it is inhabited (not top or bottom)
     *
     * @tparam F The function type which must accept a const reference to the
     * underlying fact and return a new fact
     * @return requires
     */
    template <typename F>
    requires std::is_invocable_v<F, const T&>
    LatticeElem apply(F&& Func) const
    {
        if (isBottom() || isTop()) {
            return *this;
        }
        return LatticeElem(Func(Fact_.value()));
    }
};

/**
 * @brief Gets the transfer result for the edge from the node producing
 * `OutFact` to `CurBlock`
 *
 * @tparam F
 * @param CurBlock the current block
 * @param OutFact the output fact of the previous block
 * @return F&
 */
template <Fact F>
decltype(auto) getEdgeFact(const llvm::BasicBlock* CurBlock,
                           const TransferRetType<F>& OutFact)
{
    if (std::holds_alternative<F>(OutFact)) {
        return std::get<F>(OutFact);
    } else {
        const auto& OutMap = std::get<0>(OutFact);
        return OutMap.at(CurBlock);
    }
}

/**
 * @brief Gets the data flow facts for the true branch and the
 * false branch, respectively, of a conditional branch. If `Cond` is true, the
 * true facts may be assumed, and if `Cond` is false, the false facts may be
 * assumed.
 *
 * @param Self the data flow fact for the current instruction
 * @param Cond the condition
 * @param Facts the set of all computed data flow facts
 * @param Fn a function which takes a `const F&`, `const llvm::Value*` and
 * `const DataFlowFacts<F>` and returns an optional to a tuple of (true facts,
 * false facts). If the condition depends on a phi node where one incoming value
 * is a constant, this function will recurse on the respective condition we know
 * passes/fails depending on the outcome of the branch.
 * @return Tuple of (true facts, false facts)
 */
template <Fact F, typename Func>
std::tuple<F, F> getCondFacts(const F& Self, const llvm::Value* Cond,
                              const DataFlowFacts<F>& Facts, Func&& Fn)
{
    // NOLINTNEXTLINE
    using namespace llvm;
    if (const auto Res = Fn(Self, Cond, Facts); Res.has_value()) {
        return Res.value();
    } else if (const auto Phi = dyn_cast<llvm::PHINode>(Cond); Phi != nullptr) {
        if (Phi->getNumIncomingValues() != 2 ||
            !Phi->getType()->isIntegerTy(1)) {
            return std::make_tuple(Self, Self);
        }
        const llvm::Value* NonConstUse = nullptr;
        const BasicBlock* NonConstBlock = nullptr;
        std::optional<bool> Const;
        for (auto Idx = 0u; Idx < Phi->getNumIncomingValues(); ++Idx) {
            const auto V = Phi->getIncomingValue(Idx);
            const auto B = Phi->getIncomingBlock(Idx);
            if (const auto Int = dyn_cast<ConstantInt>(V); Int != nullptr) {
                Const = Int->getValue().getBoolValue();
            } else {
                NonConstUse = V;
                NonConstBlock = B;
            }
        }
        if (Const.has_value() && NonConstUse != nullptr) {
            // phi node of [false, ] and [other,]
            // so if the true branch is taken, we can assume the `other` value
            // is true
            // OR
            // phi node of [true, ] and [other,]
            // so if the false branch is taken, we can assume the `other` value
            // is false
            const auto [TrueRes, FalseRes] =
                getCondFacts(getEdgeFact(Phi->getParent(),
                                         Facts.BlockOutFacts.at(NonConstBlock)),
                             NonConstUse, Facts, std::forward<Func>(Fn));
            return Const.value() ? std::make_tuple(Self, FalseRes)
                                 : std::make_tuple(TrueRes, Self);
        }
    }
    return std::make_tuple(Self, Self);
}

/**
 * @brief Transfers a branch instruction by populating the outgoing facts for
 * the true and false branches independently, depending on the condition.
 *
 * @tparam F
 * @tparam Func
 * @param Self the data flow fact for the current instruction
 * @param Branch the branch instruction
 * @param Facts the set of all computed data flow facts
 * @param Fn a function which takes a `const F&`, `const llvm::Value*` and
 * `const DataFlowFacts<F>` and returns an optional to a tuple of (true facts,
 * false facts). If the condition depends on a phi node where one incoming value
 * is a constant, this function will recurse on the respective condition we know
 * passes/fails depending on the outcome of the branch.
 * @return TransferRetType<F>
 */
template <Fact F, typename Func>
TransferRetType<F> transferConditionDependentBranch(
    const F& Self, const llvm::BranchInst* Branch,
    const DataFlowFacts<F>& Facts, Func&& Fn)
{
    if (!Branch->isConditional()) {
        return Self;
    }
    const auto Cond = Branch->getCondition();
    const auto [TrueRes, FalseRes] =
        getCondFacts(Self, Cond, Facts, std::forward<Func>(Fn));
    auto Ret = std::map<const llvm::BasicBlock*, F>{};
    Ret.emplace(Branch->getSuccessor(0), TrueRes);
    Ret.emplace(Branch->getSuccessor(1), FalseRes);
    assert(Branch->getNumSuccessors() == 2);
    return Ret;
}

/**
 * @brief Get the Debug Name of an instruction (such as %10)
 *
 * @param I
 * @return std::string
 */
inline std::string getDebugName(const llvm::Value* I)
{
    if (I->hasName()) {
        return I->getName().str();
    }
    std::string Res;
    llvm::raw_string_ostream Stream(Res);
    I->print(Stream, false);
    Res = Res.substr(Res.find_first_not_of(" "));
    if (auto EqIdx = Res.find_first_of("="); EqIdx != std::string::npos) {
        Res = Res.substr(0, EqIdx);
    }
    return Res;
}