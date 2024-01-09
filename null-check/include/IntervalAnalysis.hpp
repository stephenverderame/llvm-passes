#pragma once
#include <llvm-17/llvm/Analysis/LazyValueInfo.h>
#include <llvm-17/llvm/IR/BasicBlock.h>
#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/Support/raw_ostream.h>
#include <z3++.h>

#include <stack>
#include <unordered_map>

#include "IntRange.hpp"
#include "df/DataFlow.hpp"

namespace llvm
{
class AllocaInst;
class BinaryOperator;
class UnaryOperator;
class PHINode;
}  // namespace llvm

/**
 * @brief Like conditional constant propagation, but with ranges.
 * TOP: set of all values having unknown ranges (empty set)
 * BOTTOM: set of all values having infinite ranges
 *
 */
class IntervalAnalysis
{
  public:
    using TransferRet = TransferRetType<IntervalAnalysis>;
    using SingleFact = LatticeElem<IntRange>;

  private:
    /**
     * @brief Mapping from syntactic values to their ranges.
     */
    std::unordered_map<const llvm::Value*, SingleFact> Ranges_;
    /**
     * @brief Mapping from values to their latest Z3 variables.
     */
    std::unordered_map<const llvm::Value*, z3::expr> Z3Vars_;
    /**
     * @brief Mapping from canonical values to their version numbers.
     */
    mutable std::unordered_map<const llvm::Value*, uint64_t> VersionNumbers_;
    /** List of constraints on z3 variables */
    std::vector<std::shared_ptr<z3::expr>> Constraints_;

    /** @brief Z3 context */
    std::shared_ptr<z3::context> Z3Context_;

    /** @brief String representation of values for debugging */
    mutable std::unordered_map<const llvm::Value*, std::string> DebugNames_;

  private:
    // private function docs in source

    TransferRet transferAlloca(const llvm::AllocaInst* Alloca) const;
    TransferRet transferBinOp(const llvm::BinaryOperator* BinOp) const;
    TransferRet transferCast(const llvm::CastInst* Cast) const;
    TransferRet transferBranch(const llvm::BranchInst* Branch,
                               const DataFlowFacts<IntervalAnalysis>&) const;
    TransferRet transferPhi(const llvm::PHINode* Phi) const;
    TransferRet transferStore(const llvm::StoreInst* Store) const;
    TransferRet transferCall(const llvm::CallInst* Call) const;
    std::tuple<IntervalAnalysis, IntervalAnalysis> transferCmp(
        const llvm::ICmpInst* Cmp) const;

    SingleFact& getRange(const llvm::Value* V);
    SingleFact getRangeConst(const llvm::Value* V) const;
    void putRange(const llvm::Value* V, const SingleFact& Fact);
    bool contains(const llvm::Value* V) const;

    friend llvm::raw_ostream& operator<<(llvm::raw_ostream& Stream,
                                         const IntervalAnalysis& Analysis);

    /**
     * @brief  Adds a constraint to the analysis.
     * @tparam F callable object which returns a z3::expr and takes a
     * z3::expr as its first argument, and a variadic number of z3::exprs as
     * its remaining arguments. The first argument will be the z3 variable for
     * the value that the constraint is being added for, and the remaining
     * arguments will be the z3 variables for the values that the constraint
     * depends on.
     */
    template <typename F, typename... Vals>
    void addConstraint(const llvm::Value* V, F&& C, const Vals*... Vs)
    {
        const auto Z3Var = getZ3Var(V);
        Constraints_.emplace_back(
            std::make_shared<z3::expr>(C(Z3Var, getZ3Var(Vs)...)));
    }

    z3::expr getZ3Var(const llvm::Value* V);
    void newZ3Var(const llvm::Value* V);

  public:
    /// @see Fact::meet
    static IntervalAnalysis meet(const IntervalAnalysis& A,
                                 const IntervalAnalysis& B);

    /// @see Fact::transfer
    TransferRetType<IntervalAnalysis> transfer(
        const llvm::Instruction& I,
        const DataFlowFacts<IntervalAnalysis>& Facts) const;

    using Dir = Forwards;

    bool operator==(const IntervalAnalysis& Other) const;

    IntervalAnalysis& operator=(const IntervalAnalysis& Other);
    IntervalAnalysis(const IntervalAnalysis& Other);
    /**
     * @brief Construct a new Top Interval Analysis fact.
     *
     * Function arguments are initialized to have infinite ranges.
     *
     * @param F The function to analyze
     */
    explicit IntervalAnalysis(const llvm::Function& F);
    IntervalAnalysis(IntervalAnalysis&&) = default;
    IntervalAnalysis& operator=(IntervalAnalysis&&) = default;
    ~IntervalAnalysis() = default;

    /**
     * @brief Gets a conservative bound for the values that a given value can
     * take during runtime.
     *
     * @param V The value to get the range for.
     * @return an integer range if the value is known to be bounded, or an empty
     * optional if the value is unbounded/unknown.
     */
    std::optional<IntRange> getValRange(const llvm::Value* V) const;

    /**
     * @brief Get the Starting fact for this analysis.
     * The starting fact is the incoming fact for the entry block and IS NOT
     * the top of the lattice.
     *
     * @param F The function to analyze
     * @return IntervalAnalysis
     */
    [[nodiscard]] static IntervalAnalysis getStartFact(const llvm::Function& F);
};

/**
 * @brief Outputs a set representing all known value, range mappings.
 * Each value will be displayed as its name in the LLVM source (ie. '%10').
 *
 * Bottom is represented as '_'. And values that are TOP are not displayed.
 *
 * @param Stream The stream to output to
 * @param Analysis The analysis to output
 * @return llvm::raw_ostream& The modified stream
 */
llvm::raw_ostream& operator<<(llvm::raw_ostream& Stream,
                              const IntervalAnalysis& Analysis);

static_assert(Fact<IntervalAnalysis>);