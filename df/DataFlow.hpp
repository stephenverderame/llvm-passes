#pragma once
#include <bits/iterator_concepts.h>
#include <llvm-17/llvm/IR/BasicBlock.h>
#include <llvm-17/llvm/IR/CFG.h>
#include <llvm-17/llvm/IR/Instruction.h>

#include <concepts>
#include <deque>
#include <iterator>
#include <map>
#include <type_traits>

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

/// Dataflow analysis direction
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
 * @brief Dataflow analysis fact
 *
 * @tparam T
 */
template <typename T>
concept Fact = requires(const T& t) {
    /// Greatest lower bound of two facts
    {
        T::meet(t, t)
    } -> std::convertible_to<T>;

    /// Transfer function for an instruction
    {
        t.transfer(std::declval<const llvm::Instruction&>())
    } -> std::convertible_to<std::vector<T>>;

    requires std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>;

    {
        t == t
    } -> std::same_as<bool>;

    Direction<typename T::Dir>;
};

/**
 * @brief Forwards dataflow analysis direction
 *
 */
struct Forwards {
    static auto successors(const llvm::BasicBlock& BB)
    {
        return llvm::successors(&BB);
    }

    static auto begin(const llvm::BasicBlock& BB) { return BB.begin(); }

    static auto end(const llvm::BasicBlock& BB) { return BB.end(); }
};

static_assert(Direction<Forwards>);

/**
 * @brief Backwards dataflow analysis direction
 *
 */
struct Backwards {
    static auto successors(const llvm::BasicBlock& BB)
    {
        return llvm::predecessors(&BB);
    }

    static auto begin(const llvm::BasicBlock& BB) { return BB.rbegin(); }

    static auto end(const llvm::BasicBlock& BB) { return BB.rend(); }
};

static_assert(Direction<Backwards>);

/**
 * @brief Mapping from instructions to facts
 *
 * @tparam F
 */
template <Fact F>
struct DataFlowFacts {
    std::map<const llvm::Instruction*, F> InstructionInFacts;

    bool operator==(const DataFlowFacts& Other) const
    {
        return InstructionInFacts == Other.InstructionInFacts;
    }

    bool operator!=(const DataFlowFacts& Other) const
    {
        return !(*this == Other);
    }
};

/**
 * @brief Meets each output fact with the existing input fact for each
 * successor of the basic block.
 *
 * @tparam F
 * @param BB
 * @param Out
 * @param Facts
 */
template <Fact F>
DataFlowFacts<F>& broadcastOutFacts(const llvm::BasicBlock& BB,
                                    const std::vector<F>& Out,
                                    DataFlowFacts<F>& Facts)
{
    using Dir = typename F::Dir;
    if (Out.empty()) {
        return Facts;
    }
    if (Out.size() == 1) {
        for (const auto& Succ : Dir::successors(BB)) {
            Facts.InstructionInFacts[&*Dir::begin(*Succ)] = Out[0];
        }
    } else {
        auto It = Out.begin();
        for (const auto& Succ : Dir::successors(BB)) {
            const auto& SuccBB = *Succ;
            const auto& SuccFirstInst = *Dir::begin(SuccBB);
            Facts.InstructionInFacts[&SuccFirstInst] =
                F::meet(Facts.InstructionInFacts[&SuccFirstInst], *It);
            ++It;
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
            Facts.InstructionInFacts[&I] = Top;
        }
        Worklist.emplace_back(BB);
    }

    while (!Worklist.empty()) {
        const auto& BB = Worklist.front();
        Worklist.pop_front();

        auto LastOut =
            Dir::begin(BB) != Dir::end(BB)
                ? std::vector{Facts.InstructionInFacts[&*Dir::begin(BB)]}
                : std::vector{Top};
        for (auto I = Dir::begin(BB); I != Dir::end(BB); ++I) {
            const auto& Inst = *I;
            assert(LastOut.size() == 1);
            Facts.InstructionInFacts[&Inst] = LastOut[0];
            LastOut = LastOut[0].transfer(Inst);
            if (LastOut.empty()) {
                LastOut = {Top};
            }
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