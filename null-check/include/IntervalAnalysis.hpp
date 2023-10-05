#pragma once
#include <llvm-17/llvm/IR/Instructions.h>

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
     * If the range is unbounded, this is represented as an empty optional.
     */
    std::unordered_map<const llvm::Value*, std::shared_ptr<SingleFact>> Ranges_;

    mutable std::unordered_map<const llvm::Value*, std::string> DebugNames_;

    TransferRet transferAlloca(const llvm::AllocaInst* Alloca) const;
    TransferRet transferBinOp(const llvm::BinaryOperator* BinOp) const;
    TransferRet transferCast(const llvm::CastInst* Cast) const;
    TransferRet transferBranch(const llvm::BranchInst* Branch,
                               const DataFlowFacts<IntervalAnalysis>&) const;
    TransferRet transferPhi(const llvm::PHINode* Phi) const;
    TransferRet transferLoad(const llvm::LoadInst* Load) const;
    TransferRet transferStore(const llvm::StoreInst* Store) const;
    std::tuple<IntervalAnalysis, IntervalAnalysis> transferCmp(
        const llvm::ICmpInst* Cmp) const;

    std::shared_ptr<SingleFact> getRange(const llvm::Value* V);

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
    IntervalAnalysis() = default;
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
};

static_assert(Fact<IntervalAnalysis>);