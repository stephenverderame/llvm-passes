#pragma once
#include <llvm-17/llvm/IR/Instructions.h>

#include <unordered_map>

#include "df/DataFlow.hpp"
#include "external/bigint.h"

/**
 * @brief An integral range. We use bigints for arbitrary precision
 * since LLVM has arbitrary precision integers, and to prevent worrying
 * about overflow cases with things like signed and unsigned integers.
 *
 * Technically speaking, we can be less conservative by exploting
 * undefined behavior and attribute information, but this is simpler
 * and should be correct.
 *
 */
struct IntRange {
    bigint Lower;
    bigint Upper;

    /**
     * Computes the greatest lower bound of two ranges.
     * To be conservative for things like loops, if either of the bounds are
     * unequal, we set that bound to be inf or -inf, for the upper and lower
     * bounds respectively.
     */
    static IntRange meet(const IntRange& A, const IntRange& B);
    /** Computes the least upper bound of two ranges (range intersection) */
    static IntRange join(const IntRange& A, const IntRange& B);
    bool operator==(const IntRange& Other) const = default;

    /// @brief Create a range from a single integer.
    explicit IntRange(bigint&& I) : Lower(I), Upper(std::move(I)) {}
    IntRange() = default;
    IntRange(bigint&& Lower, bigint&& Upper)
        : Lower(std::move(Lower)), Upper(std::move(Upper))
    {
    }

    /// Converts this range to a signed range with the given bit width.
    IntRange toSigned(unsigned int BitWidth) const;
    /// Converts this range to an unsigned range with the given bit width.
    IntRange toUnsigned(unsigned int BitWidth) const;
    /// Computes `this ** A`
    IntRange pow(bigint&& Exponent) const;
    /// Computes `Base ** this`
    IntRange exponentiate(bigint&& Base) const;

    IntRange remainder(const IntRange& Other, bool Signed = true) const;
    IntRange unsignedRemainder(const IntRange& Other) const
    {
        return remainder(Other, false);
    }
};

IntRange operator*(const IntRange& A, const IntRange& B);
IntRange operator+(const IntRange& A, const IntRange& B);
IntRange operator-(const IntRange& A, const IntRange& B);
IntRange operator/(const IntRange& A, const IntRange& B);
IntRange operator<<(const IntRange& A, const IntRange& B);

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