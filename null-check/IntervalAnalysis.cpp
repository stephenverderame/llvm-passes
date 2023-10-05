#include "IntervalAnalysis.hpp"

#include <llvm-17/llvm/ADT/SmallVector.h>
#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/DerivedTypes.h>
#include <llvm-17/llvm/IR/InstrTypes.h>
#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/IR/Value.h>
#include <llvm-17/llvm/Support/Casting.h>
#include <llvm/ADT/SmallString.h>

#include <memory>
#include <unordered_map>

#include "df/DataFlow.hpp"
#include "external/bigint.h"

// NOLINTNEXTLINE
using namespace llvm;
using TransferRet = IntervalAnalysis::TransferRet;

namespace
{
bigint toUnsigned(const bigint& A, unsigned int BitWidth)
{
    if (A < bigint{0}) {
        return A + bigint::_big_pow(bigint(2),
                                    bigint(static_cast<int64_t>(BitWidth)));
    }
    return A;
}

bigint toSigned(const bigint& A, unsigned int BitWidth)
{
    if (A >= bigint::_big_pow(bigint(2),
                              bigint(static_cast<int64_t>(BitWidth) - 1))) {
        return A - bigint::_big_pow(bigint(2),
                                    bigint(static_cast<int64_t>(BitWidth)));
    }
    return A;
}

/**
 * @brief Returns the stricter (smaller distance between upper and lower bounds)
 * of the two ranges. If there is a tie, the first argument is returned.
 *
 * @param A
 * @param B
 * @return IntRange
 */
IntRange smallerRange(const IntRange& A, const IntRange& B)
{
    if (B.Upper - B.Lower < A.Upper - A.Lower) {
        return B;
    }
    return A;
}

/**
 * @brief Returns a new lattice element that can be assumed when
 * `LHS Cond RHS` is true. The returned lattice element will be with
 * respect to the `LHS` argument, that is if `RHS` does not have a value,
 * the returned lattice element will be `LHS`.
 *
 * @param LHS the lattice element for the left hand side of the comparison
 * @param RHS the lattice element for the right hand side of the comparison
 * @param BitWidth the bit width of the operands
 * @param Cond the condition of the comparison
 * @return LatticeElem<IntRange>
 */
// NOLINTNEXTLINE(readability-function-*)
LatticeElem<IntRange> adjustForCondition(const LatticeElem<IntRange>& LHS,
                                         const LatticeElem<IntRange>& RHS,
                                         uint64_t BitWidth,
                                         ICmpInst::Predicate Cond)
{
    switch (Cond) {
        case ICmpInst::ICMP_SLT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toSigned(BitWidth);
                Res.Upper = bigint::_big_min(
                    Res.Upper,
                    RHS.value().toSigned(BitWidth).Upper - bigint(1));
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_ULT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toUnsigned(BitWidth);
                Res.Upper = bigint::_big_min(
                    Res.Upper,
                    RHS.value().toUnsigned(BitWidth).Upper - bigint(1));
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_SLE: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toSigned(BitWidth);
                Res.Upper = bigint::_big_min(
                    Res.Upper, RHS.value().toSigned(BitWidth).Upper);
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_ULE: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toUnsigned(BitWidth);
                Res.Upper = bigint::_big_min(
                    Res.Upper, RHS.value().toUnsigned(BitWidth).Upper);
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_SGE: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = RHS.value().toSigned(BitWidth);
                Res.Lower = bigint::_big_max(
                    Res.Lower, LHS.value().toSigned(BitWidth).Lower);
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_UGE:
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = RHS.value().toUnsigned(BitWidth);
                Res.Lower = bigint::_big_max(
                    Res.Lower, LHS.value().toUnsigned(BitWidth).Lower);
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        case ICmpInst::ICMP_SGT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = RHS.value().toSigned(BitWidth);
                Res.Lower = bigint::_big_max(
                    Res.Lower,
                    LHS.value().toSigned(BitWidth).Lower + bigint(1));
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_UGT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = RHS.value().toUnsigned(BitWidth);
                Res.Lower = bigint::_big_max(
                    Res.Lower,
                    LHS.value().toUnsigned(BitWidth).Lower + bigint(1));
                return LatticeElem<IntRange>(Res);
            } else {
                return LHS;
            }
        }
        default:
            return LHS;
    }
}

}  // namespace

// roughly 80-bit signed min and max
const static bigint BigEnough = bigint("604462910000000000000000");
const static bigint SmallEnough = bigint("-604462910000000000000000");

IntRange IntRange::meet(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower == B.Lower ? A.Lower : SmallEnough;
    Res.Upper = A.Upper == B.Upper ? A.Upper : BigEnough;
    return Res;
}

IntRange IntRange::join(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    assert(A.Lower >= B.Lower && A.Upper <= B.Upper ||
           A.Lower <= B.Lower && A.Upper >= B.Upper);
    Res.Lower = bigint::_big_max(A.Lower, B.Lower);
    Res.Upper = bigint::_big_min(A.Upper, B.Upper);
    return Res;
}

IntRange operator*(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower * B.Lower;
    Res.Upper = A.Upper * B.Upper;
    return Res;
}

IntRange operator+(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower + B.Lower;
    Res.Upper = A.Upper + B.Upper;
    return Res;
}

IntRange operator-(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower - B.Upper;
    Res.Upper = A.Upper - B.Lower;
    return Res;
}

IntRange operator/(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower / B.Upper;
    Res.Upper = A.Upper / B.Lower;
    return Res;
}

IntRange IntRange::remainder(const IntRange& Other, bool Signed) const
{
    IntRange Res;
    auto AbsLower = bigint::_big_abs(Other.Lower);
    auto AbsUpper = bigint::_big_abs(Other.Upper);
    if (AbsUpper < AbsLower) {
        std::swap(AbsLower, AbsUpper);
    }
    if (!Signed) {
        Res.Lower = bigint(0);
        Res.Upper = AbsUpper;
    } else {
        Res.Lower = bigint(0) - AbsUpper;
        Res.Upper = AbsUpper;
    }
    return Res;
}

IntRange operator<<(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower * bigint::_big_pow(bigint(2), B.Lower);
    Res.Upper = A.Upper * bigint::_big_pow(bigint(2), B.Upper);
    return Res;
}

IntRange IntRange::toSigned(unsigned int BitWidth) const
{
    IntRange Res;
    const auto A = ::toSigned(Lower, BitWidth);
    const auto B = ::toSigned(Upper, BitWidth);
    Res.Lower = bigint::_big_min(A, B);
    Res.Upper = bigint::_big_max(A, B);
    return Res;
}

IntRange IntRange::toUnsigned(unsigned int BitWidth) const
{
    IntRange Res;
    const auto A = ::toUnsigned(Lower, BitWidth);
    const auto B = ::toUnsigned(Upper, BitWidth);
    Res.Lower = bigint::_big_min(A, B);
    Res.Upper = bigint::_big_max(A, B);
    return Res;
}

IntRange IntRange::pow(bigint&& Exponent) const
{
    IntRange Res;
    Res.Lower = bigint::_big_pow(Lower, Exponent);
    Res.Upper = bigint::_big_pow(Upper, Exponent);
    return Res;
}

IntRange IntRange::exponentiate(bigint&& Base) const
{
    IntRange Res;
    Res.Lower = bigint::_big_pow(Base, Lower);
    Res.Upper = bigint::_big_pow(Base, Upper);
    return Res;
}

IntervalAnalysis IntervalAnalysis::meet(const IntervalAnalysis& A,
                                        const IntervalAnalysis& B)
{
    std::unordered_map<const SingleFact*, std::shared_ptr<SingleFact>>
        ClonedRanges;
    auto Res = A;
    for (const auto& [Val, Range] : B.Ranges_) {
        if (Res.Ranges_.contains(Val)) {
            Res.Ranges_[Val] = std::make_shared<SingleFact>(
                SingleFact::meet(*Res.Ranges_[Val], *Range, IntRange::meet));
        } else if (ClonedRanges.contains(Range.get())) {
            Res.Ranges_[Val] = ClonedRanges.at(Range.get());
        } else {
            const auto Clone = std::make_shared<SingleFact>(*Range);
            Res.Ranges_[Val] = Clone;
        }
    }
    for (const auto& [Val, Name] : B.DebugNames_) {
        if (!Res.DebugNames_.contains(Val)) {
            Res.DebugNames_[Val] = Name;
        }
    }
    return Res;
}

bool IntervalAnalysis::operator==(const IntervalAnalysis& Other) const
{
    if (Ranges_.size() != Other.Ranges_.size()) {
        return false;
    }
    for (const auto& [Val, Range] : Ranges_) {
        if (!Other.Ranges_.contains(Val)) {
            return false;
        }
        if (*Range != *Other.Ranges_.at(Val)) {
            return false;
        }
    }
    return true;
}

IntervalAnalysis::IntervalAnalysis(const IntervalAnalysis& Other)
    : Ranges_(Other.Ranges_.size())
{
    std::unordered_map<const SingleFact*, std::shared_ptr<SingleFact>>
        ClonedRanges;
    for (const auto& [Val, Range] : Other.Ranges_) {
        if (const auto Clone = ClonedRanges.find(Range.get());
            Clone != ClonedRanges.end()) {
            Ranges_[Val] = Clone->second;
        } else {
            const auto NewRange = std::make_shared<SingleFact>(*Range);
            Ranges_[Val] = NewRange;
            ClonedRanges[Range.get()] = NewRange;
        }
    }
}

IntervalAnalysis& IntervalAnalysis::operator=(const IntervalAnalysis& Other)
{
    auto Tmp = Other;
    std::swap(Ranges_, Tmp.Ranges_);
    return *this;
}

TransferRet IntervalAnalysis::transferAlloca(
    const llvm::AllocaInst* Alloca) const
{
    auto Res = *this;
    Res.Ranges_[static_cast<const Value*>(Alloca)] =
        std::make_shared<SingleFact>();
    return Res;
}

/**
 * @brief Gets a range for a particular value.
 * If the value is not in the map, it is added with a bottom range.
 * If the value is a constant, it is added with a range equal to the constant.
 *
 * @param V
 * @return RangeAnalysis::SingleFact&
 */
std::shared_ptr<IntervalAnalysis::SingleFact> IntervalAnalysis::getRange(
    const Value* V)
{
    if (const auto Int = dyn_cast<ConstantInt>(V); Int != nullptr) {
        SmallString<256> IntAsStr;
        Int->getValue().toString(IntAsStr, 10, true);
        const auto NewRange = IntRange(bigint(IntAsStr.str().str()));

        if (Ranges_.contains(V)) {
            *Ranges_.at(V) = SingleFact(NewRange);
        } else {
            Ranges_.emplace(V, std::make_shared<SingleFact>(NewRange));
        }
        return Ranges_.at(V);
    } else if (Ranges_.contains(V)) {
        return Ranges_.at(V);
    } else {
        Ranges_.emplace(V,
                        std::make_shared<SingleFact>(SingleFact::makeBottom()));
        return Ranges_[V];
    }
}

// NOLINTNEXTLINE(readability-function-size)
TransferRet IntervalAnalysis::transferBinOp(const BinaryOperator* BinOp) const
{
    if (!BinOp->getType()->isIntegerTy()) {
        return *this;
    }
    auto Res = *this;
    const auto LHS = Res.getRange(BinOp->getOperand(0));
    const auto RHS = Res.getRange(BinOp->getOperand(1));
    const auto BitWidth = BinOp->getType()->getIntegerBitWidth();
    switch (BinOp->getOpcode()) {
        case Instruction::Add:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(SingleFact::meet(
                *LHS, *RHS,
                [](const auto& L, const auto& R) { return L + R; }));
            break;
        case Instruction::Mul:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(SingleFact::meet(
                *LHS, *RHS,
                [](const auto& L, const auto& R) { return L * R; }));
            break;
        case Instruction::Sub:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(SingleFact::meet(
                *LHS, *RHS,
                [](const auto& L, const auto& R) { return L - R; }));
            break;
        case Instruction::SDiv:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(
                SingleFact::meet(*LHS, *RHS, [](const auto& L, const auto& R) {
                    return L / R;
                }).apply([BitWidth](const auto& R) {
                    return R.toSigned(BitWidth);
                }));
            break;
        case Instruction::UDiv:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(
                SingleFact::meet(*LHS, *RHS, [](const auto& L, const auto& R) {
                    return L / R;
                }).apply([BitWidth](const auto& R) {
                    return R.toUnsigned(BitWidth);
                }));
            break;
        case Instruction::URem:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(
                SingleFact::meet(*LHS, *RHS, [](const auto& L, const auto& R) {
                    return L.unsignedRemainder(R);
                }));
            break;
        case Instruction::SRem:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(SingleFact::meet(
                *LHS, *RHS,
                [](const auto& L, const auto& R) { return L.remainder(R); }));
            break;
        case Instruction::Shl:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(SingleFact::meet(
                *LHS, *RHS,
                [](const auto& L, const auto& R) { return L << R; }));
            break;
        case Instruction::LShr:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(SingleFact::meet(
                *LHS, *RHS, [BitWidth](const auto& L, const auto& R) {
                    return L.toUnsigned(BitWidth) /
                           R.toUnsigned(BitWidth).exponentiate(bigint(2));
                }));
            break;
        case Instruction::AShr:
            Res.Ranges_[BinOp] = std::make_shared<SingleFact>(
                SingleFact::meet(*LHS, *RHS, [](const auto& L, const auto& R) {
                    return L / R.exponentiate(bigint(2));
                }));
        default:
            Res.Ranges_[BinOp] =
                std::make_shared<SingleFact>(SingleFact::makeBottom());
            break;
    }
    return Res;
}

TransferRet IntervalAnalysis::transferCast(const CastInst* Cast) const
{
    if (!Cast->getType()->isIntegerTy()) {
        return *this;
    }
    auto Res = *this;
    assert(Cast->getNumOperands() == 1);
    const auto Arg = Res.getRange(Cast->getOperand(0));
    Res.Ranges_[Cast] = std::make_shared<SingleFact>(*Arg);
    return Res;
}

TransferRet IntervalAnalysis::transferLoad(const LoadInst* Load) const
{
    auto Res = *this;
    const auto Ptr = Load->getPointerOperand();
    if (Res.Ranges_.contains(Ptr)) {
        Res.Ranges_[Load] = Res.Ranges_.at(Ptr);
    } else if (Load->getType()->isIntegerTy()) {
        Res.Ranges_[Load] =
            std::make_shared<SingleFact>(SingleFact::makeBottom());
    }
    return Res;
}

TransferRet IntervalAnalysis::transferStore(const StoreInst* Store) const
{
    auto Res = *this;
    const auto Ptr = Store->getPointerOperand();
    const auto Val = Store->getValueOperand();
    const auto ValRange = Res.getRange(Val);
    if (Res.Ranges_.contains(Ptr)) {
        *Res.Ranges_[Ptr] = *ValRange;
    } else {
        Res.Ranges_[Ptr] = ValRange;
    }
    return Res;
}

TransferRet IntervalAnalysis::transferPhi(const PHINode* Phi) const
{
    auto Res = *this;
    const auto NumIncoming = Phi->getNumIncomingValues();
    if (NumIncoming == 0) {
        return Res;
    }
    const auto PhiRange = Res.getRange(Phi->getIncomingValue(0));
    for (unsigned int Idx = 1; Idx < NumIncoming; ++Idx) {
        const auto IncomingRangeI = Res.getRange(Phi->getIncomingValue(Idx));
        *PhiRange =
            SingleFact::meet(*PhiRange, *IncomingRangeI, IntRange::meet);
    }
    Res.Ranges_[Phi] = PhiRange;
    return Res;
}
/**
 * @brief Determines the true and false facts that can be assumed depending on
 * the evaluation of `Cmp`.
 *
 * @param Cmp the comparison instruction
 * @return tuple of true and false facts
 */
// NOLINTNEXTLINE(readability-function-*)
std::tuple<IntervalAnalysis, IntervalAnalysis> IntervalAnalysis::transferCmp(
    const ICmpInst* Cmp) const
{
    auto TRes = *this;
    const auto LHS = Cmp->getOperand(0);
    const auto RHS = Cmp->getOperand(1);
    if (!LHS->getType()->isIntegerTy() || !RHS->getType()->isIntegerTy()) {
        return std::make_tuple(TRes, TRes);
    }
    TRes.DebugNames_[LHS] = getDebugName(LHS);
    TRes.DebugNames_[RHS] = getDebugName(RHS);
    const auto BitWidth = LHS->getType()->getIntegerBitWidth();
    const auto LHSRange = TRes.getRange(LHS);
    const auto RHSRange = TRes.getRange(RHS);
    auto FRes = TRes;
    switch (Cmp->getPredicate()) {
        case ICmpInst::ICMP_EQ:
            *TRes.Ranges_[LHS] =
                SingleFact::join(*RHSRange, *LHSRange, smallerRange);
            *TRes.Ranges_[RHS] =
                SingleFact::join(*LHSRange, *RHSRange, smallerRange);
            break;
        case ICmpInst::ICMP_NE:
            *FRes.Ranges_[LHS] =
                SingleFact::join(*RHSRange, *LHSRange, smallerRange);
            *FRes.Ranges_[RHS] =
                SingleFact::join(*LHSRange, *RHSRange, smallerRange);
            break;
        case ICmpInst::ICMP_SLT:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SLT);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SGE);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SGE);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SLT);
            break;
        case ICmpInst::ICMP_ULT:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_ULT);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_UGE);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_UGE);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_ULT);
            break;
        case ICmpInst::ICMP_SGT:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SGT);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SLE);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SLE);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SGT);
            break;
        case ICmpInst::ICMP_UGT:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_UGT);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_ULE);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_ULE);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_UGT);
            break;
        case ICmpInst::ICMP_SLE:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SLE);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SGT);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SGT);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SLE);
            break;
        case ICmpInst::ICMP_ULE:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_ULE);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_UGT);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_UGT);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_ULE);
            break;
        case ICmpInst::ICMP_SGE:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SGE);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SLT);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_SLT);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_SGE);
            break;
        case ICmpInst::ICMP_UGE:
            *TRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_UGE);
            *TRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_ULT);
            *FRes.Ranges_[LHS] = adjustForCondition(
                *LHSRange, *RHSRange, BitWidth, ICmpInst::ICMP_ULT);
            *FRes.Ranges_[RHS] = adjustForCondition(
                *RHSRange, *LHSRange, BitWidth, ICmpInst::ICMP_UGE);
            break;
        default:
            break;
    }
    return {TRes, FRes};
}

TransferRet IntervalAnalysis::transferBranch(
    const BranchInst* Branch,
    const DataFlowFacts<IntervalAnalysis>& Facts) const
{
    return transferConditionDependentBranch(
        *this, Branch, Facts,
        [](const auto& Self, const auto* Cond, const auto& /* Facts */)
            -> std::optional<std::tuple<IntervalAnalysis, IntervalAnalysis>> {
            if (const auto Cmp = dyn_cast<ICmpInst>(Cond); Cmp != nullptr) {
                return Self.transferCmp(Cmp);
            } else {
                return {};
            }
        });
}

TransferRet IntervalAnalysis::transfer(
    const llvm::Instruction& I,
    const DataFlowFacts<IntervalAnalysis>& Facts) const
{
    DebugNames_[&I] = getDebugName(&I);
    if (const auto* Alloca = dyn_cast<AllocaInst>(&I); Alloca != nullptr) {
        return transferAlloca(Alloca);
    } else if (const auto* BinOp = dyn_cast<BinaryOperator>(&I);
               BinOp != nullptr) {
        return transferBinOp(BinOp);
    } else if (const auto Cast = dyn_cast<CastInst>(&I); Cast != nullptr) {
        return transferCast(Cast);
    } else if (const auto Load = dyn_cast<LoadInst>(&I); Load != nullptr) {
        return transferLoad(Load);
    } else if (const auto Store = dyn_cast<StoreInst>(&I); Store != nullptr) {
        return transferStore(Store);
    } else if (const auto Phi = dyn_cast<PHINode>(&I); Phi != nullptr) {
        return transferPhi(Phi);
    } else if (const auto Branch = dyn_cast<BranchInst>(&I);
               Branch != nullptr) {
        return transferBranch(Branch, Facts);
    }
    return *this;
}

std::optional<IntRange> IntervalAnalysis::getValRange(const Value* V) const
{
    if (Ranges_.contains(V) && Ranges_.at(V)->hasValue()) {
        return Ranges_.at(V)->value();
    }
    return {};
}