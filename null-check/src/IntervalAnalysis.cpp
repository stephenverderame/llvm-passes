#include "IntervalAnalysis.hpp"

#include <llvm-17/llvm/ADT/SmallVector.h>
#include <llvm-17/llvm/Analysis/CGSCCPassManager.h>
#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/DerivedTypes.h>
#include <llvm-17/llvm/IR/InstrTypes.h>
#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/IR/Value.h>
#include <llvm-17/llvm/Support/Casting.h>
#include <llvm-17/llvm/Support/raw_ostream.h>
#include <llvm/ADT/SmallString.h>

#include <memory>
#include <unordered_map>

#include "df/DataFlow.hpp"
#include "df/LatticeElem.hpp"
#include "external/bigint.h"

// NOLINTNEXTLINE
using namespace llvm;
using TransferRet = IntervalAnalysis::TransferRet;

/**
 * @brief Returns true if A is at least as bounded as B.
 *
 * @param A
 * @param B
 * @return true
 * @return false
 */
bool isMoreBounded(const LatticeElem<IntRange>& A,
                   const LatticeElem<IntRange>& B)
{
    // TODO: handle RHS of conditions
    static bool Test = true;
    auto T = Test;
    Test = !Test;
    return T;
    // if (B.isBottom()) {
    //     return true;
    // }
    // if (A.isBottom()) {
    //     // B is not bottom here
    //     return false;
    // }
    // if (A.hasValue() && B.hasValue()) {
    //     return A.value().size() <= B.value().size();
    // }
    // // A and B are top
    // return true;
}

const llvm::Value* getCanonicalValue(const llvm::Value* V)
{
    if (const auto Cast = dyn_cast<CastInst>(V); Cast != nullptr) {
        return getCanonicalValue(Cast->getOperand(0));
    } else if (const auto Load = dyn_cast<LoadInst>(V); Load != nullptr) {
        return getCanonicalValue(Load->getPointerOperand());
    } else {
        return V;
    }
    // return V;
}

/**
 * @brief Gets the version of the range of `V` that is valid at the beginning of
 * `BB`. Pops the scope stack until the top of the stack dominates `BB`. Returns
 * this new top.
 *
 * @param SF the fact to get the scope of
 * @param BB the basic block to get the scope for
 * @return std::shared_ptr<IntervalAnalysis::SingleFact>
 */
IntervalAnalysis::SingleFact IntervalAnalysis::popScopeStack(
    const Value* V, const llvm::BasicBlock* BB)
{
    if (!Scopes_.contains(V)) {
        return getRange(V);
    }
    auto& Scopes = Scopes_.at(V);
    auto LastPtr = std::cref(getRange(V));
    while (!Scopes.empty()) {
        const auto& [Inst, Fact] = Scopes.front();
        if (DT_.get().dominates(Inst, BB)) {
            return LastPtr.get();
        }
        LastPtr = std::cref(Fact);
        Scopes.pop_front();
    }
    return LastPtr.get();
}

void IntervalAnalysis::putScope(const Value* V, const Instruction* I,
                                const SingleFact& Fact)
{
    V = getCanonicalValue(V);
    if (!Scopes_.contains(V)) {
        Scopes_.emplace(V, ScopeStack());
    }
    Scopes_.at(V).emplace_back(I, Fact);
}

/**
 * @brief Gets the version of the range of `V` that is valid at the beginning of
 * `BB`.
 *
 * @param V
 * @param BB
 * @return IntervalAnalysis::SingleFact
 */
IntervalAnalysis::SingleFact IntervalAnalysis::getTopScope(
    const llvm::Value* V, const llvm::BasicBlock* BB) const
{
    if (!Scopes_.contains(V)) {
        return getRangeConst(V);
    }
    auto& Scopes = Scopes_.at(V);
    auto LastPtr = getRangeConst(V);
    for (const auto& [Inst, Fact] : Scopes) {
        if (DT_.get().dominates(Inst, BB)) {
            return LastPtr;
        }
        LastPtr = Fact;
    }
    return LastPtr;
}

bool IntervalAnalysis::contains(const llvm::Value* V) const
{
    return Ranges_.contains(getCanonicalValue(V));
}

IntervalAnalysis IntervalAnalysis::meet(const IntervalAnalysis& A,
                                        const IntervalAnalysis& B,
                                        const BasicBlock* BB)
{
    auto Res = A;
    for (const auto& [Val, Range] : B.Ranges_) {
        if (Res.contains(Val)) {
            Res.putRange(
                Val, SingleFact::meet(Res.getRangeConst(Val),
                                      B.getRangeConst(Val), IntRange::meet));
        } else {
            Res.putRange(Val, Range);
        }
    }
    for (const auto& [Val, Scope] : B.Scopes_) {
        if (Res.Scopes_.contains(Val)) {
            // assert(Res.Scopes_.at(Val) == Scope);
        } else {
            Res.Scopes_.emplace(Val, Scope);
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
    // for (const auto& [Val, Range] : Ranges_) {
    //     if (Other.getRangeConst(Val) != getRangeConst(Val)) {
    //         return false;
    //     }
    // }

    // for (const auto& [Val, Range] : Other.Ranges_) {
    //     if (Other.getRangeConst(Val) != getRangeConst(Val)) {
    //         return false;
    //     }
    // }
    return Ranges_ == Other.Ranges_;
}

IntervalAnalysis::IntervalAnalysis(const IntervalAnalysis& Other) = default;
//     : Ranges_(Other.Ranges_.size()),
//       DT_(Other.DT_),
//       Scopes_(Other.Scopes_),
//       CanonicalValues_(Other.CanonicalValues_)

// {
// }

IntervalAnalysis& IntervalAnalysis::operator=(const IntervalAnalysis& Other) =
    default;
// {
//     auto Tmp = Other;
//     std::swap(Ranges_, Tmp.Ranges_);
//     std::swap(Scopes_, Tmp.Scopes_);
//     std::swap(CanonicalValues_, Tmp.CanonicalValues_);
//     return *this;
// }

TransferRet IntervalAnalysis::transferAlloca(
    const llvm::AllocaInst* Alloca) const
{
    auto Res = *this;
    if (!Alloca->getAllocatedType()->isIntegerTy()) {
        return Res;
    }
    Res.putRange(Alloca, SingleFact::makeBottom());
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
IntervalAnalysis::SingleFact& IntervalAnalysis::getRange(const Value* V)
{
    if (const auto Int = dyn_cast<ConstantInt>(V); Int != nullptr) {
        SmallString<256> IntAsStr;
        Int->getValue().toString(IntAsStr, 10, true);
        const auto NewRange = IntRange(bigint(IntAsStr.str().str()));

        if (Ranges_.contains(V)) {
            Ranges_.at(V) = SingleFact(NewRange);
        } else {
            Ranges_.emplace(V, NewRange);
        }
        return Ranges_.at(V);
    } else if (contains(V)) {
        return Ranges_.at(getCanonicalValue(V));
    } else {
        Ranges_.emplace(V, SingleFact::makeTop());
        return Ranges_[V];
    }
}

IntervalAnalysis::SingleFact IntervalAnalysis::getRangeConst(
    const llvm::Value* V) const
{
    if (const auto Int = dyn_cast<ConstantInt>(V); Int != nullptr) {
        SmallString<256> IntAsStr;
        Int->getValue().toString(IntAsStr, 10, true);
        const auto NewRange = IntRange(bigint(IntAsStr.str().str()));
        return SingleFact(NewRange);
    } else if (contains(V)) {
        return Ranges_.at(getCanonicalValue(V));
    } else {
        return SingleFact::makeTop();
    }
}

void IntervalAnalysis::putRange(const Value* V, const SingleFact& Fact)
{
    Ranges_[getCanonicalValue(V)] = Fact;
    // Ranges_[V] = Fact;
    // CanonicalValues_[V] = getCanonicalValue(V);
}

// NOLINTNEXTLINE(readability-function-size)
TransferRet IntervalAnalysis::transferBinOp(const BinaryOperator* BinOp) const
{
    if (!BinOp->getType()->isIntegerTy()) {
        return *this;
    }
    auto Res = *this;
    const auto& LHS = Res.getRange(BinOp->getOperand(0));
    const auto& RHS = Res.getRange(BinOp->getOperand(1));
    const auto BitWidth = BinOp->getType()->getIntegerBitWidth();
    switch (BinOp->getOpcode()) {
        case Instruction::Add:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L + R;
                                    }));
            break;
        case Instruction::Mul:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L * R;
                                    }));
            break;
        case Instruction::Sub:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L - R;
                                    }));
            break;
        case Instruction::SDiv:
            Res.putRange(
                BinOp,
                SingleFact::meet(LHS, RHS, [](const auto& L, const auto& R) {
                    return L / R;
                }).apply([BitWidth](const auto& R) {
                    return R.toSigned(BitWidth);
                }));
            break;
        case Instruction::UDiv:
            Res.putRange(
                BinOp,
                SingleFact::meet(LHS, RHS, [](const auto& L, const auto& R) {
                    return L / R;
                }).apply([BitWidth](const auto& R) {
                    return R.toUnsigned(BitWidth);
                }));
            break;
        case Instruction::URem:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L.unsignedRemainder(R);
                                    }));
            break;
        case Instruction::SRem:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L.remainder(R);
                                    }));
            break;
        case Instruction::Shl:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L << R;
                                    }));
            break;
        case Instruction::LShr:
            Res.putRange(
                BinOp,
                SingleFact::meet(
                    LHS, RHS, [BitWidth](const auto& L, const auto& R) {
                        return L.toUnsigned(BitWidth) /
                               R.toUnsigned(BitWidth).exponentiate(bigint(2));
                    }));
            break;
        case Instruction::AShr:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L / R.exponentiate(bigint(2));
                                    }));
        default:
            Res.putRange(BinOp, SingleFact::makeBottom());
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
    return Res;
}

TransferRet IntervalAnalysis::transferLoad(const LoadInst* Load) const
{
    auto Res = *this;
    const auto Ptr = Load->getPointerOperand();
    // if (Res.contains(Ptr)) {
    //     Res.putRange(Load, Res.getRange(Ptr));
    // } else if (Load->getType()->isIntegerTy()) {
    //     Res.putRange(Load, SingleFact::makeTop());
    // }
    return Res;
}

/**
 * @brief Updates the value of `OldVal` with `NewVal` for a store. If the new
 * value is monotonically greater than the old value, and the old value was
 * monotonically increasing or top, the new value is monotonically increasing.
 * Likewise for decreasing.
 *
 * @param OldVal the old value
 * @param NewVal the new value
 */
void overwriteAndCheckMonotonicity(LatticeElem<IntRange>& OldVal,
                                   const LatticeElem<IntRange>& NewVal)
{
    auto New = NewVal;
    if (OldVal.hasValue() && NewVal.hasValue()) {
        const auto& Range = OldVal.value();
        const auto& NewRange = NewVal.value();
        // we are overwriting a previous value
        New.value().Mutated = true;
        if (Range.Monotonicity.isTop() && NewRange.Monotonicity.isTop() &&
            NewRange == Range) {
            OldVal = NewVal;
            return;
        } else if ((Range.Monotonicity.isTop() ||
                    Range.Monotonicity.hasValue() &&
                        Range.Monotonicity.value() == Monotonic::Increasing) &&
                   NewRange.Lower >= Range.Lower &&
                   NewRange.Upper >= Range.Upper) {
            New.value().Monotonicity =
                LatticeElem<Monotonic>(Monotonic::Increasing);
        } else if ((Range.Monotonicity.isTop() ||
                    Range.Monotonicity.hasValue() &&
                        Range.Monotonicity.value() == Monotonic::Decreasing) &&
                   NewRange.Lower <= Range.Lower &&
                   NewRange.Upper <= Range.Upper) {
            New.value().Monotonicity =
                LatticeElem<Monotonic>(Monotonic::Decreasing);
        }
    }
    OldVal = New;
}

TransferRet IntervalAnalysis::transferStore(const StoreInst* Store) const
{
    const auto Ptr = Store->getPointerOperand();
    const auto Val = Store->getValueOperand();
    if (!Val->getType()->isIntegerTy()) {
        return *this;
    }
    auto Res = *this;
    const auto& ValRange = Res.getRange(Val);
    if (Res.contains(Ptr)) {
        overwriteAndCheckMonotonicity(Res.getRange(Ptr), ValRange);
    } else {
        Res.putRange(Ptr, ValRange);
    }
    return Res;
}

TransferRet IntervalAnalysis::transferPhi(const PHINode* Phi) const
{
    if (!Phi->getType()->isIntegerTy()) {
        return *this;
    }
    auto Res = *this;
    const auto NumIncoming = Phi->getNumIncomingValues();
    if (NumIncoming == 0) {
        return Res;
    }
    const auto SetIsMutating = [Phi](auto& Range, auto Idx) {
        if (Range.hasValue()) {
            const auto Incoming = Phi->getIncomingValue(Idx);
            for (const auto& Use : Incoming->uses()) {
                if (Use == Phi) {
                    // incoming node depends on this phi node -> loop
                    // and mutation
                    Range.value().Mutated = true;
                    break;
                }
            }
        }
    };
    auto& PhiRange = Res.getRange(Phi->getIncomingValue(0));
    SetIsMutating(PhiRange, 0);
    for (unsigned int Idx = 1; Idx < NumIncoming; ++Idx) {
        auto& IncomingRangeI = Res.getRange(Phi->getIncomingValue(Idx));
        SetIsMutating(IncomingRangeI, Idx);
        PhiRange = SingleFact::meet(PhiRange, IncomingRangeI, IntRange::meet);
    }
    Res.putRange(Phi, PhiRange);
    return Res;
}

const Value* getOriginal(const Value* V)
{
    if (const auto Cast = dyn_cast<CastInst>(V); Cast != nullptr) {
        return getOriginal(Cast->getOperand(0));
    } else if (const auto Load = dyn_cast<LoadInst>(V); Load != nullptr) {
        return Load->getPointerOperand();
    } else {
        return V;
    }
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
    const auto OldLHS = LHSRange;
    const auto OldRHS = RHSRange;
    const auto CanonicalLHS = getCanonicalValue(LHS);
    const auto CanonicalRHS = getCanonicalValue(RHS);
    auto FRes = TRes;
    switch (Cmp->getPredicate()) {
        case ICmpInst::ICMP_EQ:
            TRes.putRange(LHS,
                          SingleFact::join(RHSRange, LHSRange, smallerRange));
            TRes.putScope(LHS, Cmp, OldLHS);
            TRes.putRange(RHS,
                          SingleFact::join(LHSRange, RHSRange, smallerRange));
            TRes.putScope(RHS, Cmp, OldRHS);
            break;
        case ICmpInst::ICMP_NE:
            FRes.putRange(LHS,
                          SingleFact::join(RHSRange, LHSRange, smallerRange));
            FRes.putScope(LHS, Cmp, OldRHS);
            FRes.putRange(LHS,
                          SingleFact::join(LHSRange, RHSRange, smallerRange));
            FRes.putScope(RHS, Cmp, OldRHS);
            break;
        case ICmpInst::ICMP_SLT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
            break;
        case ICmpInst::ICMP_ULT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
            break;
        case ICmpInst::ICMP_SGT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
            break;
        case ICmpInst::ICMP_UGT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
            break;
        case ICmpInst::ICMP_SLE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
            break;
        case ICmpInst::ICMP_ULE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
            break;
        case ICmpInst::ICMP_SGE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
            break;
        case ICmpInst::ICMP_UGE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
                TRes.putScope(LHS, Cmp, OldLHS);
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
                FRes.putScope(LHS, Cmp, OldLHS);
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
                TRes.putScope(RHS, Cmp, OldRHS);
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
                FRes.putScope(RHS, Cmp, OldRHS);
            }
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
    if (contains(V)) {
        return getRangeConst(V).intoOptional();
    }
    return {};
}

llvm::raw_ostream& operator<<(llvm::raw_ostream& Stream,
                              const IntervalAnalysis& Analysis)
{
    Stream << "{";
    for (const auto& [Val, Range] : Analysis.Ranges_) {
        if (Range.isTop() || dyn_cast<ConstantInt>(Val) != nullptr) {
            continue;
        }
        if (Analysis.DebugNames_.contains(Val)) {
            Stream << Analysis.DebugNames_.at(Val) << ": ";
        } else {
            std::string Name;
            raw_string_ostream NameStream(Name);
            Val->printAsOperand(NameStream, false);
            Stream << Name << ": ";
        }
        if (Range.hasValue()) {
            Stream << Range.value();
        } else if (Range.isBottom()) {
            Stream << "_";
        }
        Stream << ", ";
    }
    Stream << "}";
    return Stream;
}

IntervalAnalysis::IntervalAnalysis(const llvm::Function& F,
                                   const llvm::DominatorTree& DT)
    : DT_(DT)
{
    for (const auto& Arg : F.args()) {
        if (Arg.getType()->isIntegerTy()) {
            putRange(&Arg, SingleFact::makeBottom());
            DebugNames_[&Arg] = getDebugName(&Arg);
        }
    }
}