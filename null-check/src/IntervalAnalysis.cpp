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
#include <sstream>
#include <unordered_map>
#include <unordered_set>

#include "df/DataFlow.hpp"
#include "df/LatticeElem.hpp"
#include "external/bigint.h"

// NOLINTNEXTLINE
using namespace llvm;
using TransferRet = IntervalAnalysis::TransferRet;

/**
 * @brief Get the value which is the canonical representative of `V`. The
 * canonical representative is the value that is used to represent `V` in the
 * mapping from values to ranges. For example, if `V` is a load instruction, the
 * canonical representative is the pointer operand of the load instruction.
 */
const llvm::Value* getCanonicalValue(const llvm::Value* V)
{
    if (const auto Cast = dyn_cast<CastInst>(V); Cast != nullptr) {
        return getCanonicalValue(Cast->getOperand(0));
    } else if (const auto Load = dyn_cast<LoadInst>(V); Load != nullptr) {
        return getCanonicalValue(Load->getPointerOperand());
    } else {
        return V;
    }
}

/**
 * @brief Determines if we have a range for `V` in the mapping from values to
 * ranges.
 *
 * @param V
 * @return true
 * @return false
 */
bool IntervalAnalysis::contains(const llvm::Value* V) const
{
    return Ranges_.contains(getCanonicalValue(V));
}

IntervalAnalysis IntervalAnalysis::meet(const IntervalAnalysis& A,
                                        const IntervalAnalysis& B)
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
    // for (const auto [Val, Ver] : B.VersionNumbers_) {
    //     if (Res.VersionNumbers_.contains(Val)) {
    //         Res.VersionNumbers_[Val] =
    //             std::max(Res.VersionNumbers_.at(Val), Ver);
    //     } else {
    //         Res.VersionNumbers_[Val] = Ver;
    //     }
    // }
    // if (A.Constraints_.empty()) {
    //     Res.Constraints_ = B.Constraints_;
    // } else if (!B.Constraints_.empty()) {
    //     // if b constraints are empty then we just keep the constraints from
    //     a auto i = 0; for (; i < std::min(A.Constraints_.size(),
    //     B.Constraints_.size());
    //          ++i) {
    //         // ptr equality to determine if constraints are the same
    //         if (A.Constraints_[i].get() != B.Constraints_[i].get()) {
    //             break;
    //         }
    //     }
    //     Res.Constraints_.resize(i + 1);
    // }
    // for (const auto& [Val, Z3Var] : B.Z3Vars_) {
    //     if (Res.Z3Vars_.contains(Val)) {
    //         Res.newZ3Var(Val);
    //     } else {
    //         Res.Z3Vars_.emplace(Val, Z3Var);
    //     }
    // }
    for (const auto& [Val, Name] : B.DebugNames_) {
        if (!Res.DebugNames_.contains(Val)) {
            Res.DebugNames_[Val] = Name;
        }
    }
    return Res;
}

bool IntervalAnalysis::operator==(const IntervalAnalysis& Other) const
{
    return Ranges_ == Other.Ranges_;
}

IntervalAnalysis::IntervalAnalysis(const IntervalAnalysis& Other) = default;

IntervalAnalysis& IntervalAnalysis::operator=(const IntervalAnalysis& Other) =
    default;

TransferRet IntervalAnalysis::transferAlloca(
    const llvm::AllocaInst* Alloca) const
{
    auto Res = *this;
    if (!Alloca->getAllocatedType()->isIntegerTy()) {
        return Res;
    }
    Res.putRange(Alloca, SingleFact::makeTop());
    return Res;
}

/**
 * @brief Gets a reference to a range for a particular value.
 * If the value is not in the map, it is added with a top range.
 * If the value is a constant, it is added with a range equal to the constant.
 *
 * @param V the value to get the range for
 * @return refereence to the range for `V`
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

/**
 * @brief Gets a range for `V`, without mutating the IntervalAnalysis.
 * If `V` is not in the map, returns a new Range that is either top, or in the
 * case `V` is a constant, a constant range.
 *
 * @param V the value to get the range for
 */
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

/**
 * @brief Inserts `Fact` into the mapping from values to ranges, for the
 * canonical value of `V`.
 *
 * @param V the value to insert the range for
 * @param Fact the range to insert
 */
void IntervalAnalysis::putRange(const Value* V, const SingleFact& Fact)
{
    Ranges_[getCanonicalValue(V)] = Fact;
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
    const auto DebugName = getDebugName(BinOp);
    switch (BinOp->getOpcode()) {
        case Instruction::Add:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L + R;
                                    }));
            Res.addConstraint(
                BinOp,
                [](const auto& Z3Var, const auto& LHS, const auto& RHS) {
                    return Z3Var == LHS + RHS;
                },
                BinOp->getOperand(0), BinOp->getOperand(1));
            break;
        case Instruction::Mul:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L * R;
                                    }));
            Res.addConstraint(
                BinOp,
                [](const auto& Z3Var, const auto& LHS, const auto& RHS) {
                    return Z3Var == LHS * RHS;
                },
                BinOp->getOperand(0), BinOp->getOperand(1));
            break;
        case Instruction::Sub:
            Res.putRange(BinOp, SingleFact::meet(
                                    LHS, RHS, [](const auto& L, const auto& R) {
                                        return L - R;
                                    }));
            Res.addConstraint(
                BinOp,
                [](const auto& Z3Var, const auto& LHS, const auto& RHS) {
                    return Z3Var == LHS - RHS;
                },
                BinOp->getOperand(0), BinOp->getOperand(1));
            break;
        case Instruction::SDiv:
            Res.putRange(
                BinOp,
                SingleFact::meet(LHS, RHS, [](const auto& L, const auto& R) {
                    return L / R;
                }).apply([BitWidth](const auto& R) {
                    return R.toSigned(BitWidth);
                }));
            Res.addConstraint(
                BinOp,
                [](const auto& Z3Var, const auto& LHS, const auto& RHS) {
                    return Z3Var == LHS / RHS;
                },
                BinOp->getOperand(0), BinOp->getOperand(1));
            break;
        case Instruction::UDiv:
            Res.putRange(
                BinOp,
                SingleFact::meet(LHS, RHS, [](const auto& L, const auto& R) {
                    return L / R;
                }).apply([BitWidth](const auto& R) {
                    return R.toUnsigned(BitWidth);
                }));
            Res.addConstraint(
                BinOp,
                [](const auto& Z3Var, const auto& LHS, const auto& RHS) {
                    return Z3Var == LHS / RHS;
                },
                BinOp->getOperand(0), BinOp->getOperand(1));
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
            Res.putRange(BinOp, SingleFact(IntRange::makeUnbounded()));
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

/**
 * @brief Updates the value of `OldVal` with `NewVal` for a store. Sets the
 * mutated flag on `OldVal`
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
    Res.newZ3Var(Ptr);
    Res.addConstraint(
        Ptr, [](const auto& Z3Var, const auto& Val) { return Z3Var == Val; },
        Val);
    return Res;
}

/**
 * Determines if the instruction produces a new value which is not equivalent
 * to its arguments.
 */
bool isMutatingInstruction(const llvm::User* I)
{
    return dyn_cast<PHINode>(I) == nullptr && dyn_cast<CastInst>(I) == nullptr;
}

/**
 * @brief Determines of `V` is dependent on `Other` and the dependency chain
 * between them contains a mutating instruction.
 *
 * @param V
 * @param Other
 * @param Ret
 * @return true
 * @return false
 */
bool isDependentOn(const llvm::Value* V, const llvm::Value* Other)
{
    std::queue<std::pair<const llvm::Value*, bool>> Worklist;
    std::unordered_set<const llvm::Value*> Visited;
    Worklist.push(std::make_pair(Other, false));
    while (!Worklist.empty()) {
        const auto [Curr, CurrIsMut] = Worklist.front();
        if (Curr == V) {
            return CurrIsMut;
        }
        Worklist.pop();
        Visited.emplace(Curr);
        for (const auto& Op : Curr->uses()) {
            const auto U = Op.getUser();
            if (Visited.contains(U)) {
                continue;
            }
            Worklist.push(
                std::make_pair(U, CurrIsMut || isMutatingInstruction(U)));
        }
    }
    return false;
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
    const auto DebugName = getDebugName(Phi);
    auto PhiRange = Res.getRange(Phi->getIncomingValue(0));
    if (PhiRange.hasValue() && isDependentOn(Phi->getIncomingValue(0), Phi)) {
        PhiRange.value().Mutated = true;
    }
    for (unsigned int Idx = 1; Idx < NumIncoming; ++Idx) {
        auto& IncomingRangeI = Res.getRange(Phi->getIncomingValue(Idx));
        if (IncomingRangeI.hasValue() &&
            isDependentOn(Phi->getIncomingValue(Idx), Phi)) {
            IncomingRangeI.value().Mutated = true;
        }
        PhiRange = SingleFact::meet(PhiRange, IncomingRangeI, IntRange::meet);
    }
    Res.putRange(Phi, PhiRange);
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
    // Canonical values for ease of debugging
    const auto CanonicalLHS = getCanonicalValue(LHS);
    const auto CanonicalRHS = getCanonicalValue(RHS);
    std::string CmpTxt;
    raw_string_ostream CmpStream(CmpTxt);
    Cmp->print(CmpStream);
    auto FRes = TRes;
    switch (Cmp->getPredicate()) {
        case ICmpInst::ICMP_EQ:
            TRes.putRange(LHS,
                          SingleFact::join(RHSRange, LHSRange, smallerRange));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var == RHS; },
                RHS);
            TRes.putRange(RHS,
                          SingleFact::join(LHSRange, RHSRange, smallerRange));
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var != RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_NE:
            FRes.putRange(LHS,
                          SingleFact::join(RHSRange, LHSRange, smallerRange));
            FRes.putRange(LHS,
                          SingleFact::join(LHSRange, RHSRange, smallerRange));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var != RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var == RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_SLT:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLT));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGE));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGT));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLE));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var < RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var >= RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_ULT:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULT));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGE));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGT));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULE));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var < RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var >= RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_SGT:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGT));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLE));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLT));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGE));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var > RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var <= RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_UGT:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGT));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULE));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULT));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGE));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var > RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var <= RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_SLE:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLE));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGT));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGE));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLT));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var <= RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var > RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_ULE:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULE));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGT));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGE));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULT));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var <= RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var > RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_SGE:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGE));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLT));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SLE));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_SGT));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var >= RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var < RHS; },
                RHS);
            break;
        case ICmpInst::ICMP_UGE:
            TRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGE));
            FRes.putRange(LHS, adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULT));
            TRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_ULE));
            FRes.putRange(RHS, adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                  ICmpInst::ICMP_UGT));
            TRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var >= RHS; },
                RHS);
            FRes.addConstraint(
                LHS,
                [](const auto& Z3Var, const auto& RHS) { return Z3Var < RHS; },
                RHS);
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

TransferRet IntervalAnalysis::transferCall(const CallInst* Call) const
{
    auto Res = *this;
    if (!Call->getType()->isIntegerTy()) {
        return Res;
    }
    const auto Called = Call->getCalledFunction();
    if (Called != nullptr) {
        const auto Name = Called->getName();
        if (Name == "rand") {
            Res.putRange(Call,
                         SingleFact(IntRange(bigint(0), bigint(RAND_MAX))));
        } else {
            Res.putRange(Call, SingleFact(IntRange::makeUnbounded()));
        }
    }
    return Res;
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
    } else if (const auto Store = dyn_cast<StoreInst>(&I); Store != nullptr) {
        return transferStore(Store);
    } else if (const auto Phi = dyn_cast<PHINode>(&I); Phi != nullptr) {
        return transferPhi(Phi);
    } else if (const auto Branch = dyn_cast<BranchInst>(&I);
               Branch != nullptr) {
        return transferBranch(Branch, Facts);
    } else if (const auto Call = dyn_cast<CallInst>(&I); Call != nullptr) {
        return transferCall(Call);
    }
    // we don't need to handle loads because `getCanonicalValue` will handle
    // that
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
    std::unordered_map<std::string, const llvm::Value*> RangeStrs;
    for (const auto& [Val, Range] : Analysis.Ranges_) {
        if (Analysis.DebugNames_.contains(Val)) {
            RangeStrs.emplace(Analysis.DebugNames_.at(Val), Val);
        } else {
            std::string Name;
            raw_string_ostream NameStream(Name);
            Val->printAsOperand(NameStream, false);
            RangeStrs.emplace(Name, Val);
        }
    }
    for (const auto& [Name, Val] : RangeStrs) {
        const auto& Range = Analysis.Ranges_.at(Val);
        if (Range.isTop() || dyn_cast<ConstantInt>(Val) != nullptr) {
            continue;
        }
        if (!Val->getName().empty()) {
            Stream << Val->getName() << ": ";
        } else if (Analysis.DebugNames_.contains(Val)) {
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

IntervalAnalysis::IntervalAnalysis(const llvm::Function& F)
    : Z3Context_(std::make_shared<z3::context>())
{
    for (const auto& Arg : F.args()) {
        if (Arg.getType()->isIntegerTy()) {
            putRange(&Arg, SingleFact::makeTop());
            DebugNames_[&Arg] = getDebugName(&Arg);
        }
    }
}

IntervalAnalysis IntervalAnalysis::getStartFact(const llvm::Function& F)
{
    auto Res = IntervalAnalysis(F);
    for (const auto& Arg : F.args()) {
        if (Arg.getType()->isIntegerTy()) {
            Res.putRange(&Arg, SingleFact(IntRange::makeUnbounded()));
            Res.DebugNames_[&Arg] = getDebugName(&Arg);
        }
    }
    return Res;
}

/** @brief  Gets the latest Z3 variable for a given value.
 * @param V the value to gat the variable for, the canonical value of `V`
 * will be used.
 * @return the latest Z3 variable for `V`
 */
z3::expr IntervalAnalysis::getZ3Var(const llvm::Value* V)
{
    const auto CV = getCanonicalValue(V);
    if (!VersionNumbers_.contains(CV)) {
        VersionNumbers_.emplace(CV, 0);
    }
    std::stringstream NameStream;
    NameStream << "t" << static_cast<const void*>(CV) << "."
               << VersionNumbers_.at(CV);
    if (!Z3Vars_.contains(CV)) {
        Z3Vars_.emplace(CV, Z3Context_->int_const(NameStream.str().c_str()));
    }
    return Z3Vars_.at(CV);
}

/**
 * @brief Creates a new Z3 variable for `V` and increments the version number.
 * This operation models an update to the canonical value of `V` or a brand new
 * allocation of `V`.
 */
void IntervalAnalysis::newZ3Var(const llvm::Value* V)
{
    const auto CV = getCanonicalValue(V);
    if (VersionNumbers_.find(CV) != VersionNumbers_.end()) {
        VersionNumbers_.at(CV)++;
    } else {
        VersionNumbers_.emplace(CV, 0);
    }
    std::stringstream NameStream;
    NameStream << "t" << static_cast<const void*>(CV) << "."
               << VersionNumbers_.at(CV);
    Z3Vars_.emplace(CV, Z3Context_->int_const(NameStream.str().c_str()));
}