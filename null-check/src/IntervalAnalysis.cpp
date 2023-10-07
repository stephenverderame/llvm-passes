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
    Res.putRange(Alloca, SingleFact::makeBottom());
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
    // Canonical values for ease of debugging
    const auto CanonicalLHS = getCanonicalValue(LHS);
    const auto CanonicalRHS = getCanonicalValue(RHS);
    auto FRes = TRes;
    switch (Cmp->getPredicate()) {
        case ICmpInst::ICMP_EQ:
            TRes.putRange(LHS,
                          SingleFact::join(RHSRange, LHSRange, smallerRange));
            TRes.putRange(RHS,
                          SingleFact::join(LHSRange, RHSRange, smallerRange));
            break;
        case ICmpInst::ICMP_NE:
            FRes.putRange(LHS,
                          SingleFact::join(RHSRange, LHSRange, smallerRange));
            FRes.putRange(LHS,
                          SingleFact::join(LHSRange, RHSRange, smallerRange));
            break;
        case ICmpInst::ICMP_SLT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
            }
            break;
        case ICmpInst::ICMP_ULT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
            }
            break;
        case ICmpInst::ICMP_SGT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
            }
            break;
        case ICmpInst::ICMP_UGT:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
            }
            break;
        case ICmpInst::ICMP_SLE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
            }
            break;
        case ICmpInst::ICMP_ULE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
            }
            break;
        case ICmpInst::ICMP_SGE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGE));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLT));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SLE));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_SGT));
            }
            break;
        case ICmpInst::ICMP_UGE:
            if (isMoreBounded(RHSRange, LHSRange)) {
                TRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGE));
                FRes.putRange(LHS,
                              adjustForCondition(LHSRange, RHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULT));
            }
            if (isMoreBounded(LHSRange, RHSRange)) {
                TRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_ULE));
                FRes.putRange(RHS,
                              adjustForCondition(RHSRange, LHSRange, BitWidth,
                                                 ICmpInst::ICMP_UGT));
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
    } else if (const auto Store = dyn_cast<StoreInst>(&I); Store != nullptr) {
        return transferStore(Store);
    } else if (const auto Phi = dyn_cast<PHINode>(&I); Phi != nullptr) {
        return transferPhi(Phi);
    } else if (const auto Branch = dyn_cast<BranchInst>(&I);
               Branch != nullptr) {
        return transferBranch(Branch, Facts);
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

IntervalAnalysis::IntervalAnalysis(const llvm::Function& F)
{
    for (const auto& Arg : F.args()) {
        if (Arg.getType()->isIntegerTy()) {
            putRange(&Arg, SingleFact::makeBottom());
            DebugNames_[&Arg] = getDebugName(&Arg);
        }
    }
}