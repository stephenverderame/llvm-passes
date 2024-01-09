#include "NullAbstractInterpretation.hpp"

#include <llvm-17/llvm/IR/ConstantRange.h>
#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/DerivedTypes.h>
#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/Support/Casting.h>
#include <llvm/Analysis/LazyValueInfo.h>

#include <cstddef>
#include <optional>

#include "df/DataFlow.hpp"

// NOLINTNEXTLINE
using namespace llvm;

namespace
{
/**
 * @brief Dertermines if `Value` is definitely null
 *
 * @param Value the value to check
 * @param State the abstract interpretation facts
 * @param MemState the abstract interpretation facts for the memory
 * @return true if `Value` is definitely null
 */
bool isNull(const Value* Value)
{
    return Value ==
           ConstantPointerNull::get(PointerType::get(Value->getType(), 0));
}

const auto getSizeInBytes(Type* Ty, const DataLayout* DL)
{
    return DL->getTypeStoreSize(Ty);
}

}  // namespace

bool PtrAbstractValue::operator==(const PtrAbstractValue& Other) const
{
    if (IsNull != Other.IsNull) {
        return false;
    }
    if (!Data && !Other.Data) {
        return true;
    }
    if (Data && Other.Data) {
        const auto& AData = *Data;
        const auto& BData = *Other.Data;
        return AData == BData;
    }
    return false;
}

/**
 * @brief Returns a new Abstract Value that is the greatest lower
 * bound of the two given values.
 * If meeting the two values requires creation of a new value, the new value
 * will be stored in a fresh location in `this`.
 *
 * @param A An abstract value belonging to the AbstractInterpretation of `this`
 * @param B An abstract value belonging to the AbstractInterpretation of
 * `ContextB`
 * @param ContextB The memory state of the NullAbstractInterpretation that
 * `B` belongs to.
 * @return PtrAbstractValue
 */
PtrAbstractValue NullAbstractInterpretation::meetVal(
    const PtrAbstractValue& A, const PtrAbstractValue& B,
    const NullAbstractInterpretation& ContextB)
{
    auto Result = A;
    Result.IsNull = A.IsNull == B.IsNull ? A.IsNull : NullState::MaybeNull;
    Result.Size = LatticeElem<uint64_t>::meet(
        A.Size, B.Size, [](auto A, auto B) { return std::min(A, B); });
    if (Result.IsNull == NullState::NonNull) {
        if (A.Data && B.Data) {
            const auto& AData = *A.Data;
            const auto& BData = *B.Data;
            if (AData != BData) {
                const auto Meet = meetVal(AData, BData, ContextB);
                *Result.Data = Meet;
            } else {
                Result.Data = A.Data;
            }
        }
    } else {
        Result.Data = {};
    }
    return Result;
}

/**
 * @brief Inserts a new location and value into `Res` and returns the new
 * abstract interpretation facts
 *
 * @param Res the abstract interpretation facts
 * @param Value the llvm value to insert a fact for
 * @param KV the location and value to insert
 */
NullAbstractInterpretation NullAbstractInterpretation::insertIntoRes(
    NullAbstractInterpretation Res, const Value* Value,
    std::shared_ptr<PtrAbstractValue>&& Ptr)
{
    Res.State_[Value] = std::move(Ptr);
    Res.DebugNames_[Value] = getDebugName(Value);
    return Res;
}

using TransferRet = NullAbstractInterpretation::TransferRet;
TransferRet NullAbstractInterpretation::transferAlloca(
    const AllocaInst* Alloca) const
{
    const auto Size = Alloca->getAllocatedType()->isArrayTy()
                          ? Alloca->getAllocatedType()->getArrayNumElements()
                          : 1;
    return NullAbstractInterpretation::insertIntoRes(
        *this, Alloca, PtrAbstractValue::make(NullState::NonNull, Size));
}
TransferRet NullAbstractInterpretation::transferStore(
    const StoreInst* Store) const
{
    auto Res = *this;
    auto Value = Store->getValueOperand();
    auto Pointer = Store->getPointerOperand();
    if (Res.State_.contains(Pointer)) {
        auto& PointerState = Res.State_.at(Pointer);
        if (Res.State_.contains(Value)) {
            const auto& ValueState = Res.State_.at(Value);
            PointerState->Data = ValueState;
        } else if (Value->getType()->isPointerTy()) {
            const auto NewVal = PtrAbstractValue::make(NullState::MaybeNull);
            PointerState->Data = NewVal;
            Res.DebugNames_[Value] = getDebugName(Value);
        } else {
            assert(PointerState->Data == nullptr);
        }
    }
    return Res;
}

TransferRet NullAbstractInterpretation::transferPhi(const PHINode* Phi) const
{
    if (!Phi->getType()->isPointerTy()) {
        return *this;
    }
    auto Res = *this;
    // initally TOP
    auto PhiRes = std::make_shared<PtrAbstractValue>();
    for (const auto& V : Phi->incoming_values()) {
        const auto& VState = [&Res, &V]() {
            if (Res.State_.contains(V)) {
                return Res.State_.at(V);
            } else {
                return std::make_shared<PtrAbstractValue>();
            }
        }();
        *PhiRes = Res.meetVal(*PhiRes, *VState, Res);
    }
    Res.State_[Phi] = PhiRes;
    Res.DebugNames_[Phi] = getDebugName(Phi);
    return Res;
}

TransferRet NullAbstractInterpretation::transferCall(const CallInst* Call) const
{
    const auto ReturnType = Call->getType();
    auto Res = *this;
    if (ReturnType->isPointerTy()) {
        const auto Attrib = Call->getAttributes();
        const auto NonNull = Attrib.hasAttrSomewhere(Attribute::NonNull);
        const auto Val = PtrAbstractValue::make(NonNull ? NullState::NonNull
                                                        : NullState::MaybeNull);
        Res.State_[Call] = Val;
        Res.DebugNames_[Call] = getDebugName(Call);
    }
    // TODO: intrinsics and passing pointers as arguments
    return Res;
}

TransferRet NullAbstractInterpretation::transferLoad(const LoadInst* Load) const
{
    const auto Pointer = Load->getPointerOperand();
    auto Res = *this;
    if (Load->getType()->isPointerTy()) {
        if (Res.State_.contains(Pointer)) {
            const auto& PointerState = Res.State_.at(Pointer);
            if (PointerState->Data) {
                Res.State_[Load] = PointerState->Data;
                Res.DebugNames_[Load] = getDebugName(Load);
                return Res;
            }
        }
        return insertIntoRes(std::move(Res), Load,
                             PtrAbstractValue::make(NullState::MaybeNull));
    }
    return Res;
}

/**
 * @brief Determines if all possible values of the given value is in range of
 * `0` to `Size`.
 *
 * @param Val the value to check
 * @param Inst the instruction that uses the value
 * @param Size the size to check against
 * @return true if all possible values of the given value is in range of `0` to
 * `Size`
 */
bool NullAbstractInterpretation::inRange(const Use& Val,
                                         const llvm::Instruction* Inst,
                                         uint64_t Size) const
{
    const auto Range = LVA_.get().getConstantRangeAtUse(Val);
    const auto L = Range.getLower();
    const auto U = Range.getUpper();
    if (Range.getLower().isNegative() || !Range.getUnsignedMax().ult(Size)) {
        const auto Interval =
            IntervalFacts_.get().InstructionInFacts.at(Inst).getValRange(Val);
        if (Interval.has_value()) {
            const auto IntVal = Interval.value();
            return !(IntVal.Lower.isNegative() ||
                     IntVal.Upper >=
                         bound::Bound(bigint(std::to_string(Size))));
        }
        return false;
    } else {
        return true;
    }
}

TransferRet NullAbstractInterpretation::transferGetElemPtr(
    const GetElementPtrInst* GEP) const
{
    auto Res = *this;
    Res.DebugNames_[GEP] = getDebugName(GEP);
    const auto BasePtr = GEP->getPointerOperand();
    bool Bottom = false;
    if (auto It = Res.State_.find(BasePtr); It != Res.State_.end()) {
        const auto& BaseAbstractVal = *It->second;
        if (BaseAbstractVal.Size.hasValue() ||
            GEP->getSourceElementType()->isArrayTy()) {
            for (const auto& Idx : GEP->indices()) {
                auto Size =
                    BaseAbstractVal.Size.hasValue()
                        ? BaseAbstractVal.Size.value()
                        : GEP->getSourceElementType()->getArrayNumElements();
                if (!inRange(Idx, GEP, Size)) {
                    Bottom = true;
                    break;
                }
            }
        } else {
            Bottom = true;
        }
    } else {
        Bottom = true;
    }
    if (Bottom) {
        if (Res.State_.contains(GEP)) {
            Res.State_.at(GEP)->nullify();
        } else {
            Res.State_.emplace(GEP,
                               PtrAbstractValue::make(NullState::MaybeNull));
            Res.DebugNames_[GEP] = getDebugName(GEP);
        }
    } else {
        const auto DataPtr = Res.State_.at(BasePtr)->Data;
        Res.State_[GEP] =
            DataPtr ? DataPtr : PtrAbstractValue::make(NullState::NonNull);
        Res.State_[GEP]->IsNull = NullState::NonNull;
        Res.DebugNames_[GEP] = getDebugName(GEP);
    }
    return Res;
}

/**
 * @brief Determines which of the two pointers is null and which is non null
 *
 * @param LHS pointer A
 * @param RHS pointer B
 * @param Res
 * @return tuple of the null pointer and non null pointer if one of the
 * pointers is null, otherwise nullopt
 */
std::optional<std::tuple<const Value*, const Value*>>
NullAbstractInterpretation::getNullNonNullPtr(const Value* LHS,
                                              const Value* RHS) const
{
    if (isNull(LHS)) {
        return std::make_tuple(LHS, RHS);
    } else if (isNull(RHS)) {
        return std::make_tuple(RHS, LHS);
    }
    return {};
}

/**
 * @brief Gets the abstract interpretation facts for the true branch and the
 * false branch of a branch which compares two pointers as its condition
 *
 * @param Cmp the comparison instruction
 * @param LHS pointer A
 * @param RHS pointer B
 * @param Res the abstract interpretation facts before the comparison
 * @return std::tuple<NullAbstractInterpretation,
 * NullAbstractInterpretation> of the abstract interpretation facts for the
 * true branch and the false branch
 */
std::tuple<NullAbstractInterpretation, NullAbstractInterpretation>
NullAbstractInterpretation::pointerCmp(const ICmpInst* Cmp, const Value* LHS,
                                       const Value* RHS) const
{
    const auto Ptrs = getNullNonNullPtr(LHS, RHS);
    auto TrueRes = *this;
    auto FalseRes = TrueRes;
    if (Ptrs.has_value() &&
        (Cmp->getPredicate() == CmpInst::Predicate::ICMP_EQ ||
         Cmp->getPredicate() == CmpInst::Predicate::ICMP_NE)) {
        const auto [NullPtr, NonNullPtr] = Ptrs.value();
        const auto InsertIntoResults = [NonNullPtr](auto& NullRes,
                                                    auto& NonNullRes) {
            NullRes.State_.at(NonNullPtr)->nullify();
            NonNullRes.State_.at(NonNullPtr)->IsNull = NullState::NonNull;
        };
        if (Cmp->getPredicate() == CmpInst::Predicate::ICMP_EQ) {
            InsertIntoResults(TrueRes, FalseRes);
        } else {
            InsertIntoResults(FalseRes, TrueRes);
        }
    }
    return {TrueRes, FalseRes};
}

TransferRet NullAbstractInterpretation::transferBranch(
    const BranchInst* Branch,
    const DataFlowFacts<NullAbstractInterpretation>& Facts) const
{
    return transferConditionDependentBranch(
        *this, Branch, Facts,
        [](const auto& Self, const auto* Cond, const auto& /* Facts */)
            -> std::optional<std::tuple<NullAbstractInterpretation,
                                        NullAbstractInterpretation>> {
            if (const auto Cmp = dyn_cast<ICmpInst>(Cond); Cmp != nullptr) {
                const auto LHS = Cmp->getOperand(0);
                const auto RHS = Cmp->getOperand(1);
                if (LHS->getType()->isPointerTy() &&
                    RHS->getType()->isPointerTy()) {
                    return Self.pointerCmp(Cmp, LHS, RHS);
                }
            }
            return {};
        });
}

TransferRet NullAbstractInterpretation::transfer(
    const llvm::Instruction& Inst,
    const DataFlowFacts<NullAbstractInterpretation>& Facts) const
{
    if (auto Alloca = dyn_cast<AllocaInst>(&Inst); Alloca != nullptr) {
        return transferAlloca(Alloca);
    } else if (auto Store = dyn_cast<StoreInst>(&Inst); Store != nullptr) {
        return transferStore(Store);
    } else if (auto Phi = dyn_cast<PHINode>(&Inst); Phi != nullptr) {
        return transferPhi(Phi);
    } else if (auto Call = dyn_cast<CallInst>(&Inst); Call != nullptr) {
        return transferCall(Call);
    } else if (auto Load = dyn_cast<LoadInst>(&Inst); Load != nullptr) {
        return transferLoad(Load);
    } else if (auto Branch = dyn_cast<BranchInst>(&Inst); Branch != nullptr) {
        return transferBranch(Branch, Facts);
    } else if (auto GEP = dyn_cast<GetElementPtrInst>(&Inst); GEP != nullptr) {
        return transferGetElemPtr(GEP);
    } else if (Inst.getType()->isPointerTy()) {
        return insertIntoRes(*this, &Inst,
                             PtrAbstractValue::make(NullState::MaybeNull));
    }
    return {*this};
}

NullAbstractInterpretation NullAbstractInterpretation::meet(
    const NullAbstractInterpretation& A, const NullAbstractInterpretation& B)
{
    auto Result = A;
    std::unordered_map<const PtrAbstractValue*,
                       std::shared_ptr<PtrAbstractValue>>
        ClonedVals;
    for (const auto& [Val, Ptr] : B.State_) {
        if (auto ExistingEntry = Result.State_.find(Val);
            ExistingEntry != Result.State_.end()) {
            const auto NewVal = Result.meetVal(*ExistingEntry->second, *Ptr, B);
            *ExistingEntry->second = NewVal;
        } else {
            Result.State_.emplace(Val, Ptr->clone(ClonedVals));
        }
    }
    for (const auto& [Val, Name] : B.DebugNames_) {
        Result.DebugNames_[Val] = Name;
    }
    return Result;
}

bool NullAbstractInterpretation::operator==(
    const NullAbstractInterpretation& Other) const
{
    if (State_.size() != Other.State_.size()) {
        return false;
    }
    for (const auto& [Val, Ptr] : State_) {
        if (const auto OtherIt = Other.State_.find(Val);
            OtherIt != Other.State_.end()) {
            if (*Ptr != *OtherIt->second) {
                return false;
            }
        } else {
            return false;
        }
    }

    for (const auto& [OtherVal, OtherPtr] : Other.State_) {
        if (const auto It = State_.find(OtherVal); It != Other.State_.end()) {
            if (*It->second != *OtherPtr) {
                return false;
            }
        } else {
            return false;
        }
    }

    return true;
}

PtrAbstractValue NullAbstractInterpretation::getAbstractVal(
    const Value* Val) const
{
    if (const auto It = State_.find(Val); It != State_.end()) {
        return *It->second;
    }
    return PtrAbstractValue{NullState::MaybeNull};
}

std::shared_ptr<PtrAbstractValue> PtrAbstractValue::clone(
    std::unordered_map<const PtrAbstractValue*,
                       std::shared_ptr<PtrAbstractValue>>& ClonedVals) const
{
    if (ClonedVals.contains(this)) {
        return ClonedVals.at(this);
    }
    auto Result = std::make_shared<PtrAbstractValue>(*this);
    if (Data && ClonedVals.contains(Data.get())) {
        Result->Data = ClonedVals.at(Data.get());
    } else if (Data) {
        Result->Data = Data->clone(ClonedVals);
        ClonedVals.emplace(Data.get(), Result->Data);
    }
    ClonedVals.emplace(this, Result);
    return Result;
}

NullAbstractInterpretation::NullAbstractInterpretation(
    const NullAbstractInterpretation& Other)
    : State_(Other.State_.size()),
      DebugNames_(Other.DebugNames_),
      LVA_(Other.LVA_),
      DL_(Other.DL_),
      IntervalFacts_(Other.IntervalFacts_)
{
    std::unordered_map<const PtrAbstractValue*,
                       std::shared_ptr<PtrAbstractValue>>
        ClonedVals;
    for (const auto& [Val, Ptr] : Other.State_) {
        State_.emplace(Val, Ptr->clone(ClonedVals));
    }
}

NullAbstractInterpretation& NullAbstractInterpretation::operator=(
    const NullAbstractInterpretation& Other)
{
    auto Temp = Other;
    std::swap(State_, Temp.State_);
    std::swap(DebugNames_, Temp.DebugNames_);
    return *this;
}

NullAbstractInterpretation::NullAbstractInterpretation(
    llvm::LazyValueInfo& LVA,
    const DataFlowFacts<IntervalAnalysis>& IntervalFacts, const llvm::Module& M,
    const llvm::Function& F)
    : State_(),
      DebugNames_(),
      LVA_(LVA),
      DL_(std::make_shared<llvm::DataLayout>(&M)),
      IntervalFacts_(IntervalFacts)
{
    for (const auto& Arg : F.args()) {
        if (auto PtrType = dyn_cast<PointerType>(Arg.getType());
            PtrType != nullptr) {
            const auto NullInfo = Arg.hasAttribute(Attribute::NonNull)
                                      ? NullState::NonNull
                                      : NullState::MaybeNull;
            State_.emplace(&Arg, PtrAbstractValue::make(NullInfo));
            DebugNames_.emplace(&Arg, getDebugName(&Arg));
        }
    }
}