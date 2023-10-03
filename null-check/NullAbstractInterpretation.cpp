#include "NullAbstractInterpretation.hpp"

#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/DerivedTypes.h>
#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/Support/Casting.h>

#include <cstddef>
#include <optional>

// NOLINTNEXTLINE
using namespace llvm;

PtrAbstractValue PtrAbstractValue::meet(
    const PtrAbstractValue& A, const PtrAbstractValue& B,
    NullAbstractInterpretation& Context,
    const NullAbstractInterpretation& ContextB)
{
    auto Result = A;
    Result.IsNull_ = A.IsNull_ == B.IsNull_ ? A.IsNull_ : NullState::Unknown;
    Result.IsAlias_ =
        A.IsAlias_ == B.IsAlias_ ? A.IsAlias_ : AliasState::Unknown;
    if (A.Data_ == B.Data_) {
        return Result;
    } else if (A.Data_.has_value() && B.Data_.has_value()) {
        const auto& AData = Context.MemState_.at(*A.Data_);
        const auto& BData = ContextB.MemState_.at(*B.Data_);
        const auto Meet =
            PtrAbstractValue::meet(AData, BData, Context, ContextB);
        const auto Loc = AbstractPtrLoc::nextAvailableLoc();
        Result.Data_ = Loc;
        Context.MemState_[Loc] = Meet;
    } else {
        Result.Data_ = {};
    }
    return Result;
}

namespace
{

auto insertIntoRes(NullAbstractInterpretation&& Res, const Value* Value,
                   std::tuple<AbstractPtrLoc, PtrAbstractValue>&& KV)
{
    Res.State_[Value] = std::get<0>(KV);
    Res.MemState_[std::get<0>(KV)] = std::get<1>(KV);
    return Res;
}

using TransferRet = TransferRetType<NullAbstractInterpretation>;
TransferRet transferAlloca(const AllocaInst* Alloca,
                           NullAbstractInterpretation Res)
{
    return insertIntoRes(std::move(Res), Alloca,
                         PtrAbstractValue::makeNonAlias(NullState::NonNull));
}
TransferRet transferStore(const StoreInst* Store,
                          NullAbstractInterpretation Res)
{
    auto Value = Store->getValueOperand();
    auto Pointer = Store->getPointerOperand();
    if (Res.State_.contains(Pointer)) {
        auto& PointerState = Res.MemState_.at(Res.State_.at(Pointer));
        if (Res.State_.contains(Value)) {
            const auto& ValueState = Res.State_.at(Value);
            PointerState.Data_ = ValueState;
        } else {
            const auto [NewId, NewVal] = PtrAbstractValue::make();
            PointerState.Data_ = NewId;
            Res.State_[Value] = NewId;
            Res.MemState_[NewId] = NewVal;
        }
    }
    return Res;
}

TransferRet transferPhi(const PHINode* Phi, NullAbstractInterpretation Res)
{
    std::optional<PtrAbstractValue> PhiRes;
    for (const auto& V : Phi->incoming_values()) {
        const auto& VState = Res.MemState_.at(Res.State_.at(V));
        if (PhiRes.has_value()) {
            PhiRes = PtrAbstractValue::meet(PhiRes.value(), VState, Res, Res);
        } else {
            PhiRes = VState;
        }
    }
    // assumes that the phi node has at least one incoming value
    const auto Loc = AbstractPtrLoc::nextAvailableLoc();
    Res.MemState_[Loc] = PhiRes.value();
    Res.State_[Phi] = Loc;
    return Res;
}

TransferRet transferCall(const CallInst* Call, NullAbstractInterpretation Res)
{
    const auto ReturnType = Call->getType();
    if (ReturnType->isPointerTy()) {
        const auto Attrib = Call->getAttributes();
        const auto NoAlis = Attrib.hasAttrSomewhere(Attribute::NoAlias);
        const auto NonNull = Attrib.hasAttrSomewhere(Attribute::NonNull);
        auto [Id, Val] = PtrAbstractValue::make();
        if (NonNull) {
            Val.IsNull_ = NullState::NonNull;
        }
        if (NoAlis) {
            Val.IsAlias_ = AliasState::NoAlias;
        }
        Res.State_[Call] = Id;
        Res.MemState_[Id] = Val;
    }
    return Res;
}

TransferRet transferLoad(const LoadInst* Load, NullAbstractInterpretation Res)
{
    const auto Pointer = Load->getPointerOperand();
    if (Res.State_.contains(Pointer)) {
        const auto& PointerState = Res.MemState_.at(Res.State_.at(Pointer));
        if (PointerState.Data_.has_value()) {
            Res.State_[Load] = PointerState.Data_.value();
        }
    } else if (Pointer->getType()->isPointerTy()) {
        return insertIntoRes(std::move(Res), Load, PtrAbstractValue::make());
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
std::optional<std::tuple<const Value*, const Value*>> getNullNonNullPtr(
    const Value* LHS, const Value* RHS, const NullAbstractInterpretation& Res)
{
    if (LHS == ConstantPointerNull::get(PointerType::get(LHS->getType(), 0))) {
        return std::make_tuple(LHS, RHS);
    } else if (const auto LHSVal = Res.State_.find(LHS);
               LHSVal != Res.State_.end()) {
        if (Res.MemState_.at(LHSVal->second).IsNull_ == NullState::Null) {
            return std::make_tuple(LHS, RHS);
        }
    }
    if (RHS == ConstantPointerNull::get(PointerType::get(RHS->getType(), 0))) {
        return std::make_tuple(RHS, LHS);
    } else if (const auto RHSVal = Res.State_.find(RHS);
               RHSVal != Res.State_.end()) {
        if (Res.MemState_.at(RHSVal->second).IsNull_ == NullState::Null) {
            return std::make_tuple(RHS, LHS);
        }
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
std::tuple<NullAbstractInterpretation, NullAbstractInterpretation> pointerCmp(
    const ICmpInst* Cmp, const Value* LHS, const Value* RHS,
    NullAbstractInterpretation&& Res)
{
    const auto Ptrs = getNullNonNullPtr(LHS, RHS, Res);
    auto TrueRes = std::move(Res);
    auto FalseRes = TrueRes;
    if (Ptrs.has_value() &&
        (Cmp->getPredicate() == CmpInst::Predicate::ICMP_EQ ||
         Cmp->getPredicate() == CmpInst::Predicate::ICMP_NE)) {
        const auto [NullPtr, NonNullPtr] = Ptrs.value();
        const static auto InsertIntoResults =
            [NullPtr, NonNullPtr](auto& NullRes, auto& NonNullRes) {
                NullRes.MemState_[NullRes.State_[NonNullPtr]].IsNull_ =
                    NullState::Null;
                NonNullRes.MemState_[NonNullRes.State_[NonNullPtr]].IsNull_ =
                    NullState::NonNull;
            };
        if (Cmp->getPredicate() == CmpInst::Predicate::ICMP_EQ) {
            InsertIntoResults(TrueRes, FalseRes);
        } else {
            InsertIntoResults(FalseRes, TrueRes);
        }
    }
    return {TrueRes, FalseRes};
}

TransferRet transferBranch(const BranchInst* Branch,
                           NullAbstractInterpretation Res)
{
    if (!Branch->isConditional()) {
        return Res;
    }
    const auto Cond = Branch->getCondition();
    if (const auto Cmp = dyn_cast<ICmpInst>(Cond); Cmp != nullptr) {
        const auto LHS = Cmp->getOperand(0);
        const auto RHS = Cmp->getOperand(1);
        if (LHS->getType()->isPointerTy() && RHS->getType()->isPointerTy()) {
            const auto [TrueRes, FalseRes] =
                pointerCmp(Cmp, LHS, RHS, std::move(Res));
            auto Ret =
                std::map<const BasicBlock*, NullAbstractInterpretation>{};
            Ret[Branch->getSuccessor(0)] = TrueRes;
            Ret[Branch->getSuccessor(1)] = FalseRes;
            assert(Branch->getNumSuccessors() == 2);
            return Ret;
        }
    }
    return Res;
}
}  // namespace

TransferRet NullAbstractInterpretation::transfer(
    const llvm::Instruction& Inst) const
{
    if (auto Alloca = dyn_cast<AllocaInst>(&Inst); Alloca != nullptr) {
        return transferAlloca(Alloca, *this);
    } else if (auto Store = dyn_cast<StoreInst>(&Inst); Store != nullptr) {
        return transferStore(Store, *this);
    } else if (auto Phi = dyn_cast<PHINode>(&Inst); Phi != nullptr) {
        return transferPhi(Phi, *this);
    } else if (auto Call = dyn_cast<CallInst>(&Inst); Call != nullptr) {
        return transferCall(Call, *this);
    } else if (auto Load = dyn_cast<LoadInst>(&Inst); Load != nullptr) {
        return transferLoad(Load, *this);
    } else if (auto Branch = dyn_cast<BranchInst>(&Inst); Branch != nullptr) {
        return transferBranch(Branch, *this);
    }
    return {*this};
}

NullAbstractInterpretation NullAbstractInterpretation::meet(
    const NullAbstractInterpretation& A, const NullAbstractInterpretation& B)
{
    auto Result = A;
    // review the dataflow
    for (const auto& Entry : B.State_) {
        if (auto ExistingEntry = Result.State_.find(Entry.first);
            ExistingEntry != Result.State_.end()) {
            if (ExistingEntry->second != Entry.second) {
                const auto NewVal = PtrAbstractValue::meet(
                    Result.MemState_.at(ExistingEntry->second),
                    B.MemState_.at(Entry.second), Result, B);
                Result.MemState_[ExistingEntry->second] = NewVal;
            }
        } else {
            Result.State_.insert(Entry);
        }
    }
    for (const auto& Entry : B.MemState_) {
        if (auto ExistingEntry = Result.MemState_.find(Entry.first);
            ExistingEntry != Result.MemState_.end()) {
            Result.MemState_[Entry.first] = PtrAbstractValue::meet(
                ExistingEntry->second, Entry.second, Result, B);
        } else {
            Result.MemState_.insert(Entry);
        }
    }
    return Result;
}