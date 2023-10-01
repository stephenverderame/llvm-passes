#include "NullAbstractInterpretation.hpp"

#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/Support/Casting.h>

// NOLINTNEXTLINE
using namespace llvm;

namespace
{

using TransferRet = std::vector<NullAbstractInterpretation>;
TransferRet TransferAlloca(const AllocaInst* Alloca,
                           NullAbstractInterpretation Res)
{
    Res.State_[Alloca] = PtrAbstractValue::newNonAlias(NullState::NonNull);
    return {Res};
}
TransferRet TransferStore(const StoreInst* Store,
                          NullAbstractInterpretation Res)
{
    auto Value = Store->getValueOperand();
    auto Pointer = Store->getPointerOperand();
    if (Res.State_.contains(Pointer) && Res.State_.contains(Value)) {
        auto& PointerState = Res.State_.at(Pointer);
        const auto& ValueState = Res.State_.at(Value);
        PointerState.Data_ = std::make_unique<PtrAbstractValue>(ValueState);
    }
    return {Res};
}

TransferRet TransferPhi(const PHINode* Phi, NullAbstractInterpretation Res)
{
    std::optional<PtrAbstractValue> PhiRes;
    for (const auto& V : Phi->incoming_values()) {
        const auto& VState = Res.State_.at(V);
        if (PhiRes.has_value()) {
            PhiRes = PtrAbstractValue::meet(PhiRes.value(), VState);
        } else {
            PhiRes = VState;
        }
    }
    // assumes that the phi node has at least one incoming value
    Res.State_[Phi] = PhiRes.value();
    return {Res};
}

TransferRet TransferCall(const CallInst* Call, NullAbstractInterpretation Res)
{
    const auto ReturnType = Call->getType();
    if (ReturnType->isPointerTy()) {
        const auto Attrib = Call->getAttributes();
        const auto NoAlis = Attrib.hasAttrSomewhere(Attribute::NoAlias);
        const auto NonNull = Attrib.hasAttrSomewhere(Attribute::NonNull);
        auto Val = PtrAbstractValue{};
        if (NonNull) {
            Val.IsNull_ = NullState::NonNull;
        }
        if (NoAlis) {
            Val.IsAlias_ = AliasState::NoAlias;
        }
        Res.State_[Call] = Val;
    }
    return {Res};
}

TransferRet TransferLoad(const LoadInst* Load, NullAbstractInterpretation Res)
{
    const auto Pointer = Load->getPointerOperand();
    if (Res.State_.contains(Pointer)) {
        const auto& PointerState = Res.State_.at(Pointer);
        Res.State_[Load] = *PointerState.Data_;
    } else if (Pointer->getType()->isPointerTy()) {
        Res.State_[Load] = PtrAbstractValue{};
    }
    return {Res};
}
}  // namespace

TransferRet NullAbstractInterpretation::transfer(
    const llvm::Instruction& Inst) const
{
    if (auto Alloca = dyn_cast<AllocaInst>(&Inst); Alloca != nullptr) {
        return TransferAlloca(Alloca, *this);
    } else if (auto Store = dyn_cast<StoreInst>(&Inst); Store != nullptr) {
        return TransferStore(Store, *this);
    } else if (auto Phi = dyn_cast<PHINode>(&Inst); Phi != nullptr) {
        return TransferPhi(Phi, *this);
    } else if (auto Call = dyn_cast<CallInst>(&Inst); Call != nullptr) {
        return TransferCall(Call, *this);
    } else if (auto Load = dyn_cast<LoadInst>(&Inst); Load != nullptr) {
        return TransferLoad(Load, *this);
    }
    return {*this};
}