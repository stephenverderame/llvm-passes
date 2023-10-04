#include "NullAbstractInterpretation.hpp"

#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/DerivedTypes.h>
#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/Support/Casting.h>

#include <cstddef>
#include <optional>

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
bool isNull(const Value* Value, const NullAbstractVals& State,
            const NullAbstractMem& MemState)
{
    if (Value ==
        ConstantPointerNull::get(PointerType::get(Value->getType(), 0))) {
        return true;
    }
    return false;
}

std::string getDebugName(const Value* I)
{
    if (I->hasName()) {
        return I->getName().str();
    }
    std::string Res;
    raw_string_ostream Stream(Res);
    I->print(Stream, false);
    Res = Res.substr(Res.find_first_not_of(" "));
    // Res = Res.substr(0, Res.find_first_of(" "));
    return Res;
}
}  // namespace

/**
 * @brief Simulates mutating a pointer by updating all uses of the old pointer
 * with the new pointer location.
 *
 * @param OldLoc
 * @param NewLoc
 */
void NullAbstractInterpretation::replaceLoc(AbstractPtrLoc OldLoc,
                                            AbstractPtrLoc NewLoc)
{
    for (auto& [Val, Loc] : State_) {
        if (Loc == OldLoc) {
            Loc = NewLoc;
        }
    }
    for (auto& [Loc, Val] : MemState_) {
        if (Val.Data.has_value() && Val.Data.value() == OldLoc) {
            Val.Data = NewLoc;
        }
    }
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
    if (Result.IsNull == NullState::NonNull) {
        if (A.Data.has_value() && B.Data.has_value()) {
            const auto& AData = MemState_.at(*A.Data);
            const auto& BData = ContextB.MemState_.at(*B.Data);
            if (!areAbstractValEq(AData, BData, ContextB)) {
                const auto Meet = meetVal(AData, BData, ContextB);
                const auto Loc = AbstractPtrLoc::nextAvailableLoc();
                Result.Data = Loc;
                MemState_[Loc] = Meet;
                replaceLoc(*A.Data, Loc);
            } else {
                Result.Data = A.Data;
            }
        } else if (!A.Data.has_value()) {
            // Memory for B.Data inserted from parent call to
            // NullAbstractInterpretation::meet
            Result.Data = B.Data;
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
    std::tuple<AbstractPtrLoc, PtrAbstractValue>&& KV)
{
    Res.State_[Value] = std::get<0>(KV);
    Res.MemState_[std::get<0>(KV)] = std::get<1>(KV);
    Res.DebugNames_[Value] = getDebugName(Value);
    return Res;
}

using TransferRet = NullAbstractInterpretation::TransferRet;
TransferRet NullAbstractInterpretation::transferAlloca(
    const AllocaInst* Alloca) const
{
    return NullAbstractInterpretation::insertIntoRes(
        *this, Alloca, PtrAbstractValue::make(NullState::NonNull));
}
TransferRet NullAbstractInterpretation::transferStore(
    const StoreInst* Store) const
{
    auto Res = *this;
    auto Value = Store->getValueOperand();
    auto Pointer = Store->getPointerOperand();
    if (Res.State_.contains(Pointer)) {
        auto& PointerState = Res.MemState_.at(Res.State_.at(Pointer));
        if (Res.State_.contains(Value)) {
            const auto& ValueState = Res.State_.at(Value);
            PointerState.Data = ValueState;
        } else if (Value->getType()->isPointerTy()) {
            const auto [NewId, NewVal] =
                PtrAbstractValue::make(NullState::MaybeNull);
            PointerState.Data = NewId;
            Res.State_[Value] = NewId;
            Res.MemState_[NewId] = NewVal;
            Res.DebugNames_[Value] = getDebugName(Value);
        } else {
            PointerState.Data = {};
        }
    }
    return Res;
}

TransferRet NullAbstractInterpretation::transferPhi(const PHINode* Phi) const
{
    auto Res = *this;
    std::optional<PtrAbstractValue> PhiRes;
    for (const auto& V : Phi->incoming_values()) {
        const auto& VState = Res.MemState_.at(Res.State_.at(V));
        if (PhiRes.has_value()) {
            PhiRes = Res.meetVal(PhiRes.value(), VState, Res);
        } else {
            PhiRes = VState;
        }
    }
    // assumes that the phi node has at least one incoming value
    const auto Loc = AbstractPtrLoc::nextAvailableLoc();
    Res.MemState_[Loc] = PhiRes.value();
    Res.State_[Phi] = Loc;
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
        const auto [Id, Val] = PtrAbstractValue::make(
            NonNull ? NullState::NonNull : NullState::MaybeNull);
        Res.State_[Call] = Id;
        Res.MemState_[Id] = Val;
        Res.DebugNames_[Call] = getDebugName(Call);
    }
    return Res;
}

TransferRet NullAbstractInterpretation::transferLoad(const LoadInst* Load) const
{
    const auto Pointer = Load->getPointerOperand();
    auto Res = *this;
    if (Res.State_.contains(Pointer)) {
        const auto& PointerState = Res.MemState_.at(Res.State_.at(Pointer));
        if (PointerState.Data.has_value()) {
            Res.State_[Load] = PointerState.Data.value();
            Res.DebugNames_[Load] = getDebugName(Load);
        }
    } else if (Pointer->getType()->isPointerTy()) {
        return insertIntoRes(std::move(Res), Load,
                             PtrAbstractValue::make(NullState::MaybeNull));
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
    if (isNull(LHS, State_, MemState_)) {
        return std::make_tuple(LHS, RHS);
    } else if (isNull(RHS, State_, MemState_)) {
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
            NullRes.MemState_.at(NullRes.State_.at(NonNullPtr)).nullify();
            NonNullRes.MemState_.at(NonNullRes.State_.at(NonNullPtr)).IsNull =
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

TransferRet NullAbstractInterpretation::transferBranch(
    const BranchInst* Branch) const
{
    if (!Branch->isConditional()) {
        return *this;
    }
    const auto Cond = Branch->getCondition();
    if (const auto Cmp = dyn_cast<ICmpInst>(Cond); Cmp != nullptr) {
        const auto LHS = Cmp->getOperand(0);
        const auto RHS = Cmp->getOperand(1);
        if (LHS->getType()->isPointerTy() && RHS->getType()->isPointerTy()) {
            const auto [TrueRes, FalseRes] = pointerCmp(Cmp, LHS, RHS);
            auto Ret =
                std::map<const BasicBlock*, NullAbstractInterpretation>{};
            Ret[Branch->getSuccessor(0)] = TrueRes;
            Ret[Branch->getSuccessor(1)] = FalseRes;
            assert(Branch->getNumSuccessors() == 2);
            return Ret;
        }
    }
    return *this;
}

TransferRet NullAbstractInterpretation::transfer(
    const llvm::Instruction& Inst) const
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
        return transferBranch(Branch);
    }
    return {*this};
}

NullAbstractInterpretation NullAbstractInterpretation::meet(
    const NullAbstractInterpretation& A, const NullAbstractInterpretation& B)
{
    auto Result = A;
    for (const auto& Entry : B.State_) {
        if (auto ExistingEntry = Result.State_.find(Entry.first);
            ExistingEntry != Result.State_.end()) {
            const auto NewVal =
                Result.meetVal(Result.MemState_.at(ExistingEntry->second),
                               B.MemState_.at(Entry.second), B);
            Result.MemState_[ExistingEntry->second] = NewVal;
        } else {
            Result.State_.insert(Entry);
        }
    }
    for (const auto& Entry : B.MemState_) {
        if (auto ExistingEntry = Result.MemState_.find(Entry.first);
            ExistingEntry != Result.MemState_.end()) {
            Result.MemState_[Entry.first] =
                Result.meetVal(ExistingEntry->second, Entry.second, B);
        } else {
            Result.MemState_.insert(Entry);
        }
    }
    for (const auto& [Val, Name] : B.DebugNames_) {
        Result.DebugNames_[Val] = Name;
    }
    return Result;
}
bool NullAbstractInterpretation::areAbstractValEq(
    const PtrAbstractValue& A, const PtrAbstractValue& B,
    const NullAbstractInterpretation& BContext) const
{
    if (A.IsNull != B.IsNull) {
        return false;
    }
    if (!A.Data.has_value() && !B.Data.has_value()) {
        return true;
    }
    if (A.Data.has_value() && B.Data.has_value()) {
        const auto& AData = MemState_.at(A.Data.value());
        const auto& BData = BContext.MemState_.at(B.Data.value());
        return areAbstractValEq(AData, BData, BContext);
    }
    return false;
}

bool NullAbstractInterpretation::operator==(
    const NullAbstractInterpretation& Other) const
{
    if (State_.size() != Other.State_.size()) {
        return false;
    }
    for (const auto& [Val, PtrLoc] : State_) {
        if (const auto OtherIt = Other.State_.find(Val);
            OtherIt != Other.State_.end()) {
            if (!areAbstractValEq(MemState_.at(PtrLoc),
                                  Other.MemState_.at(OtherIt->second), Other)) {
                return false;
            }
        } else {
            return false;
        }
    }

    for (const auto& [OtherVal, OtherPtrLoc] : Other.State_) {
        if (const auto It = State_.find(OtherVal); It != Other.State_.end()) {
            if (!areAbstractValEq(MemState_.at(It->second),
                                  Other.MemState_.at(OtherPtrLoc), Other)) {
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
        return MemState_.at(It->second);
    }
    return PtrAbstractValue{NullState::MaybeNull};
}