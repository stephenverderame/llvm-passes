#pragma once

#include <memory>
#include <unordered_map>
#include <unordered_set>

#include "df/DataFlow.hpp"

/**
 * @brief The state of a pointer
 */
enum class NullState : uint8_t {
    /// @brief Potentially null
    MaybeNull = 0,
    /// @brief Definitely not null
    NonNull,
};

/**
 * @brief A location which an abstract pointer can point to.
 *
 */
class AbstractPtrLoc
{
    // NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
    inline static int64_t g_NextId = 0;

  public:
    int64_t Id;

    auto operator<=>(const AbstractPtrLoc&) const = default;

    /**
     * @brief Returns a fresh, unused location.
     */
    static auto nextAvailableLoc() { return AbstractPtrLoc{g_NextId++}; }
};

class NullAbstractInterpretation;
struct PtrAbstractValue;
/// A mapping from abstract pointer locations to abstract pointer values
using NullAbstractMem = std::map<AbstractPtrLoc, PtrAbstractValue>;
/// A mapping from LLVM values to abstract pointer locations
using NullAbstractVals = std::unordered_map<const llvm::Value*, AbstractPtrLoc>;

/**
 * @brief Abstract value for a pointer type.
 * This is the representation of a pointer in the abstract domain.
 * TOP: Unknown
 * BOTTOM: Null
 *
 */
struct PtrAbstractValue {
    /// If the value is known to be null.
    NullState IsNull = NullState::MaybeNull;
    /// The value of the data referred to be the pointer, may be null.
    std::optional<AbstractPtrLoc> Data;

    PtrAbstractValue() = default;
    explicit PtrAbstractValue(NullState K) : IsNull(K), Data() {}

    /**
     * @brief Constructs a new abstract location and abstract value which is
     * equal to bottom.
     *
     * @return tuple of the new location and abstract value
     */
    static auto make(NullState N)
    {
        return std::make_tuple(AbstractPtrLoc::nextAvailableLoc(),
                               PtrAbstractValue{N});
    }

    void nullify()
    {
        IsNull = NullState::MaybeNull;
        Data.reset();
    }
};
/// Forward declarations
namespace llvm
{
class Value;
class AllocaInst;
class LoadInst;
class StoreInst;
class CallInst;
class PHINode;
class BranchInst;
class ICmpInst;
}  // namespace llvm

/**
 * @brief Data flow analysis to determine whether a pointer is null, nonnull,
 * or unknown. Based off conditional constant propagation.
 * TOP: Set of all values as unknown (empty set)
 * BOTTOM: Set of all values as potentially null
 * MEET: meet of particular value (union, except if both values are specified,
 * then meet of the respective values)
 *
 */
class NullAbstractInterpretation
{
  public:
    using TransferRet = TransferRetType<NullAbstractInterpretation>;

  private:
    /// Mapping from syntactic pointers to abstract pointer locations
    std::unordered_map<const llvm::Value*, AbstractPtrLoc> State_;
    /// The known abstract values for memory locations
    std::map<AbstractPtrLoc, PtrAbstractValue> MemState_;
    /// Mapping from values to their names
    std::unordered_map<const llvm::Value*, std::string> DebugNames_;

    bool areAbstractValEq(const PtrAbstractValue& A, const PtrAbstractValue& B,
                          const NullAbstractInterpretation& BContext) const;

    TransferRet transferAlloca(const llvm::AllocaInst* Alloca) const;
    TransferRet transferLoad(const llvm::LoadInst* Load) const;
    TransferRet transferStore(const llvm::StoreInst* Store) const;
    TransferRet transferCall(const llvm::CallInst* Call) const;
    TransferRet transferPhi(const llvm::PHINode* Phi) const;
    TransferRet transferBranch(const llvm::BranchInst* Branch) const;
    static NullAbstractInterpretation insertIntoRes(
        NullAbstractInterpretation Res, const llvm::Value* Value,
        std::tuple<AbstractPtrLoc, PtrAbstractValue>&& KV);

    std::optional<std::tuple<const llvm::Value*, const llvm::Value*>>
    getNullNonNullPtr(const llvm::Value* LHS, const llvm::Value* RHS) const;

    std::tuple<NullAbstractInterpretation, NullAbstractInterpretation>
    pointerCmp(const llvm::ICmpInst* Cmp, const llvm::Value* LHS,
               const llvm::Value* RHS) const;

    PtrAbstractValue meetVal(const PtrAbstractValue& A,
                             const PtrAbstractValue& B,
                             const NullAbstractInterpretation& ContextB);

  public:
    static NullAbstractInterpretation meet(const NullAbstractInterpretation& A,
                                           const NullAbstractInterpretation& B);

    TransferRetType<NullAbstractInterpretation> transfer(
        const llvm::Instruction& I) const;

    using Dir = Forwards;

    bool operator==(const NullAbstractInterpretation& Other) const;

    /**
     * @brief Get the AbstractValue the pointer refers to.
     * Requires `Value` to be a pointer type.
     *
     * @param Value
     * @return PtrAbstractValue
     */
    PtrAbstractValue getAbstractVal(const llvm::Value* Value) const;
};

static_assert(Fact<NullAbstractInterpretation>);