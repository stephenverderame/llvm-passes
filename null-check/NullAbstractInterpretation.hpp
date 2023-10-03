#pragma once
#include <llvm-17/llvm/IR/Value.h>

#include <memory>
#include <unordered_map>
#include <unordered_set>

#include "df/DataFlow.hpp"

/**
 * @brief The state of a pointer
 */
enum class NullState : uint8_t {
    Null = 0,
    NonNull,
    Unknown,
};

/**
 * @brief The state of a pointer alias
 */
enum class AliasState : uint8_t {
    Alias = 0,
    NoAlias,
    Unknown,
};

class AbstractPtrLoc
{
    // NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
    inline static int64_t g_NextId = 0;

  public:
    int64_t Id;

    auto operator<=>(const AbstractPtrLoc&) const = default;

    static auto nextAvailableLoc() { return AbstractPtrLoc{g_NextId++}; }
};

struct AbstractPtrOffset {
    int64_t Off;

    auto operator<=>(const AbstractPtrOffset&) const = default;
};

class NullAbstractInterpretation;

/**
 * @brief Abstract value for a pointer type
 *
 */
struct PtrAbstractValue {
    /// If the value is known to be null.
    NullState IsNull_ = NullState::Unknown;
    /// If the value is known to be non-aliasing.
    AliasState IsAlias_ = AliasState::Unknown;
    /// The value of the data referred to be the pointer, may be null.
    std::optional<AbstractPtrLoc> Data_;

    inline bool operator==(const PtrAbstractValue& Other) const = default;

    PtrAbstractValue() = default;
    explicit PtrAbstractValue(NullState K) : IsNull_(K), Data_() {}
    PtrAbstractValue(NullState K, AliasState A)
        : IsNull_(K), IsAlias_(A), Data_()
    {
    }

    /// Construct a new non-aliasing pointer abstract value.
    inline static auto makeNonAlias(NullState K)
    {
        auto Result = PtrAbstractValue{K};
        Result.IsAlias_ = AliasState::NoAlias;
        return std::make_tuple(AbstractPtrLoc::nextAvailableLoc(), Result);
    }

    static auto make()
    {
        return std::make_tuple(AbstractPtrLoc::nextAvailableLoc(),
                               PtrAbstractValue{});
    }

    /**
     * @brief Returns a new Abstract Value that is the greatest lower
     * bound of the two given values.
     *
     * @param A
     * @param B
     * @return PtrAbstractValue
     */
    static PtrAbstractValue meet(const PtrAbstractValue& A,
                                 const PtrAbstractValue& B,
                                 NullAbstractInterpretation& Context,
                                 const NullAbstractInterpretation& ContextB);
};
struct NullAbstractInterpretation {
    /// Mapping from syntactic pointers to abstract pointer locations
    std::unordered_map<const llvm::Value*, AbstractPtrLoc> State_;
    /// The known abstract values for memory locations
    std::map<AbstractPtrLoc, PtrAbstractValue> MemState_;

    static NullAbstractInterpretation meet(const NullAbstractInterpretation& A,
                                           const NullAbstractInterpretation& B);

    TransferRetType<NullAbstractInterpretation> transfer(
        const llvm::Instruction& I) const;

    using Dir = Forwards;

    bool operator==(const NullAbstractInterpretation& Other) const = default;
};

static_assert(Fact<NullAbstractInterpretation>);