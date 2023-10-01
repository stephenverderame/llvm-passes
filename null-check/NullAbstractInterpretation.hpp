#pragma once
#include <llvm-17/llvm/IR/Value.h>

#include <unordered_map>
#include <unordered_set>

#include "df/DataFlow.hpp"

enum class NullState : uint8_t {
    Null = 0,
    NonNull,
    Unknown,
};

enum class AliasState : uint8_t {
    Alias = 0,
    NoAlias,
    Unknown,
};

struct PtrAbstractValue {
    NullState IsNull_ = NullState::Unknown;
    /// The value is known to be non-aliasing.
    AliasState IsAlias_ = AliasState::Unknown;
    /// The value of the data referred to be the pointer, may be null.
    std::unique_ptr<PtrAbstractValue> Data_;

    inline bool operator==(const PtrAbstractValue& Other) const
    {
        const auto Cmp = IsNull_ == Other.IsNull_ && IsAlias_ == Other.IsAlias_;
        if (Data_ != nullptr && Other.Data_ != nullptr) {
            return Cmp && *Data_ == *Other.Data_;
        } else if (Data_ == nullptr && Other.Data_ == nullptr) {
            return Cmp;
        } else {
            return false;
        }
    }

    inline PtrAbstractValue(const PtrAbstractValue& Other)
        : IsNull_(Other.IsNull_),
          IsAlias_(Other.IsAlias_),
          Data_(Other.Data_ == nullptr
                    ? nullptr
                    : std::make_unique<PtrAbstractValue>(*Other.Data_))
    {
    }

    inline PtrAbstractValue& operator=(const PtrAbstractValue& Other)
    {
        IsNull_ = Other.IsNull_;
        IsAlias_ = Other.IsAlias_;
        Data_ = Other.Data_ == nullptr
                    ? nullptr
                    : std::make_unique<PtrAbstractValue>(*Other.Data_);
        return *this;
    }

    PtrAbstractValue() : Data_(nullptr) {}
    explicit PtrAbstractValue(NullState K) : IsNull_(K), Data_(nullptr) {}
    PtrAbstractValue(NullState K, AliasState A)
        : IsNull_(K), IsAlias_(A), Data_(nullptr)
    {
    }

    ~PtrAbstractValue() = default;
    PtrAbstractValue(PtrAbstractValue&&) = default;
    PtrAbstractValue& operator=(PtrAbstractValue&&) = default;

    /// Construct a new non-aliasing pointer abstract value.
    inline static auto newNonAlias(NullState K)
    {
        auto Result = PtrAbstractValue{K};
        Result.IsAlias_ = AliasState::NoAlias;
        return Result;
    }

    static PtrAbstractValue meet(const PtrAbstractValue& A,
                                 const PtrAbstractValue& B)
    {
        auto Result = PtrAbstractValue{};
        Result.IsNull_ =
            A.IsNull_ == B.IsNull_ ? A.IsNull_ : NullState::Unknown;
        Result.IsAlias_ =
            A.IsAlias_ == B.IsAlias_ ? A.IsAlias_ : AliasState::Unknown;
        if (A.Data_ != nullptr && B.Data_ != nullptr) {
            Result.Data_ =
                std::make_unique<PtrAbstractValue>(meet(*A.Data_, *B.Data_));
        } else {
            Result.Data_ = nullptr;
        }
        return Result;
    }
};
struct NullAbstractInterpretation {
    /// The known abstract values
    std::unordered_map<const llvm::Value*, PtrAbstractValue> State_;

    static auto meet(const NullAbstractInterpretation& A,
                     const NullAbstractInterpretation& B)
    {
        // LLVM is SSA, so this is pretty simple since we don't have to worry
        // about the same variable having different values.
        auto Result = A;
        for (const auto& Entry : B.State_) {
            Result.State_.insert(Entry);
        }
        return Result;
    }

    std::vector<NullAbstractInterpretation> transfer(
        const llvm::Instruction& I) const;

    using Dir = Forwards;

    bool operator==(const NullAbstractInterpretation& Other) const
    {
        return State_ == Other.State_;
    }
};

static_assert(Fact<NullAbstractInterpretation>);