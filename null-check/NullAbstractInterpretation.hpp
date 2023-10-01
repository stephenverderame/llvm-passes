#pragma once
#include <llvm-17/llvm/IR/Value.h>

#include <map>
#include <set>

#include "df/DataFlow.hpp"

enum class PtrAbstractValue {
    Null,
    NonNull,
};

class NullAbstractInterpretation
{
    /// The state of the analysis.
    std::map<const llvm::Value*, PtrAbstractValue> State_;
    /// The set of values that have unknown values.
    std::set<const llvm::Value*> BottomValues_;

  public:
    static auto meet(const NullAbstractInterpretation& A,
                     const NullAbstractInterpretation& B)
    {
        auto Result = A;
        for (const auto& [Key, Value] : B.State_) {
            if (Result.State_.contains(Key)) {
                if (Value != Result.State_[Key]) {
                    Result.BottomValues_.insert(Key);
                }
            } else {
                Result.State_.emplace(Key, Value);
            }
        }
        for (const auto& Bottomed : B.BottomValues_) {
            Result.BottomValues_.insert(Bottomed);
        }
        for (const auto& BottomedKey : Result.BottomValues_) {
            Result.State_.erase(BottomedKey);
        }
        return Result;
    }

    auto transfer(const llvm::Instruction& /* I */) const
    {
        return std::vector{*this};
    }

    using Dir = Forwards;

    bool operator==(const NullAbstractInterpretation& Other) const
    {
        return State_ == Other.State_ && BottomValues_ == Other.BottomValues_;
    }
};

static_assert(Fact<NullAbstractInterpretation>);