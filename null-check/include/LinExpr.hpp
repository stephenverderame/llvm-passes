#pragma once
#include <llvm-17/llvm/IR/Value.h>

#include <variant>
#include <vector>

#include "IntRange.hpp"

/**
 * @brief Newtype for a value or an integer.
 *
 */
struct AbstractInt {
    std::variant<const llvm::Value*, int64_t> V;

    explicit AbstractInt(const llvm::Value* V);
    explicit AbstractInt(int64_t V) : V(V) {}

    const bool isVal() const
    {
        return std::holds_alternative<const llvm::Value*>(V);
    }
    const bool isInt() const { return std::holds_alternative<int64_t>(V); }
    const llvm::Value* getVal() const
    {
        return std::get<const llvm::Value*>(V);
    }
    const int64_t getInt() const { return std::get<int64_t>(V); }
    const llvm::Value*& getVal() { return std::get<const llvm::Value*>(V); }
    int64_t& getInt() { return std::get<int64_t>(V); }

    bool operator==(const AbstractInt& Other) const = default;
};

/**
 * @brief An expression of a constant factor times an abstract integer.
 */
struct Term {
    int64_t Factor = 1;
    AbstractInt Val;

    explicit Term(AbstractInt Val) : Val(Val) {}
    Term(int64_t Factor, AbstractInt Val) : Factor(Factor), Val(Val) {}

    bool operator==(const Term& Other) const = default;
};

/**
 * @brief Gets a debug name for an abstract integer, either the name of the
 * value or the integer itself (as a constant).
 */
std::string getDebugName(const AbstractInt& Val);

/**
 * @brief A size of a pointer, represented as a base size and a list of offsets
 * which add to the base size.
 */
struct LinExpr {
    AbstractInt Base;
    std::vector<Term> Offsets;
    std::string DebugName;

    explicit LinExpr(AbstractInt Base)
        : Base(Base), DebugName(getDebugName(Base))
    {
    }
    LinExpr(AbstractInt Base, AbstractInt A, int64_t FactorB, AbstractInt B)
        : Base(Base), Offsets({Term(A), Term(FactorB, B)})
    {
        DebugName = getDebugName(Base) + " + " + getDebugName(A) + " + " +
                    std::to_string(FactorB) + " * " + getDebugName(B);
    }

    /**
     * @brief Adds another term to the linear expression
     */
    void addOffset(int64_t Factor, AbstractInt Val)
    {
        Offsets.push_back(Term(Factor, Val));
        DebugName += " + " + std::to_string(Factor) + " * " + getDebugName(Val);
    }

    bool operator==(const LinExpr& Other) const = default;
};
/**
 * @brief Returns true if the given abstract integer is the concrete integer
 * zero.
 */
bool isAbsIntZero(const AbstractInt& Val);
/**
 * @brief Gets the range of possible values that an abstract integer can take
 * on. May return an empty optional if the range is unbounded.
 *
 * @param V
 * @param IA
 * @return auto
 */
std::optional<IntRange> valToRange(const AbstractInt& V,
                                   const class IntervalAnalysis& IA);
/**
 * @brief Gets the range of possible values that a linear expression can take
 * on. May return an empty optional if any range in the expression is unbounded.
 *
 * @param Expr
 * @param IA
 * @return std::optional<IntRange>
 */
std::optional<IntRange> exprToRange(const LinExpr& Expr,
                                    const class IntervalAnalysis& IA);
LinExpr operator-(const LinExpr& A, const LinExpr& B);
LinExpr operator+(const LinExpr& A, const LinExpr& B);