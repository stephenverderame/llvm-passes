#include "LinExpr.hpp"

#include <llvm-17/llvm/IR/Constant.h>
#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/Instructions.h>

#include "IntervalAnalysis.hpp"

bool isAbsIntZero(const AbstractInt& Val)
{
    if (Val.isInt()) {
        return Val.getInt() == 0;
    } else {
        return false;
    }
}

AbstractInt::AbstractInt(const llvm::Value* V)
{
    if (const auto Const = dyn_cast<llvm::ConstantInt>(V); Const != nullptr) {
        this->V = Const->getSExtValue();
    } else if (V->getType()->isIntegerTy()) {
        this->V = V;
    } else {
        llvm_unreachable("valToAbstractInt called on non-integer type");
    }
}

LinExpr operator-(const LinExpr& A, const LinExpr& B)
{
    auto Result = A;
    if (!isAbsIntZero(B.Base)) {
        Result.DebugName += " - " + getDebugName(B.Base);
        Result.Offsets.push_back(Term(-1, B.Base));
    }
    for (const auto& Offset : B.Offsets) {
        if (!isAbsIntZero(Offset.Val) && Offset.Factor != 0) {
            Result.Offsets.push_back(Term(-Offset.Factor, Offset.Val));
            Result.DebugName += " - " + std::to_string(Offset.Factor) + " * " +
                                getDebugName(Offset.Val);
        }
    }
    return Result;
}

LinExpr operator+(const LinExpr& A, const LinExpr& B)
{
    auto Result = A;
    if (!isAbsIntZero(B.Base)) {
        Result.Offsets.push_back(Term(1, B.Base));
        Result.DebugName += " + " + getDebugName(B.Base);
    }
    for (const auto& Offset : B.Offsets) {
        if (!isAbsIntZero(Offset.Val) && Offset.Factor != 0) {
            Result.Offsets.push_back(Term(Offset.Factor, Offset.Val));
            Result.DebugName += " + " + std::to_string(Offset.Factor) + " * " +
                                getDebugName(Offset.Val);
        }
    }
    return Result;
}

std::optional<IntRange> valToRange(const AbstractInt& V,
                                   const IntervalAnalysis& IA)
{
    if (V.isInt()) {
        return std::make_optional(IntRange(bigint(V.getInt())));
    } else {
        const auto Val = V.getVal();
        if (const auto Const = dyn_cast<llvm::ConstantInt>(Val);
            Const != nullptr) {
            return std::make_optional(IntRange(bigint(Const->getSExtValue())));
        } else {
            return IA.getValRange(Val);
        }
    }
    llvm_unreachable("Invalid abstract integer");
}

std::optional<IntRange> exprToRange(const LinExpr& Expr,
                                    const IntervalAnalysis& IA)
{
    auto Result = valToRange(Expr.Base, IA);
    for (const auto& Offset : Expr.Offsets) {
        const auto OffsetRange = valToRange(Offset.Val, IA);
        if (OffsetRange.has_value() && Result.has_value()) {
            Result.value() = Result.value() + IntRange(bigint(Offset.Factor)) *
                                                  OffsetRange.value();
        } else {
            return {};
        }
    }
    return Result;
}

std::string getDebugName(const AbstractInt& Val)
{
    if (Val.isInt()) {
        return std::to_string(Val.getInt());
    } else {
        return getDebugName(Val.getVal());
    }
}