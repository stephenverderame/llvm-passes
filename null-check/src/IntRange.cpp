#include "IntRange.hpp"

#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/Support/raw_ostream.h>

#include "Bound.hpp"
#include "df/LatticeElem.hpp"

using namespace bound;

namespace
{
Bound toUnsigned(const Bound& A, unsigned int BitWidth)
{
    if (A.isNegInf()) {
        return Bound(0);
    } else if (A < Bound(0)) {
        return A + Bound(bigint::_big_pow(bigint(2), bigint(BitWidth)));
    }
    return A;
}

Bound toSigned(const Bound& A, unsigned int BitWidth)
{
    if (A.isPosInf()) {
        return A;
    } else if (A >= Bound(bigint::_big_pow(bigint(2), bigint(BitWidth - 1)))) {
        return A - Bound(bigint::_big_pow(bigint(2), bigint(BitWidth)));
    }
    return A;
}
/**
 * @brief Computes `LHS Cond RHS` when `LHS` is bottom and `RHS` is not top.
 *
 * @param RHS
 * @param Cond
 * @return LatticeElem<IntRange>
 */
LatticeElem<IntRange> opWhenLhsBottom(const LatticeElem<IntRange>& RHS,
                                      llvm::ICmpInst::Predicate Cond)
{
    if (RHS.isBottom()) {
        return RHS;
    }
    assert(!RHS.isTop());
    auto Res = RHS.value();
    switch (Cond) {
        case llvm::ICmpInst::ICMP_SLE:
            Res.Lower = bound::Bound::makeNegInf();
            break;
        case llvm::ICmpInst::ICMP_SLT:
            Res.Upper = Res.Upper - Bound(1);
            Res.Lower = bound::Bound::makeNegInf();
            break;
        case llvm::ICmpInst::ICMP_SGE:
            Res.Upper = bound::Bound::makePosInf();
            break;
        case llvm::ICmpInst::ICMP_SGT:
            Res.Lower = Res.Lower + Bound(1);
            Res.Upper = bound::Bound::makePosInf();
            break;
        case llvm::ICmpInst::ICMP_ULT:
            Res.Lower = bound::Bound(0);
            Res.Upper = Res.Upper - Bound(1);
            break;
        case llvm::ICmpInst::ICMP_ULE:
            Res.Lower = bound::Bound(0);
            Res.Upper = Res.Upper;
            break;
        case llvm::ICmpInst::ICMP_UGE:
            Res.Lower = bound::Bound(0);
            Res.Upper = bound::Bound::makePosInf();
            break;
        case llvm::ICmpInst::ICMP_UGT:
            Res.Lower = Res.Lower + Bound(1);
            Res.Upper = bound::Bound::makePosInf();
            break;
        default:
            assert(false);
    }
    return LatticeElem<IntRange>(Res);
}

/**
 * @brief Helper function for `adjustForCondition`. This function factors out
 * repeated bottom and top handling logic.
 *
 * If LHS and RHS has values, then the function returns the result of `Fn(LHS,
 * RHS)` after setting the mutated flag to temporary and setting the previous
 * range to LHS.
 *
 * @tparam Func function type of (IntRange, IntRange) -> IntRange
 * @param LHS the lattice element for the left hand side of the comparison
 * @param RHS the lattice element for the right hand side of the comparison
 * @param Cond the condition of the comparison
 * @param Fn a function which takes two IntRange's and returns a new IntRange
 * that can be assumed to be the range of `LHS` when `Cond` is true
 * @return LatticeElem<IntRange>
 */
template <typename Func>
LatticeElem<IntRange> adjustForConditionHelper(const LatticeElem<IntRange>& LHS,
                                               const LatticeElem<IntRange>& RHS,
                                               llvm::ICmpInst::Predicate Cond,
                                               Func&& Fn)
{
    if (LHS.isBottom() && !RHS.isTop()) {
        return opWhenLhsBottom(RHS, Cond);
    } else if (LHS.hasValue() && RHS.hasValue()) {
        auto Res = Fn(LHS, RHS);
        if (Res.Lower > Res.Upper) {
            // if the bound is inconsistent, return top
            // this occurs when, given the current information,
            // the branch could not be taken

            // so we return top so that a future iteration can correct this
            // with more updated information
            return LatticeElem<IntRange>::makeTop();
        }
        return LatticeElem(Res);
    } else if (LHS.isTop()) {
        return RHS;
    } else {
        return LHS;
    }
}

}  // namespace
// NOLINTNEXTLINE(readability-function-*)
LatticeElem<IntRange> adjustForCondition(const LatticeElem<IntRange>& A,
                                         const LatticeElem<IntRange>& B,
                                         uint64_t BitWidth,
                                         llvm::ICmpInst::Predicate Cond)
{
    // NOLINTNEXTLINE
    using namespace llvm;
    switch (Cond) {
        case ICmpInst::ICMP_SLT:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toSigned(BitWidth);
                    Res.Upper =
                        min(Res.Upper,
                            RHS.value().toSigned(BitWidth).Upper - Bound(1));
                    return Res;
                });
        case ICmpInst::ICMP_ULT:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toUnsigned(BitWidth);
                    Res.Upper =
                        min(Res.Upper,
                            RHS.value().toUnsigned(BitWidth).Upper - Bound(1));
                    return Res;
                });
        case ICmpInst::ICMP_SLE:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toSigned(BitWidth);
                    Res.Upper =
                        min(Res.Upper, RHS.value().toSigned(BitWidth).Upper);
                    return Res;
                });
        case ICmpInst::ICMP_ULE:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toUnsigned(BitWidth);
                    Res.Upper =
                        min(Res.Upper, RHS.value().toUnsigned(BitWidth).Upper);
                    return Res;
                });
        case ICmpInst::ICMP_SGE:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toSigned(BitWidth);
                    Res.Lower =
                        max(Res.Lower, RHS.value().toSigned(BitWidth).Lower);
                    return Res;
                });
        case ICmpInst::ICMP_UGE:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toUnsigned(BitWidth);
                    Res.Lower =
                        max(Res.Lower, RHS.value().toUnsigned(BitWidth).Lower);
                    return Res;
                });
        case ICmpInst::ICMP_SGT:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toSigned(BitWidth);
                    Res.Lower =
                        max(Res.Lower,
                            RHS.value().toSigned(BitWidth).Lower + Bound(1));
                    return Res;
                });
        case ICmpInst::ICMP_UGT:
            return adjustForConditionHelper(
                A, B, Cond, [BitWidth](auto LHS, auto RHS) {
                    auto Res = LHS.value().toUnsigned(BitWidth);
                    Res.Lower =
                        max(Res.Lower,
                            RHS.value().toUnsigned(BitWidth).Lower + Bound(1));
                    return Res;
                });
        default:
            return A;
    }
}

IntRange smallerRange(const IntRange& A, const IntRange& B)
{
    if (B.Upper - B.Lower < A.Upper - A.Lower) {
        return B;
    }
    return A;
}
namespace
{
/**
 * @brief Set the Upper Lower Bounds of `Res` which is the meet of `A` and `B`.
 *
 * The idea is as follows, if a bound is not equal, then we can set it to the
 * min or max of the two bounds. However, this does not work if we are
 * dealing with loops. To handle this case, `A` and `B` will have a mutated
 * flag set to `Mutation::Mutated` if they have been mutated. If the mutated
 * value has a smaller lower bound, then make the new lower bound negative
 * infinity because we are dealing with a loop. Likewise for the upper bound.
 *
 * @param Res Input/output parameter. The result of the meet of `A` and `B`.
 * @param A The first range
 * @param B The second range
 * @param Mono The meeted monotonicity of `A` and `B`
 * @return IntRange&
 */
IntRange setUpperLowerBounds(IntRange&& Res, const IntRange& A,
                             const IntRange& B)
{
    if (A.Lower != B.Lower) {
        if (A.Mutated && A.Lower < B.Lower || B.Mutated && B.Lower < A.Lower) {
            // If the mutated value has a smaller lower bound, then
            // make the new lower bound negative infinity.
            // use case: loops
            Res.Lower = Bound::makeNegInf();
        } else {
            Res.Lower = min(A.Lower, B.Lower);
        }
    } else {
        Res.Lower = A.Lower;
    }
    if (A.Upper != B.Upper) {
        if (A.Mutated && A.Upper > B.Upper || B.Mutated && B.Upper > A.Upper) {
            Res.Upper = Bound::makePosInf();
        } else {
            Res.Upper = max(A.Upper, B.Upper);
        }
    } else {
        Res.Upper = A.Upper;
    }
    return Res;
}
}  // namespace

LatticeElem<IntRange> IntRange::meet(const IntRange& A, const IntRange& B)
{
    if (A == B) {
        return LatticeElem(A);
    }
    IntRange Res;
    Res = setUpperLowerBounds(std::move(Res), A, B);
    Res.Mutated = false;
    return LatticeElem(Res);
}

IntRange operator*(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower * B.Lower;
    Res.Upper = A.Upper * B.Upper;
    return Res;
}

IntRange operator+(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower + B.Lower;
    Res.Upper = A.Upper + B.Upper;
    return Res;
}

IntRange operator-(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower - B.Upper;
    Res.Upper = A.Upper - B.Lower;
    return Res;
}

IntRange operator/(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower / B.Upper;
    Res.Upper = A.Upper / B.Lower;
    return Res;
}

IntRange IntRange::remainder(const IntRange& Other, bool Signed) const
{
    IntRange Res;
    if (!Signed) {
        auto AbsLower = abs(Other.Lower);
        auto AbsUpper = abs(Other.Upper);
        if (AbsUpper < AbsLower) {
            std::swap(AbsLower, AbsUpper);
        }
        Res.Lower = Bound(0);
        Res.Upper = AbsUpper - Bound(1);
    } else {
        const auto LL = Lower % Other.Lower;
        const auto LU = Lower % Other.Upper;
        const auto UL = Upper % Other.Lower;
        const auto UU = Upper % Other.Upper;
        Res.Lower = min({LL, LU, UL, UU});
        Res.Upper = max({LL, LU, UL, UU});
    }
    return Res;
}

IntRange operator<<(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Lower = A.Lower * pow(2, B.Lower);
    Res.Upper = A.Upper * pow(2, B.Upper);
    return Res;
}

IntRange IntRange::toSigned(unsigned int BitWidth) const
{
    IntRange Res = *this;
    const auto A = ::toSigned(Lower, BitWidth);
    const auto B = ::toSigned(Upper, BitWidth);
    Res.Lower = min(A, B);
    Res.Upper = max(A, B);
    return Res;
}

IntRange IntRange::toUnsigned(unsigned int BitWidth) const
{
    IntRange Res = *this;
    const auto A = ::toUnsigned(Lower, BitWidth);
    const auto B = ::toUnsigned(Upper, BitWidth);
    Res.Lower = min(A, B);
    Res.Upper = max(A, B);
    return Res;
}

IntRange IntRange::pow(bigint&& Exponent) const
{
    IntRange Res;
    Res.Lower = bound::pow(Lower, bound::Bound(Exponent));
    Res.Upper = bound::pow(Upper, bound::Bound(Exponent));
    return Res;
}

IntRange IntRange::exponentiate(bigint&& Base) const
{
    IntRange Res;
    Res.Lower = bound::pow(Bound(Base), Lower);
    Res.Upper = bound::pow(Bound(Base), Upper);
    return Res;
}

IntRange IntRange::fixLowerBound() const
{
    auto Res = *this;
    if (Res.Lower > Res.Upper) {
        Res.Lower = Res.Upper;
    }
    return Res;
}

IntRange IntRange::fixUpperBound() const
{
    auto Res = *this;
    if (Res.Lower > Res.Upper) {
        Res.Upper = Res.Lower;
    }
    return Res;
}

bound::Bound IntRange::size() const
{
    if (Lower.isNegInf() || Upper.isPosInf()) {
        return bound::Bound::makePosInf();
    }
    return Upper - Lower;
}

llvm::raw_ostream& operator<<(llvm::raw_ostream& Stream, const IntRange& R)
{
    const auto Lower = R.Lower.hasValue() ? R.Lower.value().to_str() : "-inf";
    const auto Upper = R.Upper.hasValue() ? R.Upper.value().to_str() : "+inf";
    Stream << "[" << Lower << ", " << Upper << "]";
    return Stream;
}