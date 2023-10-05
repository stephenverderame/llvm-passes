#include "IntRange.hpp"

using namespace bound;

namespace
{
Bound toUnsigned(const Bound& A, unsigned int BitWidth)
{
    // It would be better to have special inf and -inf values
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
}  // namespace

IntRange smallerRange(const IntRange& A, const IntRange& B)
{
    if (B.Upper - B.Lower < A.Upper - A.Lower) {
        return B;
    }
    return A;
}

// NOLINTNEXTLINE(readability-function-*)
LatticeElem<IntRange> adjustForCondition(const LatticeElem<IntRange>& LHS,
                                         const LatticeElem<IntRange>& RHS,
                                         uint64_t BitWidth,
                                         llvm::ICmpInst::Predicate Cond)
{
    // NOLINTNEXTLINE
    using namespace llvm;
    switch (Cond) {
        case ICmpInst::ICMP_SLT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toSigned(BitWidth);
                Res.Upper = RHS.value().toSigned(BitWidth).Upper - Bound(1);
                return LatticeElem<IntRange>(Res.fixLowerBound());
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_ULT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toUnsigned(BitWidth);
                Res.Upper = RHS.value().toUnsigned(BitWidth).Upper - Bound(1);
                return LatticeElem<IntRange>(Res.fixLowerBound());
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_SLE: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toSigned(BitWidth);
                Res.Upper = RHS.value().toSigned(BitWidth).Upper;
                return LatticeElem<IntRange>(Res.fixLowerBound());
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_ULE: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toUnsigned(BitWidth);
                Res.Upper = RHS.value().toUnsigned(BitWidth).Upper;
                return LatticeElem<IntRange>(Res.fixLowerBound());
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_SGE: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toSigned(BitWidth);
                Res.Lower = RHS.value().toSigned(BitWidth).Lower;
                return LatticeElem<IntRange>(Res.fixUpperBound());
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_UGE:
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toUnsigned(BitWidth);
                Res.Lower = RHS.value().toUnsigned(BitWidth).Lower;
                return LatticeElem<IntRange>(Res.fixUpperBound());
            } else {
                return LHS;
            }
        case ICmpInst::ICMP_SGT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toSigned(BitWidth);
                Res.Lower = RHS.value().toSigned(BitWidth).Lower + Bound(1);
                return LatticeElem<IntRange>(Res.fixUpperBound());
            } else {
                return LHS;
            }
        }
        case ICmpInst::ICMP_UGT: {
            if (LHS.isBottom()) {
                return RHS;
            } else if (LHS.hasValue() && RHS.hasValue()) {
                auto Res = LHS.value().toUnsigned(BitWidth);
                Res.Lower = RHS.value().toUnsigned(BitWidth).Lower + Bound(1);
                return LatticeElem<IntRange>(Res.fixUpperBound());
            } else {
                return LHS;
            }
        }
        default:
            return LHS;
    }
}

LatticeElem<Monotonic> monoMeet(const LatticeElem<Monotonic>& A,
                                const LatticeElem<Monotonic>& B)
{
    return LatticeElem<Monotonic>::meet(A, B, [](auto& A, auto& B) {
        if (A == B) {
            return LatticeElem<Monotonic>(A);
        } else {
            return LatticeElem<Monotonic>::makeBottom();
        }
    });
}

IntRange IntRange::meet(const IntRange& A, const IntRange& B)
{
    // If the two values have only been increasing, then if their lower
    // bounds don't match, we can set the lower bound to the max of the two
    // instead of `SmallEnough`. Likewise for the upper bound
    IntRange Res;
    Res.Monotonicity = monoMeet(A.Monotonicity, B.Monotonicity);
    const auto Mono = Res.Monotonicity.intoOptional();
    const auto UseNegInf =
        !Mono.has_value() || Mono.value() == Monotonic::Decreasing;
    const auto UsePosInf =
        !Mono.has_value() || Mono.value() == Monotonic::Increasing;
    Res.Lower = A.Lower == B.Lower
                    ? A.Lower
                    : (UseNegInf ? Bound::makeNegInf() : min(A.Lower, B.Lower));
    Res.Upper = A.Upper == B.Upper
                    ? A.Upper
                    : (UsePosInf ? Bound::makePosInf() : max(A.Upper, B.Upper));
    return Res;
}

IntRange IntRange::join(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    assert(A.Lower >= B.Lower && A.Upper <= B.Upper ||
           A.Lower <= B.Lower && A.Upper >= B.Upper);
    Res.Lower = max(A.Lower, B.Lower);
    Res.Upper = min(A.Upper, B.Upper);
    return Res;
}

IntRange operator*(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Monotonicity = monoMeet(A.Monotonicity, B.Monotonicity);
    if (!Res.Monotonicity.isBottom()) {
        if ((Res.Monotonicity.isTop() ||
             Res.Monotonicity.value() == Monotonic::Increasing) &&
            A.isPositive() && B.isPositive()) {
            Res.Monotonicity = LatticeElem<Monotonic>(Monotonic::Increasing);
        } else if ((Res.Monotonicity.isTop() ||
                    Res.Monotonicity.value() == Monotonic::Decreasing) &&
                   A.isNegative() && B.isNegative()) {
            Res.Monotonicity = LatticeElem<Monotonic>(Monotonic::Decreasing);
        } else {
            Res.Monotonicity = LatticeElem<Monotonic>::makeBottom();
        }
    }
    Res.Lower = A.Lower * B.Lower;
    Res.Upper = A.Upper * B.Upper;
    return Res;
}

IntRange operator+(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Monotonicity = monoMeet(A.Monotonicity, B.Monotonicity);
    if (!Res.Monotonicity.isBottom()) {
        if ((Res.Monotonicity.isTop() ||
             Res.Monotonicity.value() == Monotonic::Increasing) &&
            A.isNonNegative() && B.isNonNegative()) {
            Res.Monotonicity = LatticeElem<Monotonic>(Monotonic::Increasing);
        } else if ((Res.Monotonicity.isTop() ||
                    Res.Monotonicity.value() == Monotonic::Decreasing) &&
                   A.isNonPositive() && B.isNonPositive()) {
            Res.Monotonicity = LatticeElem<Monotonic>(Monotonic::Decreasing);
        } else {
            Res.Monotonicity = LatticeElem<Monotonic>::makeBottom();
        }
    }
    Res.Lower = A.Lower + B.Lower;
    Res.Upper = A.Upper + B.Upper;
    return Res;
}

IntRange operator-(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Monotonicity = LatticeElem<Monotonic>::makeBottom();
    Res.Lower = A.Lower - B.Upper;
    Res.Upper = A.Upper - B.Lower;
    return Res;
}

IntRange operator/(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Monotonicity = LatticeElem<Monotonic>::makeBottom();
    Res.Lower = A.Lower / B.Upper;
    Res.Upper = A.Upper / B.Lower;
    return Res;
}

IntRange IntRange::remainder(const IntRange& Other, bool Signed) const
{
    IntRange Res;
    Res.Monotonicity = LatticeElem<Monotonic>::makeBottom();
    auto AbsLower = abs(Other.Lower);
    auto AbsUpper = abs(Other.Upper);
    if (AbsUpper < AbsLower) {
        std::swap(AbsLower, AbsUpper);
    }
    if (!Signed) {
        Res.Lower = Bound(0);
        Res.Upper = AbsUpper;
    } else {
        Res.Lower = Bound(0) - AbsUpper;
        Res.Upper = AbsUpper;
    }
    return Res;
}

IntRange operator<<(const IntRange& A, const IntRange& B)
{
    IntRange Res;
    Res.Monotonicity =
        A.Monotonicity.isTop() ||
                A.Monotonicity == LatticeElem<Monotonic>(Monotonic::Increasing)
            ? LatticeElem<Monotonic>(Monotonic::Increasing)
            : LatticeElem<Monotonic>::makeBottom();
    Res.Lower = A.Lower * pow(2, B.Lower);
    Res.Upper = A.Upper * pow(2, B.Upper);
    return Res;
}

IntRange IntRange::toSigned(unsigned int BitWidth) const
{
    IntRange Res;
    const auto A = ::toSigned(Lower, BitWidth);
    const auto B = ::toSigned(Upper, BitWidth);
    Res.Monotonicity = Monotonicity;
    Res.Lower = min(A, B);
    Res.Upper = max(A, B);
    return Res;
}

IntRange IntRange::toUnsigned(unsigned int BitWidth) const
{
    IntRange Res;
    const auto A = ::toUnsigned(Lower, BitWidth);
    const auto B = ::toUnsigned(Upper, BitWidth);
    Res.Monotonicity = Monotonicity;
    Res.Lower = min(A, B);
    Res.Upper = max(A, B);
    return Res;
}

IntRange IntRange::pow(bigint&& Exponent) const
{
    IntRange Res;
    Res.Monotonicity = LatticeElem<Monotonic>::makeBottom();
    Res.Lower = bound::pow(Lower, bound::Bound(Exponent));
    Res.Upper = bound::pow(Upper, bound::Bound(Exponent));
    return Res;
}

IntRange IntRange::exponentiate(bigint&& Base) const
{
    IntRange Res;
    Res.Monotonicity = LatticeElem<Monotonic>::makeBottom();
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