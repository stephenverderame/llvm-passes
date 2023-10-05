#include "Bound.hpp"

namespace bound
{

/**
 * @brief Returns either A or B, whichever is not a value
 * starting with A, then B.
 *
 * @param A
 * @param B
 * @return Bound
 */
inline Bound nonValueOf(const Bound& A, const Bound& B)
{
    if (A.hasValue()) {
        return B;
    } else {
        return A;
    }
}

Bound operator+(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return Bound(A.value() + B.value());
    }
    return nonValueOf(A, B);
}

Bound operator-(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return Bound(A.value() - B.value());
    } else if (B.isNegInf()) {
        return Bound::makePosInf();
    } else if (B.isPosInf()) {
        return Bound::makeNegInf();
    } else {
        return A;
    }
}

Bound operator*(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return Bound(A.value() * B.value());
    }
    return nonValueOf(A, B);
}

Bound operator/(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return Bound(A.value() / B.value());
    }
    return nonValueOf(A, B);
}

Bound pow(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return Bound(bigint::_big_pow(A.value(), B.value()));
    }
    return nonValueOf(A, B);
}

Bound abs(const Bound& A)
{
    if (A.hasValue()) {
        return Bound(bigint::_big_abs(A.value()));
    } else if (A.isNegInf()) {
        return Bound::makePosInf();
    }
    return A;
}

Bound min(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return Bound(bigint::_big_min(A.value(), B.value()));
    } else if (A.isNegInf() || B.isPosInf()) {
        return A;
    } else {
        return B;
    }
}

Bound max(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return Bound(bigint::_big_max(A.value(), B.value()));
    } else if (A.isPosInf() || B.isNegInf()) {
        return A;
    } else {
        return B;
    }
}

Bound pow(int n, const Bound& B)
{
    if (B.hasValue()) {
        return Bound(bigint::_big_pow(bigint(n), B.value()));
    }
    return B;
}

bool operator>(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return A.value() > B.value();
    } else if (A.isPosInf() || B.isNegInf()) {
        return true;
    } else {
        return false;
    }
}

bool operator>=(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return A.value() >= B.value();
    } else if (A.isPosInf() || B.isNegInf()) {
        return true;
    } else {
        return false;
    }
}

bool operator<(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return A.value() < B.value();
    } else if (A.isNegInf() || B.isPosInf()) {
        return true;
    } else {
        return false;
    }
}

bool operator<=(const Bound& A, const Bound& B)
{
    if (A.hasValue() && B.hasValue()) {
        return A.value() <= B.value();
    } else if (A.isNegInf() || B.isPosInf()) {
        return true;
    } else {
        return false;
    }
}
}  // namespace bound
