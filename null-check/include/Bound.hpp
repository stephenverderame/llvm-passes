#pragma once
#include <variant>

#include "external/bigint.h"
namespace bound
{
/**
 * @brief A bound on a range. Contains a value if the bound is known, otherwise,
 * the bound can be +inf or -inf.
 */
class Bound
{
    enum class Inf : uint8_t { NegInf, PosInf };
    /// Concrete value of the bound or +/-inf
    std::variant<bigint, Inf> V_;
    /// For debugging purposes, more easily display the value of this bound
    std::string DebugVal_;

  public:
    /**
     * @brief Returns the value of this bound. Requires the bound is not
     * infinite.
     */
    // NOLINTNEXTLINE(bugprone-unchecked-*)
    inline const auto& value() const { return std::get<bigint>(V_); }
    /**
     * @brief Determines if this bound has a value (not infinity).
     */
    inline bool hasValue() const { return std::holds_alternative<bigint>(V_); }

    friend Bound operator+(const Bound& A, const Bound& B);
    friend Bound operator-(const Bound& A, const Bound& B);
    friend Bound operator*(const Bound& A, const Bound& B);
    friend Bound operator/(const Bound& A, const Bound& B);
    friend Bound pow(const Bound& A, const Bound& B);
    friend Bound abs(const Bound& A);
    friend Bound min(const Bound& A, const Bound& B);
    friend Bound max(const Bound& A, const Bound& B);
    friend bool operator<(const Bound& A, const Bound& B);
    friend bool operator<=(const Bound& A, const Bound& B);
    friend bool operator>(const Bound& A, const Bound& B);
    friend bool operator>=(const Bound& A, const Bound& B);

    bool operator==(const Bound& Other) const = default;

    explicit Bound(bigint&& V) : DebugVal_(V.to_str()), V_(V) {}
    explicit Bound(const bigint& V) : V_(V), DebugVal_(V.to_str()) {}
    explicit Bound(Inf I) : V_(I), DebugVal_(I == Inf::NegInf ? "-inf" : "inf")
    {
    }
    explicit Bound(int64_t I) : V_(bigint(I)), DebugVal_(std::to_string(I)) {}
    explicit Bound(uint64_t I) : V_(bigint(I)), DebugVal_(std::to_string(I)) {}
    explicit Bound(int32_t I) : V_(bigint(I)), DebugVal_(std::to_string(I)) {}
    explicit Bound(uint32_t I) : V_(bigint(I)), DebugVal_(std::to_string(I)) {}

    static Bound makeNegInf() { return Bound(Inf::NegInf); }
    static Bound makePosInf() { return Bound(Inf::PosInf); }

    inline bool isNegInf() const
    {
        return std::holds_alternative<Inf>(V_) &&
               std::get<Inf>(V_) == Inf::NegInf;
    }

    inline bool isPosInf() const
    {
        return std::holds_alternative<Inf>(V_) &&
               std::get<Inf>(V_) == Inf::PosInf;
    }

    inline bool isNegative() const
    {
        return isNegInf() || (hasValue() && value() < bigint(0));
    }

    inline bool isPositive() const
    {
        return isPosInf() || (hasValue() && value() > bigint(0));
    }
};
/**
 * @brief `n ** B` if B has a value. If B is +/-inf, returns B.
 */
Bound pow(int n, const Bound& B);

/**
 * @brief `A + B` if both A and B have values. If A is +/-inf, returns A. Else
 * if B is +/-inf returns B.
 */
Bound operator+(const Bound& A, const Bound& B);

/**
 * @brief `A - B` if both A and B have values. If B is +inf, returns -inf and
 * vice versa. Otherwise, returns A.
 */
Bound operator-(const Bound& A, const Bound& B);

/**
 * @brief `A * B` if both A and B have values. If A is +/-inf, returns A. Else
 * if B is +/-inf returns B.
 */
Bound operator*(const Bound& A, const Bound& B);

/**
 * @brief `A / B` if both A and B have values. If A is +/-inf, returns A. Else
 * if B is +/-inf return B
 */
Bound operator/(const Bound& A, const Bound& B);

/**
 * @brief `A ** B` if both A and B have values. If A is +/-inf, returns A. Else
 * if B is +/-inf return B
 */
Bound pow(const Bound& A, const Bound& B);

/**
 * @brief `abs(A)` if both A has a value. If A is -inf, returns +inf and vice
 * versa.
 */
Bound abs(const Bound& A);

/**
 * @brief `min(A, B)` if both A and B have values.. If either bound is +/-inf,
 * returns A if A is -inf or B is +inf, otherwise returns B.
 */
Bound min(const Bound& A, const Bound& B);

/**
 * @brief `max(A, B)` if both A and B have values.. If either bound is +/-inf,
 * returns A if A is +inf or B is -inf, otherwise returns B.
 */
Bound max(const Bound& A, const Bound& B);

/**
 * @brief `A < B` if both A and B have values. Also true if A is -inf or B is
 * +inf
 */
bool operator<(const Bound& A, const Bound& B);

/**
 * @brief `A <= B` if both A and B have values.. Also true if A is -inf or B is
 * +inf
 */
bool operator<=(const Bound& A, const Bound& B);

/**
 * @brief `A > B` if both A and B have values.. Also true if A is +inf or B is
 * -inf
 */
bool operator>(const Bound& A, const Bound& B);

/**
 * @brief `A >= B` if both A and B have values.. Also true if A is +inf or B is
 * -inf
 */
bool operator>=(const Bound& A, const Bound& B);
}  // namespace bound