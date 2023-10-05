#pragma once
#include <optional>
/**
 * @brief An element in a complete lattice.
 *
 * @tparam T
 */
template <typename T>
class LatticeElem
{
    enum Kind { Top, Inhabited, Bottom };

    std::optional<T> Fact_;
    Kind K_;

    explicit LatticeElem(Kind K) : K_(K) {}

  public:
    explicit LatticeElem(const T& Fact) : Fact_(Fact), K_(Kind::Inhabited) {}
    explicit LatticeElem(T&& Fact) : Fact_(std::move(Fact)), K_(Kind::Inhabited)
    {
    }
    /// @brief Create a top fact
    LatticeElem() : K_(Kind::Top) {}

    static LatticeElem makeTop() { return LatticeElem(); }
    static LatticeElem makeBottom() { return LatticeElem(Kind::Bottom); }

    bool isTop() const { return K_ == Kind::Top; }
    bool isBottom() const { return K_ == Kind::Bottom; }
    bool hasValue() const { return K_ == Kind::Inhabited; }
    T& value() { return Fact_.value(); }
    const T& value() const { return Fact_.value(); }

    bool operator==(const LatticeElem& Other) const = default;

    /// @brief Makes this fact bottom
    void bottomOut()
    {
        K_ = Kind::Bottom;
        Fact_ = std::nullopt;
    }

    /**
     * @brief Meets two individual lattice element (greatest lower bound).
     * If either is bottom, returns bottom. If either is top, returns the other.
     * Otherwise, returns the function applied to the two facts.
     *
     * @tparam Meet callable object to perform the meet of two inhabited facts
     * @param A The first fact
     * @param B The second fact
     * @param M The meet function, which must be callable with two facts and
     * return a new fact
     * @return a new lattice element that represents the meet of the two lattice
     * elements
     */
    template <typename Meet>
    requires std::is_invocable_v<Meet, const T&, const T&>
    static LatticeElem meet(const LatticeElem& A, const LatticeElem& B,
                            Meet&& M)
    {
        if (A.isBottom() || B.isBottom()) {
            return makeBottom();
        }
        if (A.isTop()) {
            return B;
        } else if (B.isTop()) {
            return A;
        }
        if constexpr (std::is_convertible_v<decltype(M(A.value(), B.value())),
                                            LatticeElem>) {
            return M(A, B);
        } else {
            return LatticeElem(M(A.value(), B.value()));
        }
    }

    /**
     * @brief Joins two individual lattice element (least upper bound).
     * If either is top, returns top. If either is bottom, returns the other.
     * Otherwise, returns the function applied to the two facts.
     *
     * @tparam Meet callable object to perform the meet of two inhabited facts
     * @param A The first fact
     * @param B The second fact
     * @param M The meet function, which must be callable with two facts and
     * return a new fact
     * @return a new lattice element that represents the meet of the two lattice
     * elements
     */
    template <typename Join>
    requires std::is_invocable_v<Join, const T&, const T&>
    static LatticeElem join(const LatticeElem& A, const LatticeElem& B,
                            Join&& M)
    {
        if (A.isTop() || B.isTop()) {
            return makeTop();
        }
        if (A.isBottom()) {
            return B;
        } else if (B.isBottom()) {
            return A;
        }
        if constexpr (std::is_convertible_v<decltype(M(A.value(), B.value())),
                                            LatticeElem>) {
            return M(A, B);
        } else {
            return LatticeElem(M(A.value(), B.value()));
        }
    }

    /**
     * @brief Constructs a new lattice element by applying a function to the
     * existing fact, if it is inhabited (not top or bottom)
     *
     * @tparam F The function type which must accept a const reference to the
     * underlying fact and return a new fact
     * @return requires
     */
    template <typename F>
    requires std::is_invocable_v<F, const T&>
    LatticeElem apply(F&& Func) const
    {
        if (isBottom() || isTop()) {
            return *this;
        }
        return LatticeElem(Func(Fact_.value()));
    }

    /**
     * @brief Returns an optional which contains a fact if
     * the lattice element is inhabited (not top or bottom)
     *
     * @return std::optional<T>
     */
    std::optional<T> intoOptional() const
    {
        if (isBottom() || isTop()) {
            return std::nullopt;
        }
        return Fact_;
    }
};