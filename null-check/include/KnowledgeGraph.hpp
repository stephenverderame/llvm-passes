#include <llvm-17/llvm/IR/Instructions.h>

#include <unordered_map>
#include <vector>

#include "IntervalAnalysis.hpp"
#include "df/DataFlow.hpp"

namespace std
{
template <>
struct hash<tuple<const llvm::Value*, const llvm::Value*>> {
    size_t operator()(
        const tuple<const llvm::Value*, const llvm::Value*>& T) const
    {
        return hash<ssize_t>()(hash<const llvm::Value*>()(get<0>(T)) ^
                               hash<const llvm::Value*>()(get<1>(T)));
    }
};
}  // namespace std

/**
 * @brief Analysis Pass which collects known relations between integral values
 * as determined by conditional branches.
 */
class RelationPropagation
{
  private:
    /// @brief  Mapping from (LHS, RHS) to the predicate of the relation
    std::unordered_map<std::tuple<const llvm::Value*, const llvm::Value*>,
                       llvm::CmpInst::Predicate>
        Relations_;
    bool isTop_ = true;

    /**
     * @brief Constructs a new RelationPropagation object which is not TOP.
     */
    static RelationPropagation makeNonTop()
    {
        RelationPropagation Result;
        Result.isTop_ = false;
        return Result;
    }

  public:
    using Dir = Forwards;

    [[nodiscard]] static RelationPropagation meet(const RelationPropagation& A,
                                                  const RelationPropagation& B);

    [[nodiscard]] TransferRetType<RelationPropagation> transfer(
        const llvm::Instruction& I,
        const DataFlowFacts<RelationPropagation>& Facts) const;

    bool operator==(const RelationPropagation& Other) const = default;

    const auto& getRelations() const { return Relations_; }
};

static_assert(Fact<RelationPropagation>);

/**
 * @brief Solves symbolic inequalities between integral values.
 */
class InequalitySolver
{
  private:
    std::reference_wrapper<const DataFlowFacts<IntervalAnalysis>>
        IntervalFacts_;
    std::reference_wrapper<const DataFlowFacts<RelationPropagation>>
        RelationFacts_;

  public:
    InequalitySolver(const DataFlowFacts<IntervalAnalysis>& IntervalFacts,
                     const DataFlowFacts<RelationPropagation>& RelationFacts)
        : IntervalFacts_(IntervalFacts), RelationFacts_(RelationFacts)
    {
    }
    /**
     * @brief Determines if `LHS < RHS && LHS >= 0` is always true at the given
     * instruction. If this function returns false, then given the constraints
     * of the values as determined by the definitons and static interval
     * analysis, there exists an assignment of values such that `LHS < RHS` is
     * false.
     *
     * @param I The instruction where we want to check if the inequality is true
     * @param LHS The left hand side of the inequality
     * @param RHS The right hand side of the inequality
     * @return true if `LHS < RHS` is always true at `I`
     */
    bool isAlwaysInRange(const llvm::Instruction* I, const llvm::Value* LHS,
                         std::variant<const llvm::Value*, bigint> RHS) const;
};