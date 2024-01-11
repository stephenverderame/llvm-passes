#include "KnowledgeGraph.hpp"

#include <llvm-17/llvm/ADT/SmallVector.h>
#include <llvm-17/llvm/Analysis/CGSCCPassManager.h>
#include <llvm-17/llvm/IR/Constants.h>
#include <llvm-17/llvm/IR/DerivedTypes.h>
#include <llvm-17/llvm/IR/InstrTypes.h>
#include <llvm-17/llvm/IR/Instructions.h>
#include <llvm-17/llvm/IR/Value.h>
#include <llvm-17/llvm/Support/Casting.h>
#include <z3++.h>

#include <unordered_set>

namespace
{
llvm::ICmpInst::Predicate negatePredicate(llvm::ICmpInst::Predicate Pred)
{
    switch (Pred) {
        case llvm::ICmpInst::ICMP_EQ:
            return llvm::ICmpInst::ICMP_NE;
        case llvm::ICmpInst::ICMP_NE:
            return llvm::ICmpInst::ICMP_EQ;
        case llvm::ICmpInst::ICMP_UGT:
            return llvm::ICmpInst::ICMP_ULE;
        case llvm::ICmpInst::ICMP_UGE:
            return llvm::ICmpInst::ICMP_ULT;
        case llvm::ICmpInst::ICMP_ULT:
            return llvm::ICmpInst::ICMP_UGE;
        case llvm::ICmpInst::ICMP_ULE:
            return llvm::ICmpInst::ICMP_UGT;
        case llvm::ICmpInst::ICMP_SGT:
            return llvm::ICmpInst::ICMP_SLE;
        case llvm::ICmpInst::ICMP_SGE:
            return llvm::ICmpInst::ICMP_SLT;
        case llvm::ICmpInst::ICMP_SLT:
            return llvm::ICmpInst::ICMP_SGE;
        case llvm::ICmpInst::ICMP_SLE:
            return llvm::ICmpInst::ICMP_SGT;
        default:
            llvm_unreachable("Unknown predicate");
    }
}

auto cmpToRelation(const llvm::ICmpInst& Cmp)
{
    return std::make_pair(std::make_tuple(Cmp.getOperand(0), Cmp.getOperand(1)),
                          Cmp.getPredicate());
}

auto cmpToNegatedRelation(const llvm::ICmpInst& Cmp)
{
    return std::make_pair(std::make_tuple(Cmp.getOperand(0), Cmp.getOperand(1)),
                          negatePredicate(Cmp.getPredicate()));
}

auto valToZ3Name(const llvm::Value& V)
{
    std::stringstream ss;
    ss << "t." << std::hex << static_cast<const void*>(&V);
    return ss.str();
}

auto getZ3Var(const llvm::Value* V,
              std::unordered_map<const llvm::Value*, z3::expr>& Vars,
              z3::context& Ctx)
{
    if (const auto* Const = dyn_cast<llvm::ConstantInt>(V); Const != nullptr) {
        return Ctx.int_val(Const->getSExtValue());
    } else if (const auto it = Vars.find(V); it != Vars.end()) {
        return it->second;
    } else {
        auto NewVar = Ctx.int_const(valToZ3Name(*V).c_str());
        Vars.emplace(V, NewVar);
        return NewVar;
    }
}

auto genConstraint(llvm::CmpInst::Predicate P, z3::expr&& LHS, z3::expr&& RHS)
{
    switch (P) {
        case llvm::ICmpInst::ICMP_EQ:
            return LHS == RHS;
        case llvm::ICmpInst::ICMP_NE:
            return LHS != RHS;
        case llvm::ICmpInst::ICMP_UGT:
            return LHS > RHS;
        case llvm::ICmpInst::ICMP_UGE:
            return LHS >= RHS;
        case llvm::ICmpInst::ICMP_ULT:
            return LHS < RHS;
        case llvm::ICmpInst::ICMP_ULE:
            return LHS <= RHS;
        case llvm::ICmpInst::ICMP_SGT:
            return LHS > RHS;
        case llvm::ICmpInst::ICMP_SGE:
            return LHS >= RHS;
        case llvm::ICmpInst::ICMP_SLT:
            return LHS < RHS;
        case llvm::ICmpInst::ICMP_SLE:
            return LHS <= RHS;
        default:
            llvm_unreachable("Unknown predicate");
    }
}

std::optional<z3::expr> genBopConstraint(z3::expr&& LHS, z3::expr&& RHS,
                                         llvm::BinaryOperator::BinaryOps Op)
{
    switch (Op) {
        case llvm::BinaryOperator::BinaryOps::Add:
            return LHS + RHS;
        case llvm::BinaryOperator::BinaryOps::Sub:
            return LHS - RHS;
        case llvm::BinaryOperator::BinaryOps::Mul:
            return LHS * RHS;
        case llvm::BinaryOperator::BinaryOps::SDiv:
            return LHS / RHS;
        case llvm::BinaryOperator::BinaryOps::UDiv:
            return LHS / RHS;
        case llvm::BinaryOperator::BinaryOps::SRem:
            return LHS % RHS;
        case llvm::BinaryOperator::BinaryOps::URem:
            return LHS % RHS;
        default:
            return {};
    }
}

const char* bopToStr(llvm::BinaryOperator::BinaryOps Op)
{
    switch (Op) {
        case llvm::BinaryOperator::BinaryOps::Add:
            return "+";
        case llvm::BinaryOperator::BinaryOps::Sub:
            return "-";
        case llvm::BinaryOperator::BinaryOps::Mul:
            return "*";
        case llvm::BinaryOperator::BinaryOps::SDiv:
            return "/";
        case llvm::BinaryOperator::BinaryOps::UDiv:
            return "/";
        case llvm::BinaryOperator::BinaryOps::SRem:
            return "%";
        case llvm::BinaryOperator::BinaryOps::URem:
            return "%";
        default:
            llvm_unreachable("Unknown binary operator");
    }
}

std::optional<z3::expr> genUopConstraint(z3::expr&& Op,
                                         llvm::Instruction::UnaryOps UOp)
{
    switch (UOp) {
        case llvm::Instruction::UnaryOps::FNeg:
            return -Op;
        default:
            return {};
    }
}

/**
 * @brief Adds constraints to `V` based on the specified integer range.
 *
 * @param Range The range of values that `Inst` can take
 * @param V  The instruction to add the constraints to
 * @param Vars The map of variables to z3 variables
 * @param Constraints The vector of constraints to add to
 * @param Ctx The z3 context
 * @param DebugConstraints The vector of debug constraints to add to
 */
void addRangeConstraints(const std::optional<IntRange>& Range,
                         const llvm::Value* V,
                         std::unordered_map<const llvm::Value*, z3::expr>& Vars,
                         std::vector<z3::expr>& Constraints, z3::context& Ctx,
                         std::vector<std::string>& DebugConstraints)
{
    if (Range.has_value()) {
        if (Range->Lower.hasValue()) {
            const auto C =
                getDebugName(V) + " >= " + Range->Lower.value().to_str();
            if (std::find(DebugConstraints.begin(), DebugConstraints.end(),
                          C) == DebugConstraints.end()) {
                DebugConstraints.emplace_back(C);
                Constraints.emplace_back(
                    getZ3Var(V, Vars, Ctx) >=
                    Ctx.int_val(Range->Lower.value().to_str().c_str()));
            }
        }
        if (Range->Upper.hasValue()) {
            const auto C =
                getDebugName(V) + " <= " + Range->Upper.value().to_str();
            if (std::find(DebugConstraints.begin(), DebugConstraints.end(),
                          C) == DebugConstraints.end()) {
                DebugConstraints.emplace_back(C);
                Constraints.emplace_back(
                    getZ3Var(V, Vars, Ctx) <=
                    Ctx.int_val(Range->Upper.value().to_str().c_str()));
            }
        }
    }
}

/**
 * @brief Collects all variables which are dependended on
 * (transitive closure of all operands) for the definition of
 * the given instruction.
 *
 * @param I The instruction to collect the definitions for
 * @param Intervals The result of the interval analysis
 * @param Vars The map of variables to z3 variables
 * @param Constraints The vector of constraints to add to
 * @param Ctx The z3 context
 */
void collectDefinitions(const llvm::Instruction* I,
                        const DataFlowFacts<IntervalAnalysis>& Intervals,
                        std::unordered_map<const llvm::Value*, z3::expr>& Vars,
                        std::vector<z3::expr>& Constraints, z3::context& Ctx,
                        std::vector<std::string>& DebugConstraints)
{
    std::queue<const llvm::Instruction*> Worklist;
    std::unordered_set<const llvm::Instruction*> Visited;
    Worklist.push(I);
    while (!Worklist.empty()) {
        const auto Inst = Worklist.front();
        Worklist.pop();
        if (Visited.find(Inst) != Visited.end()) {
            continue;
        }
        Visited.emplace(Inst);
        if (Inst->getType()->isIntegerTy()) {
            if (const auto* BinOp = dyn_cast<llvm::BinaryOperator>(Inst);
                BinOp != nullptr) {
                if (const auto Constraint = genBopConstraint(
                        getZ3Var(BinOp->getOperand(0), Vars, Ctx),
                        getZ3Var(BinOp->getOperand(1), Vars, Ctx),
                        BinOp->getOpcode());
                    Constraint.has_value()) {
                    const auto C = getDebugName(BinOp) +
                                   " == " + getDebugName(BinOp->getOperand(0)) +
                                   " " + bopToStr(BinOp->getOpcode()) + " " +
                                   getDebugName(BinOp->getOperand(1));
                    if (std::find(DebugConstraints.begin(),
                                  DebugConstraints.end(),
                                  C) == DebugConstraints.end()) {
                        DebugConstraints.emplace_back(C);
                        Constraints.emplace_back(getZ3Var(BinOp, Vars, Ctx) ==
                                                 *Constraint);
                    }
                }
            } else if (const auto* Uop = dyn_cast<llvm::UnaryOperator>(Inst);
                       Uop != nullptr) {
                llvm_unreachable("Unknown unary operator");
            } else if (const auto* Cast = dyn_cast<llvm::CastInst>(Inst);
                       Cast != nullptr) {
                const auto C = getDebugName(Cast) +
                               " == " + getDebugName(Cast->getOperand(0));
                if (std::find(DebugConstraints.begin(), DebugConstraints.end(),
                              C) == DebugConstraints.end()) {
                    DebugConstraints.emplace_back(C);
                    Constraints.emplace_back(
                        getZ3Var(Cast, Vars, Ctx) ==
                        getZ3Var(Cast->getOperand(0), Vars, Ctx));
                }
            } else if (const auto* NextInst = Inst->getNextNode();
                       NextInst != nullptr) {
                const auto Range =
                    Intervals.InstructionInFacts.at(NextInst).getValRange(Inst);
                addRangeConstraints(Range, Inst, Vars, Constraints, Ctx,
                                    DebugConstraints);

            } else {
                llvm_unreachable("Instr is terminator");
            }
            for (const auto& Operand : Inst->operands()) {
                if (const auto* OpI = dyn_cast<llvm::Instruction>(Operand);
                    OpI != nullptr) {
                    Worklist.push(OpI);
                } else if (auto Range = Intervals.InstructionInFacts.at(Inst)
                                            .getValRange(Operand);
                           Range.has_value() && !Vars.contains(Operand)) {
                    addRangeConstraints(Range, Operand, Vars, Constraints, Ctx,
                                        DebugConstraints);
                }
            }
        }
    }
}

/**
 * @brief Adds definitons and constraints for values which `V` depends on.
 * If `V` is an instruction, then the definitions of `V` and its dependencies
 * are added to `Vars` and `Constraints` via `collectDefinitions`. If `V` is
 * not an instruction, then the range of `V` is added to `Constraints` via
 * `addRangeConstraints`.
 *
 * @param SrcI The instruction which `V` is used in. Where to get interval
 * information from.
 * @param V The value to add the constraints for
 * @param Intervals The result of the interval analysis
 * @param Vars The map of variables to z3 variables
 * @param Constraints The vector of constraints to add to
 * @param Ctx The z3 context
 * @param DebugConstraints The vector of constraint names
 */
void addConstraintsForVal(
    const llvm::Instruction* SrcI, const llvm::Value* V,
    const DataFlowFacts<IntervalAnalysis>& Intervals,
    std::unordered_map<const llvm::Value*, z3::expr>& Vars,
    std::vector<z3::expr>& Constraints, z3::context& Ctx,
    std::vector<std::string>& DebugConstraints)
{
    if (const auto* I = dyn_cast<llvm::Instruction>(V); I != nullptr) {
        collectDefinitions(I, Intervals, Vars, Constraints, Ctx,
                           DebugConstraints);
    } else {
        const auto Range = Intervals.InstructionInFacts.at(SrcI).getValRange(V);
        addRangeConstraints(Range, V, Vars, Constraints, Ctx, DebugConstraints);
    }
}

/**
 * @brief Collects all definitons and constraints for each term in the linear
 * expression `E`.
 *
 * @param I The instruction which `E` is used in. Where to get interval
 * information from.
 * @param E The linear expression to collect the definitions for
 * @param Intervals The result of the interval analysis
 * @param Vars The map of variables to z3 variables
 * @param Constraints The vector of constraints to add to
 * @param Ctx The z3 context
 * @param DebugConstraints The vector of constraint names
 */
void collectLinExprDefinitions(
    const llvm::Instruction* I, const LinExpr& E,
    const DataFlowFacts<IntervalAnalysis>& Intervals,
    std::unordered_map<const llvm::Value*, z3::expr>& Vars,
    std::vector<z3::expr>& Constraints, z3::context& Ctx,
    std::vector<std::string>& DebugConstraints)
{
    if (E.Base.isVal()) {
        addConstraintsForVal(I, E.Base.getVal(), Intervals, Vars, Constraints,
                             Ctx, DebugConstraints);
    }
    for (auto& O : E.Offsets) {
        if (O.Val.isVal()) {
            addConstraintsForVal(I, O.Val.getVal(), Intervals, Vars,
                                 Constraints, Ctx, DebugConstraints);
        }
    }
}

/**
 * @brief Converts an abstract integer into a z3 variable.
 * If the abstract integer is a constant, then the constant is converted to a
 * z3 constant. Otherwise, the value is converted to a z3 variable.
 *
 * @param V
 * @param Vars
 * @param Ctx
 * @return z3::expr
 */
z3::expr abstractIntToZ3Var(
    const AbstractInt& V,
    std::unordered_map<const llvm::Value*, z3::expr>& Vars, z3::context& Ctx)
{
    if (V.isInt()) {
        return Ctx.int_val(V.getInt());
    } else {
        const auto Val = V.getVal();
        if (const auto Const = dyn_cast<llvm::ConstantInt>(Val);
            Const != nullptr) {
            return Ctx.int_val(Const->getSExtValue());
        } else {
            return getZ3Var(Val, Vars, Ctx);
        }
    }
    llvm_unreachable("Invalid abstract integer");
}

/**
 * @brief Converts a linear expression into a z3 expression.
 *
 * @param E The linear expression to convert
 * @param Vars The map of variables to z3 variables
 * @param Ctx The z3 context
 * @return of the z3 expression and the debug name
 */
std::tuple<z3::expr, std::string> linExprToZ3Var(
    const LinExpr& E, std::unordered_map<const llvm::Value*, z3::expr>& Vars,
    z3::context& Ctx)
{
    z3::expr Result = abstractIntToZ3Var(E.Base, Vars, Ctx);
    std::string DebugResult = getDebugName(E.Base);
    for (const auto& O : E.Offsets) {
        const auto Offset = abstractIntToZ3Var(O.Val, Vars, Ctx);
        Result = Result + Offset * Ctx.int_val(O.Factor);
        DebugResult +=
            " + " + std::to_string(O.Factor) + " * " + getDebugName(O.Val);
    }
    return std::make_tuple(Result, DebugResult);
}

}  // namespace

RelationPropagation RelationPropagation::meet(const RelationPropagation& A,
                                              const RelationPropagation& B)
{
    if (A.isTop_) {
        return B;
    } else if (B.isTop_) {
        return A;
    }
    auto Result = makeNonTop();
    // meet is intersection
    for (const auto& [Args, Pred] : B.Relations_) {
        if (auto it = A.Relations_.find(Args); it != A.Relations_.end()) {
            if (it->second == Pred) {
                // meeting where relation between two values are
                // different remember: we're in ssa so this can only
                // occur as a result of different branches
                Result.Relations_.emplace(*it);
            }
        }
    }
    return Result;
}

TransferRetType<RelationPropagation> RelationPropagation::transfer(
    const llvm::Instruction& I,
    const DataFlowFacts<RelationPropagation>& Facts) const
{
    auto Res = *this;
    Res.isTop_ = false;
    if (const auto Branch = dyn_cast<llvm::BranchInst>(&I); Branch != nullptr) {
        return transferConditionDependentBranch(
            Res, Branch, Facts,
            [](const auto& Self, const auto* Cond, const auto& Facts)
                -> std::optional<
                    std::tuple<RelationPropagation, RelationPropagation>> {
                if (const llvm::ICmpInst* Cmp = dyn_cast<llvm::ICmpInst>(Cond);
                    Cmp != nullptr) {
                    if (Cmp->getType()->isIntegerTy()) {
                        const auto LHS = Cmp->getOperand(0);
                        auto TRes = Self;
                        TRes.Relations_.emplace(cmpToRelation(*Cmp));
                        auto FRes = Self;
                        FRes.Relations_.emplace(cmpToNegatedRelation(*Cmp));
                        return std::make_optional(std::make_tuple(TRes, FRes));
                    }
                }
                return std::nullopt;
            });
    }
    return Res;
}

QueryResult InequalitySolver::isAlwaysInRange(const llvm::Instruction* I,
                                              const LinExpr& LHS,
                                              const LinExpr& RHS) const
{
    z3::context Ctx;
    const auto& Relations = RelationFacts_.get().InstructionInFacts.at(I);
    std::unordered_map<const llvm::Value*, z3::expr> Vars;
    std::vector<z3::expr> Constraints;
    std::vector<std::string> DebugConstraints;
    collectLinExprDefinitions(I, LHS, IntervalFacts_, Vars, Constraints, Ctx,
                              DebugConstraints);
    collectLinExprDefinitions(I, RHS, IntervalFacts_, Vars, Constraints, Ctx,
                              DebugConstraints);
    const auto [LHSZ3, LHSName] = linExprToZ3Var(LHS, Vars, Ctx);
    const auto [RHSZ3, RHSName] = linExprToZ3Var(RHS, Vars, Ctx);
    for (auto [Args, Pred] : Relations.getRelations()) {
        const auto [LHSArg, RHSArg] = Args;
        if (Vars.contains(LHSArg) || Vars.contains(RHSArg)) {
            addConstraintsForVal(I, LHSArg, IntervalFacts_, Vars, Constraints,
                                 Ctx, DebugConstraints);
            addConstraintsForVal(I, RHSArg, IntervalFacts_, Vars, Constraints,
                                 Ctx, DebugConstraints);
            Constraints.emplace_back(
                genConstraint(Pred, getZ3Var(LHSArg, Vars, Ctx),
                              getZ3Var(RHSArg, Vars, Ctx)));
            DebugConstraints.emplace_back(
                getDebugName(LHSArg) + " " +
                llvm::CmpInst::getPredicateName(Pred).str() + " " +
                getDebugName(RHSArg));
        }
    }
    z3::solver Solver(Ctx);
    for (const auto& Constraint : Constraints) {
        Solver.add(Constraint);
    }
    // forall A, P(A) <=> !exists B, !P(B)
    Solver.add(LHSZ3 >= RHSZ3 || LHSZ3 < 0);
    const auto Err = Solver.check_error();
    const auto Ret = Solver.check();
    if (Ret == z3::sat) {
        const auto M = Solver.get_model();
        std::unordered_map<const llvm::Value*, std::string> Assignments;
        for (const auto& [K, V] : Vars) {
            Assignments.emplace(K, M.eval(V).to_string());
        }
        std::unordered_map<std::string, std::string> DebugAssignments;
        for (const auto& [K, V] : Assignments) {
            DebugAssignments.emplace(getDebugName(K), V);
        }
        return {false, Assignments};
    }
    return {Ret == z3::unsat, {}};
}
