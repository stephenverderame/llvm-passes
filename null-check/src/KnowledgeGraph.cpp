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
 * @brief Gets the display name of the RHS value of `isAlwaysInRange`
 * @param RHS
 * @return std::string
 */
std::string RHSName(const std::variant<const llvm::Value*, bigint>& RHS)
{
    if (const auto RHSVal = std::get_if<const llvm::Value*>(&RHS);
        RHSVal != nullptr) {
        return getDebugName(*RHSVal);
    } else {
        return std::get<bigint>(RHS).to_str();
    }
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

bool InequalitySolver::isAlwaysInRange(
    const llvm::Instruction* I, const llvm::Value* LHS,
    std::variant<const llvm::Value*, bigint> RHS) const
{
    z3::context Ctx;
    const auto& Relations = RelationFacts_.get().InstructionInFacts.at(I);
    std::unordered_map<const llvm::Value*, z3::expr> Vars;
    std::vector<z3::expr> Constraints;
    std::vector<std::string> DebugConstraints;
    if (const auto* LHSI = dyn_cast<llvm::Instruction>(LHS); LHSI != nullptr) {
        collectDefinitions(LHSI, IntervalFacts_, Vars, Constraints, Ctx,
                           DebugConstraints);
    }
    const auto RHSZ3 = [&]() {
        if (const auto RHSVal = std::get_if<const llvm::Value*>(&RHS);
            RHSVal != nullptr) {
            if (const auto* RHSI = dyn_cast<llvm::Instruction>(*RHSVal);
                RHSI != nullptr) {
                collectDefinitions(RHSI, IntervalFacts_, Vars, Constraints, Ctx,
                                   DebugConstraints);
            }
            return getZ3Var(*RHSVal, Vars, Ctx);
        } else {
            return Ctx.int_val(std::get<bigint>(RHS).to_str().c_str());
        }
    }();
    const auto LHSZ3 = getZ3Var(LHS, Vars, Ctx);
    if (const auto RHSVal = std::get_if<const llvm::Value*>(&RHS);
        RHSVal != nullptr) {
    }
    for (auto [Args, Pred] : Relations.getRelations()) {
        const auto [LHSArg, RHSArg] = Args;
        if (Vars.contains(LHSArg) || Vars.contains(RHSArg)) {
            if (const auto LHSArgI = dyn_cast<llvm::Instruction>(LHSArg);
                LHSArgI != nullptr) {
                collectDefinitions(LHSArgI, IntervalFacts_, Vars, Constraints,
                                   Ctx, DebugConstraints);
            }
            if (const auto RHSArgI = dyn_cast<llvm::Instruction>(RHSArg);
                RHSArgI != nullptr) {
                collectDefinitions(RHSArgI, IntervalFacts_, Vars, Constraints,
                                   Ctx, DebugConstraints);
            }
            Constraints.emplace_back(
                genConstraint(Pred, getZ3Var(LHSArg, Vars, Ctx),
                              getZ3Var(RHSArg, Vars, Ctx)));
            DebugConstraints.emplace_back(
                getDebugName(LHSArg) + " " +
                llvm::CmpInst::getPredicateName(Pred).str() + " " +
                getDebugName(RHSArg));
        }
    }
    const auto DebugTest = getDebugName(LHS) + " >= " + RHSName(RHS) + " || " +
                           getDebugName(LHS) + " < 0";
    z3::solver Solver(Ctx);
    for (const auto& Constraint : Constraints) {
        Solver.add(Constraint);
    }
    // forall A, P(A) <=> !exists B, !P(B)
    Solver.add(LHSZ3 >= RHSZ3 || LHSZ3 < 0);
    const auto Err = Solver.check_error();
    const auto Ret = Solver.check();
    if (Ret == z3::sat) {
        const auto DebugModel = Solver.get_model();
    }
    return Ret == z3::unsat;
}
