

#include <llvm-17/llvm/IR/InstrTypes.h>
#include <llvm-17/llvm/IR/Instruction.h>
#include <llvm-17/llvm/IR/PassManager.h>
#include <llvm-17/llvm/Passes/PassBuilder.h>
#include <llvm-17/llvm/Passes/PassPlugin.h>
#include <llvm-17/llvm/Support/Casting.h>
#include <llvm-17/llvm/Support/CommandLine.h>
#include <llvm-17/llvm/Transforms/Scalar/LoopPassManager.h>

#include <exception>
#include <filesystem>
#include <sstream>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "df/DataFlow.hpp"

// NOLINTNEXTLINE
using namespace llvm;

namespace
{

std::string getName(const Value* V)
{
    std::string Str;
    raw_string_ostream Stream(Str);
    V->printAsOperand(Stream, false);
    return Str;
}

// NOLINTNEXTLINE
auto getBasicIVs(const Loop* L)
{
    std::unordered_set<const Value*> BasicIVs;
    for (llvm::BasicBlock* BB : L->getBlocks()) {
        for (llvm::Instruction& I : *BB) {
            if (auto* BinOp = dyn_cast<BinaryOperator>(&I)) {
                if (BinOp->getOpcode() != Instruction::Add &&
                    BinOp->getOpcode() != Instruction::Sub) {
                    continue;
                }
                const auto IsAdd = BinOp->getOpcode() == Instruction::Add;
                const Value* LHS = BinOp->getOperand(0);
                const Value* RHS = BinOp->getOperand(1);

                if (L->isLoopInvariant(LHS) && IsAdd) {
                    // LHS is loop invariant
                    // RHS must be a phi node that contains BinOp
                    if (auto* PhiNode = llvm::dyn_cast<llvm::PHINode>(RHS)) {
                        // NOLINTNEXTLINE(readability-identifier-naming)
                        for (unsigned i = 0;
                             i < PhiNode->getNumIncomingValues(); ++i) {
                            Value* IncomingValue = PhiNode->getIncomingValue(i);
                            if (auto Instr = dyn_cast<llvm::Instruction>(
                                    IncomingValue)) {
                                if (Instr == BinOp) {
                                    BasicIVs.insert(RHS);
                                }
                            }
                        }
                    }
                } else if (L->isLoopInvariant(RHS)) {
                    // RHS is loop invariant
                    // LHS must be a phi node that contains BinOp
                    if (auto* PhiNode = llvm::dyn_cast<llvm::PHINode>(LHS)) {
                        // NOLINTNEXTLINE(readability-identifier-naming)
                        for (unsigned i = 0;
                             i < PhiNode->getNumIncomingValues(); ++i) {
                            Value* IncomingValue = PhiNode->getIncomingValue(i);
                            if (auto Instr = dyn_cast<llvm::Instruction>(
                                    IncomingValue)) {
                                if (Instr == BinOp) {
                                    BasicIVs.insert(LHS);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return BasicIVs;
}

/**
 * @brief A derived induction variable of the form `BasicIV * Factor + Addend`.
 * A basic IV is a Derived IV with `Factor` and `Addend` as `nullptr`.
 */
struct IndVar {
    const Value* BasicIV;
    /// Null if Factor is 1
    const Value* Factor;
    /// Null if Addend is 0
    const Value* Addend;
    /// True if Addend should be negative
    bool Sub = false;

    inline bool isBasic() const { return Factor == nullptr; }
};

/**
 * @brief Inserts `BinOp` into `DerivedIVs` if it is a derived induction
 * variable. Otherwise, does nothing.
 *
 * @param DerivedIVs Current mapping of values to `DerivedIV` structs
 * @param BinOp Binary operator instruction
 * @param BasicIVs Mapping of values to basic induction variables
 * @param L loop
 * @return updated mapping of values to `DerivedIV` structs
 */
auto insertDerivedIV(std::unordered_map<const Value*, IndVar>&& DerivedIVs,
                     const BinaryOperator* BinOp,
                     const std::unordered_set<const Value*> BasicIVs,
                     const Loop* L)
{
    const auto Name = getDebugName(BinOp);
    const auto* LHS = BinOp->getOperand(0);
    const auto* RHS = BinOp->getOperand(1);
    if (BinOp->getOpcode() == Instruction::Mul) {
        if (BasicIVs.contains(LHS) && L->isLoopInvariant(RHS)) {
            DerivedIVs[BinOp] =
                IndVar{.BasicIV = LHS, .Factor = RHS, .Addend = nullptr};
        } else if (BasicIVs.contains(RHS) && L->isLoopInvariant(LHS)) {
            DerivedIVs[BinOp] =
                IndVar{.BasicIV = RHS, .Factor = LHS, .Addend = nullptr};
        }
    } else if (BinOp->getOpcode() == Instruction::Add ||
               BinOp->getOpcode() == Instruction::Sub) {
        const auto IsSub = BinOp->getOpcode() == Instruction::Sub;
        if (DerivedIVs.contains(LHS) && L->isLoopInvariant(RHS) &&
            DerivedIVs.at(LHS).Addend == nullptr) {
            DerivedIVs[BinOp] = IndVar{.BasicIV = DerivedIVs.at(LHS).BasicIV,
                                       .Factor = DerivedIVs.at(LHS).Factor,
                                       .Addend = RHS,
                                       .Sub = IsSub};
        } else if (DerivedIVs.contains(RHS) && L->isLoopInvariant(LHS) &&
                   !IsSub && DerivedIVs.at(RHS).Addend == nullptr) {
            DerivedIVs[BinOp] = IndVar{.BasicIV = DerivedIVs.at(RHS).BasicIV,
                                       .Factor = DerivedIVs.at(RHS).Factor,
                                       .Addend = LHS,
                                       .Sub = false};
        }
    }
    return DerivedIVs;
}

/**
 * @brief Get the Derived IVs object
 *
 * @param L loop
 * @param BasicIVs mapping of induction variable value pointers to `BasicIV`
 * @return mapping from derived induction variable value pointers to `DerivedIV`
 * struct
 */
auto getDerivedIVs(const Loop* L,
                   const std::unordered_set<const llvm::Value*>& BasicIVs)
{
    std::unordered_map<const Value*, IndVar> DerivedIVs;
    for (const auto BIV : BasicIVs) {
        DerivedIVs[BIV] =
            IndVar{.BasicIV = BIV, .Factor = nullptr, .Addend = nullptr};
    }
    for (const auto* BB : L->getBlocks()) {
        for (const auto& I : *BB) {
            if (const auto BinOp = dyn_cast<BinaryOperator>(&I);
                BinOp != nullptr) {
                DerivedIVs =
                    insertDerivedIV(std::move(DerivedIVs), BinOp, BasicIVs, L);
            }
        }
    }
    return DerivedIVs;
}

BasicBlock* insertPreheader(const Loop* L)
{
    const auto* Header = L->getHeader();
    if (L->getLoopPreheader() != nullptr) {
        return L->getLoopPreheader();
    }
    for (const auto* Pred : llvm::predecessors(Header)) {
        if (L->contains(Pred)) {
            continue;
        } else {
        }
    }
    return nullptr;
}

auto transformDerived(Instruction* DerivedIV)
{
    IRBuilder<> Builder(DerivedIV);
    // first add the initilization to the pre-header
}

/**
 * @brief Prints the given derived induction variables
 *
 * @param DerivedIVs
 */
void printIVs(const std::unordered_map<const Value*, IndVar>& DerivedIVs,
              std::unordered_set<const Value*>& DisplayedIVs)
{
    // sorted for deterministic output
    std::map<std::string, std::tuple<const Value*, IndVar>> SortedIVs;
    for (const auto [Val, DerivedIV] : DerivedIVs) {
        if (DisplayedIVs.contains(Val)) {
            continue;
        }
        DisplayedIVs.insert(Val);
        SortedIVs.emplace(getName(Val), std::make_tuple(Val, DerivedIV));
    }
    for (const auto& [ValStr, DerivedTuple] : SortedIVs) {
        const auto [Val, DerivedIV] = DerivedTuple;
        if (DerivedIV.isBasic()) {
            outs() << "Basic IV: " << *Val << "\n";
        } else {
            outs() << "Derived IV: " << getName(Val) << " = ";
            outs() << getName(DerivedIV.BasicIV);
            if (DerivedIV.Factor != nullptr) {
                outs() << " * " << getName(DerivedIV.Factor);
            }
            if (DerivedIV.Addend != nullptr) {
                if (DerivedIV.Sub) {
                    outs() << " - ";
                } else {
                    outs() << " + ";
                }
                outs() << getName(DerivedIV.Addend);
            }
            outs() << "\n";
        }
    }
}

/// This is so hacky
bool isPrintEnabled() { return std::filesystem::exists(".ive_enable_print"); }

void runLoop(const Loop* L, bool EnablePrint,
             std::unordered_set<const Value*>& DisplayedIVs)
{
    for (const auto SubLoop : L->getSubLoops()) {
        runLoop(SubLoop, EnablePrint, DisplayedIVs);
    }
    const auto BasicIVs = getBasicIVs(L);
    const auto DerivedIVs = getDerivedIVs(L, BasicIVs);
    if (EnablePrint) {
        printIVs(DerivedIVs, DisplayedIVs);
    }
}

struct InductionVariableElimination
    : public PassInfoMixin<InductionVariableElimination> {
    PreservedAnalyses run(Module& M, ModuleAnalysisManager& AM)
    {
        const static auto EnablePrint = isPrintEnabled();
        FunctionAnalysisManager& FAM =
            AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
        for (auto& F : M) {
            // skip empty functions
            if (F.getName().starts_with("llvm.dbg.") ||
                F.getInstructionCount() == 0) {
                continue;
            }
            llvm::LoopInfo& LI = FAM.getResult<llvm::LoopAnalysis>(F);
            std::unordered_set<const Value*> DisplayedIVs;

            // Iterate over all the loops in the function
            for (llvm::Loop* L : LI) {
                runLoop(L, EnablePrint, DisplayedIVs);
            }
        }

        return PreservedAnalyses::none();
    };
};

}  // namespace

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo()
{
    return {.APIVersion = LLVM_PLUGIN_API_VERSION,
            .PluginName = "ive",
            .PluginVersion = "v0.1",
            .RegisterPassBuilderCallbacks = [](PassBuilder& PB) {
                PB.registerOptimizerEarlyEPCallback(
                    [](ModulePassManager& PM, OptimizationLevel Opt) {
                        PM.addPass(InductionVariableElimination{});
                    });
            }};
}
