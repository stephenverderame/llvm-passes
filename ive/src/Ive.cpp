

#include <llvm-17/llvm/IR/InstrTypes.h>
#include <llvm-17/llvm/IR/Instruction.h>
#include <llvm-17/llvm/IR/PassManager.h>
#include <llvm-17/llvm/Passes/PassBuilder.h>
#include <llvm-17/llvm/Passes/PassPlugin.h>
#include <llvm-17/llvm/Support/Casting.h>
#include <llvm-17/llvm/Transforms/Scalar/LoopPassManager.h>

#include <exception>
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

struct BasicIV {
    const Value* IV;
    const Value* Addend;
};

// NOLINTNEXTLINE
auto getBasicIVs(const Loop* L)
{
    std::unordered_set<const llvm::Value*> BasicIVs;
    for (llvm::BasicBlock* BB : L->getBlocks()) {
        for (llvm::Instruction& I : *BB) {
            // outs() << I << "\n";
            if (auto* BinOp = dyn_cast<BinaryOperator>(&I)) {
                if (BinOp->getOpcode() != Instruction::Add) {
                    continue;
                }
                Value* LHS = BinOp->getOperand(0);
                Value* RHS = BinOp->getOperand(1);

                if (L->isLoopInvariant(LHS)) {  // LHS is loop invariant
                    // RHS must be a phi node that contains BinOp
                    if (auto* PhiNode = llvm::dyn_cast<llvm::PHINode>(RHS);
                        PhiNode != nullptr) {
                        // NOLINTNEXTLINE(readability-identifier-naming)
                        for (unsigned i = 0;
                             i < PhiNode->getNumIncomingValues(); ++i) {
                            Value* IncomingValue = PhiNode->getIncomingValue(i);
                            if (auto Instr = dyn_cast<llvm::Instruction>(
                                    IncomingValue)) {
                                if (Instr == BinOp) {
                                    BasicIVs.insert(BinOp);
                                }
                            }
                        }
                    }
                } else if (L->isLoopInvariant(RHS)) {  // RHS is loop invariant
                    // LHS must be a phi node that contains BinOp
                    if (auto* PhiNode = llvm::dyn_cast<llvm::PHINode>(LHS)) {
                        // NOLINTNEXTLINE(readability-identifier-naming)
                        for (unsigned i = 0;
                             i < PhiNode->getNumIncomingValues(); ++i) {
                            Value* IncomingValue = PhiNode->getIncomingValue(i);
                            if (auto Instr = dyn_cast<llvm::Instruction>(
                                    IncomingValue)) {
                                if (Instr == BinOp) {
                                    BasicIVs.insert(BinOp);
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
 * @brief A derived induction variable of the form `BasicIV * Factor + Addend`
 */
struct DerivedIV {
    const Value* BasicIV;
    /// Null if Factor is 1
    const Value* Factor;
    /// Null if Addend is 0
    const Value* Addend;
    bool Sub = false;
};

auto insertDerivedIV(std::unordered_map<const Value*, DerivedIV>&& DerivedIVs,
                     const BinaryOperator* BinOp,
                     const std::unordered_set<const Value*> BasicIVs,
                     const Loop* L)
{
    const auto* LHS = BinOp->getOperand(0);
    const auto* RHS = BinOp->getOperand(1);
    if (BinOp->getOpcode() == Instruction::Mul) {
        if (BasicIVs.contains(LHS) && L->isLoopInvariant(RHS)) {
            DerivedIVs[BinOp] =
                DerivedIV{.BasicIV = LHS, .Factor = RHS, .Addend = nullptr};
        } else if (BasicIVs.contains(RHS) && L->isLoopInvariant(LHS)) {
            DerivedIVs[BinOp] =
                DerivedIV{.BasicIV = RHS, .Factor = LHS, .Addend = nullptr};
        }
    } else if (BinOp->getOpcode() == Instruction::Add ||
               BinOp->getOpcode() == Instruction::Sub) {
        const auto IsSub = BinOp->getOpcode() == Instruction::Sub;
        if (DerivedIVs.contains(LHS) && L->isLoopInvariant(RHS)) {
            DerivedIVs[BinOp] = DerivedIV{.BasicIV = DerivedIVs.at(LHS).BasicIV,
                                          .Factor = DerivedIVs.at(LHS).Factor,
                                          .Addend = RHS,
                                          .Sub = IsSub};
        } else if (DerivedIVs.contains(RHS) && L->isLoopInvariant(LHS)) {
            DerivedIVs[BinOp] = DerivedIV{.BasicIV = DerivedIVs.at(RHS).BasicIV,
                                          .Factor = DerivedIVs.at(RHS).Factor,
                                          .Addend = LHS,
                                          .Sub = IsSub};
        }
    }
    return DerivedIVs;
}

auto getDerivedIVs(const Loop* L,
                   const std::unordered_set<const llvm::Value*>& BasicIVs)
{
    std::unordered_map<const Value*, DerivedIV> DerivedIVs;
    for (auto BIV : BasicIVs) {
        DerivedIVs[BIV] =
            DerivedIV{.BasicIV = BIV, .Factor = nullptr, .Addend = nullptr};
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

struct InductionVariableElimination
    : public PassInfoMixin<InductionVariableElimination> {
    PreservedAnalyses run(Module& M, ModuleAnalysisManager& AM)
    {
        FunctionAnalysisManager& FAM =
            AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
        for (auto& F : M) {
            // skip empty functions
            if (F.getName().starts_with("llvm.dbg.") ||
                F.getInstructionCount() == 0) {
                continue;
            }
            llvm::LoopInfo& LI = FAM.getResult<llvm::LoopAnalysis>(F);

            // Iterate over all the loops in the function
            for (llvm::Loop* L : LI) {
                // Analyze the loop, e.g., get loop header, latch, and other
                // properties
                const auto BasicIVs = getBasicIVs(L);
                for (const auto& IV : BasicIVs) {
                    outs() << *IV << "\n";
                }
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
            .PluginName = "InductionVariableElimination",
            .PluginVersion = "v0.1",
            .RegisterPassBuilderCallbacks = [](PassBuilder& PB) {
                PB.registerOptimizerEarlyEPCallback(
                    [](ModulePassManager& PM, OptimizationLevel Opt) {
                        PM.addPass(InductionVariableElimination{});
                    });
            }};
}
