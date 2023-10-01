#include <llvm-17/llvm/IR/IRBuilder.h>
#include <llvm-17/llvm/IR/Instruction.h>
#include <llvm/Pass.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Support/raw_ostream.h>

#include <vector>

#include "NullAbstractInterpretation.hpp"

// NOLINTNEXTLINE
using namespace llvm;

struct NullCheckPass : public PassInfoMixin<NullCheckPass> {
    PreservedAnalyses run(Module& M, ModuleAnalysisManager& /* AM */)
    {
        for (auto& F : M) {
            analyze(F, NullAbstractInterpretation{});
        }
        return PreservedAnalyses::all();
    };
};

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo()
{
    return {.APIVersion = LLVM_PLUGIN_API_VERSION,
            .PluginName = "NullCheckPass",
            .PluginVersion = "v0.1",
            .RegisterPassBuilderCallbacks = [](PassBuilder& PB) {
                PB.registerPipelineStartEPCallback(
                    [](ModulePassManager& MPM, OptimizationLevel /* Level */) {
                        MPM.addPass(NullCheckPass());
                    });
            }};
}
