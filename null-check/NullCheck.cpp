#include <llvm-17/llvm/IR/IRBuilder.h>
#include <llvm-17/llvm/IR/Instruction.h>
#include <llvm-17/llvm/Support/raw_ostream.h>
#include <llvm/Pass.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Support/raw_ostream.h>

#include <exception>
#include <sstream>
#include <stdexcept>
#include <vector>

#include "NullAbstractInterpretation.hpp"
#include "df/DataFlow.hpp"

// NOLINTNEXTLINE
using namespace llvm;

namespace
{
/// The type of a pointer use
enum class PtrUseType {
    Load,
    Store,
    GEP,
};

/// Type of pointer use to string
auto displayUseType(PtrUseType UseType)
{
    switch (UseType) {
        case PtrUseType::Load:
            return "load";
        case PtrUseType::Store:
            return "store";
        case PtrUseType::GEP:
            return "load";
        default:
            return "??";
    }
}

/**
 * @brief Displays line and file information for an instruction if it exists,
 * otherwise, pretty-prints the llvm instruction.
 *
 * @param Inst
 * @return std::string
 */
auto displayDebugInfo(const Instruction& Inst)
{
    const auto DebugLoc = Inst.getDebugLoc();
    if (DebugLoc) {
        std::stringstream Stream;
        Stream << DebugLoc->getFilename().str() << ":" << DebugLoc->getLine();
        return Stream.str();
    } else {
        std::string Str;
        raw_string_ostream Stream(Str);
        Stream << Inst;
        Str = Stream.str();
        if (const auto StartPos = Str.find_first_not_of(" ");
            StartPos != std::string::npos) {
            Str = Str.substr(StartPos);
        }
        std::stringstream Stream2;
        Stream2 << "instruction\n\t'\033[1m" << Str << "\033[0m'\n";
        return Stream2.str();
    }
}

/**
 * @brief Prints a safety violation message and returns true if the pointer
 * operand of the given instruction is potentially null.
 * Otherwise, returns false.
 *
 * @param AnalysisResult The result of the null analysis
 * @param Ptr The pointer operand of the instruction
 * @param Inst The instruction
 * @param UseType The type of pointer use
 * @return true if the instruction potentially uses a null pointer
 */
[[nodiscard]] auto checkPtrOpSafety(
    const DataFlowFacts<NullAbstractInterpretation>& AnalysisResult,
    const Value* Ptr, const Instruction& Inst, PtrUseType UseType)
{
    const auto InFacts = AnalysisResult.InstructionInFacts.at(&Inst);
    const auto& AbstractVal = InFacts.MemState_.at(InFacts.State_.at(Ptr));
    if (AbstractVal.IsNull_ != NullState::NonNull) {
        errs() << "\033[31mSafety Violation\033[00m: Use of potentially "
                  "null pointer in "
               << displayUseType(UseType) << " at " << displayDebugInfo(Inst)
               << "\n";
        return true;
    }
    return false;
}

/**
 * @brief Checks if any instruction in the given function uses a potentially
 * null pointer.
 * If so, prints a safety violation message and returns true.
 *
 * @param F The function
 * @param AnalysisResult The result of the null analysis
 * @return true if the function contains a safety violation
 */
[[nodiscard]] auto checkInstSafety(
    Function& F,
    const DataFlowFacts<NullAbstractInterpretation>& AnalysisResult)
{
    auto Violation = false;
    for (auto& BB : F) {
        for (auto& I : BB) {
            if (auto Store = dyn_cast<StoreInst>(&I); Store != nullptr) {
                const auto Ptr = Store->getPointerOperand();
                Violation |=
                    checkPtrOpSafety(AnalysisResult, Ptr, I, PtrUseType::Store);
            } else if (auto Load = dyn_cast<LoadInst>(&I); Load != nullptr) {
                const auto Ptr = Load->getPointerOperand();
                Violation |=
                    checkPtrOpSafety(AnalysisResult, Ptr, I, PtrUseType::Load);
            } else if (auto GEP = dyn_cast<GetElementPtrInst>(&I);
                       GEP != nullptr) {
                const auto Ptr = GEP->getPointerOperand();
                Violation |=
                    checkPtrOpSafety(AnalysisResult, Ptr, I, PtrUseType::GEP);
            }
        }
    }
    return Violation;
}
struct NullCheckPass : public PassInfoMixin<NullCheckPass> {
    PreservedAnalyses run(Module& M, ModuleAnalysisManager& /* AM */)
    {
        auto Violation = false;
        for (auto& F : M) {
            const auto AnalysisResult =
                analyze(F, NullAbstractInterpretation{});
            Violation |= checkInstSafety(F, AnalysisResult);
        }
        if (Violation) {
            std::exit(2);
        }
        return PreservedAnalyses::all();
    };
};

}  // namespace

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
