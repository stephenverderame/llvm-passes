include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../null-check/include)

add_llvm_pass_plugin(InductionVariableElimination
    # List your source files here.
    src/Ive.cpp
)
