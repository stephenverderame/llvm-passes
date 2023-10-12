include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_llvm_pass_plugin(InductionVariableElimination
    # List your source files here.
    src/Ive.cpp
)
