include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_llvm_pass_plugin(NullCheck
    # List your source files here.
    NullCheck.cpp
    NullAbstractInterpretation.cpp
)
