include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_llvm_pass_plugin(NullCheck
    # List your source files here.
    src/NullCheck.cpp
    src/NullAbstractInterpretation.cpp
    src/IntervalAnalysis.cpp
    src/IntRange.cpp
    src/Bound.cpp
)

target_link_directories(NullCheck PUBLIC /usr/lib)
target_link_libraries(NullCheck PUBLIC libz3.so)
