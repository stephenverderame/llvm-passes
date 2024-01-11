#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

int foo() { return 10; }
int bar() { return 8; }
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    int data_args = argc - 1;
    int* vec = (int*)malloc(sizeof(int) * data_args);
    assert(vec != nullptr);
    for (int i = 0; i < data_args; ++i) {
        auto in_arg = argv + i + 1;
        assert(in_arg != nullptr);
        vec[i] = atoi(*in_arg);
    }
    for (int i = 0; i < data_args; ++i) {
        printf("%d\n", vec[i]);
    }
}
// NOLINTEND