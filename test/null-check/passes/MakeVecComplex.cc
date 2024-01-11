#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

int foo() { return 10; }

// NOLINTBEGIN
int main(int argc, const char** argv)
{
    const auto one = argv + 1;
    const auto two = argv + 2;
    assert(one != nullptr && two != nullptr);
    const auto elem_size = atoi(*one);
    const auto num_elems = atoi(*two);
    auto data = (char*)malloc(elem_size * num_elems);
    for (int i = 0; i < num_elems * elem_size; ++i) {
        data[i] = 0;
    }
    int masked = foo();
    assert(masked <= elem_size);
    for (int i = 0; i < num_elems; ++i) {
        for (int j = 0; j < masked; ++j) {
            data[i * elem_size + j] = 0;
        }
    }
}
// NOLINTEND