#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

int foo() { return 10; }
int bar() { return 8; }
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    char data[144];
    auto elem_size = foo();
    assert(elem_size <= 12 && elem_size >= 0);
    auto num_elems = bar();
    assert(num_elems <= 12);
    for (int i = 0; i < num_elems; ++i) {
        data[i * elem_size] = 0;
    }
}
// NOLINTEND