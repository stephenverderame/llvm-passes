#include <cassert>
#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    int* vec = (int*)malloc(argc);
    assert(vec != nullptr);
    for (int i = 0; i < argc; ++i) {
        const auto t = argv + 1 + i;
        assert(t != nullptr);
        const auto word = *t;
        assert(word != nullptr);
        int num = atoi(word);
        vec[i] = num;
    }
}
// NOLINTEND