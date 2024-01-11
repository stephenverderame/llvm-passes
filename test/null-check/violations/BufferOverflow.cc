#include <cassert>
#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    if (argc > 256) {
        printf("Too many arguments\n");
        return 1;
    }
    char raw_buffer[100];
    assert(argc < 100);
    for (int i = 0; i < 100; ++i) {
        const auto t = argv + i;
        assert(t != nullptr);
        const auto word = *t;
        assert(word != nullptr);
        int num = atoi(word);
        *(int*)(&raw_buffer[i]) = num;
    }
}
// NOLINTEND