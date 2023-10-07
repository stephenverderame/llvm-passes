#include <cassert>
#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    if (argc > 257 || argc < 0) {
        printf("Too many arguments\n");
        return 1;
    }
    int arr_count = argc - 1;
    const char* buf[256];

    for (int i = 1, j = 0; i < argc && j < arr_count; ++i, ++j) {
        const auto arg = argv + i;
        if (arg) {
            buf[j] = *arg;
        }
    }

    for (int i = 0; i < arr_count; ++i) {
        printf("%s", buf[i]);
    }
    printf("\n");

    return 0;
}
// NOLINTEND