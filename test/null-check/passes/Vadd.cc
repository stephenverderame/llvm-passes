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
    if (arr_count % 2 != 0) {
        printf(
            "Must have even number of arguments(got %d), excluding the "
            "filename\n",
            argc);
        return 1;
    }
    int buf[256];

    for (int i = 1, j = 0; i < argc && j < arr_count; ++i, ++j) {
        const auto arg = argv + i;
        if (arg) {
            int num = atoi(*arg);
            buf[j] = num;
        }
    }

    int c[128];
    for (int i = 0; i < arr_count / 2; ++i) {
        c[i] = buf[i] + buf[i + arr_count / 2];
    }

    for (int i = 0; i < arr_count / 2; ++i) {
        printf("%d\n", c[i]);
    }

    return 0;
}
// NOLINTEND