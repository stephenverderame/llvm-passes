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
    int a[128];
    int b[128];

    for (int i = 1, j = 0; i < argc; ++i) {
        const auto arg = argv + i;
        if (arg) {
            int num = atoi(*arg);
            assert(j < arr_count / 2);
            if (i % 2 == 0) {
                a[j] = num;
                j++;
            } else {
                b[j] = num;
            }
        }
    }

    int c = 0;
    for (int i = 0; i < arr_count / 2; ++i) {
        int d = a[i] + b[i];
        c += d * d;
    }

    printf("%d\n", c);

    return 0;
}
// NOLINTEND