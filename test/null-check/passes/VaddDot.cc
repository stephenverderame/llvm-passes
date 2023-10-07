#include <cassert>
#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    // this isn't actually a correct program (ex. argv[0] is a filename), just a
    // test
    if (argc > 256 || argc < 0) {
        printf("Too many arguments\n");
        return 1;
    }
    if (argc % 2 != 0) {
        printf("Must have even number of arguments, excluding the filename\n");
        return 1;
    }
    int a[128];
    int b[128];

    for (int i = 0; i < argc; ++i) {
        const auto arg = argv + i;
        if (arg) {
            int num = atoi(*arg);
            if (i < argc / 2) {
                a[i] = num;
                b[i] = num;
            }
        }
    }

    int c = 0;
    for (int i = 0; i < argc / 2; ++i) {
        int d = a[i] + b[i];
        c += d * d;
    }

    printf("%d\n", c);

    return 0;
}
// NOLINTEND