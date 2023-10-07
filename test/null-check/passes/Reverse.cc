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
    int arg_num = argc - 1;
    const char* args[257];
    for (int i = 0; i < arg_num; ++i) {
        const auto arg = argv + i + 1;
        if (arg) {
            args[i] = *arg;
        }
    }

    for (int i = arg_num - 1; i >= 0; --i) {
        printf("%s ", args[i]);
    }
}
// NOLINTEND