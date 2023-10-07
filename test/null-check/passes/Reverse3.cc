#include <cassert>
#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    if (argc > 257) {
        printf("Too many arguments\n");
        return 1;
    }
    int arg_num = argc - 1;
    const char* args[256];
    const int size = sizeof(args) / sizeof(args[0]);
    for (int i = 0; i < arg_num; ++i) {
        const auto arg = argv + i + 1;
        if (arg) {
            args[size - 1 - i] = *arg;
        }
    }

    int start = size - arg_num;
    for (int i = start; i < size; ++i) {
        printf("%s ", args[i]);
    }
}
// NOLINTEND