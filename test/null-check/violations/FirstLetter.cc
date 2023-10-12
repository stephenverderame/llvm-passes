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
    int arg_num = argc - 1;
    char args[257];
    for (int i = 0; i < arg_num; ++i) {
        const auto arg = argv + i + 1;
        if (arg) {
            const auto word = *arg;
            if (word) {
                args[i] = *word;
            }
        }
    }

    args[arg_num] = '\0';

    printf("%s\n", args);
}
// NOLINTEND