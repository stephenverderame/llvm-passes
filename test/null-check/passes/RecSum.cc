#include <cassert>
#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN

int rec_sum(int (&args)[128], int accum, int size)
{
    if (size < 0 || size >= 128) {
        return accum;
    }
    return rec_sum(args, accum + args[size], size - 1);
}

int rec_sum(int (&args)[128], int size) { return rec_sum(args, 0, size); }

int main(int argc, const char** argv)
{
    if (argc > 129 || argc < 1) {
        printf("Too many arguments\n");
        return 1;
    }
    int arr_count = argc - 1;
    int nums[128];

    for (int i = 1, j = 0; i < argc && j < arr_count; ++i) {
        const auto arg = argv + i;
        if (arg) {
            int num = atoi(*arg);
            nums[j] = num;
            j++;
        }
    }

    printf("%d\n", rec_sum(nums, arr_count));
}
// NOLINTEND