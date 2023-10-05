#include <cstdio>
// NOLINTBEGIN
int main(int argc, char** argv)
{
    int nums[2] = {1, 2};
    nums[0] = argc;
    nums[1] = 1;

    for (int i = 0; i < 2; ++i) {
        nums[i] = argc;
    }
}
// NOLINTEND