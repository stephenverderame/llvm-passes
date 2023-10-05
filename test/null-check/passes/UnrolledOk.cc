#include <cstdio>
// NOLINTBEGIN
int main(int argc, char** argv)
{
    int nums[128];

    for (int i = 0; i < 128 - 3; i += 4) {
        nums[i] = argc;
        nums[i + 1] = 1;
        nums[i + 2] = 2;
        nums[i + 3] = 3;
    }
}
// NOLINTEND