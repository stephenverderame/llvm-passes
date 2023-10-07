#include <cstdio>
// NOLINTBEGIN
int main(int argc, char** argv)
{
    int nums[128];
    int nums2[100];

    for (int i = sizeof(nums) / sizeof(int) - 1; i >= 0; --i) {
        for (int j = 0; j < sizeof(nums2) / sizeof(int); ++j) {
            if (nums[i] != nums2[j]) {
                return 1;
            }
        }
    }
    return 0;
}
// NOLINTEND