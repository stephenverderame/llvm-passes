#include <cstdio>
// NOLINTBEGIN
int main(int argc, char** argv)
{
    int nums[128];
    int nums2[100];

    for (int i = 0; i < sizeof(nums) / sizeof(int); ++i) {
        bool fail = true;
        for (int j = 0; j < sizeof(nums2) / sizeof(int); ++j) {
            if (nums[i] == nums2[j]) {
                fail = false;
            }
        }
        if (fail) {
            return 1;
        }
    }
    return 0;
}
// NOLINTEND