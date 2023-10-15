// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    for (int i = 0; i < 10; ++i) {
        // ind var
        int p = m + i * 10;
        for (int j = 0; j < i; ++j) {
            // not ind var
            int k = j + i * p;
            for (int m = 0; m < k; m += 20) {
                int n = m + k * p;
                printf("%d\n", n);
            }

            for (int m = 0; m < k; m++) {
                // 1 ind var (m * p)
                int n = k - m * p;
                printf("%d\n", n);
            }
        }
    }
}
// NOLINTEND