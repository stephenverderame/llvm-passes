// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    for (int i = 0; i < 10; ++i) {
        int p = m + i * 10;
        for (int j = 0; j < i; ++j) {
            int k = j + i * p;
            for (int m = 0; m < k; m += 20) {
                int n = m + k * p;
                printf("%d\n", n);
            }

            for (int m = 0; m < k; m++) {
                int n = k - m * p;
                printf("%d\n", n);
            }
        }

        for (int j = 0; j < i; ++j) {
            int k = j - i * p;
            printf("%d\n", k);
        }
    }
}
// NOLINTEND