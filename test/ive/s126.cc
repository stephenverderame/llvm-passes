// NOLINTBEGIN
#include <cstdio>
const int LEN2 = 100;
const int ntimes = 1000;
int main()
{
    int k;
    for (int nl = 0; nl < 10 * (ntimes / LEN2); nl++) {
        k = 1;
        for (int i = 0; i < LEN2; i++) {
            for (int j = 1; j < LEN2; j++) {
                printf("%d %d\n", j - 1, k - 1);
                ++k;
            }
            ++k;
        }
    }
}
// NOLINTEND