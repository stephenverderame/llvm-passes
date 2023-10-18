// NOLINTBEGIN
#include <cstdio>
int main()
{
    int k = 0;
    // while (k < 100) {
    //     if (k < 50) {
    //         printf("%d\n", k);
    //     } else {
    //         int j = k * 10 + 5;
    //         printf("%d\n", j);
    //     }
    //     k += 10;
    // }

    k = 0;
    while (k < 25) {
        int j = k * 10 + 5;
        if (k < 17) {
            printf("%d\n", j);
        }

        ++k;

        if (k >= 17) {
            printf("%d\n", j * 2);
        }
    }
}
// NOLINTEND