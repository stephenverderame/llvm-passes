// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    int k = 100;
    int p = 0;
    for (int i = 5000; i >= m; i -= m) {
        printf("%d\n", k);
        k += m;
        int j = k * m + m;
        p = k + 10;
        int n = k * 10;
        int nn = n + m;
        printf("%d\n", j);
        printf("%d\n", i);
        printf("%d\n", p);
        printf("%d\n", n);
        printf("%d\n", nn);
    }
}
// NOLINTEND