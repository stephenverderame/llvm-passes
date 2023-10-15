// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 10;
    for (int i = 0; i < 100; ++i) {
        int k = i * m;
        printf("%d\n", k);
        int p = k + 10;
        printf("%d\n", p);
        int j = k + 20;
        printf("%d\n", j);
    }
}
// NOLINTEND