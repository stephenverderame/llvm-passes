// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    for (int i = 0; i < 5000; i += m) {
        int k = i * i;
        int j = m * m;
        int f = 10 - i;
        printf("%d\n", k + j + f);
    }
}
// NOLINTEND