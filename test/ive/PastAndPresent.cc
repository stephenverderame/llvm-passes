// NOLINTBEGIN
#include <cstdio>
int main()
{
    int i = 0;
    while (i < 100) {
        int k = i * 10 + 5;
        printf("%d\n", k);
        ++i;
        int m = i * 20 + 5;
        printf("%d\n", m);
    }
}
// NOLINTEND