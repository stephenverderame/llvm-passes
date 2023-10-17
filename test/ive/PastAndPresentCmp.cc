// NOLINTBEGIN
#include <cstdio>
int main()
{
    int i = 0;
    while (i < 100) {
        int k = i * 10 + 5;
        if (i < 50 && k > 100) {
            printf("%d\n", k);
        }
        ++i;
        int m = i * 20 + 5;
        if (i >= 75 || i < 10) {
            printf("%d\n", m);
        }
    }
}
// NOLINTEND