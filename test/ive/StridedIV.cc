// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    for (int i = 0; i < 5000; i += m) {
        int p = m + i * 10;
        for (int j = 0; j < i; j += p) {
            int k = j + i * p;
            printf("%d\n", k);
        }
    }
}
// NOLINTEND