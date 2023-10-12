// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    int k = 100;
    for (int i = 5000; i >= m; i -= m) {
        k -= m;
        printf("%d\n", i);
        printf("%d\n", k);
    }
}
// NOLINTEND