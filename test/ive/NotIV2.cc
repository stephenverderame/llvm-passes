// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    int j = 0;
    for (int i = 5000; i >= m; i -= m, ++j) {
        int p = j + i;
        printf("%d\n", p);
    }
}
// NOLINTEND