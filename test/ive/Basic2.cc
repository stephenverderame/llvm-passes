// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    for (int i = 5000; i >= m; i -= m) {
        printf("%d\n", i);
    }
}
// NOLINTEND