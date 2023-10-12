// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    int j = 0;
    for (int i = 5000; i >= m && j < 100; i -= m, ++j) {
        printf("%d\n", i);
    }
}
// NOLINTEND