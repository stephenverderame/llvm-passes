// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    for (int i = 5000; i >= 0; i -= m) {
        int p = m + i * 10;
        for (int j = i; j >= 0; j -= p) {
            int k = i + j * p;
            printf("%d\n", k);
        }
    }
}
// NOLINTEND