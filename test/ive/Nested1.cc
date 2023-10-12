// NOLINTBEGIN
#include <cstdio>
int main()
{
    int m = 20;
    for (int i = 0; i < 10; ++i) {
        int p = i * 10 + m;
        for (int j = 0; j < i; ++j) {
            int k = j * i + p;
            printf("%d\n", k);
        }
    }
}
// NOLINTEND