// NOLINTBEGIN
#include <cstdio>
__attribute__((optnone)) int g() { return -10; }

int main()
{
    int f = g();
    int p = g();
    for (int i = 0; i < 100; ++i) {
        int k = i * f + p;
        printf("%d\n", k);
        for (int j = 500; j >= 0; --j) {
            int m = j * k;
            printf("%d\n", m);
        }
    }

    for (int i = 100; i >= 0; --i) {
        int k = i * f + p;
        printf("%d\n", k);
    }
}
// NOLINTEND