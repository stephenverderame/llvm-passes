#include <cstdio>
int main()
{
    for (int I = 0; I < 10; ++I) {
        int J = I * 10;
        printf("%d\n", J);
    }

    for (int I = 10; I >= 0; --I) {
        int K = I * 20 + 4;
        printf("%d\n", K);
    }
}