#include <cstdio>
int main()
{
    int LI = 100;
    for (int I = 0; I < 10; ++I) {
        int J = I * LI + LI;
        printf("%d\n", J);
    }
}