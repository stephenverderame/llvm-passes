
#include <cstdio>
#include <cstdlib>
int main()
{
    // NOLINTNEXTLINE
    auto P = (double*)malloc(sizeof(double));
    if (P != nullptr) {
        *P = 1.0;
    } else {
        return 0;
    }
    // NOLINTNEXTLINE
    if (P) {
        printf("%f\n", *P);
    }
    return 0;
}