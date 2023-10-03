
#include <cstdio>
#include <cstdlib>
int main()
{
    // NOLINTNEXTLINE
    auto P = (double*)malloc(sizeof(double));
    if (P != nullptr) {
        *P = 1.0;
    }
    *P = 2.0;
}