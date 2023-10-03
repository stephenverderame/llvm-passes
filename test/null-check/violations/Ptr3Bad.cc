#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN
int main()
{
    int*** p = (int***)malloc(sizeof(int***));
    if (p) {
        *p = (int**)malloc(sizeof(int**));
        if (*p) {
            **p = (int*)malloc(sizeof(int*));
            if (**p) {
                ***p = 0;
            }
            printf("%d\n", ***p);
        }
    }
}
// NOLINTEND