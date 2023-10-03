#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN
int main()
{
    bool test = true;
    int*** p = (int***)malloc(sizeof(int***));
    if (p) {
        *p = (int**)malloc(sizeof(int**));
        if (*p) {
            **p = (int*)malloc(sizeof(int*));
            if (**p) {
                ***p = 0;
            }
        }
    }
}
// NOLINTEND