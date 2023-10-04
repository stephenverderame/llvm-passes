#include <cstdlib>
// NOLINTBEGIN
int main()
{
    bool f = true;
    int** p = (int**)malloc(sizeof(int**));
    int i = 0;
    while (p) {
        *p = (int*)malloc(sizeof(int*));
        int j = 0;
        while (*p) {
            **p = j;
            if (j == 10) {
                *p = nullptr;
            } else {
                *p = (int*)malloc(sizeof(int*));
            }
            ++j;
        }
        i++;
    }
}
// NOLINTEND