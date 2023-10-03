#include <cstdlib>
// NOLINTBEGIN
int main()
{
    int* p = (int*)malloc(sizeof(int));
    int i = 0;
    while (p) {
        *p = i;
        if (i == 10) {
            p = nullptr;
        } else {
            p = (int*)malloc(sizeof(int));
        }
        i++;
    }
}
// NOLINTEND