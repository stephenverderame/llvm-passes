#include <cstdlib>
// NOLINTBEGIN
int main()
{
    int* p = (int*)malloc(sizeof(int));
    int i = 0;
    while (p) {
        *p = i;
        p = nullptr;
        i++;
    }
}
// NOLINTEND