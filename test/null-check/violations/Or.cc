#include <cstdlib>
// NOLINTBEGIN
int main()
{
    bool f = true;
    int* p = (int*)malloc(sizeof(int*));
    while (p || f) {
        *p = 0;
    }
}
// NOLINTEND