#include <cstdlib>
// NOLINTBEGIN
int main()
{
    int*** p = new int**();
    *p = new int*();
    **p = new int();
    ***p = 0;
}
// NOLINTEND