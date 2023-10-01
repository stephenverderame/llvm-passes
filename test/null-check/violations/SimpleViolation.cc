#include <cstdlib>
int main()
{
    // NOLINTNEXTLINE
    auto p = (int*)malloc(sizeof(int));
    *p = 0;
    return 0;
}