// NOLINTBEGIN

#include <cassert>
#include <cstdio>

int foo() { return 10; }
int main()
{
    const int a = 20 + 10;
    int buf[a];
    int g = foo();
    assert(g <= 12);
    int h = g + 8;
    for (int i = 0; i < h; ++i) {
        buf[i] = i;
    }
}
// NOLINTEND