// NOLINTBEGIN

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>

struct Bar {
    int x;
    char y;
    int64_t data[20];
};

int main()
{
    Bar b;
    b.x = 10;
    b.y = 20;
    for (int i = 0; i < sizeof(b.data) / sizeof(int64_t); ++i) {
        b.data[i] = i;
    }
}
// NOLINTEND