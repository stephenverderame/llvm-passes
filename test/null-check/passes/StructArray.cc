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

void foo(int n, int i)
{
    assert(i >= 0);
    auto d = (Bar*)malloc(n * sizeof(Bar));
    assert(d != nullptr);
    if (i < n) {
        d[i].x = 10;
        d[i].y = 20;
        for (int j = 0; j < 20; ++j) {
            d[i].data[j] = j;
        }
    }
}

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