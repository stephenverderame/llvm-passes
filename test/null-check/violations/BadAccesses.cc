#include <cassert>
#include <cstdio>
#include <cstdlib>
// NOLINTBEGIN

void access(int (&foo)[100])
{
    unsigned rnd = rand();
    int srnd = rand();
    int a = rnd % 7u;
    int b = (rnd % 10u) + 6;
    int c = rnd % 12u;
    foo[b - c] = 0;
    foo[a * a * a] = 25;
    foo[a * 15 - b] = 10;
    foo[a * a + b * b] = 5;
    foo[a + a + b] = 0;
}

int main(int argc, const char** argv)
{
    int foo[100];
    access(foo);
}
// NOLINTEND