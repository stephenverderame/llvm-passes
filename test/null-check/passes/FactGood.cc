#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
// NOLINTBEGIN

int fact(int n)
{
    if (n == 0) {
        return 1;
    }
    return n * fact(n - 1);
}

int hash(int n) { return n % 99; }

int main(int argc, const char** argv)
{
    int nums[100];
    nums[hash(10) % 100u] = fact(10);
}
// NOLINTEND