#include <cstdlib>
void foo(const int* p) {}
int main()
{
    // NOLINTNEXTLINE
    int data[20];
    data[0] = 0;
    foo(data);
    int* half = data + 10;
    foo(half);
    *half = 0;
    return half[10];
}