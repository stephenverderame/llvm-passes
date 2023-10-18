// NOLINTBEGIN
#include <cstdio>
int* get(int k)
{
    int* a = new int[1000];
    for (int i = 0; i < 1000; i++) {
        a[i] = i;
    }
    return a;
}
int main()
{
    int* a = get(100);
    int* b = get(200);
    for (int i = 0; i < 1000; ++i) {
        int c = a[i] + b[i];
        printf("%d\n", c);
    }
    delete[] a;
    delete[] b;
    return 0;
}
// NOLINTEND