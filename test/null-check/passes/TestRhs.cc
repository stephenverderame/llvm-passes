// NOLINTBEGIN
#include <cstdio>
int main()
{
    int buf[512];
    int len = 512;
    for (int i = 0; len > i; ++i) {
        buf[i] = i;
        buf[len - 1] = 0;
    }

    for (int i = 0; i < len; ++i) {
        printf("%d\n", buf[i]);
    }
}
// NOLINTEND