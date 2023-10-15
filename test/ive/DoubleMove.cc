// NOLINTBEGIN
#include <cstdio>
int main()
{
    for (int i = 0; i < 10; ++i) {
        // int k = m;
        // int k = i * 10 (which is itself an induction variable)
        for (int j = 10; j < 100; ++j) {
            int k = i * j;
            printf("%d\n", k);
            // k += i;
        }
        // m += 10;
    }
}
// NOLINTEND