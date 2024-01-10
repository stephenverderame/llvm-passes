#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
// NOLINTBEGIN
int main(int argc, const char** argv)
{
    assert(argc == 2);
    const char** arg = argv + 1;
    assert(arg != nullptr);
    const auto str = *arg;
    assert(str != nullptr);
    const auto len = strlen(str);
    assert(len <= 512);
    char sorted[513];
    for (int i = 0; i < len; ++i) {
        const char* c = (str + i);
        assert(c != nullptr);
        sorted[i] = *c;
    }

    for (int i = 0; i < len; ++i) {
        char tmp = sorted[i];
        int min_idx = i;
        for (int j = i + 1; j < len; ++j) {
            if (sorted[i] > sorted[j]) {
                sorted[i] = sorted[j];
                min_idx = j;
            }
        }
        sorted[min_idx] = tmp;
    }
    sorted[len] = '\0';

    printf("%s\n", sorted);
    return 0;
}
// NOLINTEND