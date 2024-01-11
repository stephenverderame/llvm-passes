#include <cassert>
#include <cstdlib>
// NOLINTBEGIN
int main(int argc, char** argv)
{
    int* n = (int*)malloc(sizeof(int));
    assert(n != nullptr);
    if (argc < 10) {
        free(n);
    }
    *n = 0;
    return *n;
}
// NOLINTEND