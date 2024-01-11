// NOLINTBEGIN
#include <cassert>
#include <cstring>
int main(int argc, char** argv)
{
    if (argc >= 128) {
        return 1;
    }
    char words[128][128];

    for (int i = 0; i < argc; ++i) {
        const auto word = (argv + i);
        assert(word != nullptr);
        const auto word_data = *word;
        const auto word_len = strlen(word_data);
        for (int j = 0; j < word_len; ++j) {
            const auto c = word_data + j;
            assert(c != nullptr);
            words[i][j] = *c;
        }
    }
}

// NOLINTEND