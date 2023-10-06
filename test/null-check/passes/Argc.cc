// NOLINTBEGIN
int main(int argc, char** argv)
{
    if (argc >= 128) {
        return 1;
    }
    int nums[128];

    for (int i = 0; i < argc; ++i) {
        nums[i] = argc;
    }
}

// NOLINTEND