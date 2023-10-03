int main()
{
    auto F = true;
    // NOLINTNEXTLINE
    auto p = new int();
    *p = 0;
    if (F) {
        return *p;
    } else {
        return 0;
    }
}