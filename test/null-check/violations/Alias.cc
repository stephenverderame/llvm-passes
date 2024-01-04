// NOLINTBEGIN
int main()
{
    int* a = new int(6120);
    int** p = &a;
    *p = nullptr;
    *a = 100;
}
// NOLINTEND