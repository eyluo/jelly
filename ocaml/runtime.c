#include <stdio.h>

extern int test_fn();

int main(void)
{
    printf("%d\n", test_fn());
    return 0;
}