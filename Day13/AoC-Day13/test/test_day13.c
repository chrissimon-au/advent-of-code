#ifdef TEST

#include "unity.h"

#include "day13.h"

void setUp(void)
{
}

void tearDown(void)
{
}

void test_add()
{
    int output = add(10, 20);
    TEST_ASSERT_EQUAL(30, output);
}

#endif // TEST
