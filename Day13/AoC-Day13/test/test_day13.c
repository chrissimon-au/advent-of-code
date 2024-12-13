#ifdef TEST

#include "unity.h"

#include "day13.h"

void setUp(void)
{
}

void tearDown(void)
{
}

void test_simple()
{
    int output = cost_to_win("Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400");
    TEST_ASSERT_EQUAL(280, output);
}

#endif // TEST
