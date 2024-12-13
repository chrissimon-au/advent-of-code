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
    int output = cost_to_win_from_instructions("Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400");
    TEST_ASSERT_EQUAL(280, output);
}

void test_simple_game()
{
    Game game;
    game.deltaXA = 94;
    game.deltaXB = 22;
    game.deltaYA = 34;
    game.deltaYB = 67;
    game.targetX = 8400;
    game.targetY = 5400;
    int output = cost_to_win_game(game);
    TEST_ASSERT_EQUAL(280, output);
}

#endif // TEST
