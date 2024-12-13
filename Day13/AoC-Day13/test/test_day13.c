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

void test_simple2()
{
    int output = cost_to_win_from_instructions("Button A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450");
    TEST_ASSERT_EQUAL(200, output);
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

void test_simple_game2()
{
    Game game;
    game.deltaXA = 17;
    game.deltaXB = 84;
    game.deltaYA = 86;
    game.deltaYB = 37;
    game.targetX = 7870;
    game.targetY = 6450;
    int output = cost_to_win_game(game);
    TEST_ASSERT_EQUAL(200, output);
}

void test_game_that_doesnt_solve()
{
    Game game;
    game.deltaXA = 26;
    game.deltaXB = 67;
    game.deltaYA = 66;
    game.deltaYB = 21;
    game.targetX = 12748;
    game.targetY = 12176;
    int output = cost_to_win_game(game);
    TEST_ASSERT_EQUAL(0, output);
}

#endif // TEST
