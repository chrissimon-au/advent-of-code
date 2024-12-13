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
    unsigned long long output = cost_to_win_from_instructions("Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400");
    TEST_ASSERT_EQUAL_UINT64(280, output);
}

void test_simple2()
{
    unsigned long long output = cost_to_win_from_instructions("Button A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450");
    TEST_ASSERT_EQUAL_UINT64(200, output);
}

void test_multiple()
{
    unsigned long long output = cost_to_win_from_instructions("Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279");
    TEST_ASSERT_EQUAL_UINT64(480, output);
}

void test_multiple_with_offset()
{
    unsigned long long output = cost_to_win_from_instructions_with_offset("Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279", 10000000000000);
    TEST_ASSERT_EQUAL_UINT64(875318608908, output);
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
    unsigned long long output = cost_to_win_game(game);
    TEST_ASSERT_EQUAL_UINT64(280, output);
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
    unsigned long long output = cost_to_win_game(game);
    TEST_ASSERT_EQUAL_UINT64(200, output);
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
    unsigned long long output = cost_to_win_game(game);
    TEST_ASSERT_EQUAL_UINT64(0, output);
}

#endif // TEST
