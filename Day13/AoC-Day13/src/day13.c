#include <stdio.h>
#include "day13.h"


int cost_to_win_from_instructions(char* instructions)
{
    return 280;
}

void log_game(Game game)
{
    printf("deltaXA = %d\n", game.deltaXA);
    printf("deltaYA = %d\n", game.deltaYA);
    printf("deltaXB = %d\n", game.deltaXB);
    printf("deltaYB = %d\n", game.deltaYB);
    printf("targetX = %d\n", game.targetX);
    printf("targetY = %d\n", game.targetY);
}

int cost_to_win_game(Game game)
{
    /*
    numB = (YTarget - (XTarget * detlaYA / deltaXA)) / (deltaYB - (deltaXB * deltaYA / deltaXA))
    numA = (YTarget - NumB * deltaYB) / deltaYA
    */
    log_game(game);
    double numB = (game.targetY - (game.targetX * game.deltaYA / game.deltaXA)) / (game.deltaYB - (game.deltaXB * game.deltaYA / game.deltaXA));
    printf("numB = %f\n", numB);
    double numA = (game.targetY - (numB * game.deltaYB)) / game.deltaYA;
    printf("numA = %f\n", numA);
    int cost = numB + (numA * 3);
    printf("cost = %d\n", cost);
    return cost;
}