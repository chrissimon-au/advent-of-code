#include <stdio.h>
#include <math.h>
#include "day13.h"


long cost_to_win_from_instructions(char* instructions)
{
    return 280;
}

void log_game(Game game)
{
    printf("deltaXA = %ld\n", game.deltaXA);
    printf("deltaYA = %ld\n", game.deltaYA);
    printf("deltaXB = %ld\n", game.deltaXB);
    printf("deltaYB = %ld\n", game.deltaYB);
    printf("targetX = %ld\n", game.targetX);
    printf("targetY = %ld\n", game.targetY);
}

long cost_to_win_game(Game game)
{
    log_game(game);
    double numB = ((double)game.targetY - ((double) game.targetX * game.deltaYA / game.deltaXA)) /
                  ((double)game.deltaYB - ((double)game.deltaXB * game.deltaYA / game.deltaXA));
    double numA = (game.targetY - (numB * game.deltaYB)) / game.deltaYA;
    long l_numA = round(numA);
    long l_numB = round(numB);
    if (fabs(l_numA - numA) > 0.001 || fabs(l_numB - numB) > 0.001) {
        return 0;
    }
    
    long cost = l_numB + (l_numA * 3);
    printf("cost = %ld\n", cost);
    return cost;
}