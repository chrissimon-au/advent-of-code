#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "day13.h"


Game parse(char *instructions, char **str_end)
{
    Game game;
    char *end;
    char *ptr;
    if (strlen(instructions) < 12)
    {
        printf("instructions are too short!\n");
        return game;
    }
    ptr = instructions + 12;
    game.deltaXA = strtol(ptr, &end, 10);
    ptr = ptr + 6;
    game.deltaYA = strtol(ptr, &end, 10);
    ptr = ptr + 15;
    game.deltaXB = strtol(ptr, &end, 10);
    ptr = ptr + 6;
    game.deltaYB = strtol(ptr, &end, 10);
    ptr = ptr + 12;
    game.targetX = strtol(ptr, &end, 10);
    ptr = end + 4;
    game.targetY = strtol(ptr, &end, 10);
    *str_end = end;
    return game;    
}

long cost_to_win_from_instructions(char *instructions)
{
    printf("===============\n");
    printf("Instructions:\n%s\n", instructions);
    
    long total_cost = 0;
    char *ptr = instructions;

    for (;;)
    {
        char *end;
        Game game = parse(ptr, &end);
        total_cost += cost_to_win_game(game);
        if (*end == '\0')
        {
            break;
        }
        ptr = end + 2;
    }
    return total_cost;
}

void log_game(Game game)
{
    printf("----\n");
    printf("deltaXA = %ld, deltaYA = %ld\n", game.deltaXA, game.deltaYA);
    printf("deltaXB = %ld, deltaYB = %ld\n", game.deltaXB, game.deltaYB);
    printf("targetX = %ld, targetY = %ld\n", game.targetX, game.targetY);
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
        printf("no cost.\n");
        return 0;
    }
    
    long cost = l_numB + (l_numA * 3);
    printf("cost = %ld\n", cost);
    return cost;
}