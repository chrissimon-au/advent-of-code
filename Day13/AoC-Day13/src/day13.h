typedef struct Game {   
  long deltaXA;
  long deltaXB;
  long deltaYA;
  long deltaYB;
  long targetX;
  long targetY;
} Game;

long cost_to_win_game(Game game);
long cost_to_win_from_instructions(char *instructions);