typedef struct Game {   
  unsigned long long deltaXA;
  unsigned long long deltaXB;
  unsigned long long deltaYA;
  unsigned long long deltaYB;
  unsigned long long targetX;
  unsigned long long targetY;
} Game;

unsigned long long cost_to_win_game(Game game);
unsigned long long cost_to_win_from_instructions(char *instructions);
unsigned long long cost_to_win_from_instructions_with_offset(char *instructions, unsigned long long offset);