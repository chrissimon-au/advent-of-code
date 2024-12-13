typedef struct Game {   
  int deltaXA;
  int deltaXB;
  int deltaYA;
  int deltaYB;
  int targetX;
  int targetY;        
} Game;

int cost_to_win_game(Game game);
int cost_to_win_from_instructions(char* instructions);