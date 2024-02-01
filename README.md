# Battleship Game

Battleship Game implemented in Ocaml, utilizing the state monad. Play against the computer, strategically place your ships, and take shots to sink the opponent's fleet!

## How to Run:

1. Compile the project using:
   ```bash
   ocamlc -o battleship game_defs.ml state_monad.ml board_functions.ml ship_functions.ml shoot_functions.ml battleship.ml
