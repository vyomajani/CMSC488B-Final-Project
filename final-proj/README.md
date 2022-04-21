# Sudoku Solver
A playable interface for Sudoku. 

# Coding Plan of Action 
1) Write out solve function to solve an input sudoku board 
2) Create basic UI for user to put numbers into 
3) Incorporate solution into UI 
4) Create QuickCheck for checking sudoku solver 
5) Create a csv parser to parse sudoku inputs 
6) Make it all nxn


<p align="center">
  <img src="./docs/img/example.gif"/>
</p>

## installation
Installation on MacOS can be accomplished via homebrew:
```shell
brew install samtay/tui/snake
```
Arch Linux users can install from the [AUR](https://aur.archlinux.org/packages/snake-terminal-git/):
```shell
yay -S snake-terminal-git # or yaourt -S snake-terminal-git, etc.
```
Others can install from source with [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install):
```shell
git clone https://github.com/samtay/snake.git
cd snake
stack install snake
```
If you are on Debian and want to install via package manager, feel free to open an issue and I'll try to get around to it.

## playing the game

After launching the game, press any of the arrow keys or the letters 'k', 'j', 'l', or 'h' to start the game.
