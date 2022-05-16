# Sudoku Solver
A playable interface for Sudoku. 

## Playing the Game

Use `stack run` to launch the game

Press the arrow keys to move the cursor around the board

Enter numbers simply by pressing the '0' to '9' keys 

Delete numbers by pressing the Backspace key 

To recieve a hint, press the 'h' key

To recieve the solution, press the Enter key 

To switch the board size, press the 's' key

To quit, press the 'q' key 

## Coding Plan 
1) Write out solve function to solve an input sudoku board 
2) Create basic UI for user to put numbers into 
3) Incorporate solution into UI 
4) Create QuickCheck for checking sudoku solver 
5) Create a csv parser to parse sudoku inputs 
6) Automatically check whether the user's solution is valid
7) Incorporate a hints feature 
8) Make it all nxn

## Future Add-Ons
1) Freeze the original input board so users can't modify the original input 
2) Input boards via File IO
3) Ask if the user wants to see the solution yet or if they want to continue playing, upon hitting Enter
4) Allow users to switch board sizes without losing their progress
