{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Solver where

import Control.Applicative ((<|>))
import Control.Monad (guard)

import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import System.Random (Random(..), newStdGen)

--import Game 

-- Coding Plan
-- 1) Write out solve function to solve an input sudoku board (need to debug)
-- 2) DONE: Create basic UI for user to put numbers into 
-- 3) Incorporate solution into UI 
-- 4) Create QuickCheck for checking sudoku solver 
-- 5) Create a csv parser to parse sudoku inputs 
-- 6) Make it all nxn

-- Note: I'm using -- if it's a temporary comment / something to address, {--} if its something to keep that tells us what that section of code is doing 


{- Types -} 

type Loc = (Int, Int)

data Value = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine -- 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 deriving (Eq, Show)
 -- 0 means it hasn't been filled in yet? 

type Board = Map Loc Value

{- Original input board (do not modify Zeros) -} 
type Input = Map Loc Value 

{- Constants -} 

height, width :: Int 
height = 9 
width = 9

locations :: [Loc]
locations = [(x, y) | x <- [0..width - 1], y <- [0..height - 1]] 

initBoard :: Board 
initBoard = Map.empty 

rowsCols :: [[Loc]]
rowsCols = [[(x, y) | y <- [0..height - 1]] | x <- [0..width - 1]] ++ 
           [[(x, y) | x <- [0..width - 1]] | y <- [0..height - 1]]

boxes :: [[Loc]]
boxes = [[(x, y) | x <- [0..2], y <- [0..2]],
         [(x, y) | x <- [0..2], y <- [3..5]],
         [(x, y) | x <- [0..2], y <- [6..8]],
         [(x, y) | x <- [3..5], y <- [0..2]],
         [(x, y) | x <- [3..5], y <- [3..5]],
         [(x, y) | x <- [3..5], y <- [6..8]],
         [(x, y) | x <- [6..8], y <- [0..2]],
         [(x, y) | x <- [6..8], y <- [3..5]],
         [(x, y) | x <- [6..8], y <- [6..8]]]

{- Boxes -}

box1 :: [Loc]
box1 = [(x, y) | x <- [0..2], y <- [0..2]]

box2 :: [Loc]
box2 = [(x, y) | x <- [3..5], y <- [0..2]]

box3 :: [Loc]
box3 = [(x, y) | x <- [6..8], y <- [0..2]]

box4 :: [Loc]
box4 = [(x, y) | x <- [0..2], y <- [3..5]]

box5 :: [Loc]
box5 = [(x, y) | x <- [3..5], y <- [3..5]]

box6 :: [Loc]
box6 = [(x, y) | x <- [6..8], y <- [3..5]]

box7 :: [Loc]
box7 = [(x, y) | x <- [0..2], y <- [6..8]]

box8 :: [Loc]
box8 = [(x, y) | x <- [3..5], y <- [6..8]]

box9 :: [Loc]
box9 = [(x, y) | x <- [6..8], y <- [6..8]]

-- should this be a parser? How are we creating sudoku boards? 
{- Creates empty sudoku board (All 0s) -} 
initSudoku :: Board 
initSudoku = helper locations initBoard
    where 
        helper [] board = board
        helper (h : t) board = helper t (Map.insert h Zero board)
        -- for the purposes of testing? Until we have a parser? 

{- Functions -}  

next :: Value -> Value 
next Zero = One
next One = Two 
next Two = Three 
next Three = Four 
next Four = Five 
next Five = Six 
next Six = Seven
next Seven = Eight 
next Eight = Nine 
next Nine = Zero 

fillInZeroes :: Board -> Board 
fillInZeroes board = helper locations board 
    where 
        helper [] board = board 
        helper (h : t) board = 
            case Map.lookup h board of 
                Nothing -> helper t (Map.insert h Zero board)
                _ -> helper t board 

{- Sudoku Solver -} 
solve :: Board -> Board 
solve board = let board = fillInZeroes board in case solveHelper board board locations of 
    Nothing -> initSudoku
    Just b -> b
    where 
        {- Iterates through the locations to fill in sudoku board -}
        solveHelper :: Board -> Board -> [Loc] -> Maybe Board 
        solveHelper input board [] = Just board 
        solveHelper input board (h : t) = 
            case Map.lookup h board of 
                Nothing -> error "Should not happen, always initialize sudoku in initSudoku?"

                {- If the board has Zero, then no value has been tried yet -} 
                Just Zero -> case tryValue board h One of

                                {- If we can't place any value in the board, return Nothing to initiate backtracking -}
                                Zero -> Nothing 

                                {- If we can, try placing a value in the next location -}
                                value -> case solveHelper input (Map.insert h value board) t of 

                                    {- If we can't place a value in the next location, try a new value for the current location (backtrack) -}
                                    Nothing -> solveHelper input (Map.insert h value board) (h : t)

                                    Just b -> Just b 

                {- If the board has a value, but it originally had Zero there, try a new value (part of backtracking) -} 
                Just value -> if Map.lookup h input == Just Zero then 
                                case tryValue board h (next value) of

                                    {- If we can't place any value in the board, return Nothing to initiate backtracking -}
                                    Zero -> Nothing 

                                    {- If we can, try placing a value in the next location -}
                                    value -> case solveHelper input (Map.insert h value board) t of 

                                        {- If we can't place a value in the next location, try a new value for the current location (backtrack) -}
                                        Nothing -> solveHelper input (Map.insert h value board) (h : t)

                                        Just b -> Just b

                            {- If that value was given, skip it -}
                            else solveHelper input board t  
        
        {- Go through every number to see if it can go in the given location on the board -}
        tryValue :: Board -> Loc -> Value -> Value
        tryValue board location Zero = tryValue board location One 
        tryValue board location@(row, col) value = 

            let rows = [(row, c) | c <- [0..width - 1]] in 
            let cols = [(r, col) | r <- [0..height - 1]] in
            let boxes = getBox location in

            case value of 
            One -> 

                {- Place One in the board if it can be put in the given location -} 
                if checkNotExists rows cols boxes One then One 

                {- If it can't, try placing Two there -} 
                else tryValue board location Two 

            Two -> 
                if checkNotExists rows cols boxes Two then Two
                else tryValue board location Three 
            
            Three -> 
                if checkNotExists rows cols boxes Three then Three
                else tryValue board location Four 
            
            Four -> 
                if checkNotExists rows cols boxes Four then Four
                else tryValue board location Five 
            
            Five -> 
                if checkNotExists rows cols boxes Five then Five
                else tryValue board location Six 
            
            Six -> 
                if checkNotExists rows cols boxes Six then Six
                else tryValue board location Seven 
            
            Seven -> 
                if checkNotExists rows cols boxes Seven then Seven
                else tryValue board location Eight  
            
            Eight -> 
                if checkNotExists rows cols boxes Eight then Eight
                else tryValue board location Nine 
            
            Nine -> 
                
                if checkNotExists rows cols boxes Nine then Nine

                {- If no number works, return Zero to initiate backtracking -}
                else Zero

        {- Checks if the value exists in the row, column, or box -}
        checkNotExists :: [Loc] -> [Loc] -> [Loc] -> Value -> Bool 
        checkNotExists rows cols boxes val = 
                (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True boxes) && 
                (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True rows) &&
                (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True cols)
        
        {- Returns which box a given location is in -}
        getBox :: Loc -> [Loc]
        getBox (row, col) = 
            if row < 3 then 
                if col < 3 then box1 
                else if col < 6 then box2 
                else box3 
            else if row < 6 then 
                if col < 3 then box4 
                else if col < 6 then box5 
                else box6
            else 
                if col < 3 then box7
                else if col < 6 then box8 
                else box9

{- Tests -}

-- Sample Boards, temporary?
-- Test with:
-- solve sampleSudoku<i> sampleSudoku<i>
-- or
-- completeSampleSudoku == solve sampleSudoku<i> sampleSudoku<i>

-- Almost complete sudoku board, missing top left value
completeSampleSudoku  :: Board
completeSampleSudoku = 
    Map.fromList [((0,0),Four),((0,1),Three),((0,2),Five),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Seven),((0,7),Eight),((0,8),One),
                  ((1,0),Six),((1,1),Eight),((1,2),Two),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),
                  ((2,0),One),((2,1),Nine),((2,2),Seven),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),
                  ((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),One),((3,4),Nine),((3,5),Five),((3,6),Three),((3,7),Four),((3,8),Seven),
                  ((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Six),((4,4),Eight),((4,5),Two),((4,6),Nine),((4,7),One),((4,8),Five),
                  ((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Seven),((5,4),Four),((5,5),Three),((5,6),Six),((5,7),Two),((5,8),Eight),
                  ((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Eight),((6,7),Seven),((6,8),Four),
                  ((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),One),((7,7),Three),((7,8),Six),
                  ((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Two),((8,7),Five),((8,8),Nine)]

sampleSudoku1  :: Board
sampleSudoku1 = 
    Map.fromList [((0,0),Zero),((0,1),Three),((0,2),Five),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Seven),((0,7),Eight),((0,8),One),
                  ((1,0),Six),((1,1),Eight),((1,2),Two),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),
                  ((2,0),One),((2,1),Nine),((2,2),Seven),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),
                  ((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),One),((3,4),Nine),((3,5),Five),((3,6),Three),((3,7),Four),((3,8),Seven),
                  ((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Six),((4,4),Eight),((4,5),Two),((4,6),Nine),((4,7),One),((4,8),Five),
                  ((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Seven),((5,4),Four),((5,5),Three),((5,6),Six),((5,7),Two),((5,8),Eight),
                  ((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Eight),((6,7),Seven),((6,8),Four),
                  ((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),One),((7,7),Three),((7,8),Six),
                  ((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Two),((8,7),Five),((8,8),Nine)]

-- Almost complete sudoku board, missing bottom right value
sampleSudoku2  :: Board
sampleSudoku2 = Map.fromList [((0,0),Four),((0,1),Three),((0,2),Five),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Seven),((0,7),Eight),((0,8),One),((1,0),Six),((1,1),Eight),((1,2),Two),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),((2,0),One),((2,1),Nine),((2,2),Seven),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),One),((3,4),Nine),((3,5),Five),((3,6),Three),((3,7),Four),((3,8),Seven),((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Six),((4,4),Eight),((4,5),Two),((4,6),Nine),((4,7),One),((4,8),Five),((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Seven),((5,4),Four),((5,5),Three),((5,6),Six),((5,7),Two),((5,8),Eight),((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Eight),((6,7),Seven),((6,8),Four),((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),One),((7,7),Three),((7,8),Six),((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Two),((8,7),Five),((8,8),Zero)]

-- Board missing diagonal top left to bottom right
sampleSudoku3  :: Board
sampleSudoku3 = Map.fromList [((0,0),Zero),((0,1),Three),((0,2),Five),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Seven),((0,7),Eight),((0,8),One),((1,0),Six),((1,1),Zero),((1,2),Two),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),((2,0),One),((2,1),Nine),((2,2),Zero),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),Zero),((3,4),Nine),((3,5),Five),((3,6),Three),((3,7),Four),((3,8),Seven),((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Six),((4,4),Zero),((4,5),Two),((4,6),Nine),((4,7),One),((4,8),Five),((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Seven),((5,4),Four),((5,5),Zero),((5,6),Six),((5,7),Two),((5,8),Eight),((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Zero),((6,7),Seven),((6,8),Four),((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),One),((7,7),Zero),((7,8),Six),((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Two),((8,7),Five),((8,8),Zero)]

-- Same as previous board, but missing two more values in first row
sampleSudoku4  :: Board
sampleSudoku4 = Map.fromList [((0,0),Zero),((0,1),Zero),((0,2),Zero),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Seven),((0,7),Eight),((0,8),One),((1,0),Six),((1,1),Zero),((1,2),Two),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),((2,0),One),((2,1),Nine),((2,2),Zero),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),Zero),((3,4),Nine),((3,5),Five),((3,6),Three),((3,7),Four),((3,8),Seven),((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Six),((4,4),Zero),((4,5),Two),((4,6),Nine),((4,7),One),((4,8),Five),((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Seven),((5,4),Four),((5,5),Zero),((5,6),Six),((5,7),Two),((5,8),Eight),((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Zero),((6,7),Seven),((6,8),Four),((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),One),((7,7),Zero),((7,8),Six),((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Two),((8,7),Zero),((8,8),Zero)]

-- Same as previous board, but missing entire first 3x3 square
sampleSudoku5  :: Board
sampleSudoku5 = Map.fromList [((0,0),Zero),((0,1),Zero),((0,2),Zero),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Seven),((0,7),Eight),((0,8),One),((1,0),Zero),((1,1),Zero),((1,2),Zero),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),((2,0),Zero),((2,1),Zero),((2,2),Zero),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),Zero),((3,4),Nine),((3,5),Five),((3,6),Three),((3,7),Four),((3,8),Seven),((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Six),((4,4),Zero),((4,5),Two),((4,6),Nine),((4,7),One),((4,8),Five),((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Seven),((5,4),Four),((5,5),Zero),((5,6),Six),((5,7),Two),((5,8),Eight),((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Zero),((6,7),Seven),((6,8),Four),((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),One),((7,7),Zero),((7,8),Six),((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Two),((8,7),Zero),((8,8),Zero)]

-- Missing diagonal 3x3 squares
sampleSudoku6  :: Board
sampleSudoku6 = 
    Map.fromList [((0,0),Zero),((0,1),Zero),((0,2),Zero),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Seven),((0,7),Eight),((0,8),One),
                  ((1,0),Zero),((1,1),Zero),((1,2),Zero),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),
                  ((2,0),Zero),((2,1),Zero),((2,2),Zero),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),
                  ((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),Zero),((3,4),Zero),((3,5),Zero),((3,6),Three),((3,7),Four),((3,8),Seven),
                  ((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Zero),((4,4),Zero),((4,5),Zero),((4,6),Nine),((4,7),One),((4,8),Five),
                  ((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Zero),((5,4),Zero),((5,5),Zero),((5,6),Six),((5,7),Two),((5,8),Eight),
                  ((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Zero),((6,7),Zero),((6,8),Zero),
                  ((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),Zero),((7,7),Zero),((7,8),Zero),
                  ((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Zero),((8,7),Zero),((8,8),Zero)]

-- This one breaks, its exactly the same as the last one except its missing (0,6)
sampleSudoku7 :: Board
sampleSudoku7 = 
    Map.fromList [((0,0),Zero),((0,1),Zero),((0,2),Zero),((0,3),Two),((0,4),Six),((0,5),Nine),((0,6),Zero),((0,7),Eight),((0,8),One),
                  ((1,0),Zero),((1,1),Zero),((1,2),Zero),((1,3),Five),((1,4),Seven),((1,5),One),((1,6),Four),((1,7),Nine),((1,8),Three),
                  ((2,0),Zero),((2,1),Zero),((2,2),Zero),((2,3),Eight),((2,4),Three),((2,5),Four),((2,6),Five),((2,7),Six),((2,8),Two),
                  ((3,0),Eight),((3,1),Two),((3,2),Six),((3,3),Zero),((3,4),Zero),((3,5),Zero),((3,6),Three),((3,7),Four),((3,8),Seven),
                  ((4,0),Three),((4,1),Seven),((4,2),Four),((4,3),Zero),((4,4),Zero),((4,5),Zero),((4,6),Nine),((4,7),One),((4,8),Five),
                  ((5,0),Nine),((5,1),Five),((5,2),One),((5,3),Zero),((5,4),Zero),((5,5),Zero),((5,6),Six),((5,7),Two),((5,8),Eight),
                  ((6,0),Five),((6,1),One),((6,2),Nine),((6,3),Three),((6,4),Two),((6,5),Six),((6,6),Zero),((6,7),Zero),((6,8),Zero),
                  ((7,0),Two),((7,1),Four),((7,2),Eight),((7,3),Nine),((7,4),Five),((7,5),Seven),((7,6),Zero),((7,7),Zero),((7,8),Zero),
                  ((8,0),Seven),((8,1),Six),((8,2),Three),((8,3),Four),((8,4),One),((8,5),Eight),((8,6),Zero),((8,7),Zero),((8,8),Zero)]

{- Debugging Tools -}

prettyPrint :: Board -> IO ()
prettyPrint board = helper 0 (Map.toList board) 
    where 
        helper _ [] = print ""
        helper i ((_, v1) : (_, v2) : (_, v3) : (_, v4) : (_, v5) : (_, v6) : (_, v7) : (_, v8) : (_, v9) : t) = 
            if i == 2 || i == 5 then do 
            putStr (valueConverter v1)
            putStr (valueConverter v2)
            putStr (valueConverter v3)
            putStr " "
            putStr (valueConverter v4)
            putStr (valueConverter v5)
            putStr (valueConverter v6)
            putStr " "
            putStr (valueConverter v7)
            putStr (valueConverter v8)
            putStrLn (valueConverter v9)
            putStr "\n"
            helper (i + 1) t 
            else do 
            putStr (valueConverter v1)
            putStr (valueConverter v2)
            putStr (valueConverter v3)
            putStr " "
            putStr (valueConverter v4)
            putStr (valueConverter v5)
            putStr (valueConverter v6)
            putStr " "
            putStr (valueConverter v7)
            putStr (valueConverter v8)
            putStrLn (valueConverter v9)
            helper (i + 1) t 
        helper _ _ = error "Should not happen, boards should be pretty"

valueConverter :: Value -> String 
valueConverter Zero = "0" 
valueConverter One = "1" 
valueConverter Two = "2"
valueConverter Three = "3"
valueConverter Four = "4"
valueConverter Five = "5"
valueConverter Six = "6"
valueConverter Seven = "7" 
valueConverter Eight = "8"
valueConverter Nine = "9"



{- 

Sudoku Rows / Cols

00 01 02  03 04 05  06 07 08
10 11 12  13 14 15  16 17 18
20 21 22  23 24 25  26 27 28

30 31 32  33 34 35  36 37 38
40 41 42  43 44 45  46 47 48
50 51 52  53 54 55  56 57 58

60 61 62  63 64 65  66 67 68
70 71 72  73 74 75  76 77 78
80 81 82  83 84 85  86 87 88

IE, Boxes
1 2 3 
4 5 6
7 8 9

-}