{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module FourByFourSolver where

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

import Solver

-- DONE
-- 1) Write out solve function to solve an input sudoku board 
-- 2) Create basic UI for user to put numbers into 
-- 3) Incorporate solution into UI 
-- 4) Create a csv parser to parse sudoku inputs
-- 5) Automatically check whether user's solution is a valid solution
-- 6) Hints  

-- TODO
-- 1) Create QuickCheck for checking sudoku solver 
-- 2) Make it all nxn
-- 3) Freeze the original board so users can't modify original input 
-- 4) Take in file inputs 
-- 5) Ask if user want to see the solution yet or if they want to continue solving 

-- Note: I'm using -- if it's a temporary comment / something to address, {--} if its something to keep that tells us what that section of code is doing 


{- Types -} 


{- Constants -} 

height, width :: Int 
height = 4
width = 4

locations :: [Loc]
locations = [(x, y) | x <- [0..width - 1], y <- [0..height - 1]] 

rowsCols :: [[Loc]]
rowsCols = [[(x, y) | y <- [0..height - 1]] | x <- [0..width - 1]] ++ 
           [[(x, y) | x <- [0..width - 1]] | y <- [0..height - 1]]

boxes :: [[Loc]]
boxes = [[(x, y) | x <- [0,1], y <- [0,1]],
         [(x, y) | x <- [0,1], y <- [2,3]],
         [(x, y) | x <- [2,3], y <- [0,1]],
         [(x, y) | x <- [2,3], y <- [2,3]]]

{- Boxes -}

box1 :: [Loc]
box1 = [(x, y) | x <- [0,1], y <- [0,1]]

box2 :: [Loc]
box2 = [(x, y) | x <- [2,3], y <- [0,1]]

box3 :: [Loc]
box3 = [(x, y) | x <- [0,1], y <- [2,3]]

box4 :: [Loc]
box4 = [(x, y) | x <- [2,3], y <- [2,3]]

{- Creates empty sudoku board (All 0s) -} 
initSudoku :: Board 
initSudoku = helper locations initBoard
    where 
        helper [] board = board
        helper (h : t) board = helper t (Map.insert h Zero board)

{- Functions -}  

fillInZeroes :: Board -> Board 
fillInZeroes board = helper locations board 
    where 
        helper [] board = board 
        helper (h : t) board = 
            case Map.lookup h board of 
                Nothing -> helper t (Map.insert h Zero board)
                _ -> helper t board 

next :: Value -> Value 
next Zero = One
next One = Two 
next Two = Three 
next Three = Four 
next Four = Zero

{- Returns which box a given location is in -}
getBox :: Loc -> [Loc]
getBox (row, col) = 
    if col < 2 then 
        if row < 2 then box1 
        else box2
    else 
        if row < 2 then box3 
        else box4

{- Sudoku Solver -} 
solve :: Board -> Board 
solve board = let filledBoard = fillInZeroes board in case solveHelper filledBoard filledBoard locations of
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
                if checkNotExists board rows cols boxes One then One 

                {- If it can't, try placing Two there -} 
                else tryValue board location Two 

            Two -> 
                if checkNotExists board rows cols boxes Two then Two
                else tryValue board location Three 
            
            Three -> 
                if checkNotExists board rows cols boxes Three then Three
                else tryValue board location Four 
            
            Four -> 
                if checkNotExists board rows cols boxes Four then Four
                else tryValue board location Zero 

        {- Checks if the value exists in the row, column, or box -}
        checkNotExists :: Board -> [Loc] -> [Loc] -> [Loc] -> Value -> Bool 
        checkNotExists board rows cols boxes val = 
                (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True boxes) && 
                (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True rows) &&
                (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True cols)

{- Tests -}

-- Sample Boards, temporary?
-- Test with:
-- solve sampleSudoku<i> sampleSudoku<i>
-- or
-- completeSampleSudoku == solve sampleSudoku<i> sampleSudoku<i>

sampleSudoku1  :: Board
sampleSudoku1 = 
    Map.fromList [((0,0),Zero),((0,1),Three),((0,2),Four),((0,3),Zero),
                  ((1,0),Four),((1,1),Zero),((1,2),Zero),((1,3),Two),
                  ((2,0),One),((2,1),Zero),((2,2),Zero),((2,3),Three),
                  ((3,0),Zero),((3,1),Two),((3,2),One),((3,3),Zero)]
                --   0,3,4,0,4,0,0,2,1,0,0,3,0,2,1,0

sampleSudoku2  :: Board
sampleSudoku2 = Map.fromList [((0,0),Three),((0,1),Four),((0,2),One),((0,3),Zero),((1,0),Zero),((1,1),Two),((1,2),Zero),((1,3),Zero),((2,0),Zero),((2,1),Zero),((2,2),Two),((2,3),Zero),((3,0),Zero),((3,1),One),((3,2),Four),((3,3),Three)]

{- Debugging Tools -}

prettyPrint :: Board -> IO ()
prettyPrint board = helper 0 (Map.toList board) 
    where 
        helper _ [] = print ""
        helper i ((_, v1) : (_, v2) : (_, v3) : (_, v4) : t) = 
            if i == 1 then do 
            putStr (valueConverter v1)
            putStr (valueConverter v2)
            putStr " "
            putStr (valueConverter v3)
            putStr (valueConverter v4)
            putStr "\n"
            helper (i + 1) t 
            else do 
            putStr (valueConverter v1)
            putStr (valueConverter v2)
            putStr " "
            putStr (valueConverter v3)
            putStr (valueConverter v4)
            putStr "\n"
            helper (i + 1) t 
        helper _ _ = error "Should not happen, boards should be pretty"

{- 

Sudoku Rows / Cols

00 01  02 03
10 11  12 13

20 21  22 23
30 31  32 33

-}