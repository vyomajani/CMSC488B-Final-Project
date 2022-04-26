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
-- 1) Write out solve function to solve an input sudoku board 
-- 2) Create basic UI for user to put numbers into 
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

{- Sudoku Solver -} 
solve :: Board -> Board -> Board 
solve input board = solveHelper input board locations
    where 
        {- Iterates through the locations to fill in sudoku board -}
        solveHelper :: Board -> Board -> [Loc] -> Board 
        solveHelper input board [] = board 
        solveHelper input board (h : t) = 
            case Map.lookup h board of 
                Nothing -> error "Should not happen, always initialize sudoku in initSudoku?"

                {- If the board has Zero, then no value has been tried yet -} 
                Just Zero -> case tryValue board h One of
                                Zero -> solveHelper input (Map.insert h Zero board) t -- need to backtrack
                                value -> solveHelper input (Map.insert h value board) t 

                                -- this is where we would need to backtrack and change a value we had previously written 
                                -- Not sure how to do this logic tho
                                -- calls helper again on a previous location to retry the values? Or changes a previous location?

                                -- what if we returned Values or locations instead of the whole board? 


                {- If the board has a value, but it originally had Zero there, double check that the value still works -} 
                -- is this a necessary clause for backtracking? HELP 
                Just value -> if Map.lookup h input == Just Zero then 
                                case tryValue board h value of
                                    Zero -> solveHelper input (Map.insert h Zero board) t-- need to backtrack
                                    value -> solveHelper input (Map.insert h value board) t 

                            {- If that value was given, skip it -}
                            else solveHelper input board t  
        
        {- Go through every number to see if it can go in the given location on the board, 
            backtracking if no number can go -}
        tryValue :: Board -> Loc -> Value -> Value
        tryValue board location Zero = tryValue board location One 
        tryValue board location@(row, col) value = 

            let rows = [(row, c) | c <- [0..width - 1]] in 
            let cols = [(r, col) | r <- [0..height - 1]] in
            let boxes = getBox location in

            case value of 
            One -> 

                {- Place One in the board if it can be put in the given location -} 
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just One) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just One) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just One) True cols)

                then One 

                {- If it can't, try placing Two there -} 
                else tryValue board location Two 

            Two -> 

                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Two) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Two) True rows) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Two) True cols)

                then Two

                else tryValue board location Three 
            
            Three -> 
                
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Three) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Three) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Three) True cols)

                then Three

                else tryValue board location Four 
            
            Four -> 
                
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Four) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Four) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Four) True cols)

                then Four

                else tryValue board location Five 
            
            Five -> 
                
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Five) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Five) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Five) True cols)

                then Five

                else tryValue board location Six 
            
            Six -> 
                
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Six) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Six) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Six) True cols)

                then Six

                else tryValue board location Seven 
            
            Seven -> 
                
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Seven) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Seven) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Seven) True cols)

                then Seven

                else tryValue board location Eight  
            
            Eight -> 
                
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Eight) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Eight) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Eight) True cols)

                then Eight

                else tryValue board location Nine 
            
            Nine -> 
                
                if (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Nine) True boxes) 
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Nine) True rows)
                && (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just Nine) True cols)

                then Nine

                else Zero
        
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