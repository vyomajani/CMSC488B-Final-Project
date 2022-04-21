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

import Game 

-- Coding Plan
-- 1) Write out solve function to solve an input sudoku board 
-- 2) Create basic UI for user to put numbers into 
-- 3) Incorporate solution into UI 
-- 4) Create QuickCheck for checking sudoku solver 
-- 5) Create a csv parser to parse sudoku inputs 
-- 6) Make it all nxn


-- Types 

type Loc = (Int, Int)

data Value = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine -- 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 deriving (Eq, Show)
 -- 0 means it hasn't been filled in yet? 

type Board = Map Loc Value

-- Constants 

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
boxes = [(x, y) | x <- [0..2], y <- [0..2]] ++
        [(x, y) | x <- [0..2], y <- [3..5]] ++
        [(x, y) | x <- [0..2], y <- [6..8]] ++
        [(x, y) | x <- [3..5], y <- [0..2]] ++
        [(x, y) | x <- [3..5], y <- [3..5]] ++
        [(x, y) | x <- [3..5], y <- [6..8]] ++
        [(x, y) | x <- [6..8], y <- [0..2]] ++
        [(x, y) | x <- [6..8], y <- [3..5]] ++
        [(x, y) | x <- [6..8], y <- [6..8]]

-- should this be a parser? How are we creating sudoku boards? 
-- Creates empty sudoku board (All 0s)
initSudoku :: Board 
initSudoku = helper locations initBoard
    where 
        helper [] board = board
        helper (h : t) board = helper t (insert h Zero board)
        -- for the purposes of testing? Until we have a parser? 

-- Functions 

solve :: Board -> Board 
solve board = 
    helper board loc 
    where 
