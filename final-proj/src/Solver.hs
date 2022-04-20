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

-- Types 

type Loc = (Int, Int)

data Value = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine -- 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 deriving (Eq, Show)
 -- 0 means it hasn't been filled in yet? 

type Board = Map Loc Value -- would it be easier to have a 2D list? 
-- no bc you can't index 2D lists well

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

-- Functions 

-- should this be a parser? How are we creating sudoku boards? 
-- Creates empty sudoku board (All 0s)
initSudoku :: Board 
initSudoku = helper locations initBoard
    where 
        helper [] board = board
        helper (h : t) board = helper t (insert h Zero board)

solve :: Board -> Board 
solve board = 



    -- let row = -1 in let col = -1 in let isEmpty = True in 
    -- let _ = doubleFor 
    -- where 
    --     doubleFor i j = if notMember (i, j) board then -- still a missing value
    --     row = i, col = j, isEmpty = False 
    -- else 
