{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Game where

import Control.Applicative ((<|>))
import Control.Monad (guard)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import System.Random (Random(..), newStdGen)

import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ix(range)

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

-}

-- Types

type Loc = (Int, Int)

data Value = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 deriving (Eq, Show)

type Board = Map Loc Value 

data Game = Game
  { _board :: Board, 
    _cursor :: Loc,
    _full :: Bool 
  } deriving (Show)

data Direction = North | South | East | West deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int 
height = 9 
width = 9

initBoard :: Board 
initBoard = Map.empty 



-- squares :: [Loc]
-- squares = range ((0,0), (width-1, height-1))

-- Functions

-- | Step forward in time
step :: Game -> Game
step g
  | g ^. done = g
  | isJust checkWin = g & done .~ True
                        & won  .~ checkWin
  | full g = g & done .~ True
  | otherwise = g 
  where checkWin = checkForWin g

-- | Step forward in time
-- step :: Game -> Game
-- step s = flip execState s . runMaybeT $ do

  -- Do Nothing
  return s

-- Initialization
initGame :: IO Game
initGame = 
  return $ Game { _board = initBoard, 
                  _cursor = (0,0), 
                  _full = False }

-- Move cursor around board 
move :: Direction -> Game -> Game 
move North g = g & cursor . _2 %~ (\y -> (y - 1) `mod` height) 
move South g = g & cursor . _2 %~ (\y -> (y + 1) `mod` height)
move East g = g & cursor . _1 %~ (\x -> (x + 1) `mod` width)
move West g = g & cursor . _1 %~ (\x -> (x - 1) `mod` width)

-- Upon hitting Enter, checks the solution 
register :: Game -> Game 
register g 
  | g ^. full = -- check solution 
  | otherwise = -- display that not full (could also drop a hint?)

  -- | g ^. full = g -- if the board is full, don't place an X or an O
  -- | isJust (g ^. board . at (g ^. cursor)) = g -- If the location has a value already, don't place 
  -- | otherwise = g & board . at (g ^. cursor) %~ Map.insertWith (flip const) (g ^. cursor) (g ^. player) -- more stuff 


-- Checks if the board is full 
isFull :: Game -> Bool 
isFull g = all (\l -> isJust (g ^. board . at l)) squares 


-- Check if the solution on the board is correct 
checkForWin :: Game -> Bool 
checkForWin g = 
  if isFull g then 
  else False 

-- checkForWin :: Game -> Maybe (Player, Loc)
  -- case map (uncurry . isWinning g) [(ls, p) | ls <- traversals, p <- [X, O]] of 
  -- [] -> Nothing 
  -- (x : _) -> Just x -- some more stuff
