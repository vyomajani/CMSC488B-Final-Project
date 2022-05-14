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

import System.IO
import System.Directory
import Control.Monad
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ix(range)

import Solver
import Parser 
import FileIO

-- Types

data Game = Game
  { _board :: Board, 
    _cursor :: Loc,
    _solved :: Bool,
    _solution :: Board 
  } deriving (Show)

data Direction = North | South | East | West deriving (Eq, Show)

makeLenses ''Game

-- -- Constants



-- -- squares :: [Loc]
-- -- squares = range ((0,0), (width-1, height-1))

-- -- Functions

-- | Step forward in time
-- step :: Game -> Game
-- step g
--   | g ^. done = g
--   | isJust checkWin = g & done .~ True
--                         & won  .~ checkWin
--   | full g = g & done .~ True
--   | otherwise = g 
--   where checkWin = checkForWin g

-- | Step forward in time
step :: Game -> Game
step s 
  | checkSolution s = s & solved .~ True
  | otherwise = s 
    --flip execState s . runMaybeT $ do

  -- Do Nothing
  --return s

-- Initialization
initGame :: IO Game -- Using sampleSudoku
initGame = let b = boardConverter "0,0,0,2,6,9,7,8,1,0,0,0,5,7,1,4,9,3,0,0,0,8,3,4,5,6,2,8,2,6,0,0,0,3,4,7,3,7,4,0,0,0,9,1,5,9,5,1,0,0,0,6,2,8,5,1,9,3,2,6,0,0,0,2,4,8,9,5,7,0,0,0,7,6,3,4,1,8,0,0,0" in
  return $ Game { _board = b, --initBoard, 
                  _cursor = (0,0), 
                  _solved = False,
                  _solution = Solver.solve b }

-- SampleSudoku7
-- "0,0,0,2,6,9,0,8,1,0,0,0,5,7,1,4,9,3,0,0,0,8,3,4,5,6,2,8,2,6,0,0,0,3,4,7,3,7,4,0,0,0,9,1,5,9,5,1,0,0,0,6,2,8,5,1,9,3,2,6,0,0,0,2,4,8,9,5,7,0,0,0,7,6,3,4,1,8,0,0,0"

-- SampleSudoku6
-- "0,0,0,2,6,9,7,8,1,0,0,0,5,7,1,4,9,3,0,0,0,8,3,4,5,6,2,8,2,6,0,0,0,3,4,7,3,7,4,0,0,0,9,1,5,9,5,1,0,0,0,6,2,8,5,1,9,3,2,6,0,0,0,2,4,8,9,5,7,0,0,0,7,6,3,4,1,8,0,0,0"

{- Move cursor around board -}
move :: Direction -> Game -> Game 
move North g = g & cursor . _1 %~ (\x -> (x - 1) `mod` height)
move South g = g & cursor . _1 %~ (\x -> (x + 1) `mod` height)
move East g = g & cursor . _2 %~ (\y -> (y + 1) `mod` width)
move West g = g & cursor . _2 %~ (\y -> (y - 1) `mod` width)

{- Inputs a value into the playable board -}
register :: Value -> Game -> Game 
register val g = g & board  %~ Map.insert (g ^. cursor) val

{- Shows the solution from the original input board upon hitting Enter -}
showSolution :: Game -> Game 
showSolution g 
  | checkSolution g = g & solved .~ True
  | otherwise = g & board .~ (g ^. solution) 
                  & solved .~ True 

{- Return True if correct, False if incorrect -}
checkSolution :: Game -> Bool 
checkSolution g = helper (g ^. board) (g ^. solution) locations
  where 
    helper b s [] = True 
    helper b s (h : t)
      | Map.lookup h b == Map.lookup h s = helper b s t 
      | otherwise = False 

{- Shows a hint on the board upon hitting the H key -}
showHint :: Game -> Game 
showHint g = 
  let b = g ^. board in 
  let s = g ^. solution in 
  helper b s locations 
  where
    helper :: Board -> Board -> [Loc] -> Game
    helper b s [] = g & solved .~ True 
    helper b s (h : t) 
      | Map.lookup h b == Map.lookup h s = helper b s t 
      | otherwise = case Map.lookup h s of 
                      Nothing -> error "Should not happen"
                      Just x -> g & board .~ (Map.insert h x b)

{- Loads a board from a file "input.txt" -}
loadBoard :: Game -> Game 
loadBoard g = do
  newGame <- loadBoardHelper g
  return newGame
  
loadBoardHelper :: Game -> IO Game 
loadBoardHelper g = do
  newBoard <- boardFromFile "input.txt"
  return $ g & board .~ newBoard

