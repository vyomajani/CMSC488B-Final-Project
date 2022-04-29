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

import Solver
import Parser 

-- Types

data Game = Game
  { _board :: Board, 
    _cursor :: Loc,
    _full :: Bool,
    _input :: Board 
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
step s = flip execState s . runMaybeT $ do

  -- Do Nothing
  return s

-- Initialization
initGame :: IO Game -- Using sampleSudoku7
initGame = let b = boardConverter "0,0,0,2,6,9,0,8,1,0,0,0,5,7,1,4,9,3,0,0,0,8,3,4,5,6,2,8,2,6,0,0,0,3,4,7,3,7,4,0,0,0,9,1,5,9,5,1,0,0,0,6,2,8,5,1,9,3,2,6,0,0,0,2,4,8,9,5,7,0,0,0,7,6,3,4,1,8,0,0,0" in
  return $ Game { _board = b, --initBoard, 
                  _cursor = (0,0), 
                  _full = False,
                  _input = b }

{- Move cursor around board -}
move :: Direction -> Game -> Game 
move North g = g & cursor . _2 %~ (\y -> (y - 1) `mod` height) 
move South g = g & cursor . _2 %~ (\y -> (y + 1) `mod` height)
move East g = g & cursor . _1 %~ (\x -> (x + 1) `mod` width)
move West g = g & cursor . _1 %~ (\x -> (x - 1) `mod` width)

{- Inputs a value into the playable board -}
register :: Value -> Game -> Game 
register val g = g & board  %~ Map.insert (g ^. cursor) val

-- register :: Game -> Game
-- register g
--   | g ^. done = g
--   | isJust (g ^. board . at (g ^. cursor)) = g
--   | otherwise = g & board  %~ Map.insert (g ^. cursor) (g ^. player)
--                   & player %~ next

{- Shows the solution from the original input board upon hitting Enter,
   using input board in case user's solution is wrong -}
showSolution :: Game -> Game 
showSolution g = g & input %~ Solver.solve -- need to reassign board as well 


-- Checks if the board is full 
-- isFull :: Game -> Bool 
-- isFull g = all (\l -> isJust (g ^. board . at l)) squares 


-- Check if the solution on the board is correct 
-- checkForWin :: Game -> Bool 
-- checkForWin g = 
--   if isFull g then 
--   else False 

-- checkForWin :: Game -> Maybe (Player, Loc)
  -- case map (uncurry . isWinning g) [(ls, p) | ls <- traversals, p <- [X, O]] of 
  -- [] -> Nothing 
  -- (x : _) -> Just x -- some more stuff
