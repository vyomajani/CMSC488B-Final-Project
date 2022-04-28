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

-- Types

data Game = Game
  { _board :: Board, 
    _cursor :: Loc,
    _full :: Bool 
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
initGame :: IO Game
initGame = return $ Game { _board = initBoard, 
                  _cursor = (0,0), 
                  _full = False }

-- Move cursor around board 
move :: Direction -> Game -> Game 
move North g = g & cursor . _2 %~ (\y -> (y - 1) `mod` height) 
move South g = g & cursor . _2 %~ (\y -> (y + 1) `mod` height)
move East g = g & cursor . _1 %~ (\x -> (x + 1) `mod` width)
move West g = g & cursor . _1 %~ (\x -> (x - 1) `mod` width)

-- Upon hitting Enter, checks the solution 
register :: Value -> Game -> Game 
register val g = g & board  %~ Map.insert (g ^. cursor) val

-- register :: Game -> Game
-- register g
--   | g ^. done = g
--   | isJust (g ^. board . at (g ^. cursor)) = g
--   | otherwise = g & board  %~ Map.insert (g ^. cursor) (g ^. player)
--                   & player %~ next

-- need to initialize remaining cells with 0s before calling solve
showSolution :: Game -> Game 
showSolution g = g & board %~ Solver.solve 


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
