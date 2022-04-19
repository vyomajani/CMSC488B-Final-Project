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

data Value = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 deriving (Eq, Show)
 -- 0 means it hasn't been filled in yet

type Board = Map Loc Value -- would it be easier to have a 2D list? 
-- no bc you can't index 2D lists well

-- Constants 

height, width :: Int 
height = 9 
width = 9

-- Functions 

solve :: Board -> Board 
solve board = 
    let row = -1 in let col = -1 in let isEmpty = True in 
    let _ = doubleFor 
    where 
        doubleFor i j = if notMember (i, j) board then -- still a missing value
        row = i, col = j, isEmpty = False 
    else 
