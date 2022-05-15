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

-- DONE
-- 1) Write out solve9 function to solve9 an input sudoku board 
-- 2) Create basic UI for user to put numbers into 
-- 3) Incorporate solution into UI 
-- 4) Create a csv parser to parse sudoku inputs
-- 5) Automatically check whether user's solution is a valid solution
-- 6) Hints  
-- 7) Make it all nxn

-- TODO
-- 1) Create QuickCheck for checking sudoku solver 
-- 3) Freeze the original board so users can't modify original input 
-- 4) Take in file inputs 
-- 5) Ask if user want to see the solution yet or if they want to continue solving 

-- Note: I'm using -- if it's a temporary comment / something to address, {--} if its something to keep that tells us what that section of code is doing 

{- Types -} 

type Loc = (Int, Int)

{- Zero means empty -}
data Value = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine 
 deriving (Eq, Show)

type Board = Map Loc Value

{- Original input board (do not modify Zeros) -} 
type Input = Map Loc Value 

{- Constants -} 

initBoard :: Board 
initBoard = Map.empty 

{- Functions -}

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
