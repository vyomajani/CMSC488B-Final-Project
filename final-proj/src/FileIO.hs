{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module FileIO where

import System.IO
import System.Directory
import Control.Monad
import Parser
import Solver
import Data.Map (Map)
import qualified Data.Map as Map

-- Takes a filename for a csv of digits, converts that to a board, then pretty prints it
-- Not totally useful right now, but the prettyprint could be replaced with something else
boardFromFile :: String -> IO Board
boardFromFile fileName = do
  input <- openFile fileName ReadMode
  inputContents <- hGetContents input
--  hClose input
  return $ boardConverter inputContents

-- Takes a board and saves it to a file as CSV
boardToFile :: Board -> String -> IO ()
boardToFile b fileName = do
  h <- openFile fileName WriteMode
  hPutStr h $ boardToString b
  hClose h

-- helper for above
boardToString :: Board -> String
boardToString b = helper (Map.toList b)
  where
    helper [] = ""
    helper ((_, v) : []) =
      (valueConverter v)
    helper ((_, v) : t) =
      (valueConverter v) ++ "," ++ (helper t)