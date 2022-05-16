module Parser where 

import Solver
import NineByNineSolver 
import FourByFourSolver

import Control.Applicative
import Control.Monad (guard)
import Data.Char
import Text.Read (readMaybe)
import Prelude hiding (filter)

import Data.Map (Map)
import qualified Data.Map as Map

{- 

parser parses a string of dig, dig, dig, ...

-}

{- Types -}

newtype Parser a = P { doParse :: String -> Maybe (a, String) }

{- Library -}

instance Functor Parser where 
    fmap f a = P $ \s -> do 
        (c, cs) <- doParse a s 
        return (f c, cs)

instance Applicative Parser where
    pure x = P $ \s -> Just (x, s)

    (<*>) p1 p2 = P $ \s -> do
        (f, s') <- doParse p1 s
        (x, s'') <- doParse p2 s'
        return (f x, s'')

{- Parses a given char -}
char :: Char -> Parser Char 
char c = satisfy (== c)

satisfy :: (Char -> Bool) -> Parser Char 
satisfy f = P $ \s -> case s of 
    (h : t) -> if (f h) then Just (h, t) else Nothing 
    [] -> Nothing 


{- Choosing a parser -}
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing y = y

chooseFirstP :: Parser a -> Parser a -> Parser a
p1 `chooseFirstP` p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

{- Functions -}

parseVal :: Parser Value 
parseVal = P $ \s -> 
    case s of 
        [] -> Nothing 
        (h : t) -> case h of 
            '1' -> Just (One, t)
            '2' -> Just (Two, t)
            '3' -> Just (Three, t)
            '4' -> Just (Four, t)
            '5' -> Just (Five, t)
            '6' -> Just (Six, t)
            '7' -> Just (Seven, t)
            '8' -> Just (Eight, t)
            '9' -> Just (Nine, t)
            _ -> Just (Zero, t)

parseComma :: Parser Value 
parseComma = parseVal <* char ','

manyVals :: Parser [Value]
manyVals = ((:) <$> parseComma <*> manyVals) `chooseFirstP` (fmap (:[]) parseVal )--pure []
-- doParse (manyVals) "1,2,3,4"
-- Outputs a list [One,Two,Three,Four]


{- Converts a string of "dig, dig, dig, ..." to a Sudoku board -}
boardConverter9x9 :: String -> Board 
boardConverter9x9 s = case doParse (manyVals) s of 
    Nothing -> NineByNineSolver.initSudoku 
    Just (lst, _) -> readBoard9x9 lst 

readBoard9x9 :: [Value] -> Board 
readBoard9x9 lst = helper lst 0 0 NineByNineSolver.initSudoku where 
    helper [] row col board = board 
    helper (h : t) row col board = 
        if row >= 9 then board 
        else if col >= 9 then 
            if row <= 7 then helper t (row + 1) 1 (Map.insert (row + 1, 0) h board)
            else board 
        else helper t row (col + 1) (Map.insert (row, col) h board)

boardConverter4x4 :: String -> Board 
boardConverter4x4 s = case doParse (manyVals) s of 
    Nothing -> FourByFourSolver.initSudoku 
    Just (lst, _) -> readBoard4x4 lst 

readBoard4x4 :: [Value] -> Board 
readBoard4x4 lst = helper lst 0 0 FourByFourSolver.initSudoku where 
    helper [] row col board = board 
    helper (h : t) row col board = 
        if row >= 4 then board 
        else if col >= 4 then 
            if row <= 2 then helper t (row + 1) 1 (Map.insert (row + 1, 0) h board)
            else board 
        else helper t row (col + 1) (Map.insert (row, col) h board)

-- prettyPrint (boardConverter "1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,2,2,2,3,4,5,6,2,3,4,4,3,2,3,4,3,2,3,4,3,2,3,4,3,2,3,4,3,2,3,4,3,1,2,3,4,5,6,7,6,5,4,3,2,1,2,3,4,5,6,5,4,3,2,1")