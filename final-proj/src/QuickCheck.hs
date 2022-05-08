module QuickCheck where 

import Control.Monad (liftM2, liftM3)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
--import Test.QuickCheck
{-
import Test.QuickCheck
  ( Arbitrary (..),
         Gen,
         OrderedList (..),
         Property,
         Testable (..),
         choose,
         classify,
         elements,
         forAll,
         frequency,
         label,
         oneof,
         quickCheck,
         sample,
         sized,
         withMaxSuccess,
         (==>),
     ) -}
import Control.Lens hiding (element)
import Control.Lens.TH
import Solver 
import Game

{-
readBoard :: [Value] -> Board
readBoard lst = helper lst 0 0 initSudoku where
    helper [] row col board = board
    helper (h : t) row col board =
        if row >= 9 then board
        else if col >= 9 then
            if row <= 7 then helper t (row + 1) 1 (Map.insert (row + 1, 0) h board)
            else board
        else helper t row (col + 1) (Map.insert (row, col) h board)

{- Inputs a value into the playable board -}
register :: Value -> Game -> Game
register val g = g & board  %~ Map.insert (g ^. cursor) val

quarterRotate :: Game -> Game
quarterRotate g = (over board boardHelper g) where
  boardHelper :: Board -> Board
  boardHelper b = Map.fromList (listHelper (Map.toList b)) where
    listHelper :: 
-}

-- (over cursor (\_ -> (0,width-1)) g)
-- Map.toList view board g

getValues :: Game -> [Value]
getValues g = helper (Map.toList (view board g)) where
  helper :: [(Loc, Value)] -> [Value]
  helper ((_,v):xs) = v:(helper xs)
  helper [] = []

rightRotate :: Game -> Game
rightRotate g = helper (over cursor (\_ -> (0,width-1)) g) (0, width-1) (getValues g) where
  helper g (r,c) [] = g
  helper g (r,c) (v:vs) =
    if c < 0 then g
    else if r == height-1 then helper (move South (move West (register v g))) (0,c-1) vs
    else helper (move South (register v g)) (r+1,c) vs

flipY :: Game -> Game
flipY g = helper (over cursor (\_ -> (height-1,0)) g) (height-1,0) (getValues g) where
  helper g (r,c) [] = g
  helper g (r,c) (v:vs) =
    if r < 0 then g
    else if c == width-1 then helper (move North (move East (register v g))) (r-1,0) vs
    else helper (move East (register v g)) (r,c+1) vs

flipX :: Game -> Game
flipX g = helper (over cursor (\_ -> (0,width-1)) g) (0,width-1) (getValues g) where
  helper g (r,c) [] = g
  helper g (r,c) (v:vs) =
    if r == height then g
    else if c == 0 then helper (move South (move West (register v g))) (r+1,width-1) vs
    else helper (move West (register v g)) (r,c-1) vs

{-
randFromList :: [Value] -> IO Value
randFromList xs = (Test.QuickCheck.generate . Test.QuickCheck.elements) xs

randFromList2 :: [Value] -> Value
randFromList2 xs = do
  x <- Test.QuickCheck.elements xs
  return x
-}
{-
mapHelper :: Value -> [Value] -> [(Value,Value)]
mapHelper Zero _ = [(Zero, Zero)]
mapHelper v ps = do
  g <- randFromList ps
  return $ (v, v):(mapHelper (next v) (filter (\a -> a /= v) ps)) 
-}

--convertMap :: Map Value Value
--convertMap = Map.fromList $ (mapHelper One [One, Two, Three, Four, Five, Six, Seven, Eight, Nine])

{- How can we generate an arbitrary board? 
    instance (Ord a, Arbitrary a) => Arbitrary Board where
    arbitrary :: Gen Board
    arbitrary = sized gen where 
        gen :: Int -> Gen Board
        gen n = 
            frequency 
            [
                (1, return E), 
                (n, do 
                    e <- arbitrary 
                    new <- gen (n `div` 2)
                    return $ insert e new) 
            ]

    shrink :: AVL a -> [AVL a]
    shrink E = [] -- can't do [E] bc infinite loop 
    shrink (N _ E _ E) = [E]
    shrink (N _ l e r) = [(N 1 E e E), l, r] ++ shrink l ++ shrink r -} 
    
prop_valid_solution :: Board -> Bool 
prop_valid_solution board = helper board locations 
    where 
        helper board [] = True 
        helper board ((row, col) : t) = 
            let rows = [(row, c) | c <- [0..width - 1]] in 
            let cols = [(r, col) | r <- [0..height - 1]] in
            let boxes = getBox (row, col) in
            let v = Map.lookup (row, col) board in 
            case v of 
                Nothing -> error "Should not happen"
                Just val -> 
                    (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True boxes) && 
                    (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True rows) &&
                    (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True cols) && (helper board t)

prop_valid_location :: Loc -> Board -> Bool 
prop_valid_location (row, col) board = 
    let rows = [(row, c) | c <- [0..width - 1]] in 
    let cols = [(r, col) | r <- [0..height - 1]] in
    let boxes = getBox (row, col) in
    let v = Map.lookup (row, col) board in 
    case v of 
        Nothing -> error "Should not happen"
        Just val -> 
            (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True boxes) && 
            (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True rows) &&
            (foldr (\loc -> \acc -> acc && Map.lookup loc board /= Just val) True cols)

    
