module QuickCheck where 
import Control.Monad (liftM2, liftM3)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Solver 
    
prop_valid_solution :: Board -> Bool 
prop_valid_solution board = helper (solve board) locations 
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
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 boxes) == 1 && 
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 rows) == 1 &&
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 cols) == 1 && (helper board t)

prop_valid_location :: Loc -> Board -> Bool 
prop_valid_location (row, col) inBoard = 
    let rows = [(row, c) | c <- [0..width - 1]] in 
    let cols = [(r, col) | r <- [0..height - 1]] in
    let boxes = getBox (row, col) in
    let board = solve inBoard in
    let v = Map.lookup (row, col) board in 
    case v of 
        Nothing -> error "Should not happen"
        Just val -> 
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 boxes) == 1 && 
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 rows) == 1 &&
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 cols) == 1 

prop_no_blanks :: Board -> Bool
prop_no_blanks board =  helper (solve board) locations 
    where 
        helper board [] = True 
        helper board ((row, col) : t) = 
            let rows = [(row, c) | c <- [0..width - 1]] in 
            let cols = [(r, col) | r <- [0..height - 1]] in
            let boxes = getBox (row, col) in
            let v = Map.lookup (row, col) board in 
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True boxes) && 
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True rows) &&
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True cols) && (helper board t)