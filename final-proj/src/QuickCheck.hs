module QuickCheck where 
import Control.Monad (liftM2, liftM3)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Solver
import NineByNineSolver
import FourByFourSolver
    
{- PBTs for 9x9 boards - assume input board is a valid 9x9 sudoku input board -}

prop_valid_solution_9x9 :: Board -> Bool 
prop_valid_solution_9x9 board = helper (NineByNineSolver.solve board) NineByNineSolver.locations 
    where 
        helper board [] = True 
        helper board ((row, col) : t) = 
            let rows = [(row, c) | c <- [0..NineByNineSolver.width - 1]] in 
            let cols = [(r, col) | r <- [0..NineByNineSolver.height - 1]] in
            let boxes = NineByNineSolver.getBox (row, col) in
            let v = Map.lookup (row, col) board in 
            case v of 
                Nothing -> error "Should not happen"
                Just val -> 
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 boxes) == 1 && 
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 rows) == 1 &&
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 cols) == 1 && (helper board t)

prop_valid_location_9x9 :: Loc -> Board -> Bool 
prop_valid_location_9x9 (row, col) inBoard = 
    let rows = [(row, c) | c <- [0..NineByNineSolver.width - 1]] in 
    let cols = [(r, col) | r <- [0..NineByNineSolver.height - 1]] in
    let boxes = NineByNineSolver.getBox (row, col) in
    let board = NineByNineSolver.solve inBoard in
    let v = Map.lookup (row, col) board in 
    case v of 
        Nothing -> error "Should not happen"
        Just val -> 
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 boxes) == 1 && 
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 rows) == 1 &&
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 cols) == 1 

prop_no_blanks_9x9 :: Board -> Bool
prop_no_blanks_9x9 board =  helper (NineByNineSolver.solve board) NineByNineSolver.locations 
    where 
        helper board [] = True 
        helper board ((row, col) : t) = 
            let rows = [(row, c) | c <- [0..NineByNineSolver.width - 1]] in 
            let cols = [(r, col) | r <- [0..NineByNineSolver.height - 1]] in
            let boxes = NineByNineSolver.getBox (row, col) in
            let v = Map.lookup (row, col) board in 
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True boxes) && 
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True rows) &&
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True cols) && (helper board t)


{- PBTs for 4x4 boards - assume input board is a valid 4x4 sudoku input board -}

prop_valid_solution_4x4 :: Board -> Bool 
prop_valid_solution_4x4 board = helper (FourByFourSolver.solve board) FourByFourSolver.locations 
    where 
        helper board [] = True 
        helper board ((row, col) : t) = 
            let rows = [(row, c) | c <- [0..FourByFourSolver.width - 1]] in 
            let cols = [(r, col) | r <- [0..FourByFourSolver.height - 1]] in
            let boxes = FourByFourSolver.getBox (row, col) in
            let v = Map.lookup (row, col) board in 
            case v of 
                Nothing -> error "Should not happen"
                Just val -> 
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 boxes) == 1 && 
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 rows) == 1 &&
                    (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 cols) == 1 && (helper board t)

prop_valid_location_4x4 :: Loc -> Board -> Bool 
prop_valid_location_4x4 (row, col) inBoard = 
    let rows = [(row, c) | c <- [0..FourByFourSolver.width - 1]] in 
    let cols = [(r, col) | r <- [0..FourByFourSolver.height - 1]] in
    let boxes = FourByFourSolver.getBox (row, col) in
    let board = FourByFourSolver.solve inBoard in
    let v = Map.lookup (row, col) board in 
    case v of 
        Nothing -> error "Should not happen"
        Just val -> 
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 boxes) == 1 && 
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 rows) == 1 &&
            (foldr (\loc -> \acc -> acc + (if Map.lookup loc board == Just val then 1 else 0)) 0 cols) == 1 

prop_no_blanks_4x4 :: Board -> Bool
prop_no_blanks_4x4 board =  helper (FourByFourSolver.solve board) FourByFourSolver.locations 
    where 
        helper board [] = True 
        helper board ((row, col) : t) = 
            let rows = [(row, c) | c <- [0..FourByFourSolver.width - 1]] in 
            let cols = [(r, col) | r <- [0..FourByFourSolver.height - 1]] in
            let boxes = FourByFourSolver.getBox (row, col) in
            let v = Map.lookup (row, col) board in 
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True boxes) && 
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True rows) &&
            (foldr (\loc -> \acc -> acc && (Map.lookup loc board /= Just Zero)) True cols) && (helper board t)