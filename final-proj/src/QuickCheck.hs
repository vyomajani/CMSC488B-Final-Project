module QuickCheck where 

    import Control.Monad (liftM2, liftM3)
    import Data.List (nub)
    import Data.Map (Map)
    import qualified Data.Map as Map
    -- import Test.QuickCheck
    --     ( Arbitrary (..),
    --         Gen,
    --         OrderedList (..),
    --         Property,
    --         Testable (..),
    --         choose,
    --         classify,
    --         elements,
    --         forAll,
    --         frequency,
    --         label,
    --         oneof,
    --         quickCheck,
    --         sample,
    --         sized,
    --         withMaxSuccess,
    --         (==>),
    --     )

    import Solver 

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

    
