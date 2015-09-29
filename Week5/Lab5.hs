module Lab5 where

import Data.List
import System.Random
import Lecture5

-- 1.

-- See Assignment1.hs

-- 2.

-- See Assignment2.hs

-- 3.

-- TestMinimal will generate a random Sudoku problem P using the code from the lecture. 
-- It will test if the generated problem is minimal, which means that the original problem P has only one unique solution 
--  and that if you remove a single hint from the problem, a new Sudoku problem P' will arise that has multiple solutions.
-- First we generate a list with all the positions of the hints in the Sudoku grid.
-- Then we check if the Sudoku P is unique or not.
-- We then check if the Sudoku P' still has a unique solution using uniqueSol when we remove the the first hint, if so the Suduku is not minimal.
-- If the Sudoku with a missing hint is doesn't have a unique solution, we try it again but only then we remove only the second hint
-- And if all P' of P don't have a unique solutions, the Soduko P is minimal.

testMinimal :: IO ()
testMinimal = do [r] <- rsolveNs [emptyN]
                 s <- genProblem r
                 showNode s
                 if (uniqueSol s) == True then
                  print ("P has a unique solution.") else
                   error ("P does not have a unique solution, thus P is minimal")
                 if (tryRemovingHint s (filledPositions (fst s)) 0) == True then
                  print ("All P' of P have multiple solutions, thus P is minimal") else
                   error ("A P' of P also has a unique solution, thus P is not minimal")

tryRemovingHint :: Node -> [(Row,Column)] -> Int -> Bool
tryRemovingHint s rc x | uniqueSol ( eraseN s (rc!!x) ) == True = False
                       | uniqueSol ( eraseN s (rc!!x) ) == False = if (length rc > x+1) then tryRemovingHint s rc (x+1) else True

-- 4.

blockies :: Int -> [(Row,Column)]
blockies 1 = [ (x, y) | x <- [1..3], y <- [1..3]]
blockies 2 = [ (x, y) | x <- [1..3], y <- [4..6]]
blockies 3 = [ (x, y) | x <- [1..3], y <- [7..9]]
blockies 4 = [ (x, y) | x <- [4..6], y <- [1..3]]
blockies 5 = [ (x, y) | x <- [4..6], y <- [4..6]]
blockies 6 = [ (x, y) | x <- [4..6], y <- [7..9]]
blockies 7 = [ (x, y) | x <- [7..9], y <- [1..3]]
blockies 8 = [ (x, y) | x <- [7..9], y <- [4..6]]
blockies 9 = [ (x, y) | x <- [7..9], y <- [7..9]]

eraseSBlock :: Sudoku -> (Row,Column) -> Sudoku
eraseSBlock s (r,c) (x,y) | (r,c) == (x,y) = 0
                      | otherwise      = s (x,y)

eraseNBLock :: Node -> [(Row,Column)] -> Node
eraseNBLock n ((r,c):rcs) = (s, constraints s) 
  where s = eraseSBlock (fst n) (r,c)

minimalizeBlocks :: Node -> [(Row,Column)] -> Node
minimalizeBlocks n [] = n
minimalizeBlocks n ((r,c):rcs) | uniqueSol n' = minimalizeBlocks n' rcs
                               | otherwise    = minimalizeBlocks n  rcs
   where n' = eraseNBLock n (blockies c)





eraseS2 :: Sudoku -> (Row,Column) -> Sudoku
eraseS2 s (r,c) (x,y) | (r,c) == (x,y) = 0
                      | otherwise      = s (x,y)

eraseN2 :: Node -> (Row,Column) -> Node
eraseN2 n (r,c) = (s, constraints s) 
  where s = eraseS2 (fst n) (r,c) 

minimalize2 :: Node -> [(Row,Column)] -> Node
minimalize2 n [] = n
minimalize2 n ((r,c):rcs) | uniqueSol n' = minimalize2 n' rcs
                          | otherwise    = minimalize2 n  rcs
   where n' = eraseN2 n (r,c)

filledPositions2 :: Sudoku -> [(Row,Column)]
filledPositions2 s = [ (r,c) | r <- positions,  
                              c <- positions, s (r,c) /= 0 ]

genProblem2 :: Node -> IO Node
genProblem2 n = do ys <- randomize xs
                   --return (minimalize2 n (filledPositions2 (fst (minimalizeBlocks n ys))))
                   return (minimalizeBlocks n ys)
   where xs = filledPositions2 (fst n)

main2 :: IO ()
main2 = do [r] <- rsolveNs [emptyN]
           showNode r
           s  <- genProblem2 r
           showNode s