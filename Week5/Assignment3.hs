module Assignment3 where

import Lecture5

-- TestMinimal will generate a random Sudoku problem P using the code from the lecture. 
-- It will test if the generated problem is minimal, which means that the original problem P has only one unique solution 
--  and that if you remove a single hint from the problem, a new Sudoku problem P' will arise that has multiple solutions.
-- First we generate a list with all the positions of the hints in the Sudoku grid.
-- Then we check if the Sudoku P is unique or not.
-- We then check if the Sudoku P' still has a unique solution using uniqueSol when we remove the the first hint, if so the Suduku is not minimal.
-- If the Sudoku with a missing hint is doesn't have a unique solution, we try it again but only then we remove only the second hint
-- And if all P' of P don't have a unique solutions, the Soduko P is minimal.

-- Run using: testMinimal

-- Time spent: 2 hours

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