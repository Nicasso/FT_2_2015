module Assignment4

where 

import Lecture5

-- To be able to generate Sudoku problems with three or more empty blocks we first generate a filled in Sudoku.
-- By doing so we can reverse the solution into a problem.
-- The main2 function is the starting point which will first generate a filled in Sudoku problem and it will pass it on to genProblem2.
-- genProblem2 first calls the minimalizeBlocks function with the node, all (Row,Column) possibilities 
-- per block (which it will get from the allBLocks function) and the max amount of blocks it may delete.
-- We put this value to 9 (of course it will never empty all blocks) but this way it will try to empty as many as possible.
-- The minimalizeBlocks function will check if the Sudoku still has a unique solution after deleting all (Row,Column) values from a block.
-- The emptying of all those values happens recursively using foldl in combination with the eraseN function, so one by one it will empty the values of a block.
-- If the block does has a unique solution it will keep the new Sudoku and try again with the next block, this will continue till until all blocks are checked.
-- If a block does not have unique solution after emptying a single block, it will not empty that block and try the next one.
-- As a result main2 will return Sudoku problems with 3 and sometimes 4 empty blocks.

-- Run using: main2

-- Time spent: 6 hours

allBlocks :: [[(Row,Column)]]
allBlocks = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks]

minimalizeBlocks :: Node -> [[(Row,Column)]] -> Int -> Node
minimalizeBlocks n [] m = n
minimalizeBlocks n _ 0 = n
minimalizeBlocks n (x:xs) m | uniqueSol n' = minimalizeBlocks n' xs (m-1)
                            | otherwise = minimalizeBlocks n xs m
    where n' = foldl eraseN n x

genProblem2 :: Node -> IO Node
genProblem2 n = do 
                  let m = minimalizeBlocks n allBlocks 9
                  o <- randomize (filledPositions (fst m))
                  return (minimalize m o)

main2 :: IO ()
main2 = do
          [r] <- rsolveNs [emptyN]
          s  <- genProblem2 r
          showNode s