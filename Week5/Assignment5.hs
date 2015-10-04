module Assignment5
  
where 
  
import Assignment1

-- Our NRC implementation of assignment one doesn't only make it possible to solve NRC Sudoku problems, but also to generate them.
-- So by importing Assignment1 and creating a seperate mainNRC function we can now generate NRC Sudoku problems.

-- Run using: mainNrc

-- Time spent: 0.5 hours

mainNrc :: IO ()
mainNrc = do 
            [r] <- rsolveNs [emptyN]
            s <- genProblem r
            showNode s