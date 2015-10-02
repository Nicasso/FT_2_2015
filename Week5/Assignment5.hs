module Assignment5
  
where 
  
import Assignment1

mainNrc :: IO ()
mainNrc = do 
            [r] <- rsolveNs [emptyN]
            s <- genProblem r
            showNode s