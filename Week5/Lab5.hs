module Lab5 where

import Data.List
import System.Random
import Lecture5

-- 1.

sudokuToSolve :: Grid
sudokuToSolve = [[0,0,0,3,0,0,0,0,0],
                [0,0,0,7,0,0,3,0,0],
                [2,0,0,0,0,0,0,0,8],
                [0,0,6,0,0,5,0,0,0],
                [0,9,1,6,0,0,0,0,0],
                [3,0,0,0,7,1,2,0,0],
                [0,0,0,0,0,0,0,3,1],
                [0,8,0,0,4,0,0,0,0],
                [0,0,2,0,0,0,0,0,0]]