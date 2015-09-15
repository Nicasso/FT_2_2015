
module Lab3 where

import Data.List
import System.Random
import Lecture3
import Testing
import Lab2Tests

-- 1. Propositional logic

-- Time spent 45 minutes

tautologyExample = Dsj [p, Neg (p)]

contradictionExample = Cnj [p, Neg (p)]

satisfiableExample = Equiv (Impl p q) (Impl (Neg q) (Neg p))

contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment 
entails :: Form -> Form -> Bool
entails x y = tautology (Impl x y)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv x y = tautology (Equiv x y)

-- 2. Testing the parse function

testParse :: (String, [Form]) -> Bool
testParse (x, y) = (parse x == y)

parseTests :: [Test]
parseTests = [ Test "parse test" testParse
             [("+(1 2)", [Dsj [p, q]]), ("*(1 +(2 -3))", [Cnj [p, Dsj[q, Neg (r)]]])]]

-- 3. Converting formulas into CNF

cnfExample = Impl p q

-- 4. Test the correctness of CNF Convertor with random tests using QuickCheck

props = [p, q, r]

genFormula :: Form
genFormula = do
				y <- (getRandomInt 4)
				formulaGenerator y

formulaGenerator :: Int -> Form
formulaGenerator n = do
						x <- (getRandomInt 2)
						if n < 0 then props !! x 
						else 
							if n == 0 then Equiv (formulaGenerator n-1) (formulaGenerator n-2)
							else
								if n == 1 then Impl (formulaGenerator n-1) (formulaGenerator n-2)
								else
									if n == 2 then Dsj [(formulaGenerator n-1), (formulaGenerator n-2)]
									else
										if n == 3 then Cnj [(formulaGenerator n-1), (formulaGenerator n-2)]
											else
												if n == 4 then Neg (formulaGenerator n-1) else props !! x

-----------------------------------------

-- Forms:

-- form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
-- form1 = (p -> q) <=> (not(q) -> not(p))
--          1  1 1   1      0	 1	  0
--  		1  0 0	 1      1	 0	  0	
--  		0  1 1   1      0    1    1
--			0  1 0   1      1    1    1
--
--					tautology

-- form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
-- form2 = (p -> q) <=> (not(p) -> not(q))
--          1  1 1   1     0	 1	  0
--  		1  0 0	 0     0	 1	  1	
--  		0  1 1   0     1     0    0
--			0  1 0   1     1     1    1
--
--					satisfiable

-- form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
-- form3 = (p -> q) /\ (q -> r) -> (p -> r)
--          1  1 1   1  1  1 1   1  1  1 1
--  		1  1 1   0  1  0 0   1  1  0 0
--  		1  0 0   0  0  1 1   1  1  1 1
--			1  0 0   0  0  1 0   1  1  0 0
--          0  1 1   1  1  1 1   1  0  1 1
--  		0  1 1   0  1  0 0   1  0  1 0
--  		0  1 0   1  0  1 1   1  0  1 1
--			0  1 0   1  0  1 0   1  0  1 0
--
--					tautology

