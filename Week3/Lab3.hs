
module Lab3 where

import Data.List
import System.Random
import Lecture3
import Testing
import Data.Char

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

-- Time spent 1 hour

testParse :: (String, [Form]) -> Bool
testParse (x, y) = (parse x == y)

parseTests :: [Test]
parseTests = [ Test "parse test" testParse
             [("+(1 2)", [Dsj [p, q]]), ("*(1 +(2 -3))", [Cnj [p, Dsj[q, Neg (r)]]])]]

-- 3. Converting formulas into CNF

-- Time spent 6 hours

cnfExample1 = Equiv p q
cnfExample2 = Impl (Impl p q) r
cnfExample3 = Equiv p (Cnj [q, r])
cnfExample4 = Dsj [p, (Cnj [q, r])]
cnfExample5 = Cnj [p, (Dsj [q, r])]
cnfExample6 = Dsj [p, (Dsj [q, r])]
cnfExample7 = Cnj [p, (Cnj [q, r])]

convertToCNF :: Form -> Form
convertToCNF x = cnf(nnf(arrowfree x))

cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg x) = Neg x
cnf (Cnj x) = Cnj (map cnf x)
cnf (Dsj x) = distribute (map cnf x)

distributionLaw :: Form -> Form -> Form
distributionLaw (Cnj x) y = Cnj (map (\z -> distributionLaw z y) x)    
distributionLaw x (Cnj y) = Cnj (map (\z -> distributionLaw x z) y)
distributionLaw x y = Dsj [x, y]

distribute :: [Form] -> Form
distribute [x] = x
distribute (x:xs) = distributionLaw x (distribute xs)

-- 4. Test the correctness of CNF Convertor with random tests using QuickCheck

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

props = [p, q, r]

-- s is length of props
startGenForm :: Int -> IO ()
startGenForm s = genFormula s 1 100

genFormula :: Int -> Int -> Int -> IO ()
genFormula s k m = if k == m then print (show m ++ " tests passed")
                else do 
					n <- (getRandomInt 4)
					p <- (getRandomInt (s-1))
					print ("''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''")
					print ("TEST " ++ show k) 
					print ("Original form: ")
					print (formulaGenerator n p (s-1))
					print ("CNF: ")
					print (convertToCNF (formulaGenerator n p (s-1)))
					genFormula s (k+1) m

formulaGenerator :: Int -> Int -> Int -> Form
formulaGenerator n p s  | (n < 0 && p >= 0 && p < s) = props !! p
					    | (n == 0 && p >= 0 && p < s) = Equiv (formulaGenerator (n-1) (p-1) s) (formulaGenerator (n-2) p s)
						| (n == 1 && p >= 0 && p < s) = Impl (formulaGenerator (n-1) p s) (formulaGenerator n (p+1) s)
						| (n == 2 && p >= 0 && p < s) = Dsj [(formulaGenerator (n-1) p s), (formulaGenerator n (p-1)) s]
						| (n == 3 && p >= 0 && p < s) = Cnj [(formulaGenerator n (p+1) s), (formulaGenerator (n-1) (p+1) s)]
						| (n == 4 && p >= 0 && p < s) = Neg (formulaGenerator (n-1) p s) 
						| otherwise = props !! s

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