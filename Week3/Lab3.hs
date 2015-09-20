
module Lab3 where

import Data.List
import System.Random
import Lecture3
import Testing
import Data.Char
import Test.QuickCheck

-- 1. Propositional logic

-- The contradiction is tested by checking if the formula is false in all the possible valuations.
-- The tautology is tested by checking if the formula is true in all the possible valuations.
-- The logical entailment is tested by checking if the implication between the formula x to y is true in all possible valuations.
-- The logical equivalence is tested by checking if the equivalence between the formula x and y is true in all possible valuations.

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

-- The parse function is tested using the testing method from last week.
-- We pass a String containing a prefix form and a Form containing the same formula (the expected result). 
-- If the parse method parses the String correctly it should be equal to the given Formula.
-- And when the outcome of the parse method is not equal to the expected result an error will occur.

-- Run using: runTests parseTests

-- Time spent 1 hour

testParse :: (String, [Form]) -> Bool
testParse (x, y) = (parse x == y)

parseTests :: [Test]
parseTests = [ Test "parse test" testParse
             [("+(1 2)", [Dsj [p, q]]), ("*(1 +(2 -3))", [Cnj [p, Dsj[q, Neg (r)]]])]]

-- 3. Converting formulas into CNF

-- To begin converting a formula into a CNf we begin with making the formula "arrow free". 
-- So we replace the implication and equivalence with conjunctions and disjunctions.
-- After that the nnf function is used to move all the negations inwards. 
-- This results in a formula where the negations are only used on properties directly.
-- So after this the only step that is left is applying the distribution rules on the formula.
-- When a disjunction is found in the formula it will call the distribute function and pass the disjunction with all its sub formulas as a list of Forms.
-- The distribute function will then call the distributionLaw function with the head of the list as the first argument 
-- and the distributed tail of the list as the second.
-- By doing this the distributionLaw function will be able to apply the distribution law on all the sub formulas of the disjunction.
-- And the distributionLaw function will in the end apply the distribution law to the formulas and return a single formula.
-- It will try to distribute a conjunction followed by a form or else it will try the other way round.
-- When neither of those guards are called both formulas will form a disjunction.

-- Run using: convertToCNF cnfExample1
-- (Or another cnfExample of course)

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

distribute :: [Form] -> Form
distribute [x] = x
distribute (x:xs) = distributionLaw x (distribute xs)

distributionLaw :: Form -> Form -> Form
distributionLaw (Cnj x) y = Cnj (map (\z -> distributionLaw z y) x)    
distributionLaw x (Cnj y) = Cnj (map (\z -> distributionLaw x z) y)
distributionLaw x y = Dsj [x, y]

-- Robin's CNF function

-- By figuring out all the possible ways the distribution rule can be applied we have written multiple guards for the formula.
-- The given formula will recursively be rewritten by the distribution function.

-- Run using: cnf2 cnfExample1
-- (Or another cnfExample of course)

-- Time spent 5 hours

cnf2 :: Form -> Form
cnf2 form = 
  if updated == form
  then form
  else cnf2 updated
  where
    updated = distribution( nnf( arrowfree( form)))

distribution :: Form -> Form
distribution (Dsj [p, (Cnj [q, r])]) = (Cnj [Dsj [distribution p, distribution q], Dsj [distribution p, distribution r]]) 
distribution (Dsj [(Cnj [q, r]), p]) = (Cnj [Dsj [distribution q, distribution p], Dsj [distribution r, distribution p]])
distribution (Dsj [p, q]) = (Dsj [(distribution p), (distribution q)])
distribution (Cnj [p, q]) = (Cnj [(distribution p), (distribution q)])
distribution (Neg p) = (Neg (distribution p))
distribution (p) = (p)

-- 4. Test the correctness of CNF Converter with random tests using QuickCheck

-- We have built our own formulaGenerator for testing the correctness of the cnf conversion function.
-- By running the startGenForm method a 100 random formulas will be tested by the genFormula.
-- The genFormula will parse random Integers and use them for the formulaGenerator function which will eventually generate a formula.
-- Those random formulas are also tested by the genFormula function. 
-- It will check if the generated formula and the cnf version of the generated formula are equivalent.
-- If that is the case the cnf conversion was successful.

-- Run using: startGenForm 3

-- We also made a quickcheck test for checking the validity of the cnf function. Only the quickcheck test doesn't use random formulas.

-- Run using: quickCheck testCNF

-- Time spent: 6 hours

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
          if equiv (formulaGenerator n p (s-1)) (convertToCNF (formulaGenerator n p (s-1))) then
            print ("Success")
          else 
            print ("Failed")
          genFormula s (k+1) m

formulaGenerator :: Int -> Int -> Int -> Form
formulaGenerator n p s  | (n < 0 && p >= 0 && p < s) = props !! p
              | (n == 0 && p >= 0 && p < s) = Equiv (formulaGenerator (n-1) (p-1) s) (formulaGenerator (n-2) (p-1) s)
            | (n == 1 && p >= 0 && p < s) = Impl (formulaGenerator (n-1) p s) (formulaGenerator n (p+1) s)
            | (n == 2 && p >= 0 && p < s) = Dsj [(formulaGenerator (n-1) p s), (formulaGenerator n (p-1)) s]
            | (n == 3 && p >= 0 && p < s) = Cnj [(formulaGenerator n (p+1) s), (formulaGenerator (n-1) (p+1) s)]
            | (n == 4 && p >= 0 && p < s) = Neg (formulaGenerator (n-1) p s) 
            | otherwise = props !! s

instance Arbitrary Form where 
  arbitrary = elements [cnfExample1, cnfExample2, cnfExample3, cnfExample4, cnfExample5, cnfExample6, cnfExample7]

testCNF :: Form -> Bool
testCNF x = equiv x (convertToCNF x)

prop_cnf :: IO ()
prop_cnf = quickCheck (\ x -> testCNF x == True)

-- 5. Bonus

type Clauses = [Clause]
type Clause  = [Int]

cnf2cls :: Form -> Clauses
cnf2cls x = clauses (cnf x)

clauses :: Form -> Clauses
clauses (Cnj [p, q]) = clauses p ++ clauses q
clauses (p) = [clause p]

clause :: Form -> Clause
clause (Dsj [p, q]) = clause p ++ clause q
clause (p) = [read(show p)]


-- Forms:

form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
-- form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
-- form1 = (p -> q) <=> (not(q) -> not(p))
--          1  1 1   1      0   1    0
--          1  0 0   1      1   0    0  
--          0  1 1   1      0   1    1
--          0  1 0   1      1   1    1
--
--          tautology

form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
-- form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
-- form2 = (p -> q) <=> (not(p) -> not(q))
--          1  1 1   1     0     1    0
--          1  0 0   0     0     1    1  
--          0  1 1   0     1     0    0
--          0  1 0   1     1     1    1
--
--          satisfiable

form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
-- form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
-- form3 = (p -> q) /\ (q -> r) -> (p -> r)
--          1  1 1   1  1  1 1   1  1  1 1
--          1  1 1   0  1  0 0   1  1  0 0
--          1  0 0   0  0  1 1   1  1  1 1
--          1  0 0   0  0  1 0   1  1  0 0
--          0  1 1   1  1  1 1   1  0  1 1
--          0  1 1   0  1  0 0   1  0  1 0
--          0  1 0   1  0  1 1   1  0  1 1
--          0  1 0   1  0  1 0   1  0  1 0
--
--          tautology