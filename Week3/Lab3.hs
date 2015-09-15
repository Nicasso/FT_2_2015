
module Lab3 where

import Data.List
import System.Random
import Lecture3
import Testing


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