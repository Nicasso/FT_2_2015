
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

testParse :: (String, [Form]) -> Bool
testParse (x, y) = (parse x == y)

parseTests :: [Test]
parseTests = [ Test "parse test" testParse
             [("+(1 2)", [Dsj [p, q]]), ("*(1 +(2 -3))", [Cnj [p, Dsj[q, Neg (r)]]])]]

-- 3. Converting formulas into CNF

cnfExample = Impl p q