module Tests where

import Types
import Evaluation

zero, one, two, seven, vx, va, vb :: Expr
zero = NLit 0
one = NLit 1
two = NLit 2
seven = NLit 7
vx = ValOf "x"
va = ValOf "a"
vb = ValOf "b"

true, false :: Cond
true = BLit True
false = BLit False


exprToNr :: Expr -> Maybe Nr
exprToNr expr = lookup "out" mem
  where mem = finalMemory (Assign "out" expr)

condToBl :: Cond -> Maybe Bl
condToBl cond = case lookup "out" mem of
    Just n  -> Just (n /= 0)
    Nothing -> Nothing
  where mem = finalMemory (If cond (Assign "out" (NLit 1)) (Assign "out" (NLit 0)))


tests :: [Bool]
tests = [
    exprToNr one                   == Just 1
  , exprToNr (AOp one Add two)     == Just 3
  -- , exprToNr vx                    == Nothing -- undefined x
  -- , exprToNr (fromList [("x", 5)]) vx    == 5

  , condToBl true                  == Just True
  , condToBl (BOp zero Le one)     == Just True
  , condToBl (BOp zero Eq zero)    == Just True
  , condToBl (BOp one Ne two)      == Just True

  , finalMemory Skip               == []
  , finalMemory (Assign "x" one)                             == [("x", 1)]
  -- , stmtToMem (fromList [("x", 1)]) (Assign "x" two)             == [("x", 2)] -- update
  , finalMemory (If true  (Assign "x" one) (Assign "x" two)) == [("x", 1)]
  , finalMemory (If false (Assign "x" one) (Assign "x" two)) == [("x", 2)]
  , finalMemory (While false (Assign "x" one))               == [] -- no loop
  , finalMemory (Seq (Assign "x" zero) (While (BOp vx Ne one) (Assign "x" one))) == [("x", 1)] -- one loop
  , finalMemory (Seq (Assign "x" zero) (While (BOp vx Lt seven) (Assign "x" (AOp vx Add one))))     == [("x", 8)] -- multiple loop

  , finalMemory progAssigments == [("x", 3), ("y", 2)]
  , finalMemory progMin        == [("a", 1), ("b", 24), ("min", 1)]
  , finalMemory progFact       == [("fact", 120), ("n", 0)]
  , finalMemory progEuclid     == [("a", 6), ("b", 6)]
  ]



progAssigments, progMin, progFact, progEuclid :: Stmt
{-
x = 1 + 2
y = x - 1
-}
progAssigments = Seq
    (Assign "x" (AOp one Add two))
    (Assign "y" (AOp vx Sub one))

{-
a = 1
b = 24
if a < b
  min = a
else
  min = b
-}
progMin = Seq
  (Seq (Assign "a" (NLit 1)) (Assign "b" (NLit 24)))
  (If (BOp va Lt vb)
    (Assign "min" va)
    (Assign "min" vb)
  )


{-
n = 5
fact = 1
while n != 0
  fact = fact * n
  n = n - 1
-}
progFact = Seq
  (Seq (Assign "n" (NLit 5)) (Assign "fact" one))
  (While (BOp (ValOf "n") Ne zero) (Seq
    (Assign "fact" (AOp (ValOf "fact") Mul (ValOf "n")))
    (Assign "n"    (AOp (ValOf "n")    Sub one))
  ))

{-
a = 18
b = 30
while a != b
  if a > b
    a = a - b
  else
    b = b - a
-}
progEuclid = Seq
  (Seq (Assign "a" (NLit 18)) (Assign "b" (NLit 30)))
  (While (BOp va Ne vb) (
      If (BOp va Gt vb)
        (Assign "a" (AOp va Sub vb))
        (Assign "b" (AOp vb Sub va))
  ))


main :: IO()
main = putStrLn $ if all (==True) tests
  then "all tests passed"
  else (\b -> if b then '.' else 'F') `map` tests
