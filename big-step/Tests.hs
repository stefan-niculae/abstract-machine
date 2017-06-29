module Tests where

import Data.Map (empty, fromList, toList)
import Control.Monad.State (runState)

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

incrementX :: Stmt
incrementX = Assign "x" (AOp vx Add one)

progAssigments, progMin, progFact, progEuclid :: [Stmt]
{-
x = 1 + 2
y = x - 1
-}
progAssigments = [
    (Assign "x" (AOp one Add two))
  , (Assign "y" (AOp vx Sub one))
  ]

{-
a = 1
b = 24
if a < b
  min = a
else
  min = b
-}
progMin = [
    (Assign "a" (NLit 1))
  , (Assign "b" (NLit 24))
  , (If (BOp va Lt vb)
       [Assign "min" va]
       [Assign "min" vb]
    )
  ]

{-
n = 5
fact = 1
while n != 0
  fact = fact * n
  n = n - 1
-}
progFact = [
    (Assign "n" (NLit 5))
  , (Assign "fact" one)
  , (While (BOp (ValOf "n") Ne zero) [
       (Assign "fact" (AOp (ValOf "fact") Mul (ValOf "n")))
     , (Assign "n"    (AOp (ValOf "n")    Sub one))
     ])
  ]

{-
a = 18
b = 30
while a != b
  if a > b
    a = a - b
  else
    b = b - a
-}
progEuclid = [
    (Assign "a" (NLit 18))
  , (Assign "b" (NLit 30))
  , (While (BOp va Ne vb) [
      If (BOp va Gt vb)
        [Assign "a" (AOp va Sub vb)]
        [Assign "b" (AOp vb Sub va)]
    ])
  ]


exprToNr :: Memory -> Expr -> Nr
exprToNr mem e = n
  where (n, _) = runState (evalExpr e) initial {memory = mem}

condToBl :: Memory -> Cond -> Bl
condToBl mem c = b
  where (b, _) = runState (evalCond c) initial {memory = mem}

stmtToMem :: Memory -> Stmt -> [(Var, Nr)]
stmtToMem mem s = toList (memory config)
    where (_, config) = runState (evalStmt s) initial {memory = mem}

tests :: [Bool]
tests = [
    exprToNr empty one                   == 1
  , exprToNr empty (AOp one Add two)     == 3
  , exprToNr empty vx                    == 0 -- default is zero
  , exprToNr (fromList [("x", 5)]) vx    == 5

  , condToBl empty true                  == True
  , condToBl empty (BOp zero Le one)     == True
  , condToBl empty (BOp zero Eq zero)    == True
  , condToBl empty (BOp one Ne two)      == True

  -- , stmtToMem empty Skip                  == []
  , stmtToMem empty (Assign "x" one)                             == [("x", 1)]
  , stmtToMem (fromList [("x", 1)]) (Assign "x" two)             == [("x", 2)] -- update
  , stmtToMem empty (If true  [Assign "x" one] [Assign "x" two]) == [("x", 1)]
  , stmtToMem empty (If false [Assign "x" one] [Assign "x" two]) == [("x", 2)]
  , stmtToMem empty (While false [Assign "x" one])               == [] -- no loop
  , stmtToMem empty (While (BOp vx Ne one) [Assign "x" one])     == [("x", 1)] -- one loop
  , stmtToMem empty (While (BOp vx Lt seven) [incrementX])       == [("x", 8)] -- multiple loop

  , eval progAssigments == [("x", 3), ("y", 2)]
  , eval progMin        == [("a", 1), ("b", 24), ("min", 1)]
  , eval progFact       == [("fact", 120), ("n", 0)]
  , eval progEuclid     == [("a", 6), ("b", 6)]
  ]
