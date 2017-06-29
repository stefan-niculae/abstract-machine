module Axioms where

import Control.Monad.State (State)
import Data.Set (Set, fromList, difference, union, unions, empty)
import Types
import Memory


trans :: Expr -> State Memory Expr
trans Skip      = error "trans cannot be called with a Skip directly"
trans (NLit _)  = error "trans cannot be called with a number literal directly"
trans (BLit _)  = error "trans cannot be called with a boolean literal directly"
trans (Arg _)   = error "trans cannot be called with an argument directly"
trans (Fun _ _) = error "trans cannot be called with an function directly"

{- Statements -}
-- delete leading skip
trans (Seq Skip stmt) =
  return stmt

-- first statement in sequence can still be advanced
trans (Seq stmt1 stmt2) = do
  stmt1' <- trans stmt1
  return (Seq stmt1' stmt2)

-- expression is done being evaluated (it's a literal)
trans (Assign var (NLit n)) = do
  upsert var n
  return Skip

-- condition is done being evaluated (it's a literal)
trans (If (BLit b) st sf) =
  return (if b then st else sf) -- branch
-- condition can still be evaluated (it's an operation)
trans (If cond st sf) = do
  cond' <- trans cond
  return (If cond' st sf)

-- unroll the loop
trans w@(While cond stmt) =
  return (If cond (Seq stmt w) Skip)

{- Arithmethic expressions -}
-- transform a variable into a literal
trans (ValOf x) = do
  n <- val x
  return (NLit n)

-- both sides are done being evaluated (they're literals)
trans (AOp (NLit n1) op (NLit n2)) = do
  let func = aOpFunc op
  let n = n1 `func` n2
  return (NLit n)

-- lhs is done being evaluated (it's a literal),
-- rhs can still be evaluated (it's an operation/variable)
trans (AOp e1@(NLit _) op e2) = do
  e2' <- trans e2
  return (AOp e1 op e2')

-- lhs can still be evaluated (it's an operation/variable)
trans (AOp e1 op e2) = do
  e1' <- trans e1
  return (AOp e1' op e2)

{- Conditions -}
-- both sides are done being evaluated (they're literals)
trans (BOp (NLit n1) op (NLit n2)) = do
  let func = bOpFunc op
  let b = n1 `func` n2
  return (BLit b)

-- lhs is done being evaluated (it's a literal),
-- rhs can still be evaluated (it's an operation/variable)
trans (BOp e1@(NLit _) op e2) = do
  e2' <- trans e2
  return (BOp e1 op e2')

-- lhs can still be evaluated (it's an operation/variable)
trans (BOp e1 op e2) = do
  e1' <- trans e1
  return (BOp e1' op e2)


{- Functions -}
-- strict evaluation
-- (S@) when you reached a function body and a value, evaluate it
trans (Apply (Fun (Arg x) body) e)
  | isVal e = return $ replace body x e
  -- otherwise, go to the next patterns

-- (S@D) reduce argument as much as possible
trans (Apply f@(Fun _ _) arg) = do
  arg' <- trans arg
  trans (Apply f arg')

-- (S@S) reduce function body as much as possible
trans (Apply f arg) = do
  f' <- trans f
  trans (Apply f' arg)

trans _ = undefined


isVal :: Expr -> Bool
isVal (NLit _) = True
isVal (BLit _) = True
isVal (Fun _ _) = True
isVal _ = False


-- replace every occurence of arg with nr in body
replace :: Expr -> Var -> Expr -> Expr -- stmt var anything
replace = undefined


-- free variables, ie: not bound
frees :: Expr -> Set Var
frees (Arg x) = fromList [x]
frees (Fun (Arg x) body) = frees body `difference` fromList [x]
frees (Apply f arg) = frees f `union` frees arg

frees (NLit _) = empty
frees (BLit _) = empty
frees (Skip)   = empty

frees (AOp e1 _ e2) = frees e1 `union` frees e2
frees (BOp e1 _ e2) = frees e1 `union` frees e2
frees (If c st sf) = unions (map frees [c, st, sf])
frees (Seq s1 s2) = frees s1 `union` frees s2
frees (While c body) = frees c `union` frees body

frees (Ref e) = frees e
frees (ValOf _) = empty      -- TODO is this right?
frees (Assign _ e) = frees e -- TODO is this right?

frees _ = undefined
