module Axioms where

import Control.Monad.State (State)
import Types
import Memory


transS :: Stmt -> State Memory Stmt

transS Skip =
  error "transS cannot be called with a Skip directly"

 -- delete leading skip
transS (Seq Skip stmt) =
  return stmt

-- first statement in sequence can still be advanced
transS (Seq stmt1 stmt2) = do
  stmt1' <- transS stmt1
  return (Seq stmt1' stmt2)

-- expression is done being evaluated (it's a literal)
transS (Assign var (NLit n)) = do
  upsert var n
  return Skip

-- expression can still be evaluated (it's an operation/variable)
transS (Assign var expr) = do
  expr' <- transE expr
  return (Assign var expr')

-- condition is done being evaluated (it's a literal)
transS (If (BLit b) st sf) =
  return (if b then st else sf) -- branch
-- condition can still be evaluated (it's an operation)
transS (If cond st sf) = do
  cond' <- transC cond
  return (If cond' st sf)

-- unroll the loop
transS w@(While cond stmt) =
  return (If cond (Seq stmt w) Skip)



transE :: Expr -> State Memory Expr

transE (NLit _) =
  error "transE cannot be called with a literal argument directly"

-- transform a variable into a literal
transE (ValOf x) = do
  n <- val x
  return (NLit n)

-- both sides are done being evaluated (they're literals)
transE (AOp (NLit n1) op (NLit n2)) = do
  let func = aOpFunc op
  let n = n1 `func` n2
  return (NLit n)

-- lhs is done being evaluated (it's a literal),
-- rhs can still be evaluated (it's an operation/variable)
transE (AOp e1@(NLit _) op e2) = do
  e2' <- transE e2
  return (AOp e1 op e2')

-- lhs can still be evaluated (it's an operation/variable)
transE (AOp e1 op e2) = do
  e1' <- transE e1
  return (AOp e1' op e2)



transC :: Cond -> State Memory Cond

transC (BLit _) =
  error "transE cannot be called with a literal argument directly"

-- both sides are done being evaluated (they're literals)
transC (BOp (NLit n1) op (NLit n2)) = do
  let func = bOpFunc op
  let b = n1 `func` n2
  return (BLit b)

-- lhs is done being evaluated (it's a literal),
-- rhs can still be evaluated (it's an operation/variable)
transC (BOp e1@(NLit _) op e2) = do
  e2' <- transE e2
  return (BOp e1 op e2')

-- lhs can still be evaluated (it's an operation/variable)
transC (BOp e1 op e2) = do
  e1' <- transE e1
  return (BOp e1' op e2)
