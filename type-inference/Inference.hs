module Inference where

import Prelude hiding (lookup)
import qualified Data.Map as Map  -- for context & substitutions
import Data.Map
import qualified Data.Set as Set  -- sets of type variables
import Control.Monad.Except

import Syntax
import Types
import InferenceMonad


{- Unification -}
unify :: Type -> Type -> TI Subst -- most general unifier
unify (TFun l r) (TFun l' r') = do s1 <- unify l l'
                                   s2 <- unify (apply s1 r) (apply s1 r')
                                   return (s1 `compose` s2)
unify (TVar u) t              = varBind u t
unify t (TVar u)              = varBind u t
unify TInt TInt               = return nullSubst
unify TBool TBool             = return nullSubst
unify t1 t2                   = throwError $ "types do not unify: " ++ show t1 ++
                                             " vs " ++ show t2

-- bind a variable to a type and return that binding as a substitution
varBind :: String -> Type -> TI Subst
varBind u t
  | t == TVar u          = return nullSubst
  | u `Set.member` ftv t = throwError $ "infinite type: " ++ u ++
                                        " vs " ++ show t
  | otherwise            = return $ Map.singleton u t


{- Polymorphism -}
-- replaces all bound type variables in a type scheme with fresh type variables
instantiate :: Scheme -> TI Type
instantiate (Forall vars t) = do
  nvars <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars nvars
  return $ apply s t

-- generalize abstracts a type over all type variables which are free in the type
-- but not free in the given type environment
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vars t
  where vars = Set.toList $ ftv t `Set.difference` ftv env


{- Inference rules -}
infer ::  TypeEnv -> Exp -> TI (Subst, Type)
infer env expr = case expr of
  -- a literal has the same type in any context
  -- :INT (slide 7)
  Lit (LInt _)  -> return (nullSubst, TInt)
  -- :BOOL (slide 7)
  Lit (LBool _) -> return (nullSubst, TBool)

  Var n -> case lookup n env of
    Nothing    -> throwError $ "unbound variable: " ++ n
    Just sigma -> do t <- instantiate sigma
                     return (nullSubst, t)

  -- :IOP (slide 8)
  AOp e1 _ e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) TInt
    s4 <- unify (apply s3 t2) TInt
    return (composeAll [s1, s2, s3, s4], TInt)

  -- :BOP (slide 8)
  BOp e1 _ e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) TInt
    s4 <- unify (apply s3 t2) TInt
    return (composeAll [s1, s2, s3, s4], TBool)

  -- :IF (slide 8)
  If eb e1 e2 -> do
    (sb, tb) <- infer env eb
    sb' <- unify (apply sb tb) TBool -- do I apply sb here?
    -- I don't think it matters if we apply the substitution from unifying the condition
    -- when infering the types of the statements (e1 and e2)
    t <- fresh
    (s1, t1) <- infer (apply sb' env) e1
    (s2, t2) <- infer (apply s1 env)  e2
    s3 <- unify (apply s2 t1) t
    s4 <- unify (apply s3 t2) t1 -- t1 or t?
    return (composeAll [sb, s1, s2, s3, s4], t)
    `catchError`
    \err -> throwError $ err ++ "\n in " ++ show (If eb e1 e2)

  -- :FN (slide 10)
  Fun x e -> do
    t <- fresh
    let envWithoutX = remove env x
        -- add [x |-> t]
        env' = envWithoutX `Map.union` Map.singleton x (Forall [] t)
    (s1, t') <- infer env' e
    return (s1, TFun (apply s1 t) t')

  -- :APP (slide 10)
  App f x -> do
    t' <- fresh
    (s1, fType) <- infer env f
    (s2, t) <- infer (apply s1 env) x
    s3 <- unify (apply s2 fType) (TFun t t')
    return (composeAll [s1, s2, s3], apply s3 t')
    `catchError`
    \err -> throwError $ err ++ "\n in " ++ show (App f x)

  -- :LET (slide 17)
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let envWithoutX = remove env x
        t1' = generalize (apply s1 env) t1
        env' = insert x t1' envWithoutX
    (s2, t2) <- infer (apply s1 env') e2
    return (s1 `compose` s2, t2)

  -- ~ :LETREC (slide 23)
  Fix f -> do
    (s, ft) <- infer env f
    t <- fresh
    s' <- unify (TFun t t) ft
    return (s', apply s t)





typeInference :: Exp -> TI Type
typeInference e =
  do (s, t) <- infer Map.empty e  -- start from no environment
     return (apply s t)
