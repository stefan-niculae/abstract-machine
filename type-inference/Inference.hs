module Inference where

import qualified Data.Map as Map  -- for context & substitutions
import Data.Map hiding (map, lookup)
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
-- TODO pair
-- TODO record
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
  -- :INT (C8 slide 7)
  Lit (LInt _)  -> return (nullSubst, TInt)
  -- :BOOL (C8 slide 7)
  Lit (LBool _) -> return (nullSubst, TBool)

  Var n -> case Map.lookup n env of
    Nothing    -> throwError $ "unbound variable: " ++ n
    Just sigma -> do t <- instantiate sigma
                     return (nullSubst, t)

  -- :IOP (C8 slide 8)
  AOp e1 _ e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) TInt
    s4 <- unify (apply s3 t2) TInt
    return (composeAll [s1, s2, s3, s4], TInt)

  -- :BOP (C8 slide 8)
  BOp e1 _ e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) TInt
    s4 <- unify (apply s3 t2) TInt
    return (composeAll [s1, s2, s3, s4], TBool)

  -- :IF (C8 slide 8)
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

  -- :FN (C8 slide 10)
  Fun x e -> do
    t <- fresh
    let envWithoutX = remove env x
        -- add [x |-> t]
        env' = envWithoutX `Map.union` Map.singleton x (Forall [] t)
    (s1, t') <- infer env' e
    return (s1, TFun (apply s1 t) t')

  -- :APP (C8 slide 10)
  App f x -> do
    t' <- fresh
    (s1, fType) <- infer env f
    (s2, t) <- infer (apply s1 env) x
    s3 <- unify (apply s2 fType) (TFun t t')
    return (composeAll [s1, s2, s3], apply s3 t')
    `catchError`
    \err -> throwError $ err ++ "\n in " ++ show (App f x)

  -- :LET (C8 slide 17)
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let envWithoutX = remove env x
        t1' = generalize (apply s1 env) t1
        env' = insert x t1' envWithoutX
    (s2, t2) <- infer (apply s1 env') e2
    return (s1 `compose` s2, t2)

  -- :LETREC (C8 slide 23)
  LRc x e1 e2 -> do
    -- get the type t1 of e1 inferred from the context where t1 is known

    t <- fresh
    let envWithoutX = remove env x
        -- env' = punem t in env
        env' = envWithoutX `Map.union` Map.singleton x (Forall [] t) -- TODO make this a function
    (s1, t1) <- infer env' e1
    s2 <- unify (apply s1 t) t1
    (s3, t2) <- infer (apply (s1 `compose` s2) env') e2
    return (composeAll [s1, s2, s3], t2)

  -- Pointfix operator
  Fix f -> do
    (s, ft) <- infer env f
    t <- fresh
    s' <- unify (TFun t t) ft
    return (s', apply s t)

  -- tPAIR (C9 slide 2)
  Tup e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    return (s1 `compose` s2, TPair t1 t2)

  -- tFST (C9 slide 2)
  Fst e -> do
    (s, t) <- infer env e
    t1 <- fresh
    t2 <- fresh
    s2 <- unify t (TPair t1 t2)
    return (s `compose` s2, apply s2 t1)

  -- tSND (C9 slide 2)
  Snd e -> do
    (s, t) <- infer env e
    case t of
      TPair _ t2 -> return (s, t2)
      _          -> throwError $ "snd applied to a non-pair: " ++ show e

  -- tRECORD (C9 slide 9)
  Rcd entries -> do
    -- infer independently, don't propagate substitutions
    let labels = map fst entries  -- entry::(label, exp)
    let exps   = map snd entries
    infers <- mapM (infer env) exps
    let substs = map fst infers -- infer::(subst, type)
    let types  = map snd infers
    let composed = composeAll substs
    return (composed, TRecd $ labels `zip` (map (apply composed) types))

  -- tField (C9 slide 9)
  Acc e label -> do
    (s1, t) <- infer env e
    tlabel <- fresh
    trest <- fresh
    s2 <- unify t (URecd (TRecd [(label, tlabel)]) trest)
    return (s1 `compose` s2, apply s2 tlabel)


    -- -- TODO
    -- case t of
    --   TRecd ts -> case lookup label ts of
    --                 Just t' -> return (s, t')
    --                 Nothing -> throwError $ "tried to access non-existant field " ++ label ++ " from record: " ++ show e
    --   _        -> throwError $ "tried to access field " ++ label ++ " from a non-record: " ++ show e

typeInference :: Exp -> TI Type
typeInference e =
  do (s, t) <- infer Map.empty e  -- start from no environment
     return (apply s t)
