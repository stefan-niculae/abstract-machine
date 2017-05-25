{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Prelude hiding (lookup)
import qualified Data.Map as Map  -- for context & substitutions
import Data.Map hiding (union, foldr, map, empty, difference)
import qualified Data.Set as Set  -- sets of type variables
import Data.Set hiding (map, foldr, delete)
import Text.PrettyPrint hiding (empty)

import Syntax hiding (prField)
import Utils

type TField = (Label, Type)

data Type = TVar String
          | TInt
          | TBool
          | TFun Type Type
          | TPair Type Type
          | TRecd [TField]
          deriving (Eq, Ord)

-- A scheme is a type in which a number of polymorphic type variables
-- are bound to a universal quantifier
data Scheme = Forall [String] Type


{-
  substitutions only replace free type variables;
  qunatified ones are not affected
-}
class Substituable a where
  ftv   :: a -> Set String -- free type variables of a type
  apply :: Subst -> a -> a -- apply substitution
  -- No memory so no need for free variables

instance Substituable Type where
  ftv (TVar n)      = Set.singleton n
  ftv TInt          = Set.empty
  ftv TBool         = Set.empty
  ftv (TFun t1 t2)  = ftv t1 `union` ftv t2
  ftv (TPair t1 t2) = ftv t1 `union` ftv t2
  ftv (TRecd ts)    = foldr union empty $ map ftv (map snd ts)

  apply _ TInt          = TInt
  apply _ TBool         = TBool
  apply s (TVar n)      = case lookup n s of
                            Nothing  -> TVar n
                            Just t   -> t
  apply s (TFun  t1 t2) = TFun (apply s t1) (apply s t2)
  apply s (TPair t1 t2) = TPair (apply s t1) (apply s t2)
  apply s (TRecd ts)    = TRecd [(l, apply s t) | (l, t) <- ts]

instance Substituable Scheme where
    ftv (Forall vars t)     = (ftv t) `Set.difference` (Set.fromList vars)
    apply s (Forall vars t) = Forall vars (apply (foldr delete s vars) t)

instance Substituable a => Substituable [a] where
    apply s = map (apply s)
    ftv l   = foldr union empty (map ftv l)


{-
  Substitutions
  mappings from type variables to types
-}
type Subst = Map String Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

composeAll :: [Subst] -> Subst
composeAll = foldr compose nullSubst


-- Environment, Gamma
type TypeEnv = Map String Scheme

remove :: TypeEnv -> String -> TypeEnv
remove env var = delete var env -- Gamma \ x


instance Substituable TypeEnv where
  ftv env     = ftv (Map.elems env)
  apply s env = Map.map (apply s) env










{-
Print formatting
-}

instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> Doc
prType (TVar n)      = text n
prType TInt          = text "int"
prType TBool         = text "bool"
prType (TFun t s)    = prParenType t <+> text "->" <+> prType s
prType (TPair t1 t2) = text "(" <+> prType t1 <+> text "," <+> prType t2 <+> text ")"
prType (TRecd ts)    = text "{" <+> intersperse (text ";") (map prField ts) <+> text "}"
  where prField :: TField -> Doc
        prField (label, t) = text label <+> text ":" <+> prType t

prParenType :: Type -> Doc
prParenType t = case t of
                  TFun _ _  -> parens (prType t)
                  _         -> prType t


instance Show Scheme where
  showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> Doc
prScheme (Forall vars t)  =   text "All" <+>
                              hcat
                                (punctuate comma (map text vars))
                              <> text "." <+> prType t
