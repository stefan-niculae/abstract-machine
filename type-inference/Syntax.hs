module Syntax where

import Text.PrettyPrint


data Exp  = Var String          -- variable (integer)
          | Lit Lit             -- literal value (integer | boolean)
          | AOp Exp AOp Exp     -- int :+: int
          | BOp Exp BOp Exp     -- int :<: int
          | If  Exp Exp Exp     -- if cond then statement true else st false
          | Fun String Exp      -- function definition: arg -> body
          | App Exp Exp         -- function application: f(arg)
          | Let String Exp Exp  -- local variable definition: let name = x in ..., allows polymorphic functions
          | Fix Exp             -- fixpoint operator, allows recursivity
          deriving (Eq, Ord)

data Lit  = LInt Integer
          | LBool Bool
          deriving (Eq, Ord)

data AOp  = Add
          | Sub
          | Mul
          | Div
          deriving (Eq, Ord)

data BOp  = Eq
          | Ne
          | Lt
          | Gt
          | Le
          | Ge
          deriving (Eq, Ord)




{-
Print formatting
-}

instance Show Exp where
  showsPrec _ x = shows (prExp x)

prExp ::  Exp -> Doc
prExp (Var name)     = text name
prExp (Lit lit)      = prLit lit
prExp (Let x b body) = text "let" <+> text x <+> text "=" <+> prExp b <+> text "in" $$
                         nest 2 (prExp body)
prExp (App e1 e2)    = prExp e1 <+> prParenExp e2
prExp (Fun n e)      = char '\\' <> text n <+> text "->" <+> prExp e
prExp (AOp e1 o e2)  = prExp e1 <+> text (show o) <+> prExp e2
prExp (BOp e1 o e2)  = prExp e1 <+> text (show o) <+> prExp e2
prExp (If c st sf)   = text "if" <+> prExp c $$
                         nest 2 (text "then" <+> prExp st) $$
                         nest 2 (text "else" <+> prExp sf)
prExp (Fix f)        = text "*" <+> prExp f

prParenExp ::  Exp -> Doc
prParenExp t  =   case t of
                    Let _ _ _  -> parens (prExp t)
                    App _ _    -> parens (prExp t)
                    Fun _ _    -> parens (prExp t)
                    AOp _ _ _  -> parens (prExp t)
                    BOp _ _ _  -> parens (prExp t)
                    If  _ _ _  -> parens (prExp t)
                    Fix _      -> parens (prExp t)
                    Lit _      -> prExp t
                    Var _      -> prExp t

instance Show Lit where
  showsPrec _ x = shows (prLit x)

prLit  ::  Lit -> Doc
prLit (LInt i)  = integer i
prLit (LBool b) = if b then text "True" else text "False"

instance Show AOp where
  show o = case o of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

instance Show BOp where
  show o = case o of
     Eq -> "=="
     Ne -> "!="
     Lt -> "<"
     Le -> "<="
     Gt -> ">"
     Ge -> ">="
