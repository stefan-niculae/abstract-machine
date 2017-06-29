module Types where

{- Everything is an expression! -}
data Expr = NLit Nr
          | BLit Bl
          | Arg Var

          | AOp Expr AOp Expr -- arith aop arith
          | BOp Expr BOp Expr -- arith bop arith

          | If Expr Expr Expr -- cond stmt stmt
          | While Expr Expr -- cond stmt

          | Seq Expr Expr -- stmt stmt
          | Skip -- stmt

          | Assign Var Expr -- var arith
          | ValOf Var

          | Ref Expr -- ref artith
          | Fun Expr Expr -- arg stmt
          | Apply Expr Expr -- fun stmt


{- Data Types -}
type Nr  = Integer
type Bl  = Bool
type Var = String


{- Operators -}
data AOp = Add
         | Sub
         | Mul
         | Div

data BOp = Eq
         | Ne
         | Lt
         | Le
         | Gt
         | Ge

{- Operators Implementation -}
aOpFunc :: AOp -> (Nr -> Nr -> Nr)
aOpFunc Add = (+)
aOpFunc Sub = (-)
aOpFunc Mul = (*)
aOpFunc Div = \a b -> if b /= 0 then a `div` b else error "division by zero"

bOpFunc :: BOp -> (Nr -> Nr -> Bl)
bOpFunc Eq = (==)
bOpFunc Ne = (/=)
bOpFunc Lt = (<=)
bOpFunc Le = (<)
bOpFunc Gt = (>=)
bOpFunc Ge = (>)


{- Printing -}
instance Show Expr where
  show Skip = "()"
  show (Seq s1 s2) = (show s1) ++ " ; " ++ (show s2)
  show (Assign var n) = (show var) ++ " = " ++ (show n)
  show (While cond body) = "while " ++ (show cond) ++ " {" ++ (show body) ++ "}"
  show (If cond st sf) = "if " ++ (show cond) ++ " {" ++ (show st) ++ "}"
    ++ (case sf of
          Skip -> ""
          _    -> " else {" ++ (show sf) ++ "}")
  show (NLit n) = show n
  show (ValOf var) = "*" ++ show var
  show (AOp e1 o e2) = (show e1) ++ " " ++ (show o) ++ " " ++ (show e2) -- TODO brackets
  show (BLit b) = show b
  show (BOp e1 o e2) = (show e1) ++ " " ++ (show o) ++ " " ++ (show e2) -- TODO brackets
  show (Ref v) = "&" ++ show v
  show (Fun x body) = "(" ++ show x ++ ") -> { " ++ show body ++ " }"
  show (Apply f x) = show f ++ "(" ++ show x ++ ")"
  show (Arg x) = x

instance Show AOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show BOp where
  show Eq = "=="
  show Ne = "!="
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="
