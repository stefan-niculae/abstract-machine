module Types where

{- Statements -}
data Stmt = Assign Var Expr
          | If Cond Stmt Stmt
          | While Cond Stmt
          | Seq Stmt Stmt
          | Skip

{- Arithmetic Expressions -}
data Expr = NLit Nr
          | ValOf Var
          | AOp Expr AOp Expr

{- Boolean Conditions -}
data Cond = BLit Bl
          | BOp Expr BOp Expr

{- Data Types -}
type Var = String
type Nr  = Integer
type Bl  = Bool

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
instance Show Stmt where
  show Skip = "()"
  show (Seq s1 s2) = (show s1) ++ " ; " ++ (show s2)
  show (Assign var n) = var ++ " = " ++ (show n)
  show (While cond body) = "while " ++ (show cond) ++ " {" ++ (show body) ++ "}"
  show (If cond st sf) = "if " ++ (show cond) ++ " {" ++ (show st) ++ "}"
    ++ (case sf of
          Skip -> ""
          _    -> " else {" ++ (show sf) ++ "}")

instance Show Expr where
  show (NLit n) = show n
  show (ValOf var) = var
  show (AOp e1 o e2) = (show e1) ++ " " ++ (show o) ++ " " ++ (show e2) -- TODO brackets

instance Show Cond where
  show (BLit b) = show b
  show (BOp e1 o e2) = (show e1) ++ " " ++ (show o) ++ " " ++ (show e2) -- TODO brackets

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
