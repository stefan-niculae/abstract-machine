-- https://github.com/amutake/imp/blob/master/src/Imp/Syntax.hs
module Types where

import Data.Map (Map)


data Stmt = Assign Var Expr
          | If Cond [Stmt] [Stmt]
          | While Cond [Stmt]
          deriving Show

data Expr = NLit Nr
          | ValOf Var
          | AOp Expr AOp Expr
          deriving Show

data Cond = BLit Bl
          | BOp Expr BOp Expr
          deriving Show

type Var = String
type Nr  = Double
type Bl  = Bool


data AOp = Add
         | Sub
         | Mul
         | Div

instance Show AOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

aOpFunc :: AOp -> (Nr -> Nr -> Nr)
aOpFunc Add = (+)
aOpFunc Sub = (-)
aOpFunc Mul = (*)
aOpFunc Div = (/)


data BOp = Eq
         | Ne
         | Lt
         | Le
         | Gt
         | Ge

instance Show BOp where
  show Eq = "=="
  show Ne = "!="
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="

bOpFunc :: BOp -> (Nr -> Nr -> Bl)
bOpFunc Eq = (==)
bOpFunc Ne = (/=)
bOpFunc Lt = (<=)
bOpFunc Le = (<)
bOpFunc Gt = (>=)
bOpFunc Ge = (>)


type Memory = Map Var Nr
