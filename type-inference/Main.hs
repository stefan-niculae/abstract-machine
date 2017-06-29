module Main where


import Syntax
import InferenceMonad
import Inference


zero, one, ten, seven, true, false, varx :: Exp
zero  = Lit $ LInt 0
one   = Lit $ LInt 1
ten   = Lit $ LInt 10
seven = Lit $ LInt 7
true  = Lit $ LBool True
false = Lit $ LBool False
varx  = Var "x"

testExpressions :: [Exp]
testExpressions = [
  -- 10
  -- :: int
  ten,

  -- \x -> x + 10
  -- :: int -> int
  Fun "x" (AOp varx Add ten),

  -- id
  -- \x -> x
  -- :: t0 -> t0
  Fun "x" (Var "x"),

  -- \x -> y
  -- unbound variable: y
  Fun "x" (Var "y"),

  -- fst
  -- \a -> \b -> a
  -- :: t0 -> t1 -> t0
  Fun "a"
    (Fun ("b") (Var "a")),

  -- if 0 <= 1
  --   then 7
  --   else 10
  -- :: int
  If (BOp zero Le one)
    seven
    ten,

  -- if 0 <= 1
  --   then True
  --   else 0
  -- types do not unify: int vs bool
  --  in if 0 <= 1
  --   then True
  --   else 0
  If (BOp zero Le one)
    true
    zero,


  -- let id = \x -> x in
  --   id 10
  -- :: int
  Let "id" (Fun "x" (Var "x")) $
    App (Var "id") ten,

  -- let id = \x -> x in
  --   id id
  -- :: t3 -> t3
  Let "id" (Fun "x" (Var "x")) $
    App (Var "id") (Var "id"),

  -- polymorphic with if
  -- let id = \x -> x in
  -- if id (0 <= 1)
  --   then 1
  --   else id 0
  -- :: int
  Let "id" (Fun "x" (Var "x")) $
    If (App (Var "id") (BOp zero Le one))
      one
      (App (Var "id") zero),


  -- \x -> x x
  -- occurs check fails: t0 vs t0 -> t1
  --  in x x
  Fun "x" (App varx varx),


  -- boolean applied to int
  -- True 1
  -- types do not unify: bool vs. int -> t0
  --  in True 1
  App true one,

  -- not a function is applied
  -- let f = True in
  --   f 10
  -- types do not unify: bool vs int -> t0
  --  in f 10
  Let "f" true $
    App (Var "f") ten,


  -- recursivity
  -- let fib = * \x -> if x == 0
  --                   then 1
  --                   else * fib (x - 1) in
  -- fib 7
  -- TODO
  -- Let "fib" (Fix $
  --   Fun "x" $
  --     If (BOp varx Eq zero)
  --       one
  --       (App (Var "fib") (AOp varx Sub one))
  -- ) $ App (Var "fib") seven,

  -- recursivity
  -- let fib = * \x -> if x == 0
  --                   then 1
  --                   else * fib (x - 1) in
  -- fib 7
  -- TODO
  LRc "fib" (
    Fun "x" $
      If (BOp varx Eq zero)
        one
        (App (Var "fib") (AOp varx Sub one))
  ) $ Var "fib",

  LRc "fib" (
    Fun "x" $
      If (BOp varx Eq zero)
        one
        (App (Var "fib") (AOp varx Sub one))
  ) $ App (Var "fib") seven,

  -- homogenous pair
  -- ( 0 , 1 )
  -- :: ( int , int )
  Tup zero one,

  -- heterogenous pair
  -- ( 0 , True )
  -- :: ( int , bool )
  Tup zero true,

  -- nested pair
  -- ( ( True , False ) , \x -> x + x )
  -- :: ( ( bool , bool ) , int -> int )
  Tup (Tup true false) (Fun "x" (AOp varx Add varx)),

  -- pair as argument
  -- \p -> fst p + snd p
  -- :: ( int , t2 ) -> int
  Fun "p" $
    AOp (Fst $ Var "p") Add one,

  -- pair as argument - correct argument type
  -- let addPair = \p -> fst p + snd p in
  --   addPair (( 1 , 0 ))
  -- TODO
  Let "addPair" (
    Fun "p" $
      AOp (Fst $ Var "p") Add (Snd $ Var "p")
  ) $ App (Var "addPair") (Tup one zero),

  -- pair as argument - wrong argument type
  -- let addPair = \pair -> fst pair + snd pair in
  --   addPair (( 1 , 0 ))
  -- TODO
  Let "addPair" (
    Fun "pair" $
      AOp (Fst $ Var "pair") Add (Snd $ Var "pair")
  ) $ App (Var "addPair") (Tup one true),

  -- record
  -- { a = 1 ;  b = True }
  -- :: { a : int ; b : bool }
  Rcd [("a", one), ("b", true)],

  -- nested record
  -- { a = 1 ;  b = True ;  r = { } }
  -- :: { a : int ; b : bool ; r : { } }
  Rcd [("a", one), ("b", true), ("r", Rcd [])],

  -- correct access of receord field
  -- { a = 1 } . a
  -- :: int
  Acc (Rcd [("a", one)]) "a",

  -- access of non-existant field
  -- { a = 1 } . b
  -- tried to access non-existant field b from record: { a = 1 }
  Acc (Rcd [("a", one)]) "b",

  -- illegal access
  -- True . x
  -- tried to access field x from a non-record: True
  Acc true "x",

  -- record as argument
  -- \r -> r . a + r . b
  -- TODO
  Fun "r" $
    AOp (Acc (Var "r") "a") Add
        (Acc (Var "r") "b")


  -- g = \f -> (f {x=3, y=7}).a + f({a=2, x=3})
  -- :: ({x:int} -> {a:int, b:int}) -> int

  -- f = \o -> {a=3, z=o.x, b=7}
  -- :: {x:t} -> {a:int, z:t, b:int}

  -- g f
  -- :: int

  ]


test :: Exp -> IO ()
test e = do
  (res, _) <- runTI $ typeInference e
  case res of
    Left err  ->  putStrLn $ err
    Right t   ->  putStrLn $ ":: " ++ show t

main :: IO ()
main = mapM_ test testExpressions
