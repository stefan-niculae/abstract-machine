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
  Let "fib" (Fix $
    Fun "x" $
      If (BOp varx Eq zero)
        one
        (App (Var "fib") (AOp varx Sub one))
  ) $ App (Var "fib") seven

  ]


test :: Exp -> IO ()
test e = do
  (res, _) <- runTI $ typeInference e
  case res of
    Left err  ->  putStrLn $ err
    Right t   ->  putStrLn $ ":: " ++ show t

main :: IO ()
main = mapM_ test testExpressions
