module Evaluation where

import Prelude hiding (lookup, log)
import Control.Monad.State (State, get, put, runState)
import Data.Map (empty, lookup, insert, toList)

import Types

data Config = Config {
    indentation :: Int
  , log         :: String
  , memory      :: Memory
}


{- Logging -}
viz :: String -> State Config ()
viz s = do
  config <- get
  let indentSymb = concat $ replicate (indentation config) "      "
  let log' = log config ++ indentSymb ++ (contract s) ++ "\n"
  put config {log = log'}


maxShowLen :: Int
maxShowLen = 100
contract :: String -> String
contract s = if n <= maxShowLen then s
  else take n2 s ++ " .. " ++ drop (n-n2) s
  where n = length s
        n2 = (maxShowLen-4) `div` 2 -- length " .. " == 4


changeIndentation :: Int -> State Config ()
changeIndentation n = do
  config <- get
  let i' = (indentation config) + n
  put config {indentation = i'}

indent, dedent :: State Config ()
indent = changeIndentation   1
dedent = changeIndentation (-1)


logIO :: (Show i, Show o) => (i -> State Config o) -> (i -> State Config o)
logIO f inp = do
  viz $ "➡️ " ++ show inp
  out <- f inp
  viz $ "👈 " ++ show out
  return out


{- Memory IO -}
val :: Var -> State Config Nr
val x = do config <- get
           case lookup x (memory config) of
             Nothing -> return 0 -- default val
             Just n  -> return n

upsert :: Var -> Nr -> State Config ()
upsert v n = do config <- get
                let memory' = insert v n (memory config)
                put $ config {memory = memory'}



{- Semantic rules -}
evalExpr, evalExpr' :: Expr -> State Config Nr
evalExpr' = logIO evalExpr

evalExpr (NLit n)       = return n
evalExpr (ValOf x)      = val x
evalExpr (AOp e1 op e2) = do indent
                             n1 <- evalExpr' e1
                             n2 <- evalExpr' e2
                             dedent
                             return $ (aOpFunc op) n1 n2


evalCond, evalCond' :: Cond -> State Config Bl
evalCond' = logIO evalCond

evalCond (BLit b)       = return b
evalCond (BOp e1 op e2) = do indent
                             n1 <- evalExpr' e1
                             n2 <- evalExpr' e2
                             dedent
                             return $ (bOpFunc op) n1 n2


evalStmt, evalStmt' :: Stmt -> State Config ()
evalStmt' = logIO evalStmt

-- evalStmt Skip         = return ()
-- evalStmt (Seq s1 s2)  = do evalStmt' s1
--                            viz "" -- newline
--                            evalStmt' s2
evalStmt (Assign v e) = do indent
                           n <- evalExpr' e
                           dedent
                           upsert v n
evalStmt (If c st sf) = do indent
                           b <- evalCond' c
                           let ss = if b then st else sf -- branch
                           evalStmts ss
                           dedent
evalStmt (While c ss) = do indent
                           b <- evalCond' c
                           if b then do
                             --indent
                             evalStmts ss -- execute the body
                             --dedent
                             evalStmt' (While c ss) -- loop again
                             dedent
                           else do dedent

evalStmts :: [Stmt] -> State Config ()
evalStmts []     = return () -- effectively Skip
evalStmts (s:ss) = do evalStmt' s
                      viz "" -- newline
                      evalStmts ss



initial :: Config
initial = Config {
    indentation = 0
  , log         = ""
  , memory      = empty
}

eval :: [Stmt] -> [(Var, Nr)]
eval program = toList (memory config)
  where (_, config) = runState (evalStmts program) initial


-- putStr $ vizExecution progAssigments
vizExecution :: [Stmt] -> String
vizExecution program = log config
  where (_, config) = runState (evalStmts program) initial
