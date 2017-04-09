module Evaluation where

import Control.Monad.State (State, runState, get)
import Data.Map (empty, toList)
import Types
import Memory
import Axioms


finalMemory :: Stmt -> [(Var, Nr)]
finalMemory program = toList . snd $ runState (eval program) empty

eval :: Stmt -> State Memory Stmt
eval Skip = return Skip
eval code = do
  code' <- transS code
  eval code'


type Config = (Stmt, Memory)

showConfig :: Config -> String
showConfig (code, memory) = (show code) ++ "\t" ++ (showMemory memory)

executionConfigs :: Stmt -> [Config]
executionConfigs program = configs -- putStrLn $ "\n" `intercalate` (map showConfig configs)
  where ((_, configs), _) = runState (evalSteps program []) empty


evalSteps :: Stmt -> [Config] -> State Memory (Stmt, [Config])
evalSteps Skip configs = do
  mem <- get
  return (Skip, configs ++ [(Skip, mem)])
evalSteps code configs = do
  mem <- get
  code' <- transS code
  evalSteps code' (configs ++ [(code, mem)])
