{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Memory where

import Data.Map (Map, lookup, insert, toList)
import Prelude hiding (lookup)
import Control.Monad.State (State, get, put)
import Types

type Memory = Map Var Nr


val :: Var -> State Memory Nr
val x = do
  mem <- get
  case lookup x mem of
    Nothing -> error ("undefined variable " ++ x)
    Just n  -> return n


upsert :: Var -> Nr -> State Memory ()
upsert x n = do
  mem <- get
  put (insert x n mem)
  return ()


showMemory :: Memory -> String
showMemory memory = "{" ++ joined ++ "}"
  where joined = unwords . map showTuple . toList $ memory
        showTuple (var, n) = var ++ ":" ++ (show n)
