module InferenceMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Types

{-
Taken as is.
Also supplies fresh names
-}

data TIEnv = TIEnv  {}

data TIState = TIState { count :: Int }

-- TODO this can be simplified https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs#L26
type TI = ExceptT String (ReaderT TIEnv (StateT TIState IO))

runTI :: TI a -> IO (Either String a, TIState)
runTI t = do
  (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
  return (res, st)
  where initTIEnv = TIEnv
        initTIState = TIState{count = 0}

fresh :: TI Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return (TVar  ("t" ++ show (count s)))
