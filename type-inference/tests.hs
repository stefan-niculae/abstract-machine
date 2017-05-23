module A where

import Syntax
import Types
{-
 env = t1: forall [a,b] . a -> c -> TInt -> b
 t: a -> d -> c
 generalize map t = forall [a, d] . a -> d -> c

 luam ftv(t)   = toate: a, d, c
 luam tfv(env) = c
-}
