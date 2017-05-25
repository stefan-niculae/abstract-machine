module Utils where

import Text.PrettyPrint

intersperse :: Doc -> [Doc] -> Doc
intersperse _     []     = empty
intersperse _     [x]    = x
intersperse delim (x:xs) = x <+> delim <+> intersperse delim xs
