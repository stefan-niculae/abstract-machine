module Main where

import Tests


main :: IO()
main = print $ if all (==True) tests
  then "all tests passed"
  else (\b -> if b then '.' else 'F') `map` tests
