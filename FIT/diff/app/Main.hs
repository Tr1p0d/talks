module Main where

import Data.Foldable (traverse_)

import Common (foo1, foo2)
import Example1
import Example2
import Example3

main :: IO ()
main = putStrLn `traverse_` diffed
  where
    diffed =
      [ "Example 1 - using algebraic abstraction:"
      , show $ runDiff fooD foo1 foo2
      , show $ runDiff fooD foo1 foo1
      , "Example 2 - the same using generics:"
      , show $ Example2.diff foo1 foo2
      , show $ Example2.diff foo1 foo1
      , "Example 2 - truly typesafe diff using generics:"
      , show $ Example3.diff ("1234", 1.1 :: Double) ("4321", 2.1 :: Double)
      , show $ Example3.diff ("1234", 1.1 :: Double) ("1234", 1.1 :: Double)
      ]
