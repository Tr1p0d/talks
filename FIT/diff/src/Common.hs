{-# LANGUAGE DeriveGeneric #-}

module Common (
  Result(..),
  Foo(Foo),
  Bar(Bar),
  foo1,
  foo2
  ) where

import GHC.Generics
import Data.UUID (UUID, nil)

data Foo = Foo String UUID Bar
  deriving (Show, Generic)

foo1 = Foo "1234" nil bar1
foo2 = Foo "4321" nil bar2

data Bar = Bar Double String
  deriving (Show, Generic)

bar1 = Bar 1.0 "abcd"
bar2 = Bar 2.0 "bcda"

data Result
    = DiffDouble Double
    | DiffString Bool
    | Tuple Result Result
    | Empty
  deriving Show
