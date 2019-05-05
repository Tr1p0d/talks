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

-- | Data type representing all possible outcomes of a diff.
-- It is what 'Generic' does for product types but without
-- such strong type safety.
data Result
    = DiffDouble Double
    | DiffString Bool
    -- ^ It is not flexible since we have to defined what the outcome
    -- of diffing a constant is. What if we wanted to diff String
    -- with different outcome than 'Bool'?
    | Tuple Result Result
    -- ^ Create a pair of two results. Very similar to ':*:'.
    | Empty
    -- ^ Neutral element with respect to product types. Equivalent
    -- to 'U1'.
  deriving Show
