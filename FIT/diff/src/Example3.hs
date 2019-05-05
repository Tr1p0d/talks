{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Example3 where

import GHC.Generics

-- | Similar to 'Example2' but the result type of a diff is propagated
-- through the second parameter 'g'. It brings truly typesafe diff.
--
-- Two core extensions are used to make this work:
-- * MultiParamTypeClasses: https://wiki.haskell.org/Multi-parameter_type_class
-- * FunctionalDependencies: https://wiki.haskell.org/Functional_dependencies
class Diff a b | a -> b where
  diff :: a -> a -> b

  default diff :: (Generic a, Generic b, Diff' (Rep a) (Rep b)) => a -> a -> b
  diff a a' = to $ diff' (from a) (from a')

class Diff' f g where
  diff' :: f p -> f p -> g q

instance Diff' V1 V1 where
  diff' _ _ = undefined

instance Diff' U1 U1 where
  diff' _ _ = U1

instance (Diff' f f', Diff' g g') => Diff' (f :*: g) (f' :*: g') where
  diff' (x :*: y) (x' :*: y') = (diff' x x') :*: (diff' y y')

instance (Diff c c') => Diff' (K1 i c) (K1 i c') where
  diff' (K1 x) (K1 x') = K1 $ diff x x'

instance (Diff' f f') => Diff' (M1 i t f) (M1 i t f') where
  diff' (M1 x) (M1 x') = M1 $ diff' x x'

instance Diff Double Double where
  diff = (-)

instance Diff [Char] Bool where
  diff = (==)

instance Diff (String, Double) (Bool, Double)
