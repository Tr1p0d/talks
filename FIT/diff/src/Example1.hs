module Example1 where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.UUID

import Common (Bar(Bar), Result(..), Foo(Foo))

-- | Wrapper around a function taking two instances of the same type
-- and returning a 'Result'
newtype Diff a = Diff { runDiff :: a -> a -> Result }

-- | Dual of 'Functor'. 'contramap' is 'map' with all arrows flipped.
-- Unlike 'Functor' which is used to map over result, 'Contravariant'
-- is used to "map" over arguments, which is exactly the case of 'Diff'.
instance Contravariant Diff where
  contramap f diff' = Diff $ \a a' -> runDiff diff' (f a) (f a')

-- | (A sort of) dual of 'Applicative'. It allows us to build
-- larger 'Diff's from smaller ones similarly to how 'Applicative'
-- works. It is not as elegant nor seamless since Haskell does not
-- support universal construction (well generics do) on product types.
instance Divisible Diff where
  conquer = Diff $ \a a' -> Empty

  divide f fb fc = Diff $ \a a' -> case f a of
    (b, c) -> case f a' of
      (b', c') ->
        let diffb = runDiff fb b b'
            diffc = runDiff fc c c'
        in Tuple diffb diffc

contramap2 :: Divisible f => (a -> (b, c)) -> f b -> f c -> f a
contramap2 = divide

contramap3 :: Divisible f => (a -> (b, (c, d))) -> f b -> f c -> f d -> f a
contramap3 f fb fc fd = divide f fb $ divided fc fd

-- | Can you see the pattern? Its just 'Applicative' in reverse...
contramap4 :: Divisible f => (a -> (b, (c, (d, e)))) -> f b -> f c -> f d -> f e -> f a
contramap4 f fb fc fd fe = divide f fb $ divided fc $ divided fd fe

doubleD :: Diff Double
doubleD = Diff $ \x0 x1 -> DiffDouble $ x0 - x1

stringD :: Diff String
stringD = Diff $ \x0 x1 -> DiffString $ x0 == x1

uuidD :: Diff UUID
uuidD = contramap toString stringD

barD :: Diff Bar
barD = contramap2 (\(Bar d s) -> (d, s)) doubleD stringD

fooD :: Diff Foo
fooD = contramap3 (\(Foo s id bar) -> (s, (id, bar))) stringD uuidD barD
