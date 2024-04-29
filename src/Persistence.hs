module Persistence (
  Version(..),
  PartialPersistentOps(..),
) where

newtype Version a = Version (a -> a, Maybe (Version a))

class PartialPersistentOps t where
  history :: a -> t a -> [a]
  patchLast :: t a -> (a -> a) -> t a

instance PartialPersistentOps Version where
  history x (Version (f, Nothing)) = [x, f x]
  history x (Version (f, Just p)) = (history x p) ++ [f x]
  patchLast v@(Version (f, _)) g = Version (g . f, Just v)