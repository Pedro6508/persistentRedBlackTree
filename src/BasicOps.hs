module BasicOps (
    BasicOps(..)
  ) where

class BasicOps t where
  insert :: (Ord a) => a -> t a -> t a
  delete :: (Ord a) => a -> t a -> Maybe (t a)
  search :: (Ord a) => a -> t a -> Maybe a
  contains :: (Ord a) => a -> t a -> Bool
  contains x t = case search x t of
    Just _ -> True
    Nothing -> False
  inorder :: t a -> [a]