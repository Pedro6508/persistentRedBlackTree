module RedBlack
  (
    RedBlack(..),
    Color(..),
    RedBlackOps(..)
  ) where

import BasicOps
import Tree

class RedBlackOps t where
  leftRotate :: t a -> t a
  rightRotate :: t a -> t a

instance RedBlackOps RedBlack where
  leftRotate = undefined
  rightRotate = undefined

data Color a = Black a
  | Red a deriving (Show, Read, Eq)

newtype RedBlack a = Tree (Color a) deriving (Show, Read, Eq)
