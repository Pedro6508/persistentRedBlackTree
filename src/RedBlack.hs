module RedBlack
  (
    RedBlack(..),
    Color(..),
  ) where

import BasicOps
import Tree

class RedBlackOps t where
  leftRotate :: t a -> t a
  rightRotate :: t a -> t a

data Color = Black
  | Red deriving (Show, Read, Eq)

newtype RedBlack a = RedBlack (Tree a, Color) deriving (Show, Eq)

instance BasicOps RedBlack where
  insert x rt@(RedBlack (t, c))
    | t == Empty = RedBlack (Node x Empty Empty, Black)
    | otherwise = undefined
  delete x (RedBlack (t, c)) = undefined
  search x (RedBlack (t, c)) = undefined
  inorder (RedBlack (t, c)) = undefined