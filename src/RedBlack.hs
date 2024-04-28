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

newtype RedBlack a = RedBlack (Tree (a, Color)) deriving (Show, Eq)

instance BasicOps RedBlack where
  insert x (RedBlack t)
    | t == Empty = RedBlack (Node (x, Black) Empty Empty)
    | t@(Node (a, Black) Empty Empty) <- t = if x < a
      then RedBlack (Node (a, Black) (Node (x, Red) Empty Empty) Empty)
      else RedBlack (Node (a, Black) Empty (Node (x, Red) Empty Empty))
  delete x (RedBlack t) = undefined
  search x (RedBlack t) = undefined
  inorder (RedBlack t) = undefined