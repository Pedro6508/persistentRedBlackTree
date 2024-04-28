module RedBlack
  (
    RedBlack(..),
    Color(..),
    balance
  ) where

import BasicOps
import Tree

class RedBlackOps t where
  leftRotate :: t a -> t a
  rightRotate :: t a -> t a

data Color = B
  | R deriving (Show, Read, Eq)

newtype RedBlack a = RedBlack (Tree (a, Color)) deriving (Show, Eq)
leaf :: a -> Color -> Tree (a, Color)
leaf x c = N (x, c) E E

balance :: Tree (a, Color) -> RedBlack a
balance (N (z, B) (N (y, R) (N (x, R) a b) c) d)
	= RedBlack (N (y, R) left right)
	where
		left = N (x, B) a b
		right = N (z, B) c d
balance _ = undefined

instance BasicOps RedBlack where
  insert x (RedBlack t)
    | t == E = RedBlack (N (x, B) E E)
    | t@(N (a, B) E E) <- t = if x < a
      then RedBlack (N (a, B) x' E)
      else RedBlack (N (a, B) E x')
      where x' = leaf x R
  delete x (RedBlack t) = undefined
  search x (RedBlack t) = undefined
  inorder (RedBlack t) = undefined