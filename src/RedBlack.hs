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

instance Ord Color where
  compare B R = GT
  compare R B = LT
  compare _ _ = EQ

newtype RedBlack a = RedBlack (Tree (a, Color)) deriving (Show, Eq)
leaf :: a -> Color -> Tree (a, Color)
leaf x c = N (x, c) E E

fmt (x, y, z) (a, b, c, d) = RedBlack (N (y, R) left right)
    where
      left = N (x, B) a b
      right = N (z, B) c d

balance :: RedBlack a -> RedBlack a
balance (RedBlack t)
	| t@(N (z, B) (N (y, R) (N (x, R) a b) c) d) <- t = fmt (x, y, z) (a, b, c, d)
	| t@(N (z, B) (N (x, R) a (N (y, R) b c)) d) <- t = fmt (x, y, z) (a, b, c, d)
	| t@(N (x, B) a (N (z, R) (N (y, R) b c) d)) <- t = fmt (x, y, z) (a, b, c, d)
	| t@(N (x, B) a (N (z, R) b (N (y, R) c d))) <- t = fmt (x, y, z) (a, b, c, d)
	| otherwise = RedBlack t

instance BasicOps RedBlack where
  insert x (RedBlack t)
    | t == E = RedBlack (N (x, B) E E)
    | t@(N (a, R) l r) <- t = if x < a
      then balance (RedBlack (N (a, R) (insert (x, R) l) r))
      else balance (RedBlack (N (a, R) l (insert (x, R) r)))
    | t@(N (a, B) l r) <- t = if x < a
      then (RedBlack (N (a, B) (insert (x, R) l) r))
      else (RedBlack (N (a, B) l (insert (x, R) r)))
  delete x (RedBlack t) = undefined
  search x (RedBlack t) = undefined
  inorder (RedBlack t) = undefined