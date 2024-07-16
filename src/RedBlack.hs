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

balanceT :: Tree (a, Color) -> Tree (a, Color)
balanceT t = t'
  where (RedBlack t') = balance (RedBlack t)

del x t@(N (y, _) l r)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = fuse l r

delL x t@(N (y, R) t1 t2) = (N (y, R) (del x t1) t2)
delL x t@(N (y, B) t1 t2) = balL $ (N (y, R) (del x t1) t2)

delR x t@(N (y, R) t1 t2) = (N (y, R) t1 (del x t2))
delR x t@(N (y, B) t1 t2)= balR $ (N (y, B) t1 (del x t2))

fuse t1@(N (_, B) _ _) (N (y, R) t3 t4) = N (y, R) (fuse t1 t3) t4
fuse (N (x, R) t1 t2) t3@(N (_, B) _ _) = N (x, R) t1 (fuse t2 t3)
fuse (N (x, R) t1 t2) (N (y, R) t3 t4) = case r of 
  (N (z, R) r1 r2) -> (N (z, R) (N (x, R) t1 r1) (N (y, R) r2 t4))
  (N (_, B) _ _) -> (N (x, R) t1 (N (y, R) r t4))
  where r = fuse t1 t2
fuse (N (x, B) t1 t2) (N (y, B) t3 t4) = case r of 
  (N (z, R) r1 r2) -> (N (z, R) (N (x, B) t1 r1) (N (y, B) r2 t4))
  (N (z, B) r1 r2) -> (balL (N (x, B) t1 (N (y, B) r t4)))
  where r = fuse t1 t2

balL (N (y, B) (N (x, R) t1 t2) t3) =(N (y, R) (N (x, B) t1 t2) t3)
balL (N (x, B) t1 (N (y, B) t2 t3)) = balanceT (N (x, B) t1 (N (y, R) t2 t3))
balL (N (y, B) t1 (N (z, R) (N (u, B) t2 t3) t4@(N (v, B) l r))) = balanceT (N (u, R) (N (y, B) t1 t2) t3')
  where t3' = balanceT (N (z, B) t3 (N (v, R) l r))

balR (N (y, B) t1 (N (x, R) t2 t3)) = (N (y, R) t1 (N (x, B) t2 t3))
balR (N (y, B) (N (z, B) t1 t2) t3) = balanceT (N (y, B) (N (z, R) t1 t2) t3)
balR (N (y, B) (N (z, R) t1@(N (v, B) l r) (N (u, B) t2 t3)) t4) = N (u, R) t2' (N (y, B) t3 t4)
  where t2' = balanceT (N (z, B) (N (v, R) l r) t2)

instance BasicOps RedBlack where
  insert x (RedBlack t)
    | t == E = RedBlack (N (x, B) E E)
    | t@(N (a, R) l r) <- t = if x < a
      then balance (RedBlack (N (a, R) (insert (x, R) l) r))
      else balance (RedBlack (N (a, R) l (insert (x, R) r)))
    | t@(N (a, B) l r) <- t = if x < a
      then (RedBlack (N (a, B) (insert (x, R) l) r))
      else (RedBlack (N (a, B) l (insert (x, R) r)))
  delete x (RedBlack t) = mkBlack (del x t)
    where mkBlack (N (a, _) l r) = RedBlack (N (a, B) l r)
    
  search x (RedBlack t)
    | t == E = Nothing
    | t@(N (a, _) l r) <- t = if x == a
      then Just a
      else if x < a
        then search x (RedBlack l)
        else search x (RedBlack r)
  inorder (RedBlack t)
    | t@(N (a, _) l r) <- t = inorder (RedBlack l) ++ [a] ++ inorder (RedBlack r)
    | otherwise = []