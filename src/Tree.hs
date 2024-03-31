module Tree
  (
    Tree(..),
    BasicOps(..)
  ) where

data Tree a = Empty
  | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

extractMin :: Tree a -> (a, Tree a)
extractMin (Node a Empty r) = (a, r)
extractMin (Node a l r) =
  let (m, l') = extractMin l in (m, Node a l' r)

extractMax :: Tree a -> (a, Tree a)
extractMax (Node a l Empty) = (a, l)
extractMax (Node a l r) =
  let (m, r') = extractMax r in (m, Node a l r')

instance BasicOps Tree where
  insert x Empty = Node x Empty Empty
  insert x (Node a l r)
    | x == a = Node a l r
    | x < a = Node a (insert x l) r
    | x > a = Node a l (insert x r)

  delete x Empty = Nothing
  delete x (Node a l r)
    | x < a = Node a <$> delete x l <*> pure r
    | x > a = Node a l <$> delete x r
    | otherwise = case (l, r) of
      (Empty, Empty) -> Just Empty
      (Empty, _) -> Just r
      (_, Empty) -> Just l
      (_, _) -> Just $ Node next l r'
        where (next, r') = extractMin r

  search x Empty = Nothing
  search x (Node a l r)
    | x == a = Just a
    | x < a = search x l
    | x > a = search x r

  inorder Empty = []
  inorder (Node a l r) = inorder l ++ [a] ++ inorder r

class BasicOps t where
  insert :: (Ord a) => a -> t a -> t a
  delete :: (Ord a) => a -> t a -> Maybe (t a)
  search :: (Ord a) => a -> t a -> Maybe a
  contains :: (Ord a) => a -> t a -> Bool
  contains x t = case search x t of
    Just _ -> True
    Nothing -> False
  inorder :: t a -> [a]

