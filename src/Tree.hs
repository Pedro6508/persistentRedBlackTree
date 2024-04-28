module Tree
  (
    Tree(..)
  ) where

import BasicOps

data Tree a = E
  | N a (Tree a) (Tree a) deriving (Show, Read, Eq)

extractMin :: Tree a -> (a, Tree a)
extractMin (N a E r) = (a, r)
extractMin (N a l r) =
  let (m, l') = extractMin l in (m, N a l' r)

extractMax :: Tree a -> (a, Tree a)
extractMax (N a l E) = (a, l)
extractMax (N a l r) =
  let (m, r') = extractMax r in (m, N a l r')

instance BasicOps Tree where
  insert x E = N x E E
  insert x (N a l r)
    | x == a = N a l r
    | x < a = N a (insert x l) r
    | x > a = N a l (insert x r)

  delete x E = Nothing
  delete x (N a l r)
    | x < a = N a <$> delete x l <*> pure r
    | x > a = N a l <$> delete x r
    | otherwise = case (l, r) of
      (E, E) -> Just E
      (E, _) -> Just r
      (_, E) -> Just l
      (_, _) -> Just $ N next l r'
        where (next, r') = extractMin r

  search x E = Nothing
  search x (N a l r)
    | x == a = Just a
    | x < a = search x l
    | x > a = search x r

  inorder E = []
  inorder (N a l r) = inorder l ++ [a] ++ inorder r

