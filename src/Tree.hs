module Tree
  (
    Tree(..)
  ) where

import BasicOps

data Tree a = E
  | N a (Tree a) (Tree a) deriving (Show, Read, Eq)

extractMin :: Tree a -> Maybe (a, Tree a)
extractMin E = Nothing
extractMin (N a E r) = Just (a, r)
extractMin (N a l r) = do
  (m, l') <- extractMin l
  return (m, N a l' r)

extractMax :: Tree a -> Maybe (a, Tree a)
extractMax E = Nothing
extractMax (N a l E) = Just (a, l)
extractMax (N a l r) = do
  (m, r') <- extractMax r
  return (m, N a l r')

deleteMaybe :: (Ord a) => a -> Tree a -> Maybe (Tree a)
deleteMaybe x E = Nothing
deleteMaybe x (N a l r)
  | x < a = N a <$> deleteMaybe x l <*> pure r
  | x > a = N a l <$> deleteMaybe x r
  | otherwise = case (l, r) of
    (E, E) -> Just E
    (E, _) -> Just r
    (_, E) -> Just l
    otherwise -> do
      (next, r') <- extractMin r
      return (N next l r')

instance BasicOps Tree where
  insert x E = N x E E
  insert x (N a l r)
    | x == a = N a l r
    | x < a = N a (insert x l) r
    | x > a = N a l (insert x r)

  delete x t = case deleteMaybe x t of
    Just t' -> t'
    Nothing -> t

  search x E = Nothing
  search x (N a l r)
    | x == a = Just a
    | x < a = search x l
    | x > a = search x r

  inorder E = []
  inorder (N a l r) = inorder l ++ [a] ++ inorder r

