{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module FileOps
  ( readOpsFile,
    writeOutput,
    Ops (..),
  )
where

import BasicOps
-- import Paths_redBlackTree (version)
import Persistence
import RedBlack
import Tree
import Prelude hiding (last)

extractLastVersion :: Version (RedBlack a) -> RedBlack a
extractLastVersion (Version (f, Nothing)) = f (RedBlack E)
extractLastVersion (Version (f, Just p)) = f (extractLastVersion p)

data Ops = INC Int | REM Int | SUCC Int Int | IMP Int deriving (Show, Eq)

showVersion t = foldl (\acc node -> acc ++ showNode node ++ " ") "" (inorderWithColor t 0)

showNode :: (Show a) => (a, Int, Color) -> String
showNode (a, deep, color) = (show a) ++ "," ++ (show deep) ++ "," ++ (show color)

inorderWithColor :: (Num b) => RedBlack a -> b -> [(a, b, Color)]
inorderWithColor (RedBlack (N (a, c) l r)) deep = inorderWithColor (RedBlack l) (deep + 1) ++ [(a, deep, c)] ++ inorderWithColor (RedBlack r) (deep + 1)
inorderWithColor (RedBlack E) _ = []

readOp :: String -> Ops
readOp s = case words s of
  ["INC", x] -> INC (read x)
  ["REM", x] -> REM (read x)
  ["SUCC", x, y] -> SUCC (read x) (read y)
  ["IMP", x] -> IMP (read x)
  otherwise -> error "Invalid operation"

readOpsFile :: String -> IO [Ops]
readOpsFile path = do
  content <- readFile path
  return $ map readOp (lines content)

extractMaxRB :: RedBlack a -> Maybe (a, RedBlack a)
extractMaxRB (RedBlack (N (a, c) l r))
  | E <- r = Just (a, RedBlack l)
  | otherwise = do
      (m, RedBlack r') <- extractMaxRB (RedBlack r)
      return (m, RedBlack (N (a, c) l r'))
extractMaxRB (RedBlack E) = Nothing

findNext :: (Ord a) => a -> RedBlack a -> Maybe a
findNext x (RedBlack t@(N (a, c) l r))
  | x < a = findNext x (RedBlack l)
  | x > a = findNext x (RedBlack r)
  | otherwise = case extractMaxRB (RedBlack l) of
      Just (m, _) -> Just m
      Nothing -> Nothing
findNext x (RedBlack E) = Nothing

takeNth :: Int -> [a] -> Maybe a
takeNth n xs = case drop n xs of
  [] -> Nothing
  (y : _) -> Just y

applyOp :: [Char] -> Version (RedBlack Int) -> Ops -> ([Char], Version (RedBlack Int))
applyOp acc version (INC x) = (acc, patchLast version (insert x))
applyOp acc version (REM x) = (acc, patchLast version (delete x))
applyOp acc version (IMP x) =
  ( acc
      ++ showVersion
        ( case takeNth x (history (RedBlack E) version) of
            Nothing -> extractLastVersion version
            Just t -> t
        )
      ++ "\n",
    version
  )
applyOp acc version (SUCC x v) = case history (RedBlack E) version of
  [] -> (acc ++ show (findNext x (extractLastVersion version)), version)
  list -> case takeNth v list of
    Nothing -> (acc ++ "INF", version)
    Just t -> (acc ++ maybe "INF" show (findNext x t)
      , version)

writeOutput :: FilePath -> String -> IO String
writeOutput path opsPath = do
  ops <- readOpsFile opsPath
  writeFile path $ fst $ foldl (\(acc, version) op -> applyOp acc version op) ("", Version (id, Nothing)) ops
  readFile path
