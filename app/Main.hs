module Main (main) where

import Tree
import BasicOps

main :: IO ()
main = do
  print "Hello, World!"
  let tree = foldr insert E [5, 3, 7, 1, 4, 6, 8]
  print $ inorder tree
