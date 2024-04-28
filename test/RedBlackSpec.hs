module RedBlackSpec
  (
    rbSpec
  ) where

import Test.Hspec
import RedBlack (RedBlack(..), Color(..))
import Tree (Tree(..))
import BasicOps (insert)

rbSpec :: SpecWith ()
rbSpec = describe "RedBlack" $ do
  it "adds a new node to an empty tree" $ do
    let tree = RedBlack Empty
    let tree' = insert 1 tree
    tree' `shouldBe` RedBlack (Node (1, Black) Empty Empty)
  it "adds a new node to a tree with a single black node" $ do
    let tree = RedBlack (Node (1, Black) Empty Empty)
    let tree' = insert 2 tree
    tree' `shouldBe` RedBlack (Node (1, Black) Empty (Node (2, Red) Empty Empty))