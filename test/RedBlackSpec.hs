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
    let tree = RedBlack E
    let tree' = insert 1 tree
    tree' `shouldBe` RedBlack (N (1, B) E E)
  it "adds a new node to a tree with a single black node" $ do
    let tree = RedBlack (N (1, B) E E)
    let tree' = insert 2 tree
    tree' `shouldBe` RedBlack (N (1, B) E (N (2, R) E E))