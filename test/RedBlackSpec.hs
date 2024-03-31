module RedBlackSpec
  (
    rbSpec
  ) where

import Test.Hspec
import RedBlack
import BasicOps
import Tree

rbSpec = describe "RedBlack" $ do
  it "left rotates" $ do
    let tree = Tree (Black 5)
    let newTree = leftRotate tree
    newTree `shouldBe` Tree (Black 5)
  it "right rotates" $ do
    let tree = Tree (Black 5)
    let newTree = rightRotate tree
    newTree `shouldBe` Tree (Black 5)