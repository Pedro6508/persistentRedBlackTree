module TreeSpec
  (
    treeSpec
  ) where

import Test.Hspec
import Tree
import BasicOps

populateTree :: [Int] -> Tree Int
populateTree = foldr insert Empty

treeSpec :: SpecWith ()
treeSpec = describe "Tree" $ do
  it "inserts a value" $ do
    let tree = populateTree [8, 7, 6, 5, 2, 1]
    contains 5 tree `shouldBe` True
  it "deletes a value" $ do
    let tree = populateTree [8, 7, 6, 5, 1]
    let newTree = delete 5 tree
    case newTree of
      Just t -> contains 5 t `shouldBe` False
      Nothing -> True `shouldBe` True
  it "searches for a value" $ do
    let tree = populateTree [8, 7, 6, 5, 4, 3, 2, 1]
    search 5 tree `shouldBe` Just 5
  it "performs an inorder traversal" $ do
    let tree = populateTree [8, 7, 6, 5, 4, 3, 1]
    inorder tree `shouldBe` [1, 3, 4, 5, 6, 7, 8]