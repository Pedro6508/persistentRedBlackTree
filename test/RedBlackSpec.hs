module RedBlackSpec
  (
    rbSpec
  ) where

import Test.Hspec
import RedBlack (RedBlack(..), Color(..), balance)
import Tree (Tree(..))
import BasicOps (insert)

populateRedBlack :: [Int] -> RedBlack Int
populateRedBlack = foldr insert (RedBlack E)

findDoubleRed :: RedBlack a -> Bool
findDoubleRed (RedBlack E) = False
findDoubleRed (RedBlack (N (_, R) (N (_, R) _ _) _)) = True
findDoubleRed _ = False

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
  it "balance a tree with a red parent and son" $ do
		let tree = populateRedBlack [3, 4, 5, 1, 2, 5, 6, 7, 9, 10]
		let hasDoubleRed' = findDoubleRed (balance tree)
		hasDoubleRed' `shouldBe` False
  it "inserts multiple values" $ do
    let tree = populateRedBlack [8, 7, 6, 5, 2, 1]
    let hasDoubleRed = findDoubleRed tree
    hasDoubleRed `shouldBe` False