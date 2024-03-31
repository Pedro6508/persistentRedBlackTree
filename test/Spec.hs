import Test.Hspec
import Tree
import BasicOps

main :: IO ()
main = hspec $ do
  describe "Tree" $ do
   it "inserts a value" $ do
     let tree = insert 5 Empty
     contains 5 tree `shouldBe` True

   it "deletes a value" $ do
     let tree = insert 5 Empty
     let newTree = delete 5 tree
     case newTree of
       Just t -> contains 5 t `shouldBe` False
       Nothing -> True `shouldBe` True

   it "searches for a value" $ do
     let tree = insert 5 Empty
     search 5 tree `shouldBe` Just 5

   it "performs an inorder traversal" $ do
     let tree = foldr insert Empty [5, 3, 7, 1, 4, 6, 8]
     inorder tree `shouldBe` [1, 3, 4, 5, 6, 7, 8]


