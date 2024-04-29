module PersistenceSpec (
  persistenceSpec
  , persistentTreeSpec
) where

import Test.Hspec
import Persistence (Version(..), PartialPersistentOps(..))
import BasicOps (BasicOps(..))
import Tree (Tree(..))

foldVersion :: Version a -> [a -> a] -> Version a
foldVersion v [] = v
foldVersion v (f:fs) = foldVersion (patchLast v f) fs

persistenceSpec :: SpecWith ()
persistenceSpec = describe "Persistence" $ do
  it "creates a new version" $ do
    let version = foldVersion (Version ((+0), Nothing)) [(+1), (+2), (*2)]
    history 1 version `shouldBe` [1, 1, 2, 4, 8]
  it "patches the last version" $ do
    let version = patchLast (Version ((+2), Nothing)) (*2)
    history 1 version `shouldBe` [1, 3, 6]

deleteIfPresent :: (Ord a) => a -> Tree a -> Tree a
deleteIfPresent x t = case delete x t of
  Just t' -> t'
  Nothing -> t

persistentTreeSpec :: SpecWith ()
persistentTreeSpec = describe "Persistent Tree" $ do
  it "version history" $ do
    let version = foldVersion (Version ((insert 9), Nothing)) (map insert [1, 2, 3, 0])
    let treeHistory = map (inorder) (history E version)
    treeHistory `shouldBe` [[],[9],[1,9],[1,2,9],[1,2,3,9],[0,1,2,3,9]]
  it "patch last version" $ do
    let version = patchLast (Version ((insert 9), Nothing)) (insert 0)
    let treeHistory = map (inorder) (history E version)
    treeHistory `shouldBe` [[],[9],[0,9]]
  it "delete from last version" $ do
    let version = patchLast (Version ((insert 9), Nothing)) (insert 0)
    let version' = patchLast version (deleteIfPresent 0)
    let treeHistory = map (inorder) (history E version')
    treeHistory `shouldBe` [[],[9],[0, 9], [9]]
  it "history of a tree" $ do
    let insertAndDelete x = if x `mod` 3 == 0 then (deleteIfPresent x) else (insert x)
    let version = foldVersion (Version ((insert 9), Nothing)) (map insertAndDelete [1, 2, 9, 9, 3, 4, 5, 0])
    let treeHistory = map (inorder) (history E version)
    treeHistory `shouldBe` [[],[9],[1,9],[1,2,9],[1,2],[1,2],[1,2],[1,2,4],[1,2,4,5],[1,2,4,5]]
