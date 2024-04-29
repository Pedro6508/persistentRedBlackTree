module PersistenceSpec (
  persistenceSpec
) where

import Test.Hspec
import Persistence (Version(..), PartialPersistentOps(..))

foldVersion :: Version Int -> [Int -> Int] -> Version Int
foldVersion v [] = v
foldVersion v (f:fs) = foldVersion (patchLast v f) fs

persistenceSpec :: SpecWith ()
persistenceSpec = describe "Persistence" $ do
  it "creates a new version" $ do
    let version = foldVersion (Version ((+0), Nothing)) [(+1), (+2), (*2)]
    history 1 version `shouldBe` [1, 2, 4, 8]