module FileOpsSpec (
  fileOpsSpec  
) where

import Test.Hspec
-- import Test.QuickCheck
-- import Test.QuickCheck.IO ()
import FileOps (Ops(..), readOpsFile)

fileOpsSpec :: SpecWith ()
fileOpsSpec = describe "FileOps" $ do
  it "reads a file" $ do
    (readOpsFile "test/test.txt") `shouldReturn` [INC 13,INC 42,REM 42,INC 14,SUCC 13 2]
  it "reads a empty file" $ do
    (readOpsFile "test/empty.txt") `shouldReturn` []
