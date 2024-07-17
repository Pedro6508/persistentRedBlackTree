module FileOpsSpec (
  fileOpsSpec
) where

import Test.Hspec
-- import Test.QuickCheck
-- import Test.QuickCheck.IO ()
import FileOps (Ops(..), readOpsFile, writeOutput)

fileOpsSpec :: SpecWith ()
fileOpsSpec = describe "FileOps" $ do
  it "reads a file" $ do
    (readOpsFile "test/test.txt") `shouldReturn`  [INC 13,INC 42,REM 42,INC 1,INC 2,INC 3,INC 4,IMP 3,INC 5,INC 14,SUCC 13 2,IMP 9]
  it "reads a empty file" $ do
    (readOpsFile "test/empty.txt") `shouldReturn` []
  it "writes output to a file" $ do
    (writeOutput "test/output.txt" "test/test.txt") `shouldReturn` "13,0,B 42,1,R \nINF1,1,R 2,2,R 3,3,R 4,4,R 5,5,R 13,0,B \n"
