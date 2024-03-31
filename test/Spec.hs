import Test.Hspec
import Tree
import BasicOps

import RedBlackSpec
import TreeSpec

main :: IO ()
main = hspec $ do
  treeSpec
  rbSpec


