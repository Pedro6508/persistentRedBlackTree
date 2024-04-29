import Test.Hspec
import Tree
import BasicOps

import RedBlackSpec
import TreeSpec
import PersistenceSpec

main :: IO ()
main = hspec $ do
  treeSpec
  rbSpec
  persistenceSpec


