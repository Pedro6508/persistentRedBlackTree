import Test.Hspec
import Tree
import BasicOps

import RedBlackSpec
import TreeSpec
import PersistenceSpec (persistenceSpec, persistentTreeSpec)
import FileOpsSpec (fileOpsSpec)

main :: IO ()
main = hspec $ do
  treeSpec
  rbSpec
  persistenceSpec
  persistentTreeSpec
  fileOpsSpec

