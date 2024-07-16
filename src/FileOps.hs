module FileOps (
 readOpsFile,
 Ops(..)
) where

data Ops = INC Int | REM Int | SUCC Int Int | IMP deriving (Show, Eq)

readOp :: String -> Ops
readOp s = case words s of
  ["INC", x] -> INC (read x)
  ["REM", x] -> REM (read x)
  ["SUCC", x, y] -> SUCC (read x) (read y)
  ["IMP"] -> IMP

readOpsFile :: String -> IO [Ops]
readOpsFile path = do
  content <- readFile path
  return $ map readOp (lines content)
        
