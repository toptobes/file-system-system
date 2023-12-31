module FSS.Frontend.DirExplorer
( buildDirTree,
  DirTree (..),
) where

import Data.List (isSuffixOf)
import Data.Text qualified as T
import System.Directory
import System.FilePath

data DirTree where
  Dir  :: { fname :: Text, subdirs :: [DirTree] } -> DirTree
  File :: { fname :: Text } -> DirTree
  deriving (Show)

buildDirTree :: FilePath -> IO DirTree
buildDirTree = buildDirTreeGeneric (filter (".fs" `isSuffixOf`))

buildDirTree' :: FilePath -> IO DirTree
buildDirTree' = buildDirTreeGeneric id

buildDirTreeGeneric :: ([FilePath] -> [FilePath]) -> FilePath -> IO DirTree
buildDirTreeGeneric fn dirName = do
  allNames <- sort <$> listDirectory dirName
  let filtered = fn allNames
      allNames' = (dirName </>) <$> filtered
  (files, dirs) <- partitionM doesFileExist allNames'
  subdirs <- mapM buildDirTree' dirs
  pure $ Dir (T.pack dirName) $ subdirs <> map File (T.pack <$> files)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
  px <- p x
  (ys, zs) <- partitionM p xs
  if px
    then return (x:ys, zs)
    else return (ys, x:zs)
