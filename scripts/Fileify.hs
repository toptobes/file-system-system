{-# LANGUAGE BlockArguments, OverloadedStrings #-}

import Control.Monad
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Data.Text.IO qualified as TIO
import System.Directory
import System.FilePath

main :: IO ()
main = getArgs >>= \case
  ["default-delim"] -> putTextLn "∙"

  [fp, out] -> fileify '∙' out fp

  [fp, out, [delim]] -> fileify delim out fp

  _ -> crash "usage: 'fileify <input> <outdir> [char delim]' | 'fileify default-delim'"

fileify :: Char -> FilePath -> FilePath -> IO ()
fileify delim outDir = TIO.readFile >=> foldM_ makeFileAndAdjustStack [] . T.lines
  where
    makeFileAndAdjustStack stack line = do
      let indentation = T.length (T.takeWhile (== delim) line)
          newStack = take indentation stack ++ [T.dropWhile (== delim) line]
          path = outDir </> T.unpack (T.intercalate "/" newStack)

      if T.last line == '/'
        then createDirectoryIfMissing True path
        else writeFile path ""

      pure newStack

crash :: Text -> IO ()
crash = hPutStrLn stderr >=> const exitFailure
