module Main where

import FSS
import Data.Text.IO qualified as TIO

main :: IO ()
main = getArgs >>= \case
  [fp] -> fss2js fp "minify"
  [fp, "minify"] -> fss2js fp "minify"
  [fp, "pretty"] -> fss2js fp "pretty"
  _ -> printUsageAndExit

printUsageAndExit :: IO ()
printUsageAndExit = TIO.hPutStrLn stderr "usage: ./fss <path-to-program> [minify|pretty]" >> exitFailure
