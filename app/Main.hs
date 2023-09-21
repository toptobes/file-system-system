module Main where

import CodeGen.Generators
import Data.Text.IO qualified as TIO
import Parsing.Parser
import Parsing.DirExplorer

main :: IO ()
main = getArgs >>= \case
  [fp] -> fss2js fp "minify"
  [fp, "minify"] -> fss2js fp "minify"
  [fp, "pretty"] -> fss2js fp "pretty"
  _ -> printUsageAndExit

fss2js :: FilePath -> Text -> IO ()
fss2js fp ft = do
  dirTree <- buildDirTree fp
  let asts = buildAST dirTree
      outs = map (genJS ft <$>) asts
  mapM_ putTextLn (showJS <$> outs)

showJS :: Either (Text, Text) (Text, Text) -> Text
showJS (Left (fname, err)) = "Error in '" <> fname <> "':\n" <> err
showJS (Right (mname, js)) = mname <> ":\n" <> js

printUsageAndExit :: IO ()
printUsageAndExit = TIO.hPutStrLn stderr "usage: ./fss <path-to-program> [minify|pretty]" >> exitFailure
