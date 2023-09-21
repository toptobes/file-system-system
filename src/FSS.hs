module FSS (fss2js) where

import FSS.Backend.Generators
import FSS.Frontend.DirExplorer
import FSS.Frontend.Parser

fss2js :: FilePath -> Text -> IO ()
fss2js fp ft = do
  dirTree <- buildDirTree fp
  let asts = buildAST dirTree
      outs = map (genJS ft <$>) asts
  mapM_ putTextLn (showJS <$> outs)

showJS :: Either (Text, Text) (Text, Text) -> Text
showJS (Left (fname, err)) = "Error in '" <> fname <> "':\n" <> err
showJS (Right (mname, js)) = mname <> ":\n" <> js
