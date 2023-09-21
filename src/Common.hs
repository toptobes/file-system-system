module Common where

import qualified Data.Text as T

takeDirectory :: Text -> Text
takeDirectory fp = case viaNonEmpty init $ T.splitOn "/" fp of
  Nothing -> ""
  Just xs -> T.intercalate "/" xs

takeFileName :: Text -> Text
takeFileName = snd . T.breakOnEnd "/"
