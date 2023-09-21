{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

module FSS.Backend.Minified
( genMinifiedJS,
) where

import FSS.Common
import FSS.Frontend.AST
import Data.Char (isLetter)
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T

type FST2JS a = a -> Text

genMinifiedJS :: Module -> (Text, Text)
genMinifiedJS = moduleG

moduleG :: Module -> (Text, Text)
moduleG (Module name imports body) = (takeFileName name, importG imports <> noBracketsBodyG body)

importG :: FST2JS [Text]
importG imports = T.intercalate ";" (mkImport <$> imports)
  where mkImport path = "import\"" <> T.replace "\\" "/" path <> "\""

noBracketsBodyG :: Body -> Text
noBracketsBodyG (Body content) = T.intercalate ";" (gen content)
  where gen = map $ either exprG stmtG

bodyG :: Body -> Text
bodyG body = "{" <> noBracketsBodyG body <> "}"

exprG :: FST2JS Expr
exprG (Call fn args chain)
  | isBuilder  = fold [T.init fn, T.intercalate "," (exprG <$> args), T.tail fn, chained]
  | isOperator = fold [exprG (args Unsafe.!! 0), fn, exprG (args Unsafe.!! 1)]
  | otherwise  = fold [fn, "(", T.intercalate "," (exprG <$> args), ")", chained]
    where
      isBuilder  = fn == "()" || fn == "[]" || fn == "{}"
      isOperator = let c = T.index fn 0 in not (isLetter c || c == '$' || c == '_')
      chained = case chain of
        Just c  -> "." <> exprG c
        Nothing -> ""

exprG (Func flags name params body) = fold [async, export, "function", gen, name, "(", T.intercalate "," params, ")", bodyG body]
  where
    gen    = if isGeneratorFn flags then "*"       else " "
    async  = if isAsyncFn     flags then "async "  else T.empty
    export = if isExportFn    flags then "export " else T.empty

exprG (SymOrLit value) = value

stmtG :: FST2JS Stmt
stmtG (Switch match cases dflt) = fold ["switch(", exprG match, "){", T.concat (caseG <$> cases), dfltG dflt, "}"]
  where
    caseG (cond, body) = fold ["case ", exprG cond, ":", bodyG body]
    dfltG body = "default:" <> bodyG body

stmtG (While cond body) = fold ["while(", exprG cond, ")", bodyG body]

stmtG (VarDef flags defType name expr) = fold [export, defType, " ", name, "=", exprG expr]
  where export = if isExportVar flags then "export " else T.empty

stmtG (CFStmt cfType expr) = cfType <> " " <> exprG expr
