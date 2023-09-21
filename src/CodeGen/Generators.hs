module CodeGen.Generators where

import CodeGen.Minified
import CodeGen.Pretty (genPrettyJS)
import Parsing.AST (Module)

genJS :: Text -> Module -> (Text, Text)
genJS "minify" = genMinifiedJS
genJS "pretty" = genPrettyJS
genJS _ = error "CodeGen.Generators.genJS: Invalid format type"
