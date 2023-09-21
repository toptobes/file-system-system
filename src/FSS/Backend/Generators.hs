module FSS.Backend.Generators where

import FSS.Backend.Minified
import FSS.Backend.Pretty (genPrettyJS)
import FSS.Frontend.AST (Module)

genJS :: Text -> Module -> (Text, Text)
genJS "minify" = genMinifiedJS
genJS "pretty" = genPrettyJS
genJS _ = error "CodeGen.Generators.genJS: Invalid format type"
