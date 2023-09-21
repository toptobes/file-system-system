module FSS.Frontend.Parser
( buildAST,
) where

import FSS.Common
import FSS.Frontend.AST
import FSS.Frontend.DirExplorer
import Data.Char (isDigit)
import qualified Data.Text as T

type FSTErr = (Text, Text)

newtype FSTParser a = FSTParser { runFSTParser :: DirTree -> Either FSTErr a }
  deriving (Functor)

instance Applicative FSTParser where
  pure :: a -> FSTParser a
  pure = FSTParser . const . Right

  (<*>) :: FSTParser (a -> b) -> FSTParser a -> FSTParser b
  (FSTParser f) <*> (FSTParser a) = FSTParser \dir -> do
    f' <- f dir
    a' <- a dir
    pure $ f' a'

instance Monad FSTParser where
  (>>=) :: FSTParser a -> (a -> FSTParser b) -> FSTParser b
  (FSTParser p) >>= f = FSTParser \dir -> p dir >>= \a -> runFSTParser (f a) dir

instance Alternative FSTParser where
  empty :: FSTParser a
  empty = fstErr "[EMPTY]"

  (<|>) :: FSTParser a -> FSTParser a -> FSTParser a
  (FSTParser p1) <|> (FSTParser p2) = FSTParser \dir -> go (p1 dir) (p2 dir) where
    go (Left e1) (Left e2) = Left (rootier e1 e2)
    go (Left _)  right     = right
    go right     _         = right

(<!>) :: FSTParser a -> a -> FSTParser a
p <!> a = p <|> pure a

buildAST :: DirTree -> [Either FSTErr Module]
buildAST (File _) = error "Parsing.Parser.buildAST: Called w/ file instead of directory"
buildAST (Dir _ dirs) = map (\dir -> runFSTParser (moduleP dir) dir) dirs

moduleP :: DirTree -> FSTParser Module
moduleP (File _) = fstErr "modules must be folders!"
moduleP (Dir fname _) = do
  body <- bodyP
  imports <- importP <!> []
  pure $ Module (takeFileName fname) imports body

importP :: FSTParser [Text]
importP = withDir "import" getFileNames

bodyP :: FSTParser Body
bodyP = withDir "body" $ Body <$> withAllDirs (try2 exprP stmtP "Non statement/expression in body")

exprP :: FSTParser Expr
exprP = callP <|> funcP <|> symOrLitP <|> builderP

stmtP :: FSTParser Stmt
stmtP = cfExitP <|> varDefP <|> switchP <|> whileP

callP :: FSTParser Expr
callP = withDir "call" do
  chain <- chainP
  args <- argsP <!> []
  name <- nameP
  pure $ Call name args chain
    where argsP = withDir "args" (withAllDirs exprP)

builderP :: FSTParser Expr
builderP = go "{}" <|> go "[]" <|> go "()" where
  go cons = withDir cons do
    chain <- chainP
    elems <- withAllDirs exprP
    pure $ Call cons elems chain

chainP :: FSTParser (Maybe Expr)
chainP = (withDir "chain" (callP <|> symOrLitP) <&> Just) <!> Nothing

funcP :: FSTParser Expr
funcP = withDir "function"  do
  name <- nameP <!> T.empty
  body <- bodyP
  args <- argsP <!> []
  mods <- modsP
  pure $ Func mods name args body
    where
      argsP = withDir "params" getFileNames
      modsP = withDir "flags" (getFileNames <&> \flags -> FuncFlags
        { isGeneratorFn = "generator" `elem` flags
        , isAsyncFn     = "async"     `elem` flags
        , isExportFn    = "export"    `elem` flags
        }) <!> FuncFlags False False False

cfExitP :: FSTParser Stmt
cfExitP = go "await" <|> go "yield" <|> go "return" <|> go "throw" where
  go cfType = withDir cfType (CFStmt cfType <$> exprP)

switchP :: FSTParser Stmt
switchP = withDir "switch" do
  match <- withDir "on" exprP <!> SymOrLit "true"
  cases <- withDirs "case" casesP
  dfalt <- withDir "default" bodyP <!> Body []
  pure $ Switch match cases dfalt
    where
      casesP = do
        cond <- condP
        body <- bodyP
        pure (cond, body)
      condP = withDir "cond" exprP

varDefP :: FSTParser Stmt
varDefP = go "let" <|> go "const" where
  go defType = withDir defType do
    name <- nameP
    expr <- withDir "=" exprP <!> SymOrLit "undefined"
    mods <- withDir "flags" (VarFlags . ("export" `elem`) <$> getFileNames) <!> VarFlags False
    pure $ VarDef mods defType name expr

whileP :: FSTParser Stmt
whileP = withDir "while" do
  cond <- withDir "cond" exprP
  While cond <$> bodyP

nameP :: FSTParser Text
nameP = withDir "name" getOnlyFilesName

symOrLitP :: FSTParser Expr
symOrLitP = SymOrLit <$> getOnlyFilesName

withDirs :: Text -> FSTParser b -> FSTParser [b]
withDirs reqname (FSTParser fn) = FSTParser \dir -> do
  let matchingDirs = [d | d@(Dir dirname _) <- subdirs dir, normalizeFname dirname == reqname]
  mapM fn matchingDirs

withDir :: Text -> FSTParser a -> FSTParser a
withDir reqname (FSTParser fn) = FSTParser \dir -> do
  let matchingDirs = [d | d@(Dir dirname _) <- subdirs dir, normalizeFname dirname == reqname]

  case nonEmpty matchingDirs of
    Nothing   -> Left (fname dir, "must have single dir called " <> reqname)
    (Just ne) -> fn $ head ne

withAllDirs :: FSTParser a -> FSTParser [a]
withAllDirs (FSTParser fn) = FSTParser $ mapM (\dir -> fn (Dir (takeDirectory $ fname dir) [dir])) . subdirs

getFileNames :: FSTParser [Text]
getFileNames = FSTParser \dir -> do
  let fnames = [normalizeFname name | (File name) <- subdirs dir]

  if length (subdirs dir) /= length fnames
  then Left (fname dir, "directory requires only files")
  else pure fnames

getOnlyFilesName :: FSTParser Text
getOnlyFilesName = getFileNames <&> nonEmpty >>= \case
  Nothing   -> fstErr $ "must have single dir called " <> "directory requires a single file"
  (Just ne) -> pure $ head ne

normalizeFname :: Text -> Text
normalizeFname = stripNumPrefix . takeFileName

stripNumPrefix :: Text -> Text
stripNumPrefix str
  | "@" `T.isPrefixOf` dropNums str = T.dropWhile (=='@') $ dropNums str
  | otherwise = str
  where dropNums = T.dropWhile isDigit

try2 :: FSTParser a -> FSTParser b -> Text -> FSTParser (Either a b)
try2 p1 p2 err = FSTParser \dir ->
  case runFSTParser ((Left <$> p1) <|> (Right <$> p2)) dir of
    l@(Left err') -> if "[OR]" `T.isInfixOf` snd err' then Left (fname dir, err) else l
    right -> right

rootier :: FSTErr -> FSTErr -> FSTErr
rootier e1 e2
  | c1 == c2  = (fst e1, snd e1 <> " [OR] " <> snd e2)
  | c1 >= c2  = e1
  | otherwise = e2
  where c1 = numParents e1; c2 = numParents e2

numParents :: FSTErr -> Int
numParents (fp, _) = T.count "/" fp

fstErr :: Text -> FSTParser a
fstErr err = FSTParser \dir -> Left (fname dir, err)
