{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Parsing.AST where

data Module = Module Text [Text] Body
  deriving Show

{- This could be a Stmt -}
data Body = Body [Either Expr Stmt]
  deriving Show

data Expr where
  Call :: Text -> [Expr] -> Maybe Expr -> Expr
  Func :: FuncFlags -> Text -> [Text] -> Body -> Expr
  SymOrLit :: Text -> Expr
    deriving Show

data Stmt where
  CFStmt :: Text -> Expr -> Stmt 
  Switch :: Expr -> [(Expr, Body)] -> Body -> Stmt
  VarDef :: VarFlags -> Text -> Text -> Expr -> Stmt
  While  :: Expr -> Body -> Stmt
    deriving Show

data FuncFlags = FuncFlags
  { isExportFn    :: Bool
  , isAsyncFn     :: Bool
  , isGeneratorFn :: Bool
  } deriving (Show)

data VarFlags = VarFlags
  { isExportVar   :: Bool
  } deriving (Show)
