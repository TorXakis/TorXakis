{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map

import TorXakis.Parser.Data
import TorXakis.Compiler.Data

generateVarDecls :: [FuncDecl] -> CompilerM (Map (Loc ExpE) VarDecl)
generateVarDecls fs = Map.fromList . concat <$> 
    traverse generateVarDeclsForFD fs

generateVarDeclsForFD :: FuncDecl -> CompilerM [(Loc ExpE, VarDecl)]
generateVarDeclsForFD f =
    let VarExp n _ = funcBody f
    in do
        fd <- lookupM (toText n) (fdMap (funcParams  f))
              "variable declaration for "
        return [(getLoc (funcBody f), fd)]

fdMap :: [VarDecl] -> Map Text VarDecl
fdMap fs =
    Map.fromList $ zip (varName <$> fs) fs
