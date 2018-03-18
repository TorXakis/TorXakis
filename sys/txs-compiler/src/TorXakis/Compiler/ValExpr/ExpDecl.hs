{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map

import TorXakis.Parser.Data
import TorXakis.Compiler.Data

generateVarDecls :: [FuncDecl] -> CompilerM (Map (Loc Exp) FieldDecl)
generateVarDecls fs = Map.fromList . concat <$> 
    traverse generateVarDeclsForFD fs

generateVarDeclsForFD :: FuncDecl -> CompilerM [(Loc Exp, FieldDecl)]
generateVarDeclsForFD f =
    let VarExp n m = funcBody . child $ f
    in do
        fd <- lookupM (toText n) (fdMap (funcParams . child $ f))
              "variable declaration for "
        return [(loc m, fd)]

fdMap :: [FieldDecl] -> Map Text FieldDecl
fdMap fs =
    Map.fromList $ zip (nodeNameT <$> fs) fs
