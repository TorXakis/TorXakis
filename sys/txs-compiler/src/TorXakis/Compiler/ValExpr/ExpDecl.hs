{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

-- | Generate a map from locations of variable references, the declarations of
-- those variables.
generateVarDecls :: [FuncDecl] -> CompilerM (Map (Loc ExpDeclE) VarDecl)
generateVarDecls fs = Map.fromList . concat <$>
    traverse generateVarDeclsForFD fs

generateVarDeclsForFD :: FuncDecl -> CompilerM [(Loc ExpDeclE, VarDecl)]
generateVarDeclsForFD f =
    let VarExp n _ = funcBody f
    in do
        fd <- lookupM (toText n) (fdMap (funcParams  f))
              "variable declaration for "
        return [(getLoc (funcBody f), fd)]

fdMap :: [VarDecl] -> Map Text VarDecl
fdMap fs =
    Map.fromList $ zip (varName <$> fs) fs
