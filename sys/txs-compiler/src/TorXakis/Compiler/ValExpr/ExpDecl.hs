{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

-- | Generate a map from locations of variable references, the declarations of
-- those variables.
generateVarDecls :: [FuncDecl] -> CompilerM (Map (Loc VarRefE) VarDecl)
generateVarDecls fs = Map.fromList . concat <$>
    traverse generateVarDeclsForFD fs

generateVarDeclsForFD :: FuncDecl -> CompilerM [(Loc VarRefE, VarDecl)]
generateVarDeclsForFD f = traverse getVarDecl $ expVars (funcBody f)
    where
      funcParmsMap = fdMap (funcParams  f)
      fdMap :: [VarDecl] -> Map Text VarDecl
      fdMap fs =
          Map.fromList $ zip (varName <$> fs) fs
      getVarDecl :: (Name VarRefE, Loc VarRefE) -> CompilerM (Loc VarRefE, VarDecl)
      getVarDecl (n, l) = do
        fd <- lookupM (toText n) funcParmsMap
              "variable declaration for "
        return (l, fd)


