{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Control.Monad.Error.Class (catchError)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

-- | Generate a map from locations of variable references, the declarations of
-- those variables.
generateVarDecls :: [FuncDecl] -> CompilerM (Map (Loc VarRefE) (Either VarDecl FuncDecl))
generateVarDecls fs = Map.fromList . concat <$>
    traverse (generateVarDeclsForFD fdMap) fs
    where
      fdMap :: Map Text FuncDecl
      fdMap = Map.fromList $ zip (funcName <$> fs) fs

generateVarDeclsForFD :: Map Text FuncDecl -- ^ Existing function declarations.
                      -> FuncDecl
                      -> CompilerM [(Loc VarRefE, Either VarDecl FuncDecl)]
generateVarDeclsForFD fdMap f = traverse getVarDecl $ expVars (funcBody f)
    where
      fpMap = vdMap (funcParams  f)
      vdMap :: [VarDecl] -> Map Text VarDecl
      vdMap vs =
          Map.fromList $ zip (varName <$> vs) vs
      getVarDecl :: (Name VarRefE, Loc VarRefE)
                 -> CompilerM (Loc VarRefE, Either VarDecl FuncDecl)
      getVarDecl (n, l) = do
        fd <- fmap Left  (lookupM (toText n) fpMap "variable declaration for ")
              `catchError`
              const (fmap Right (lookupM (toText n) fdMap "function declaration for "))
        return (l, fd)
