{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Control.Arrow             ((+++))
import           Control.Monad.Error.Class (catchError)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

-- | Generate a map from locations of variable references, the declarations of
-- those variables.
generateVarDecls :: [FuncDecl]
                 -> CompilerM (Map (Loc VarRefE) (Either (Loc VarDeclE) (Loc FuncDeclE)))
generateVarDecls fs = Map.fromList . concat <$>
    traverse (generateVarDeclsForFD fdMap) fs
    where
      fdMap :: Map Text (Loc FuncDeclE)
      fdMap = Map.fromList $ zip (funcName <$> fs) (getLoc <$> fs)

generateVarDeclsForFD :: Map Text (Loc FuncDeclE) -- ^ Existing function declarations.
                      -> FuncDecl
                      -> CompilerM [(Loc VarRefE, Either (Loc VarDeclE) (Loc FuncDeclE))]
generateVarDeclsForFD fdMap f = varDeclsFromExpDecl (mkVdMap (funcParams f)) (funcBody f)
    where
      mkVdMap :: (IsVariable v, HasLoc v VarDeclE)
              => [v] -> Map Text (Loc VarDeclE)
      mkVdMap vs =
          Map.fromList $ zip (varName <$> vs) (getLoc <$> vs)
      varDeclsFromExpDecl :: Map Text (Loc VarDeclE)
                          -> ExpDecl
                          -> CompilerM [(Loc VarRefE, Either (Loc VarDeclE) (Loc FuncDeclE))]
      varDeclsFromExpDecl vdMap ex = case expChild ex of
          VarRef n rLoc -> do
              dLoc <- fmap Left (lookupM (toText n) vdMap "variable declaration for ")
                  `catchError`
                  const (fmap Right (lookupM (toText n) fdMap "function declaration for "))
              return [(rLoc, dLoc)]
          ConstLit _ -> return []
          LetExp vs subEx -> do
              vdVs <- concat <$> traverse (varDeclsFromExpDecl vdMap) (varDeclExp <$> vs)
              -- If there are variables in the LET expression that shadows a
              -- more global variable, then we overwrite this global occurrence
              -- with the one at the LET expression.
              let vdMap' = Map.unionWith (flip const) vdMap (mkVdMap vs)
              vdSubEx <- varDeclsFromExpDecl vdMap' subEx
              return $ vdSubEx ++ vdVs
                  
          If ex0 ex1 ex2 ->
              concat <$> traverse (varDeclsFromExpDecl vdMap) [ex0, ex1, ex2]
