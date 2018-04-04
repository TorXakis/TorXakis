{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Control.Arrow             ((+++))
import           Control.Monad.Error.Class (catchError)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

-- | Generate a map from the locations of variable references to the declarations of
-- those variables.
generateVarDecls :: [PredefName] -- Predefined functions
                 -> [FuncDecl]
                 -> CompilerM (Map (Loc VarRefE) (Loc VarDeclE :| Loc FuncDeclE :| PredefName))
generateVarDecls ps fs = Map.fromList . concat <$>
    traverse (generateVarDeclsForFD fdMap) fs
    where
      -- | Map of function names to the location where they are defined. The
      -- function defined by the user have precedence over the predefined functions.
      fdMap :: Map Text (Loc FuncDeclE :| PredefName)
      fdMap =
          -- Note the union is left biased, so functions defined by the user
          -- will have precedence over predefined functions.
          Map.fromList (zip (funcName <$> fs) (Left . getLoc <$> fs))
          `Map.union`
          Map.fromList (zip (predefName <$> ps) (Right <$> ps))

-- | Map a variable reference to a variable declaration, for a function declaration.
--
-- TODO: property to check:
--
-- the number of 'Loc FuncDeclE' entities in the function declaration should equal the
-- length of the list returned by this function.
--
-- This ensures that the mapping returned is complete.
generateVarDeclsForFD :: Map Text (Loc FuncDeclE :| PredefName) -- ^ Existing function declarations.
                      -> FuncDecl
                      -> CompilerM [(Loc VarRefE, Loc VarDeclE :| Loc FuncDeclE :| PredefName)]
generateVarDeclsForFD fdMap f = varDeclsFromExpDecl (mkVdMap (funcParams f)) (funcBody f)
    where
      mkVdMap :: (IsVariable v, HasLoc v VarDeclE)
              => [v] -> Map Text (Loc VarDeclE)
      mkVdMap vs =
          Map.fromList $ zip (varName <$> vs) (getLoc <$> vs)
      varDeclsFromExpDecl :: Map Text (Loc VarDeclE)
                          -> ExpDecl
                          -> CompilerM [(Loc VarRefE, Loc VarDeclE :| Loc FuncDeclE :| PredefName)]
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
          Fappl _ _ exs ->
              -- TODO: factor out the duplication w.r.t. `If`
              concat <$> traverse (varDeclsFromExpDecl vdMap) exs
