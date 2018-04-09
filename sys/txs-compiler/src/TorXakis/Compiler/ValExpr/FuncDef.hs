{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module TorXakis.Compiler.ValExpr.FuncDef where

import           Control.Arrow                     (left)
import           Data.Either                       (partitionEithers)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Monoid                       (mempty, (<>))
import qualified Data.Text                         as T
import           GHC.Exts                          (fromList)

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId, funcsort)
import           ValExpr                           (cstrVar)
import           VarId                             (VarId)

import           Control.Monad.Error.Class         (liftEither)
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

funcDeclsToFuncDefs :: (HasSortIds e, HasVarDecls e, HasVarIds e, HasFuncIds e)
                    => e
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId (FuncDef VarId))
funcDeclsToFuncDefs e fs = liftEither $ gFuncDeclsToFuncDefs mempty fs
    where
      gFuncDeclsToFuncDefs :: SEnv (Map FuncId (FuncDef VarId))
                           -> [FuncDecl]
                           -> Either Error (Map FuncId (FuncDef VarId))
      gFuncDeclsToFuncDefs e' gs =
          case partitionEithers (funcDeclToFuncDef e e' <$> gs) of
              ([], rs) -> Right $ fromSEnv $ fromList rs <> e'
              (ls, []) -> Left $ Errors (fst <$> ls)
              (ls, rs) -> gFuncDeclsToFuncDefs (fromList rs <> e') (snd <$> ls)

-- | Create a function definition for the given function declaration.
--
-- TODO: we pass two environments, since e cannot be extended. We should try
-- to solve this by implementing this:
--
-- https://stackoverflow.com/a/49546517/2289983
--
funcDeclToFuncDef :: (HasSortIds e, HasVarDecls e, HasVarIds e, HasFuncIds e, HasFuncDefs e')
                  => e
                  -> e'
                  -> FuncDecl
                  -> Either (Error, FuncDecl) (FuncId, FuncDef VarId)
funcDeclToFuncDef e e' f = left (,f) $ do
    fId  <- findFuncIdForDecl e (getLoc f)
    pIds <- traverse (findVarId e . getLoc) (funcParams f)
    vExp <- expDeclToValExpr e e' (funcsort fId) (funcBody f)
    return (fId, FuncDef pIds vExp)

