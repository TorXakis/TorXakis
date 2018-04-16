{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module TorXakis.Compiler.ValExpr.FuncDef where

import           Prelude hiding (lookup)
import           Control.Arrow                     (left)
import           Data.Either                       (partitionEithers)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Monoid                       (mempty, (<>))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Exts                          (fromList)

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId, funcsort)
import           SortId                            (SortId)
import           ValExpr                           (cstrVar)
import           VarId                             (VarId)

import           Control.Monad.Error.Class         (liftEither)
import           TorXakis.Compiler.Data            hiding (lookup)
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

funcDeclsToFuncDefs :: ( HasVarDecls e, HasFuncIds e
                       , MapsTo (Loc VarDeclE) VarId mm )
                    => mm
                    -> e
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId (FuncDef VarId))
funcDeclsToFuncDefs mm e fs = liftEither $ gFuncDeclsToFuncDefs mempty fs
    where
      gFuncDeclsToFuncDefs :: SEnv (Map FuncId (FuncDef VarId))
                           -> [FuncDecl]
                           -> Either Error (Map FuncId (FuncDef VarId))
      gFuncDeclsToFuncDefs e' gs =
          case partitionEithers (funcDeclToFuncDef mm e e' <$> gs) of
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
funcDeclToFuncDef :: ( MapsTo (Loc VarDeclE) VarId mm
                     , HasVarDecls e, HasFuncIds e, HasFuncDefs e')
                  => mm
                  -> e
                  -> e'
                  -> FuncDecl
                  -> Either (Error, FuncDecl) (FuncId, FuncDef VarId)
funcDeclToFuncDef mm e e' f = left (,f) $ do
    fId  <- findFuncIdForDecl e (getLoc f)
    pIds <- traverse ((`lookup` mm) . getLoc) (funcParams f)
    vExp <- expDeclToValExpr mm e e' (funcsort fId) (funcBody f)
    return (fId, FuncDef pIds vExp)

