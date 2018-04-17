{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies     #-}
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
import           Data.List                 (find)

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId, funcsort)
import           SortId                            (SortId)
import           ValExpr                           (cstrVar)
import           VarId                             (VarId)

import           Control.Monad.Error.Class         (liftEither)
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

funcDeclsToFuncDefs :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                       , MapsTo FuncDefInfo FuncId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , In (FuncId, FuncDef VarId) (Contents mm) ~ 'False )
                    => mm
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId (FuncDef VarId))
funcDeclsToFuncDefs mm fs = liftEither $ gFuncDeclsToFuncDefs mempty fs
    where
      gFuncDeclsToFuncDefs :: Map FuncId (FuncDef VarId)
                           -> [FuncDecl]
                           -> Either Error (Map FuncId (FuncDef VarId))
      gFuncDeclsToFuncDefs mFDef gs =
          case partitionEithers (funcDeclToFuncDef (mFDef :& mm) <$> gs) of
              ([], rs) -> Right $ fromList rs <> mFDef
              (ls, []) -> Left $ Errors (fst <$> ls)
              (ls, rs) -> gFuncDeclsToFuncDefs (fromList rs <> mFDef) (snd <$> ls)

-- | Create a function definition for the given function declaration.
--
-- TODO: we pass two environments, since e cannot be extended. We should try
-- to solve this by implementing this:
--
-- https://stackoverflow.com/a/49546517/2289983
--
funcDeclToFuncDef :: ( MapsTo (Loc VarDeclE) VarId mm
                     , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                     , MapsTo FuncDefInfo FuncId mm
                     , MapsTo FuncId (FuncDef VarId) mm )
                  => mm
                  -> FuncDecl
                  -> Either (Error, FuncDecl) (FuncId, FuncDef VarId)
funcDeclToFuncDef mm f = left (,f) $ do
    fId  <- findFuncIdForDecl mm (getLoc f)
    pIds <- traverse ((`lookup` mm) . getLoc) (funcParams f)
    vExp <- expDeclToValExpr mm (funcsort fId) (funcBody f)
    return (fId, FuncDef pIds vExp)
