{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.FuncDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' function definitions.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.FuncDef
    ( FuncDefInfo
    , funcHandler
    , funcDeclToSH
    , funcDeclsToFuncDefs
    , innerSigHandlerMap
    , funcDef
    )
where

import           Control.Arrow                     (left, (&&&))
import           Control.Monad.Except              (liftEither)
import           Data.Either                       (partitionEithers)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           GHC.Exts                          (fromList)
import           Prelude                           hiding (lookup)

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId, funcargs, funcsort)
import           FuncTable                         (Handler,
                                                    Signature (Signature))
import           ValExpr                           (cstrFunc)
import           VarId                             (VarId)

import           TorXakis.Compiler.Data            (CompilerM)
import           TorXakis.Compiler.Error           (Error (Errors))
import           TorXakis.Compiler.Maps            (join, (.@), (.@@))
import           TorXakis.Compiler.Maps.VarRef     (varDefsFromExp)
import           TorXakis.Compiler.MapsTo          ((:&) ((:&)), Contents, In,
                                                    MapsTo, innerMap, lookup)
import           TorXakis.Compiler.ValExpr.ValExpr (expDeclToValExpr)
import           TorXakis.Parser.Data              (FuncDecl, FuncDeclE, Loc,
                                                    VarDeclE, VarRefE, funcBody,
                                                    funcParams, getLoc)

-- | Make a function @Signature@-@Handler@ pair from a function @FuncDecl@.
funcDeclToSH :: Map (Loc FuncDeclE) FuncId
             -> FuncDecl
             -> CompilerM (Loc FuncDeclE, (Signature, Handler VarId))
funcDeclToSH fids f = do
    fid  <- fids .@@ getLoc f
    let sig     = Signature (funcargs fid) (funcsort fid)
        handler = cstrFunc (Map.empty :: Map FuncId (FuncDef VarId)) fid
    return (getLoc f, (sig, handler))

-- | Compile function declarations to function definitions.
funcDeclsToFuncDefs :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , In (Loc FuncDeclE, (Signature, Handler VarId)) (Contents mm) ~ 'False )
                    => mm
                    -> Map (Loc FuncDeclE) (Signature, Handler VarId) -- ^ Standard functions, ADT functions, and user defined functions.
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId FuncDefInfo)
funcDeclsToFuncDefs mm fSHs fs = liftEither $ do
    fVDs <- varDefsFromExp (fSHs :& mm) fs
    gFuncDeclsToFuncDefs mempty fVDs fs
    where
      -- | Generalized version of `funcDeclsToFuncDefs`, which accumulates the
      -- results in the first parameter.
      gFuncDeclsToFuncDefs :: Map FuncId FuncDefInfo
                           -> Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                           -> [FuncDecl]
                           -> Either Error (Map FuncId FuncDefInfo)
      gFuncDeclsToFuncDefs mFDef fVDs gs =
          case partitionEithers (funcDeclToFuncDef (mFDef :& mm) fSHs fVDs <$> gs) of
              ([], rs) -> Right $ fromList rs <> mFDef
              (ls, []) -> Left $ Errors (fst <$> ls)
              (ls, rs) -> gFuncDeclsToFuncDefs (fromList rs <> mFDef) fVDs (snd <$> ls)

-- | Create a function definition (@FuncDef@), signature (@Signature@), and
-- handler (@Handler@) for the given function declaration.
--
funcDeclToFuncDef :: ( MapsTo (Loc VarDeclE) VarId mm
                     , MapsTo (Loc FuncDeclE) FuncId mm
                     )
                  => mm
                  -> Map (Loc FuncDeclE) (Signature, Handler VarId)
                  -> Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                  -> FuncDecl
                  -> Either (Error, FuncDecl) (FuncId, FuncDefInfo)
funcDeclToFuncDef mm fSHs fVDs f = left (,f) $ do
    fid  <- mm .@ getLoc f
    (sig, handler') <- fSHs .@ getLoc f :: Either Error (Signature, Handler VarId)
    pIds <- traverse ((`lookup` mm) . getLoc) (funcParams f)
    vExp <- expDeclToValExpr fVDs (funcsort fid) (funcBody f)
    let fdef = FuncDef pIds vExp
        -- We recalculate the handler to simplify constant expressions.
        fhandler =
            case funcargs fid of
                [] -> const vExp
                _  -> handler'
    return (fid, FuncDefInfo fdef sig fhandler)

-- | Return the inner signal handler map of the composite map.
innerSigHandlerMap :: ( MapsTo (Loc FuncDeclE) FuncId mm
                     ,  MapsTo FuncId FuncDefInfo mm )
                   => mm -> Map (Loc FuncDeclE) (Signature, Handler VarId)
innerSigHandlerMap mm = join fdeclsMap sigHdlrMap
    where
      fdisMap :: Map FuncId FuncDefInfo
      fdisMap = innerMap mm
      sigHdlrMap :: Map FuncId (Signature, Handler VarId)
      sigHdlrMap = (funcSig &&& funcHandler) <$> fdisMap
      fdeclsMap :: Map (Loc FuncDeclE) FuncId
      fdeclsMap = innerMap mm

-- | Information about a function definition.
data FuncDefInfo = FuncDefInfo
    { funcDef     :: FuncDef VarId
    , funcSig     :: Signature
    , funcHandler :: Handler VarId
    }
