{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
module TorXakis.Compiler.ValExpr.FuncDef where

import           Control.Arrow                     (left, (&&&))
import           Control.Lens                      ((^..))
import           Control.Lens.Plated               (cosmosOn)
import           Data.Data.Lens                    (biplate, template)
import           Data.Either                       (partitionEithers)
import           Data.List                         (find)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Monoid                       (mempty, (<>))
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Exts                          (fromList)
import           Prelude                           hiding (lookup)

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId, funcargs, funcsort)
import           FuncTable                         (Handler,
                                                    Signature (Signature))
import           SortId                            (SortId)
import           ValExpr                           (cstrConst, cstrFunc,
                                                    cstrVar)
import           VarId                             (VarId)

import           Control.Monad.Error.Class         (liftEither)
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.VarRef
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

-- | Make a function @Signature@-@Handler@ pair from a function @FuncDecl@.
funcDeclToSH :: Map (Loc FuncDeclE) FuncId
             -> FuncDecl
             -> CompilerM (Loc FuncDeclE, (Signature, Handler VarId))
funcDeclToSH fids f = do
    fid  <- fids .@@ getLoc f
    let sig     = Signature (funcargs fid) (funcsort fid)
        handler = cstrFunc (Map.empty :: Map FuncId (FuncDef VarId)) fid
    return (getLoc f, (sig, handler))

-- | Version that replaces the constraint:
--
-- > MapsTo (Loc FuncDeclE) FuncId mm
--
-- By the constraint:
--
-- > MapsTo FuncId (Signature, Handler VarId)
--
funcDeclsToFuncDefs :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , In (FuncId, FuncDefInfo) (Contents mm) ~ 'False
                       , In (Loc FuncDeclE, (Signature, Handler VarId)) (Contents mm) ~ 'False )
                    => mm
                    -> Map (Loc FuncDeclE) (Signature, Handler VarId) -- ^ Standard functions, ADT functions, and user defined functions.
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId FuncDefInfo)
funcDeclsToFuncDefs mm fSHs fs = liftEither $ do
    -- TODO: this map should be calculated globally.
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
                      , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                      , MapsTo (Loc FuncDeclE) FuncId mm
                      , MapsTo FuncId FuncDefInfo mm
                      , In (Loc FuncDeclE, (Signature, Handler VarId)) (Contents mm) ~ 'False )
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

data FuncDefInfo = FuncDefInfo
    { funcDef     :: FuncDef VarId
    , funcSig     :: Signature
    , funcHandler :: Handler VarId
    }
