{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
module TorXakis.Compiler.ValExpr.FuncDef where

import           Control.Arrow                     (left, (&&&))
import           Data.Either                       (partitionEithers)
import           Data.List                         (find)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Monoid                       (mempty, (<>))
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
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

-- funcDeclsToFuncDefs :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
--                        , MapsTo (Loc FuncDeclE) FuncId mm
--                        , MapsTo (Loc VarDeclE) VarId mm
--                        , In (FuncId, FuncDef VarId) (Contents mm) ~ 'False )
--                     => mm
--                     -> [FuncDecl]
--                     -> CompilerM (Map FuncId (FuncDef VarId))
-- funcDeclsToFuncDefs mm fs = liftEither $ gFuncDeclsToFuncDefs mempty fs
--     where
--       gFuncDeclsToFuncDefs :: Map FuncId (FuncDef VarId)
--                            -> [FuncDecl]
--                            -> Either Error (Map FuncId (FuncDef VarId))
--       gFuncDeclsToFuncDefs mFDef gs =
--           case partitionEithers (funcDeclToFuncDef (mFDef :& mm) <$> gs) of
--               ([], rs) -> Right $ fromList rs <> mFDef
--               (ls, []) -> Left $ Errors (fst <$> ls)
--               (ls, rs) -> gFuncDeclsToFuncDefs (fromList rs <> mFDef) (snd <$> ls)

-- -- | Create a function definition for the given function declaration.
-- --
-- funcDeclToFuncDef :: ( MapsTo (Loc VarDeclE) VarId mm
--                      , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
--                      , MapsTo (Loc FuncDeclE) FuncId mm
--                      , MapsTo FuncId (FuncDef VarId) mm )
--                   => mm
--                   -> FuncDecl
--                   -> Either (Error, FuncDecl) (FuncId, FuncDef VarId)
-- funcDeclToFuncDef mm f = left (,f) $ do
--     fId  <- mm .@@ getLoc f
--     pIds <- traverse ((`lookup` mm) . getLoc) (funcParams f)
--     vExp <- expDeclToValExpr mm (funcsort fId) (funcBody f)
--     return (fId, FuncDef pIds vExp)

-- | Version that replaces the constraint:
--
-- > MapsTo (Loc FuncDeclE) FuncId mm
--
-- By the constraint:
--
-- > MapsTo FuncId (Signature, Handler VarId)
--
funcDeclsToFuncDefs2 :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , In (FuncId, FuncDefInfo) (Contents mm) ~ 'False
                       , In (Loc FuncDeclE, (Signature, Handler VarId)) (Contents mm) ~ 'False )
                    => mm
                    -> Map (Loc FuncDeclE) (Signature, Handler VarId) -- ^ Standard functions, and ADT functions.
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId FuncDefInfo)
funcDeclsToFuncDefs2 mm stdFuncs fs = liftEither $ gFuncDeclsToFuncDefs mempty fs
    where
      mm' = stdFuncs :& mm
      gFuncDeclsToFuncDefs :: Map FuncId FuncDefInfo
                           -> [FuncDecl]
                           -> Either Error (Map FuncId FuncDefInfo)
      gFuncDeclsToFuncDefs mFDef gs =
          case partitionEithers (funcDeclToFuncDef2 (mFDef :& mm') <$> gs) of
              ([], rs) -> Right $ fromList rs <> mFDef
              (ls, []) -> Left $ Errors (fst <$> ls)
              (ls, rs) -> gFuncDeclsToFuncDefs (fromList rs <> mFDef) (snd <$> ls)

-- | Create a function definition for the given function declaration.
--
funcDeclToFuncDef2 :: ( MapsTo (Loc VarDeclE) VarId mm
                      , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                      , MapsTo (Loc FuncDeclE) FuncId mm
                      , MapsTo FuncId FuncDefInfo mm
                      , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm )
                   => mm
                   -> FuncDecl
                   -> Either (Error, FuncDecl) (FuncId, FuncDefInfo)
funcDeclToFuncDef2 mm f = left (,f) $ do
    fId  <- mm .@@ getLoc f
    pIds <- traverse ((`lookup` mm) . getLoc) (funcParams f)
    let
        locToSigHdlrMap = innerSigHandlerMap mm
    vExp <- expDeclToValExpr2 (locToSigHdlrMap <.+> mm) (funcsort fId) (funcBody f)
    let fdef = FuncDef pIds vExp
        fsig = Signature (funcargs fId) (funcsort fId)
        fidFdef :: Map FuncId (FuncDef VarId)
        fidFdef = Map.map funcDef (innerMap mm)
        fhandler = cstrFunc fidFdef fId

    return (fId, FuncDefInfo fdef fsig fhandler)

innerSigHandlerMap :: ( MapsTo (Loc FuncDeclE) FuncId mm
                     ,  MapsTo FuncId FuncDefInfo mm )
                   => mm -> Map (Loc FuncDeclE) (Signature, Handler VarId)
innerSigHandlerMap mm = closure2 fdeclsMap sigHdlrMap
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
