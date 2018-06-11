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

-- | Make a function @Signature@-@Handler@ pair from a function @FuncDecl@.
funcDeclToSH :: Map (Loc FuncDeclE) FuncId
             -> FuncDecl
             -> CompilerM (Loc FuncDeclE, (Signature, Handler VarId))
funcDeclToSH fids f = do
    fid  <- fids .@ getLoc f
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
funcDeclsToFuncDefs2 :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , In (FuncId, FuncDefInfo) (Contents mm) ~ 'False
                       , In (Loc FuncDeclE, (Signature, Handler VarId)) (Contents mm) ~ 'False )
                    => mm
                    -> Map (Loc FuncDeclE) (Signature, Handler VarId) -- ^ Standard functions, ADT functions, and user defined functions.
                    -> [FuncDecl]
                    -> CompilerM (Map FuncId FuncDefInfo)
funcDeclsToFuncDefs2 mm fSHs fs = liftEither $ gFuncDeclsToFuncDefs mempty fs
    where
      gFuncDeclsToFuncDefs :: Map FuncId FuncDefInfo
                           -> [FuncDecl]
                           -> Either Error (Map FuncId FuncDefInfo)
      gFuncDeclsToFuncDefs mFDef gs =
          case partitionEithers (funcDeclToFuncDef2 (mFDef :& mm) fSHs <$> gs) of
              ([], rs) -> Right $ fromList rs <> mFDef
              (ls, []) -> Left $ Errors (fst <$> ls)
              (ls, rs) -> gFuncDeclsToFuncDefs (fromList rs <> mFDef) (snd <$> ls)

-- | Create a function definition for the given function declaration.
--
-- TODO: mutually recursive functions are not supported :/ In principle there's
-- no reason why we have to put this restriction, since:
--
-- We can make signatures for all the functions! Only the funcdefs need the value expressions!
--
-- fsig = Signature (funcargs fid) (funcsort fid)
--        handler' = cstrFunc (Map.empty :: Map FuncId (FuncDef VarId)) fid
funcDeclToFuncDef2 :: ( MapsTo (Loc VarDeclE) VarId mm
                      , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                      , MapsTo (Loc FuncDeclE) FuncId mm
                      , MapsTo FuncId FuncDefInfo mm
                      , In (Loc FuncDeclE, (Signature, Handler VarId)) (Contents mm) ~ 'False )
                   => mm
                   -> Map (Loc FuncDeclE) (Signature, Handler VarId)
                   -> FuncDecl
                   -> Either (Error, FuncDecl) (FuncId, FuncDefInfo)
funcDeclToFuncDef2 mm fSHs f = left (,f) $ do
    fid  <- mm .@@ getLoc f
    (sig, handler') <- fSHs .@@ getLoc f :: Either Error (Signature, Handler VarId)
    pIds <- traverse ((`lookup` mm) . getLoc) (funcParams f)
    let
        -- fsig = Signature (funcargs fid) (funcsort fid)
        -- handler' = cstrFunc (Map.empty :: Map FuncId (FuncDef VarId)) fid
        -- In case of a recursive function, we need to make the information of
        -- the current function available to @expDeclToValExpr@.
        -- locToSigHdlrMap =
        --     Map.insert (getLoc f) (fsig, handler') (innerSigHandlerMap mm)
        mm' =  fSHs :& mm
        -- TODO: make sure these maps are passed as parameter.
        varDecls :: Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
        varDecls = innerMap mm'
        varIds :: Map (Loc VarDeclE) VarId
        varIds = innerMap mm'
        funcSHs :: Map (Loc FuncDeclE) (Signature, Handler VarId)
        funcSHs = fSHs
    -- varDefs <- varRefsToVarDefs varDecls varIds funcSHs
    vExp <- expDeclToValExpr mm' (funcsort fid) (funcBody f)
    -- vExp <- expDeclToValExpr_2 (locToSigHdlrMap <.+> mm) (funcsort fid) (funcBody f)
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
