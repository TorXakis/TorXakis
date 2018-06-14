{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler.ValExpr.ValExpr where

import           Control.Arrow                       (second, (+++))
import           Data.Either                         (partitionEithers)
import           Data.Foldable                       (traverse_)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Semigroup                      ((<>))
import qualified Data.Set                            as Set
import qualified Data.Text                           as T

import           FuncDef                             (FuncDef)
import           FuncId                              (FuncId, funcargs,
                                                      funcsort)
import           FuncTable                           (Handler, Signature,
                                                      sortArgs, sortRet)
import           SortId                              (SortId, sortIdBool)
import           SortOf                              (sortOf)
import           ValExpr                             (ValExpr, cstrAnd,
                                                      cstrConst, cstrFunc,
                                                      cstrITE, cstrVar, subst)
import           VarId                               (VarId, varsort)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.VarRef
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Parser.Data

expDeclToValExpr :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)]) -- ^ Variable definitions associated to a given reference.
                  -> SortId -- ^ Expected SortId for the expression.
                  -> ExpDecl
                  -> Either Error (ValExpr VarId)
expDeclToValExpr vdefs eSid ex = case expChild ex of
    VarRef _ l -> do
        vdef <- vdefs .@@ (l :: Loc VarRefE)
        case vdef of
            Left vId -> do
                checkSortIds (varsort vId) eSid
                return $ cstrVar vId
            Right shs -> do
                (sig, h)  <- determineSH shs [] (Just eSid)
                checkSortIds (sortRet sig) eSid
                return $ h []
    ConstLit c -> do
        traverse_ (checkSortIds eSid) (sortIdConst c)
        return $ cstrConst (constToConstDef eSid c)
    LetExp vss subEx -> do
        let
            letValDeclsToMaps :: Either Error [Map VarId (ValExpr VarId)]
            letValDeclsToMaps = traverse (parValDeclToMap vdefs) vss
            fsM :: Map.Map FuncId (FuncDef VarId)
            fsM = Map.empty
        subValExpr <- expDeclToValExpr vdefs eSid subEx
        vsM <- letValDeclsToMaps
        return $ foldr (`subst` fsM) subValExpr vsM
    If ex0 ex1 ex2 -> do
        ve0 <- expDeclToValExpr vdefs sortIdBool ex0
        ve1 <- expDeclToValExpr vdefs eSid ex1
        ve2 <- expDeclToValExpr vdefs eSid ex2
        return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
    Fappl _ l exs -> do
        shs <- findFuncDefs vdefs l
        case partitionEithers (tryMkValExpr <$> shs) of
            (ls, []) -> Left Error
                        { _errorType = UndefinedRef
                        , _errorLoc  = getErrorLoc l
                        , _errorMsg   = "Could not resolve function: " <> T.pack (show ls)}
            (_, [vex]) -> Right vex
            (_, vexs)  -> Left Error
                          { _errorType = UnresolvedIdentifier
                          , _errorLoc  = getErrorLoc l
                          , _errorMsg   = "Function not uniquely resolved: " <> T.pack (show vexs)
                          }
        where
          tryMkValExpr :: (Signature, Handler VarId) -> Either Error (ValExpr VarId)
          tryMkValExpr (sig, h) = do
              checkSortIds (sortRet sig) eSid
              if length (sortArgs sig) /= length exs
                  then Left Error
                       { _errorType = UndefinedRef
                       , _errorLoc  = NoErrorLoc
                       , _errorMsg  = "Length of arguments don't match"
                                     <> T.pack (show sig)
                       }
                  else do
                  vexs <- traverse (uncurry $ expDeclToValExpr vdefs) $
                                zip (sortArgs sig) exs
                  return $ h vexs

parValDeclToMap :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                  -> [LetVarDecl]
                  -> Either Error (Map VarId (ValExpr VarId))
parValDeclToMap mm vs = Map.fromList <$>
    traverse (letValDeclToMap mm) vs

letValDeclToMap :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                  -> LetVarDecl
                  -> Either Error (VarId, ValExpr VarId)
letValDeclToMap vrvds vd = do
    vId   <- varIdForRef vrvds (asVarReflLoc (getLoc vd))
    vdExp <- expDeclToValExpr vrvds (varsort vId) (varDeclExp vd)
    return (vId, vdExp)
