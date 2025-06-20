{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.ValExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' value expressions.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.ValExpr
    ( expDeclToValExpr
    , parValDeclToMap
    )
where

import           Data.Either                         (partitionEithers)
import           Data.Foldable                       (traverse_)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import qualified Data.Text                           as T
import           GHC.Exts                            (toList)

import           FuncDef                             (FuncDef)
import           FuncId                              (FuncId)
import           FuncTable                           (Handler, Signature,
                                                      sortArgs, sortRet)
import           SortId                              (SortId, sortIdBool)
import           ValExpr                             (ValExpr, cstrAnd,
                                                      cstrConst, cstrITE,
                                                      cstrVar, subst)
import           VarId                               (VarId, varsort)

import           TorXakis.Compiler.Error             (Entity (Function),
                                                      Error (Error),
                                                      ErrorLoc (NoErrorLoc),
                                                      ErrorType (Ambiguous, Undefined),
                                                      getErrorLoc, _errorLoc,
                                                      _errorMsg, _errorType)
import           TorXakis.Compiler.Maps              (determineSH, findRight,
                                                      (.@))
import           TorXakis.Compiler.Maps.VarRef       (varIdForRef)
import           TorXakis.Compiler.ValExpr.Constant  (constToConstant)
import           TorXakis.Compiler.ValExpr.SortId    (checkSortIds, sortIdConst)
import           TorXakis.Parser.Data                (ExpChild (ConstLit, Fappl, If, LetExp, VarRef),
                                                      ExpDecl, LetVarDecl, Loc,
                                                      ParLetVarDecl, VarRefE,
                                                      asVarReflLoc, expChild,
                                                      getLoc, varDeclExp)

-- | Compile an expression declaration into a value expression, given a map of
-- variable references to the entities they refer, and the expected sort id.
expDeclToValExpr :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)]) -- ^ Variable definitions associated to a given reference.
                  -> SortId                                                       -- ^ Expected SortId for the expression.
                  -> ExpDecl
                  -> Either Error (ValExpr VarId)
expDeclToValExpr vdefs eSid ex = case expChild ex of
    VarRef _ l -> do
        vdef <- vdefs .@ (l :: Loc VarRefE)
        case vdef of
            Left vId -> do
                checkSortIds (varsort vId) eSid
                return $ cstrVar vId
            Right shs -> do
                (sig, h)  <- determineSH shs [] (Just eSid) (getErrorLoc l)
                checkSortIds (sortRet sig) eSid
                return $ h []
    ConstLit c -> do
        traverse_ (checkSortIds eSid) (sortIdConst c)
        return $ cstrConst (constToConstant eSid c)
    LetExp vss subEx -> do
        let
            letValDeclsToMaps :: Either Error [Map VarId (ValExpr VarId)]
            letValDeclsToMaps = traverse (parValDeclToMap vdefs) vss
            fsM :: Map.Map FuncId (FuncDef VarId)
            fsM = Map.empty
        subValExpr <- expDeclToValExpr vdefs eSid subEx
        foldr (`subst` fsM) subValExpr <$> letValDeclsToMaps
    If ex0 ex1 ex2 -> do
        ve0 <- expDeclToValExpr vdefs sortIdBool ex0
        ve1 <- expDeclToValExpr vdefs eSid ex1
        ve2 <- expDeclToValExpr vdefs eSid ex2
        return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
    Fappl _ l exs -> do
        -- The location should correspond to a function (hence we look for the
        -- right value of the @Either@).
        shs <- findRight vdefs l
        case partitionEithers (tryMkValExpr <$> shs) of
            (ls, []) -> Left Error
                        { _errorType = Undefined Function
                        , _errorLoc  = getErrorLoc l
                        , _errorMsg   = "Could not resolve function: " <> T.pack (show ls)}
            (_, [vex]) -> Right vex
            (_, vexs)  -> Left Error
                          { _errorType = Ambiguous Function
                          , _errorLoc  = getErrorLoc l
                          , _errorMsg   = "Function not uniquely resolved: " <> T.pack (show vexs)
                          }
        where
          tryMkValExpr :: (Signature, Handler VarId) -> Either Error (ValExpr VarId)
          tryMkValExpr (sig, h) = do
              checkSortIds (sortRet sig) eSid
              if length (sortArgs sig) /= length exs
                  then Left Error
                       { _errorType = Undefined Function
                       , _errorLoc  = NoErrorLoc
                       , _errorMsg  = "Length of arguments don't match "
                                     <> T.pack (show sig)
                       }
                  else do
                  vexs <- traverse (uncurry $ expDeclToValExpr vdefs) $
                                zip (sortArgs sig) exs
                  return $ h vexs

-- | Compile a parallel let-variable declaration into a map of variable id's to
-- the expressions they are associated with.
parValDeclToMap :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                  -> ParLetVarDecl
                  -> Either Error (Map VarId (ValExpr VarId))
parValDeclToMap mm vs = Map.fromList <$>
    traverse (letValDeclToMap mm) (toList vs)

-- | Compile a let-variable declaration into a map of variable id's to the
-- expressions they are associated with.
letValDeclToMap :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                  -> LetVarDecl
                  -> Either Error (VarId, ValExpr VarId)
letValDeclToMap vrvds vd = do
    vId   <- varIdForRef vrvds (asVarReflLoc (getLoc vd))
    vdExp <- expDeclToValExpr vrvds (varsort vId) (varDeclExp vd)
    return (vId, vdExp)
