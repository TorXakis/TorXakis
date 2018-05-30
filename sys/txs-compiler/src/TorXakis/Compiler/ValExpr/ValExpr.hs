{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler.ValExpr.ValExpr where

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
import           SortId                              (SortId, sortIdBool)
import           SortOf                              (sortOf)
import           ValExpr                             (ValExpr, cstrAnd,
                                                      cstrConst, cstrFunc,
                                                      cstrITE, cstrVar, subst)
import           VarId                               (VarId, varsort)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Parser.Data

-- | Make a 'ValExpr' from the given expression-declaration.
--
expDeclToValExpr :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                    , MapsTo (Loc FuncDeclE) FuncId mm
                    , MapsTo FuncId (FuncDef VarId) mm
                    , MapsTo (Loc VarDeclE) VarId mm)
                 => mm
                 -> SortId -- ^ Expected SortId for the expression.
                 -> ExpDecl
                 -> Either Error (ValExpr VarId)
expDeclToValExpr mm eSid ex = case expChild ex of
    VarRef _ l -> do
        vLocfLoc <- mm .@@ (l :: Loc VarRefE)
        case vLocfLoc of
            Left vLoc -> do
                vId <- mm .@@ (vLoc :: Loc VarDeclE)
                checkSortIds (varsort vId) eSid
                return $ cstrVar vId
            Right fdis -> do
                let matchingFdis = determineF mm fdis [] (Just eSid)
                fdi  <- getUniqueElement matchingFdis
                fId  <- mm .@@ fdi
                checkSortIds (funcsort fId) eSid
                return $ cstrFunc (innerMap mm :: Map FuncId (FuncDef VarId)) fId []
    ConstLit c -> do
        traverse_ (checkSortIds eSid) (sortIdConst c)
        return $ cstrConst (constToConstDef eSid c)
    LetExp vss subEx -> do
        let
            letValDeclsToMaps :: Either Error [Map VarId (ValExpr VarId)]
            letValDeclsToMaps = traverse parValDeclToMap vss
                where
                  parValDeclToMap :: [LetVarDecl] -> Either Error (Map VarId (ValExpr VarId))
                  parValDeclToMap vs = Map.fromList <$>
                      traverse letValDeclToMap vs

                  letValDeclToMap :: LetVarDecl -> Either Error (VarId, ValExpr VarId)
                  letValDeclToMap vd = do
                      vId   <- mm .@@ getLoc vd
                      vdExp <- expDeclToValExpr mm (varsort vId) (varDeclExp vd)
                      return (vId, vdExp)

            fsM :: Map.Map FuncId (FuncDef VarId)
            fsM = Map.empty
        subValExpr <- expDeclToValExpr mm eSid subEx
        vsM <- letValDeclsToMaps
        return $ foldr (`subst` fsM) subValExpr vsM
    If ex0 ex1 ex2 -> do
        ve0 <- expDeclToValExpr mm sortIdBool ex0
        ve1 <- expDeclToValExpr mm eSid ex1
        ve2 <- expDeclToValExpr mm eSid ex2
        return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
    Fappl _ l exs -> do
        fdis <- findFuncDecl mm l
        case partitionEithers (tryMkValExpr <$> fdis) of
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
          tryMkValExpr :: (Loc FuncDeclE) -> Either Error (ValExpr VarId)
          tryMkValExpr fdi = do
              fId  <- mm .@@ fdi
              checkSortIds (funcsort fId) eSid
              if length (funcargs fId) /= length exs
                  then Left Error
                       { _errorType = UndefinedRef
                       , _errorLoc  = NoErrorLoc
                       , _errorMsg  = "Length of arguments don't match"
                                     <> T.pack (show fId)
                       }
                  else do
                  vexs <- traverse (uncurry $ expDeclToValExpr mm) $
                                zip (funcargs fId) exs
                  return $ cstrFunc (innerMap mm :: Map FuncId (FuncDef VarId)) fId vexs
