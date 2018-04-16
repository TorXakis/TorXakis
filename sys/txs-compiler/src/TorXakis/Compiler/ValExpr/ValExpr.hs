{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module TorXakis.Compiler.ValExpr.ValExpr where

import           Prelude hiding (lookup)
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

import           TorXakis.Compiler.Data hiding (lookup)
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Parser.Data

-- | Make a 'ValExpr' from the given expression-declaration.
--
expDeclToValExpr :: ( HasVarDecls e
                    , HasFuncIds e
                    , HasFuncDefs e'
                    , MapsTo (Loc VarDeclE) VarId mm)
                 => mm
                 -> e
                 -> e'
                 -> SortId -- ^ Expected SortId for the expression.
                 -> ExpDecl
                 -> Either Error (ValExpr VarId)
expDeclToValExpr mm e e' eSid ex = case expChild ex of
    VarRef _ l -> do
        vLocfLoc <- findVarDecl e l
        case vLocfLoc of
            Left vLoc -> do
                vId <- lookup vLoc mm
                checkSortIds (varsort vId) eSid
                return $ cstrVar vId
            Right fdis -> do
                let matchingFdis = determineF e fdis [] (Just eSid)
                fdi  <- getUniqueElement matchingFdis
                fId  <- findFuncId e fdi
                checkSortIds (funcsort fId) eSid
                return $ cstrFunc (getFuncDefT e') fId []
    ConstLit c -> do
        traverse_ (checkSortIds eSid) (sortIdConst c)
        return $ cstrConst (constToConstDef eSid c)
    LetExp vs subEx -> do
        let
            letValDeclsToMaps :: Either Error [Map VarId (ValExpr VarId)]
            letValDeclsToMaps = traverse letValDeclToMap vs
                where
                  letValDeclToMap :: LetVarDecl -> Either Error (Map VarId (ValExpr VarId))
                  letValDeclToMap vd = do
                      vId   <- lookup (getLoc vd) mm
                      vdExp <- expDeclToValExpr mm e e' (varsort vId) (varDeclExp vd)
                      return $ Map.singleton vId vdExp
            fsM :: Map.Map FuncId (FuncDef VarId)
            fsM = Map.empty
        subValExpr <- expDeclToValExpr mm e e' eSid subEx
        vsM <- letValDeclsToMaps
        return $ foldr (`subst` fsM) subValExpr vsM
    If ex0 ex1 ex2 -> do
        ve0 <- expDeclToValExpr mm e e' sortIdBool ex0
        ve1 <- expDeclToValExpr mm e e' eSid ex1
        ve2 <- expDeclToValExpr mm e e' eSid ex2
        return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
    Fappl _ l exs -> do
        fdis <- findFuncDecl e l
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
          tryMkValExpr :: FuncDefInfo -> Either Error (ValExpr VarId)
          tryMkValExpr fdi = do
              fId  <- findFuncId e fdi
              checkSortIds (funcsort fId) eSid
              if length (funcargs fId) /= length exs
                  then Left Error
                       { _errorType = UndefinedRef
                       , _errorLoc  = NoErrorLoc
                       , _errorMsg  = "Length of arguments don't match"
                                     <> T.pack (show fId)
                       }
                  else do
                  vexs <- traverse (uncurry $ expDeclToValExpr mm e e') $
                                zip (funcargs fId) exs
                  return $ cstrFunc (getFuncDefT e') fId vexs
