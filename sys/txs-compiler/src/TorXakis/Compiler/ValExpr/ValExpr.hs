{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler.ValExpr.ValExpr where

import           Data.Either                         (partitionEithers)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Semigroup                      ((<>))
import qualified Data.Set                            as Set
import qualified Data.Text                           as T

import           FuncDef                             (FuncDef)
import           FuncId                              (FuncId, funcargs)
import           SortId                              (SortId, sortIdBool)
import           SortOf                              (sortOf)
import           ValExpr                             (ValExpr, cstrAnd,
                                                      cstrConst, cstrFunc,
                                                      cstrITE, cstrVar, subst)
import           VarId                               (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Parser.Data

-- | Make a 'ValExpr' from the given expression-declaration.
--
expDeclToValExpr :: (HasVarDecls e, HasVarIds e, HasFuncIds e, HasFuncDefs e')
                 => e -> e'
                 -> Maybe SortId -- ^ Expected SortId for the expression (if known).
                 -> ExpDecl
                 -> Either Error (ValExpr VarId)
expDeclToValExpr e e' eSid ex = case expChild ex of
    VarRef _ l -> do
        vLocfLoc <- findVarDecl e l
        case vLocfLoc of
            Left vLoc -> do
                vId   <- findVarId e vLoc
                return $ cstrVar vId
            Right fdis -> do
                fdi <- determineF e fdis [] eSid
                fId <- findFuncId e fdi
                return $ cstrFunc (getFuncDefT e') fId []
    ConstLit c ->
        return $ cstrConst (constToConstDef c)
    LetExp vs subEx -> do
        let
            letValDeclsToMaps :: Either Error [Map VarId (ValExpr VarId)]
            letValDeclsToMaps = traverse letValDeclToMap vs
                where
                  letValDeclToMap :: LetVarDecl -> Either Error (Map VarId (ValExpr VarId))
                  letValDeclToMap vd = do
                      vId   <- findVarId e (getLoc vd)
                      vdExp <- expDeclToValExpr e e' eSid (varDeclExp vd)
                      return $ Map.singleton vId vdExp
            fsM :: Map.Map FuncId (FuncDef VarId)
            fsM = Map.empty
        subValExpr <- expDeclToValExpr e e' eSid subEx
        vsM <- letValDeclsToMaps
        return $ foldr (`subst` fsM) subValExpr vsM
    If ex0 ex1 ex2 -> do
        ve0 <- expDeclToValExpr e e' (Just sortIdBool) ex0
        ve1 <- expDeclToValExpr e e' eSid ex1
        ve2 <- expDeclToValExpr e e' eSid ex2
        return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
    Fappl _ l exs -> do
        fdis <- findFuncDecl e l

        -- case partitionEithers (tryMkValExpr <$> fdis) of
        --     (ls, []) -> Left Error
        --         { errorType = UndefinedRef
        --         , errorLoc  = getErrorLoc l
        --         , errorMsg   = "Could not resolve function: " <> T.pack (show ls)}
        --     (_, [vex]) -> Right vex
        --     (_, vexs)  -> Left Error
        --         { errorType = UnresolvedIdentifier
        --         , errorLoc  = getErrorLoc l
        --         , errorMsg   = "Function not uniquely resolved: " <> T.pack (show vexs)}

        vexs <- traverse (expDeclToValExpr e e' Nothing) exs
        fdi  <- determineF e fdis (sortOf <$> vexs) eSid
        fId  <- findFuncId e fdi
        return $ cstrFunc (getFuncDefT e') fId vexs

        -- where
        --   tryMkValExpr :: FuncDefInfo -> Either Error (ValExpr VarId)
        --   tryMkValExpr fdi = do
        --       fId  <- findFuncId e fdi
        --       vexs <- traverse (uncurry $ expDeclToValExpr e e') $
        --                        zip (Just <$> funcargs fId) exs
        --       return $ cstrFunc (getFuncDefT e') fId vexs
