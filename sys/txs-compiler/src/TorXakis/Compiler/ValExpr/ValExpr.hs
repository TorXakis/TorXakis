-- |

module TorXakis.Compiler.ValExpr.ValExpr where

import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set

import           FuncDef                             (FuncDef)
import           FuncId                              (FuncId)
import           ValExpr                             (ValExpr, cstrAnd,
                                                      cstrConst, cstrFunc,
                                                      cstrITE, cstrVar, subst)
import qualified ValExpr
import           VarId                               (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Parser.Data

expDeclToValExpr :: (HasVarDecls e, HasVarIds e, HasFuncIds e, HasFuncDefs e')
                 => e -> e' -> ExpDecl -> Either Error (ValExpr VarId)
expDeclToValExpr e e' ex = case expChild ex of
    VarRef _ l -> do
        vLocfLoc <- findVarDecl e l
        case vLocfLoc of
            Left vLoc -> do
                vId   <- findVarId e vLoc
                return $ cstrVar vId
            Right fLoc -> do
                fId <- findFuncId e fLoc
                -- TODO: check that the function with id 'fId' takes no arguments!
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
                      vdExp <- expDeclToValExpr e e' (varDeclExp vd)
                      return $ Map.singleton vId vdExp
            fsM :: Map.Map FuncId (FuncDef VarId)
            fsM = Map.empty
        subValExpr <- expDeclToValExpr e e' subEx
        vsM <- letValDeclsToMaps
        return $ foldr (`subst` fsM) subValExpr vsM
    If e0 e1 e2 -> do
        ve0 <- expDeclToValExpr e e' e0
        ve1 <- expDeclToValExpr e e' e1
        ve2 <- expDeclToValExpr e e' e2
        return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
