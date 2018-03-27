-- |

module TorXakis.Compiler.ValExpr.ValExpr where

import           Data.Map                            (Map)
import qualified Data.Map                            as Map

import           FuncDef                             (FuncDef)
import           FuncId                              (FuncId)
import           ValExpr                             (ValExpr, cstrConst,
                                                      cstrFunc, cstrVar, subst)
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
        vDecldfDecl <- findVarDecl e l
        case vDecldfDecl of
            Left vDecl -> do
                vId   <- findVarId e (getLoc vDecl)
                return $ cstrVar vId
            Right fDecl -> do
                fId <- findFuncId e (getLoc fDecl)
                -- TODO: check that the function with id 'fId' takes no arguments!
                return $ cstrFunc (getFuncDefT e') fId []
    ConstLit c ->
        return $ cstrConst (constToConstDef c)
    LetExp vs subEx -> do
        -- TODO: Do we need to use vs here? My guess is **no**! We resolved the
        -- variable scope in previous steps!

        -- If you look at what is done at TxsHappy@2686, the LET clause is
        -- translated as a substitution! Considering that having a LET
        -- expression here might get confusing, we might consider doing a
        -- previous step that transforms a ExpDecl with LET clauses into a
        -- ExpDecl without.
        --
        -- > LET NeValueDefList IN ValExpr1 EndIn
        -- > $$ = foldr (\x -> subst x (Map.empty :: Map.Map FuncId (FuncDef VarId)) ) $4 $2
        --
        -- > subst :: (Variable v, Integral (ValExpr v), Variable w, Integral (ValExpr w))
        -- > => Map.Map v (ValExpr v)
        -- > -> Map.Map FuncId (FuncDef w)
        -- > -> ValExpr v
        -- > -> ValExpr v
        -- QUESTION: why is the following line needed:
        --
        -- > $4.inhVarSigs = map (\(IdVar v) -> v )
        -- >                     $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $2.synVarSigs)
        --
        -- If a substitution (via 'subst' above) took place.
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
