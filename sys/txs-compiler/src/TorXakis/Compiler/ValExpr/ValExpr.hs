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
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.ConstDefs
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Parser.Data

-- | Make a 'ValExpr' from the given expression-declaration.
--
-- expDeclToValExpr :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
--                     , MapsTo (Loc FuncDeclE) FuncId mm
--                     , MapsTo FuncId (FuncDef VarId) mm
--                     , MapsTo (Loc VarDeclE) VarId mm )
--                  => mm
--                  -> SortId -- ^ Expected SortId for the expression.
--                  -> ExpDecl
--                  -> Either Error (ValExpr VarId)
-- expDeclToValExpr mm eSid ex = case expChild ex of
--     VarRef _ l -> do
--         vLocfLoc <- mm .@@ (l :: Loc VarRefE)
--         case vLocfLoc of
--             Left vLoc -> do
--                 vId <- mm .@@ (vLoc :: Loc VarDeclE)
--                 checkSortIds (varsort vId) eSid
--                 return $ cstrVar vId
--             Right fdis -> do
--                 let matchingFdis = determineF mm fdis [] (Just eSid)
--                 fdi  <- getUniqueElement matchingFdis
--                 fId  <- mm .@@ fdi
--                 checkSortIds (funcsort fId) eSid
--                 return $ cstrFunc (innerMap mm :: Map FuncId (FuncDef VarId)) fId []
--     ConstLit c -> do
--         traverse_ (checkSortIds eSid) (sortIdConst c)
--         return $ cstrConst (constToConstDef eSid c)
--     LetExp vss subEx -> do
--         let
--             letValDeclsToMaps :: Either Error [Map VarId (ValExpr VarId)]
--             letValDeclsToMaps = traverse (parValDeclToMap mm) vss
--             fsM :: Map.Map FuncId (FuncDef VarId)
--             fsM = Map.empty
--         subValExpr <- expDeclToValExpr mm eSid subEx
--         vsM <- letValDeclsToMaps
--         return $ foldr (`subst` fsM) subValExpr vsM
--     If ex0 ex1 ex2 -> do
--         ve0 <- expDeclToValExpr mm sortIdBool ex0
--         ve1 <- expDeclToValExpr mm eSid ex1
--         ve2 <- expDeclToValExpr mm eSid ex2
--         return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
--     Fappl _ l exs -> do
--         fdis <- findFuncDecl mm l
--         case partitionEithers (tryMkValExpr <$> fdis) of
--             (ls, []) -> Left Error
--                         { _errorType = UndefinedRef
--                         , _errorLoc  = getErrorLoc l
--                         , _errorMsg   = "Could not resolve function: " <> T.pack (show ls)}
--             (_, [vex]) -> Right vex
--             (_, vexs)  -> Left Error
--                           { _errorType = UnresolvedIdentifier
--                           , _errorLoc  = getErrorLoc l
--                           , _errorMsg   = "Function not uniquely resolved: " <> T.pack (show vexs)
--                           }
--         where
--           tryMkValExpr :: (Loc FuncDeclE) -> Either Error (ValExpr VarId)
--           tryMkValExpr fdi = do
--               fId  <- mm .@@ fdi
--               checkSortIds (funcsort fId) eSid
--               if length (funcargs fId) /= length exs
--                   then Left Error
--                        { _errorType = UndefinedRef
--                        , _errorLoc  = NoErrorLoc
--                        , _errorMsg  = "Length of arguments don't match"
--                                      <> T.pack (show fId)
--                        }
--                   else do
--                   vexs <- traverse (uncurry $ expDeclToValExpr mm) $
--                                 zip (funcargs fId) exs
--                   return $ cstrFunc (innerMap mm :: Map FuncId (FuncDef VarId)) fId vexs


-- parValDeclToMap :: ( MapsTo FuncId (FuncDef VarId) mm
--                    , MapsTo (Loc FuncDeclE) FuncId mm
--                    , MapsTo (Loc VarDeclE) VarId mm
--                    , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm )
--                 => mm -> [LetVarDecl] -> Either Error (Map VarId (ValExpr VarId))
-- parValDeclToMap mm vs = Map.fromList <$>
--     traverse (letValDeclToMap mm) vs

-- letValDeclToMap :: ( MapsTo FuncId (FuncDef VarId) mm
--                    , MapsTo (Loc FuncDeclE) FuncId mm
--                    , MapsTo (Loc VarDeclE) VarId mm
--                    , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm )
--                 => mm -> LetVarDecl -> Either Error (VarId, ValExpr VarId)
-- letValDeclToMap mm vd = do
--     vId   <- mm .@@ getLoc vd
--     vdExp <- expDeclToValExpr mm (varsort vId) (varDeclExp vd)
--     return (vId, vdExp)

-- | Version that replaces the constraints :
--
-- > MapsTo FuncId (FuncDef VarId) mm
-- > MapsTo (Loc FuncDeclE) FuncId mm
--
-- By the constraint:
--
-- > MapsTo (Loc FuncDeclE) (Signature, Handler VarId)
--
expDeclToValExpr :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                    , MapsTo (Loc VarDeclE) VarId mm
                    , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm )
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
                let sigHdlrsMap :: Map (Loc FuncDeclE) (Signature, Handler VarId)
                    sigHdlrsMap = innerMap mm
                    sigsMap = fst <$> sigHdlrsMap
                    matchingFdis = determineF sigsMap fdis [] (Just eSid)
                fLoc  <- getUniqueElement matchingFdis
                (sig, h)  <- mm .@@ fLoc :: Either Error (Signature, Handler VarId)
                checkSortIds (sortRet sig) eSid
                return $ h []
    ConstLit c -> do
        traverse_ (checkSortIds eSid) (sortIdConst c)
        return $ cstrConst (constToConstDef eSid c)
    LetExp vss subEx -> do
        let
            letValDeclsToMaps :: Either Error [Map VarId (ValExpr VarId)]
            letValDeclsToMaps = traverse (parValDeclToMap mm) vss
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
          tryMkValExpr :: Loc FuncDeclE -> Either Error (ValExpr VarId)
          tryMkValExpr l = do
              (sig, h)  <- mm .@@ l :: Either Error (Signature, Handler VarId)
              checkSortIds (sortRet sig) eSid
              if length (sortArgs sig) /= length exs
                  then Left Error
                       { _errorType = UndefinedRef
                       , _errorLoc  = NoErrorLoc
                       , _errorMsg  = "Length of arguments don't match"
                                     <> T.pack (show sig)
                       }
                  else do
                  vexs <- traverse (uncurry $ expDeclToValExpr mm) $
                                zip (sortArgs sig) exs
                  return $ h vexs


parValDeclToMap :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                    , MapsTo (Loc VarDeclE) VarId mm
                    , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm )
                 => mm -> [LetVarDecl] -> Either Error (Map VarId (ValExpr VarId))
parValDeclToMap mm vs = Map.fromList <$>
    traverse (letValDeclToMap mm) vs

letValDeclToMap :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                    , MapsTo (Loc VarDeclE) VarId mm
                    , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm )
                 => mm -> LetVarDecl -> Either Error (VarId, ValExpr VarId)
letValDeclToMap mm vd = do
    vId   <- mm .@@ getLoc vd
    vdExp <- expDeclToValExpr mm (varsort vId) (varDeclExp vd)
    return (vId, vdExp)

--
-- Third attempt of simplification...
--

-- | Join the three given maps. Every declaration location must have an image
-- in the second o third map, otherwise an error is returned.
varRefsToVarDefs :: Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
                 -> Map (Loc VarDeclE) VarId
                 -> Map (Loc FuncDeclE) (Signature, Handler VarId)
                 -> Either Error (Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)]))
varRefsToVarDefs vdecls vids fshs = do
    defs <- traverse declToDef (snd <$> vdecls')
    return $ Map.fromAscList $ zip (fst <$> vdecls') defs
    where
      vdecls' :: [(Loc VarRefE, Either (Loc VarDeclE) [Loc FuncDeclE])]
      vdecls' = Map.toList vdecls
      declToDef :: Either (Loc VarDeclE) [Loc FuncDeclE]
                -> Either Error (Either VarId [(Signature, Handler VarId)])
      declToDef (Left vid)  = Left <$> varIdforDecl vid
      declToDef (Right fls) = Right <$> traverse shForDecl fls
      varIdforDecl :: Loc VarDeclE -> Either Error VarId
      varIdforDecl l = maybe vidNotFound Right (Map.lookup l vids)
          where
            vidNotFound = Left Error
                { _errorType = UndefinedRef
                , _errorLoc = getErrorLoc l
                , _errorMsg = "varRefsToVarDefs: VarId not found."
                }
      shForDecl :: Loc FuncDeclE -> Either Error (Signature, Handler VarId)
      shForDecl l = maybe shNotFound Right (Map.lookup l fshs)
          where
            shNotFound = Left Error
                { _errorType = UndefinedRef
                , _errorLoc = getErrorLoc l
                , _errorMsg =  "varRefsToVarDefs: Signature-Handler not found."
                }

expDeclToValExpr_2 :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)]) -- ^ Variable definitions associated to a given reference.
                  -> SortId -- ^ Expected SortId for the expression.
                  -> ExpDecl
                  -> Either Error (ValExpr VarId)
expDeclToValExpr_2 vdefs eSid ex = case expChild ex of
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
            letValDeclsToMaps = traverse (parValDeclToMap_2 vdefs) vss
            fsM :: Map.Map FuncId (FuncDef VarId)
            fsM = Map.empty
        subValExpr <- expDeclToValExpr_2 vdefs eSid subEx
        vsM <- letValDeclsToMaps
        return $ foldr (`subst` fsM) subValExpr vsM
    If ex0 ex1 ex2 -> do
        ve0 <- expDeclToValExpr_2 vdefs sortIdBool ex0
        ve1 <- expDeclToValExpr_2 vdefs eSid ex1
        ve2 <- expDeclToValExpr_2 vdefs eSid ex2
        return $ cstrITE (cstrAnd (Set.fromList [ve0])) ve1 ve2
    Fappl _ l exs -> do
        shs <- findFuncDecl_2 vdefs l
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
                  vexs <- traverse (uncurry $ expDeclToValExpr_2 vdefs) $
                                zip (sortArgs sig) exs
                  return $ h vexs

parValDeclToMap_2 :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                  -> [LetVarDecl]
                  -> Either Error (Map VarId (ValExpr VarId))
parValDeclToMap_2 mm vs = Map.fromList <$>
    traverse (letValDeclToMap_2 mm) vs

letValDeclToMap_2 :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
                  -> LetVarDecl
                  -> Either Error (VarId, ValExpr VarId)
letValDeclToMap_2 vdefs vd = do
    -- TODO: introduce a function that checks whether the location is mapped
    -- onto a variable.
    -- TODO: this requires that a variable declaration is also a reference!
    let Just (Left vId) = Map.lookup (asVarReflLoc (getLoc vd)) vdefs
    vdExp <- expDeclToValExpr_2 vdefs (varsort vId) (varDeclExp vd)
    return (vId, vdExp)
