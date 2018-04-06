{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module TorXakis.Compiler.ValExpr.SortId where

import           Control.Arrow             (left, (|||))
import           Control.Monad             (when)
import           Control.Monad.Error.Class (liftEither)
import           Data.Either               (partitionEithers)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Exts                  (fromList)

import           FuncId                    (funcargs, funcsort)
import           Id                        (Id (Id))
import           SortId                    (SortId (SortId), sortIdBool,
                                            sortIdInt, sortIdString)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Parser.Data

-- | Construct a list of sort ID's from a list of ADT declarations.
--
-- The ID of the ADT will coincide with the ID of the SortId.
compileToSortId :: [ADTDecl] -> CompilerM (Map Text SortId)
compileToSortId ds = Map.fromList . zip (adtName <$> ds) <$>
    traverse adtDeclToSortId ds

adtDeclToSortId :: ADTDecl -> CompilerM SortId
adtDeclToSortId a = do
    i <- getNextId
    return $ SortId (adtName a) (Id i)

-- sortIdOfFieldDecl :: HasSortIds e => e -> FieldDecl -> Either Error SortId
-- sortIdOfFieldDecl e = findSortId e . fieldSort

-- sortIdOfFieldDeclM :: HasSortIds e => e -> FieldDecl -> CompilerM SortId
-- sortIdOfFieldDeclM e f = liftEither $ sortIdOfFieldDecl e f

sortIdOfVarDecl :: HasSortIds e => e -> VarDecl -> Either Error SortId
sortIdOfVarDecl e = findSortId e . varDeclSort

sortIdOfVarDeclM :: HasSortIds e => e -> VarDecl -> CompilerM SortId
sortIdOfVarDeclM e f = liftEither $ sortIdOfVarDecl e f

-- | TODO: QUESTION: do we return an error when there are variables whose types
-- couldn't be inferred, or do we leave the error occur when some other
-- function asks for the type of the variable later on?
inferTypes :: (HasSortIds e, HasVarDecls e, HasFuncIds e)
           => e -> [FuncDecl] -> CompilerM (Map (Loc VarDeclE) SortId)
inferTypes e fs = liftEither $ do
    paramsVdSid <- Map.fromList . concat <$> traverse fParamLocSorts fs
    letVdSid    <- gInferTypes (SEnv paramsVdSid) allLetVarDecls
    return $ Map.union letVdSid paramsVdSid
    where
      allLetVarDecls = concatMap letVarDeclsInFunc fs
      gInferTypes :: SEnv (Map (Loc VarDeclE) SortId)
                  -> [LetVarDecl]
                  -> Either Error (Map (Loc VarDeclE) SortId)
      gInferTypes e' vs =
          case partitionEithers (inferVarDeclType e e' <$> vs) of
              ([], rs) -> Right $ fromSEnv $ fromList rs <> e'
              (ls, []) -> Left  Error
                          { errorType = UndefinedType
                          , errorLoc  = NoErrorLoc -- TODO: we could generate
                                                   -- multiple errors, giving
                                                   -- all the locations in 'ls'
                          , errorMsg  =  "Could not infer the types: " <> (T.pack . show . (fst <$>)) ls
                          }
              (ls, rs) -> gInferTypes (fromList rs <> e') (snd <$> ls)
      fParamLocSorts :: FuncDecl -> Either Error [(Loc VarDeclE, SortId)]
      fParamLocSorts fd = zip (getLoc <$> funcParams fd) <$> fParamSorts
          where
            fParamSorts :: Either Error [SortId]
            fParamSorts = traverse (findSortId e) (varDeclSort <$> funcParams fd)

letVarDeclsInFunc :: FuncDecl -> [LetVarDecl]
letVarDeclsInFunc fd = expLetVarDecls (funcBody fd)

inferVarDeclType :: (HasSortIds e, HasVarDecls e, HasFuncIds e)
                 => e
                 -> SEnv (Map (Loc VarDeclE) SortId)
                 -> LetVarDecl -> Either (Error, LetVarDecl) (Loc VarDeclE, SortId)
inferVarDeclType e vdSid vd = left (,vd) $
    case letVarDeclSortName vd of
    Just sn -> do -- If the sort is declared, we just return it.
        sId <- findSortId e sn
        return (getLoc vd, sId)
    Nothing -> do -- If the sort is not declared, we try to infer it from the expression.
        expSid <- inferExpType e vdSid (varDeclExp vd)
        return (getLoc vd, expSid)

inferExpType :: (HasSortIds e, HasVarDecls e, HasFuncIds e)
             => e
             -> SEnv (Map (Loc VarDeclE) SortId)
             -> ExpDecl
             -> Either Error SortId
inferExpType e vdSid ex =
    case expChild ex of
    VarRef _ l ->
        -- Find the location of the variable reference
        -- If it is a variable, return the sort id of the variable declaration.
        -- If it is a function, return the sort id of the function.
        (findVarDeclSortId vdSid ||| findFuncSortId e) =<< findVarDecl e l
    ConstLit c ->
        return $ sortIdConst c
    LetExp vs subEx -> do
        vsSids <- traverse (inferExpType e vdSid) (varDeclExp <$> vs)
        let vdSid' = fromList (zip (getLoc <$> vs) vsSids) <> vdSid
        inferExpType e vdSid' subEx
    -- TODO: shouldn't if be also a function? Defined in terms of the Haskell's @if@ operator.
    If e0 e1 e2 -> do
        [se0, se1, se2] <- traverse (inferExpType e vdSid) [e0, e1, e2]
        when (se0 /= sortIdBool)
            (Left Error
                { errorType = TypeMismatch
                , errorLoc  = getErrorLoc e0
                , errorMsg  = "Guard expression must be a boolean."
                           <> " Got " <> T.pack (show se0)
                })
        when (se1 /= se2)
            (Left Error
                { errorType = TypeMismatch
                , errorLoc  = getErrorLoc ex
                , errorMsg  = "The sort of the two IF branches don't match."
                           <> "(" <> T.pack (show se1)
                           <>" and " <> T.pack (show se2) <> ")"
                }
             )
        return se1
    Fappl _ l exs -> do
        ses <- traverse (inferExpType e vdSid) exs
        -- TODO: check the return type of the function.
        fdis <- findFuncDecl e l
        fdi  <- determineF e fdis ses Nothing
        fId  <- findFuncId e fdi
        when (ses /= funcargs fId)
            (Left Error
             { errorType = TypeMismatch
             , errorLoc  = getErrorLoc l
             , errorMsg  = "Function arguments sorts do not match "
                        <> T.pack (show ses)
             })
        return $ funcsort fId


sortIdConst :: Const -> SortId
sortIdConst (BoolConst _)   = sortIdBool
sortIdConst (IntConst _ )   = sortIdInt
sortIdConst (StringConst _) = sortIdString










