-- |

module TorXakis.Compiler.ValExpr.SortId where

import           Control.Arrow             (left, (|||))
import           Control.Monad.Error.Class (liftEither)
import           Data.Either               (partitionEithers)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               (mempty, (<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Exts                  (fromList)

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

-- TODO: let's infer some types...

-- | TODO: QUESTION: do we return an error when there are variables whose types
-- couldn't be inferred, or do we leave the error occur when some other
-- function asks for the type of the variable later on?

inferTypes :: (HasSortIds e, HasVarDecls e)
           => e -> [FuncDecl] -> CompilerM (Map (Loc VarDeclE) SortId)
inferTypes e fs = liftEither $ do
    letVdSid    <- gInferTypes mempty allLetVarDecls
    paramsVdSid <- Map.fromList . concat <$> traverse fParamLocSorts fs
    -- TODO: join this with the sort of the function parameters
    return $ Map.union letVdSid paramsVdSid
    where
      allLetVarDecls = concatMap letVarDeclsInFunc fs
      gInferTypes :: SEnv (Map (Loc VarDeclE) SortId)
                  -> [LetVarDecl]
                  -> Either Error (Map (Loc VarDeclE) SortId)
      gInferTypes e' vs =
          case partitionEithers (inferVarDeclType e e' <$> vs) of
              ([], rs) -> Right $ fromSEnv $ fromList rs <> e'
              (ls, []) -> Left  $ T.pack $ "Could not infer the types of" ++ show ls
              (ls, rs) -> gInferTypes (fromList rs <> e') ls
      fParamLocSorts :: FuncDecl -> Either Error [(Loc VarDeclE, SortId)]
      fParamLocSorts fd = zip (getLoc <$> funcParams fd) <$> fParamSorts
          where
            fParamSorts :: Either Error [SortId]
            fParamSorts = traverse (findSortId e) (varDeclSort <$> funcParams fd)

letVarDeclsInFunc :: FuncDecl -> [LetVarDecl]
letVarDeclsInFunc fd = expLetVarDecls (funcBody fd)

inferVarDeclType :: (HasSortIds e, HasVarDecls e)
                 => e
                 -> SEnv (Map (Loc VarDeclE) SortId)
                 -> LetVarDecl -> Either LetVarDecl (Loc VarDeclE, SortId)
inferVarDeclType e vdSid vd = left (const vd) $
    case letVarDeclSortName vd of
    Just sn -> do -- If the sort is declared, we just return it.
        sId <- findSortId e sn
        return (getLoc vd, sId)
    Nothing -> do -- If the sort is not declared, we try to infer it from the expression.
        expSid <- inferExpType e vdSid (varDeclExp vd)
        return (getLoc vd, expSid)

inferExpType :: (HasSortIds e, HasVarDecls e)
             => e
             -> SEnv (Map (Loc VarDeclE) SortId)
             -> ExpDecl
             -> Either Error SortId
inferExpType e vdSid ex =
    case expChild ex of
    VarRef _ l -> do
        -- Find the location of the variable reference
        vdL <- findVarDecl e l
        -- If it is a variable, return the sort id of the variable declaration.
        -- If it is a function, return the sort id of the function.
        findVarDeclSortId vdSid . getLoc ||| findSortId e . funcRetSort $ vdL
    ConstLit c ->
        return $ sortIdConst c
    LetExp _ subEx ->
        -- We don't use the information contained in the (let) variable
        -- declarations, since we assume this information will be eventually
        -- present in the 'vdSid' map.
        inferExpType e vdSid subEx

sortIdConst :: Const -> SortId
sortIdConst (BoolConst _)   = sortIdBool
sortIdConst (IntConst _ )   = sortIdInt
sortIdConst (StringConst _) = sortIdString
