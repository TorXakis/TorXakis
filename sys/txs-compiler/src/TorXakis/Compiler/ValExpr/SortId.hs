{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
module TorXakis.Compiler.ValExpr.SortId where

import Debug.Trace (trace)

import           Prelude                   hiding (lookup)
import           Control.Arrow             (left, (|||))
import           Control.Monad             (when)
import           Control.Monad.Error.Class (liftEither)
import           Data.Either               (partitionEithers)
import           Data.List                 (intersect)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Traversable          (for)
import           GHC.Exts                  (fromList)
import           Data.Maybe                (catMaybes)
    
import           FuncId                    (funcargs, funcsort, FuncId)
import           Id                        (Id (Id))
import           SortId                    (SortId (SortId), sortIdBool,
                                            sortIdInt, sortIdRegex,
                                            sortIdString)
import           ChanId (ChanId, chansorts)                 

import           TorXakis.Compiler.Data  
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.MapsTo 
import           TorXakis.Compiler.Maps
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

sortIdOfVarDecl :: MapsTo Text SortId mm => mm -> VarDecl -> Either Error SortId
sortIdOfVarDecl mm = findSortId mm . varDeclSort

sortIdOfVarDeclM :: MapsTo Text SortId mm => mm -> VarDecl -> CompilerM SortId
sortIdOfVarDeclM mm f = liftEither $ sortIdOfVarDecl mm f

-- | Infer the types in a list of function declaration.
inferTypes :: ( MapsTo Text SortId mm
              , MapsTo (Loc VarDeclE) SortId mm
              , MapsTo FuncDefInfo FuncId mm
              , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm )
           => mm
           -> [FuncDecl]
           -> CompilerM (Map (Loc VarDeclE) SortId)
inferTypes mm fs = liftEither $ do
    paramsVdSid <- Map.fromList . concat <$> traverse fParamLocSorts fs
    letVdSid    <- gInferTypes (paramsVdSid <.+> mm)  allLetVarDecls
    return $ Map.union letVdSid paramsVdSid
    where
      allLetVarDecls = concatMap letVarDeclsInFunc fs
      fParamLocSorts :: FuncDecl -> Either Error [(Loc VarDeclE, SortId)]
      fParamLocSorts fd = zip (getLoc <$> funcParams fd) <$> fParamSorts
          where
            fParamSorts :: Either Error [SortId]
            fParamSorts = traverse (findSortId mm) (varDeclSort <$> funcParams fd)


gInferTypes :: ( MapsTo Text SortId mm
               , MapsTo (Loc VarDeclE)  SortId mm
               , MapsTo FuncDefInfo FuncId mm
               , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm )
            => mm
            -> [LetVarDecl]
            -> Either Error (Map (Loc VarDeclE) SortId)
gInferTypes mm vs =
    case partitionEithers (inferVarDeclType mm <$> vs) of
        ([], rs) -> Right $ fromList rs <> innerMap mm
        (ls, []) -> Left  Error
                    { _errorType = UndefinedType
                    , _errorLoc  = NoErrorLoc -- TODO: we could generate
                                             -- multiple errors, giving
                                             -- all the locations in 'ls'
                    , _errorMsg  =  "Could not infer the types: " <> (T.pack . show . (fst <$>)) ls
                    }
        (ls, rs) -> gInferTypes (fromList rs <.+> mm) (snd <$> ls)

letVarDeclsInFunc :: FuncDecl -> [LetVarDecl]
letVarDeclsInFunc fd = expLetVarDecls (funcBody fd)

inferVarDeclType :: ( MapsTo Text SortId mm
                    , MapsTo (Loc VarDeclE) SortId mm
                    , MapsTo FuncDefInfo FuncId mm
                    , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm )
                 => mm
                 -> LetVarDecl -> Either (Error, LetVarDecl) (Loc VarDeclE, SortId)
inferVarDeclType mm vd = left (,vd) $
    case letVarDeclSortName vd of
    Just sn -> do -- If the sort is declared, we just return it.
        sId <- findSortId mm sn
        return (getLoc vd, sId)
    Nothing -> do -- If the sort is not declared, we try to infer it from the expression.
        expSids <- inferExpTypes mm (varDeclExp vd)
        expSid <- getUniqueElement expSids
        return (getLoc vd, expSid)

-- | Infer the type of an expression. Due to function overloading an expression
-- could have multiple types, e.g.:
--
-- > fromString("33")
--
-- Could be a TorXakis 'Int', 'String', 'Bool', or even an 'ADT'.
--
inferExpTypes :: ( MapsTo Text SortId mm
                 , MapsTo (Loc VarDeclE) SortId mm
                 , MapsTo FuncDefInfo FuncId mm
                 , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm)
              => mm
              -> ExpDecl
              -> Either Error [SortId]
inferExpTypes mm ex =
    case expChild ex of
    VarRef _ l ->
        -- Find the location of the variable reference
        -- If it is a variable, return the sort id of the variable declaration.
        -- If it is a function, return the sort id's of the functions.
        (fmap pure . (`lookup` mm) ||| findFuncSortIds mm)
            =<< (lookup l mm :: Either Error (Either (Loc VarDeclE) [FuncDefInfo]))
    ConstLit c ->
        return $ -- The type of any is any sort known!
            maybe (values @Text mm) pure (sortIdConst c)
    LetExp vs subEx -> do
        vsSidss <- traverse (inferExpTypes mm) (varDeclExp <$> vs)
        -- Here we make sure that each variable expression has a unique type.
        vsSids <- traverse getUniqueElement vsSidss
        let vdSid = fromList (zip (getLoc <$> vs) vsSids)
        inferExpTypes (vdSid <.+> mm) subEx
    -- TODO: shouldn't if be also a function? Defined in terms of the Haskell's @if@ operator.
    If e0 e1 e2 -> do
        [se0s, se1s, se2s] <- traverse (inferExpTypes mm) [e0, e1, e2]
        when (sortIdBool `notElem` se0s)
            (Left Error
                { _errorType = TypeMismatch
                , _errorLoc  = getErrorLoc e0
                , _errorMsg  = "Guard expression must be a Boolean."
                           <> " Got " <> T.pack (show se0s)
                })
        let ses = se1s `intersect` se2s
        when (null ses)
            (Left Error
                { _errorType = TypeMismatch
                , _errorLoc  = getErrorLoc ex
                , _errorMsg  = "The sort of the two IF branches don't match."
                           <> "(" <> T.pack (show se1s)
                           <>" and " <> T.pack (show se2s) <> ")"
                }
             )
        return ses
    Fappl _ l exs -> concat <$> do
        sess <- traverse (inferExpTypes mm) exs
        for (sequence sess) $ \ses -> do
              fdis <- findFuncDecl mm l
              let matchingFdis = determineF mm fdis ses Nothing
              for matchingFdis $ \fdi -> do
                  fId  <- lookup fdi mm
                  when (ses /= funcargs fId)
                      (Left Error
                       { _errorType = TypeMismatch
                       , _errorLoc  = getErrorLoc l
                       , _errorMsg  = "Function arguments sorts do not match "
                                     <> T.pack (show ses)
                       })
                  return $ funcsort fId

sortIdConst :: Const -> Maybe SortId
sortIdConst (BoolConst _)   = Just sortIdBool
sortIdConst (IntConst _ )   = Just sortIdInt
sortIdConst (StringConst _) = Just sortIdString
sortIdConst (RegexConst _)  = Just sortIdRegex
-- Any does not have a sort id associated with it.
--
-- Note that it seems like a bad design decision to change 'AnyConst' to
-- include the 'SortId', since the parser does not need to know anything about
-- the internal representations used by 'TorXakis'.
sortIdConst AnyConst        = Nothing

checkSortIds :: SortId -> SortId -> Either Error ()
checkSortIds sId0 sId1 =
    when (sId0 /= sId1) $ Left Error
    { _errorType = TypeMismatch
    , _errorLoc  = NoErrorLoc
    , _errorMsg  = "Sorts do not match "
                  <> T.pack (show sId0) <> T.pack (show sId1)
    }

class HasTypedVars e where
    inferVarTypes :: ( MapsTo Text SortId mm
                     , MapsTo Text ChanId mm
                     , MapsTo (Loc VarDeclE) SortId mm
                     , MapsTo FuncDefInfo FuncId mm
                     , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm )
                  => mm -> e -> CompilerM [(Loc VarDeclE, SortId)]

instance HasTypedVars BExpDecl where
    inferVarTypes _ Stop = return []
    inferVarTypes mm (ActPref ao be) = do
        xs <- inferVarTypes mm ao
        -- The implicit variables in the offers are needed in subsequent expressions.
        ys <- inferVarTypes (Map.fromList xs <.+> mm) be
        return $ xs ++ ys
    inferVarTypes mm (LetBExp vs be) = do
        xs <- Map.toList <$> liftEither (gInferTypes mm vs)
        ys <- inferVarTypes mm be
        return $ xs ++ ys 

instance HasTypedVars ActOfferDecl where
    inferVarTypes mm (ActOfferDecl os mEx) = (++) <$> inferVarTypes mm os <*> inferVarTypes mm mEx

instance HasTypedVars e => HasTypedVars (Maybe e) where
    inferVarTypes mm = maybe (return []) (inferVarTypes mm)

instance HasTypedVars e => HasTypedVars [e] where
    inferVarTypes mm es = concat <$> traverse (inferVarTypes mm) es

instance HasTypedVars ExpDecl where
    inferVarTypes mm ex = liftEither $
        Map.toList <$> gInferTypes mm (expLetVarDecls ex)

instance HasTypedVars OfferDecl where
    inferVarTypes mm (OfferDecl cr os) = do
        chId <- mm .@!! (chanRefName cr, cr)
        let
            vds :: [Maybe (Loc VarDeclE, SortId)]
            vds = zipWith (\o sId -> ((, sId) . getLoc) <$> chanOfferIvarDecl o)
                          os
                          (chansorts chId)
        return $ catMaybes vds

sortIds :: (MapsTo Text SortId mm) => mm -> [OfSort] -> CompilerM [SortId]
sortIds mm xs = traverse (mm .@!!) $ zip (sortRefName <$> xs) (getLoc <$> xs)
