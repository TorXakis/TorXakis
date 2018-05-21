{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module TorXakis.Compiler.ValExpr.SortId where

import           Control.Arrow                    (left, (|||))
import           Control.Monad                    (foldM, when)
import           Control.Monad.Error.Class        (liftEither, throwError)
import           Data.Either                      (partitionEithers)
import           Data.List                        (intersect)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Traversable                 (for)
import           GHC.Exts                         (fromList)
import           Prelude                          hiding (lookup)

import           ChanId                           (ChanId, chansorts)
import           FuncId                           (FuncId, funcargs, funcsort)
import           Id                               (Id (Id))
import           ProcId                           (ExitSort (Exit, Hit, NoExit),
                                                   ProcId, exitSortIds,
                                                   procchans, procexit,
                                                   procvars)
import qualified ProcId
import           SortId                           (SortId (SortId), sortIdBool,
                                                   sortIdInt, sortIdRegex,
                                                   sortIdString)
import qualified SortId
import           TxsDefs                          (ProcDef)
import           VarId                            (varsort)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.Common
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
              , MapsTo (Loc FuncDeclE) FuncId mm
              , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm )
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
               , MapsTo (Loc FuncDeclE) FuncId mm
               , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm )
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
                    , MapsTo (Loc FuncDeclE) FuncId mm
                    , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm )
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
                 , MapsTo (Loc FuncDeclE) FuncId mm
                 , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm )
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
            =<< (lookup l mm :: Either Error (Either (Loc VarDeclE) [(Loc FuncDeclE)]))
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
                     , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
                     , MapsTo (Loc ChanDeclE) ChanId mm
                     , MapsTo (Loc VarDeclE) SortId mm
                     , MapsTo (Loc FuncDeclE) FuncId mm
                     , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm
                     , MapsTo ProcId () mm )
                  => mm -> e -> CompilerM [(Loc VarDeclE, SortId)]

instance HasTypedVars BExpDecl where
    inferVarTypes _ Stop =
        return []
    inferVarTypes mm (ActPref ao be) = do
        xs <- inferVarTypes mm ao
        -- The implicit variables in the offers are needed in subsequent expressions.
        ys <- inferVarTypes (Map.fromList xs <.+> mm) be
        return $ xs ++ ys
    inferVarTypes mm (LetBExp vs be) = do
        xs <- Map.toList <$> liftEither (gInferTypes mm vs)
        ys <- inferVarTypes mm be
        return $ xs ++ ys
    inferVarTypes mm (Pappl _ _ _ exs) =
        inferVarTypes mm exs
    inferVarTypes mm (Par _ _ be0 be1) =
        (++) <$> inferVarTypes mm be0 <*> inferVarTypes mm be1
    inferVarTypes mm (Enable _ be0 (Accept _ ofrs be1)) = do
        xs <- inferVarTypes mm be0
        es <- exitSort (xs <.++> mm) be0
        let sIds = exitSortIds es
        when (length ofrs /= length sIds)
            (error $  "Exit sorts and offers don't match:\n"
                  ++ "Offers: " ++ show ofrs
                  ++ "Exit sorts: " ++ show sIds)
            -- (throwError undefined) -- TODO: give the appropriate error message here
        let ofrs' = addType <$> zip sIds ofrs
        ys <- inferVarTypes mm ofrs'
        zs <- inferVarTypes (Map.fromList ys <.+> mm) be1
        return $ xs ++ ys ++ zs
        where
          -- Add the sort id's to the list of offers
          addType :: (SortId , ChanOfferDecl) -> ChanOfferDecl
          addType (sId, QuestD ivd) =
              QuestD $ mkIVarDecl (varName ivd)
                                  (getLoc ivd)
                                  (Just $ mkOfSort (SortId.name sId) undefined) -- TODO: do we use the same location here as the variable declaration 'vid'?
          addType (_, excl) = excl
    inferVarTypes mm (Enable _ be0 be1) =
        (++) <$> inferVarTypes mm be0 <*> inferVarTypes mm be1
    -- The enable operator has to take care of handle the `Accept` constructor.
    -- If 'ACCEPT' does not follow an enable operator then an error will be
    -- thrown.
    inferVarTypes _ (Accept l _ _)     =
        throwError Error
            { _errorType = ParseError
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "ACCEPT cannot be used here."
            }
    inferVarTypes mm (Disable _ be0 be1) =
        (++) <$> inferVarTypes mm be0 <*> inferVarTypes mm be1
    inferVarTypes mm (Interrupt _ be0 be1) =
        (++) <$> inferVarTypes mm be0 <*> inferVarTypes mm be1
    inferVarTypes mm (Choice _ be0 be1) =
        (++) <$> inferVarTypes mm be0 <*> inferVarTypes mm be1
    inferVarTypes mm (Guard ex be) =
        (++) <$> inferVarTypes mm ex <*> inferVarTypes mm be
    inferVarTypes mm (Hide _ _ be) =
        inferVarTypes mm be

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
        chId <- lookupChId mm (getLoc cr)
        -- Collect the variable declarations to @SortId@ maps from the output
        -- offers of the form 'Ch ! exp'.
        exclVds <- inferVarTypes mm os
        let
            -- Variables declared by the question offer.
            vds :: [Maybe (Loc VarDeclE, SortId)]
            vds = zipWith (\o sId -> ((, sId) . getLoc) <$> chanOfferIvarDecl o)
                          os
                          (chansorts chId)
        return $ catMaybes vds ++ exclVds

instance HasTypedVars ChanOfferDecl where
    -- We don't have the @SortId@ of the variable, so we cannot know its type
    -- at this level. Refer to the 'instance HasTypedVars OfferDecl' to see how
    -- this is handled.
    inferVarTypes mm (QuestD vd) = case ivarDeclSort vd of
        Nothing -> return []
        Just sr ->
            pure . (getLoc vd, ) <$> (mm .@!! (sortRefName sr, getLoc sr))
    inferVarTypes mm (ExclD ex)         = inferVarTypes mm ex

sortIds :: (MapsTo Text SortId mm) => mm -> [OfSort] -> CompilerM [SortId]
sortIds mm xs = traverse (mm .@!!) $ zip (sortRefName <$> xs) (getLoc <$> xs)

-- | The expression has exit sorts associated to it.
class HasExitSorts e where
    -- | Obtain the exit sorts for an expression.
    exitSort :: ( MapsTo Text SortId mm
                , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
                , MapsTo (Loc ChanDeclE) ChanId mm
                , MapsTo ProcId () mm
                , MapsTo (Loc VarDeclE) SortId mm
                , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm
                , MapsTo (Loc FuncDeclE) FuncId mm
                )
             => mm -> e -> CompilerM ExitSort

instance HasExitSorts BExpDecl where
    exitSort _ Stop = return NoExit
    exitSort mm (ActPref aos be) = do
        es0 <- exitSort mm aos
        es1 <- exitSort mm be
        es0 <<+>> es1 -- TODO: modify `ActPref` to include a location then we can use <!!> l
    exitSort mm (LetBExp _ be) = exitSort mm be
    exitSort mm (Pappl n l crs exps) = do
        chIds <- chRefsToIds mm crs
        -- Cartesian product of all the possible sorts that can be inferred:
        expsSidss <- sequence <$> liftEither (traverse (inferExpTypes mm) exps)
        let candidate :: ProcId -> Bool
            candidate pId =
                   toText   n                     == ProcId.name pId
                && fmap chansorts (procchans pId) == fmap chansorts chIds -- Compare the sort id's of the channels
                && fmap varsort (procvars pId )  `elem` expsSidss
        case filter candidate $ keys @ProcId @() mm of
            [pId] -> return $ procexit pId
            []    -> throwError Error
                { _errorType = ProcessNotDefined
                , _errorLoc  = getErrorLoc l
                , _errorMsg  = "No matching process found"
                }
            _     -> throwError Error
                { _errorType = MultipleDefinitions
                , _errorLoc  = getErrorLoc l
                , _errorMsg  = "Multiple matching processes found"
                }
    exitSort mm (Par l _ be0 be1) = do
        es0 <- exitSort mm be0
        es1 <- exitSort mm be1
        (es0 <<->> es1) <!!> l
    exitSort mm (Enable _ _ be) = exitSort mm be
    exitSort mm (Accept _ _ be) = exitSort mm be
    exitSort mm (Disable l be0 be1) = do
        es0 <- exitSort mm be0
        es1 <- exitSort mm be1
        (es0 <<+>> es1) <!!> l
    exitSort mm (Interrupt l be0 be1) = do
        es1 <- exitSort mm be1
        when (es1 /= Exit [])
            (error $ "\nTXS2233: " ++ show l ++ ". Exit sorts do not match in Interrupt\n") -- TODO: throwError
        exitSort mm be0
    exitSort mm (Choice l be0 be1) = do
        es0 <- exitSort mm be0
        es1 <- exitSort mm be1
        (es0 <<+>> es1) <!!> l
    exitSort mm (Guard _ be) = exitSort mm be
    exitSort mm (Hide _ _ be) =
        exitSort mm be

instance HasExitSorts ActOfferDecl where
    exitSort mm (ActOfferDecl os _) =
        exitSort mm os

instance HasExitSorts e => HasExitSorts [e] where
    exitSort mm exps = do
        es <- traverse (exitSort mm) exps
        foldM (<<+>>) NoExit es

instance HasExitSorts OfferDecl where
    exitSort mm (OfferDecl cr ofrs) = case chanRefName cr of
        "EXIT"  -> do
            sIds <- traverse (offerSid mm) ofrs
            return $ Exit sIds
        "ISTEP" -> return NoExit
        "QSTEP" -> return Hit
        "HIT"   -> return Hit
        "MISS"  -> return Hit
        _       -> return NoExit


-- | Combine exit sorts for choice, disable: max of exit sorts
(<<+>>) :: ExitSort -> ExitSort -> CompilerM ExitSort
NoExit   <<+>> NoExit    = return NoExit
NoExit   <<+>> Exit exs  = return $ Exit exs
NoExit   <<+>> Hit       = return Hit
Exit exs <<+>> NoExit    = return $ Exit exs
Exit exs <<+>> Exit exs' = do
    when (exs /= exs')
         (throwError undefined) -- TODO:"\nTXS2222: Exit sorts do not match\n"
    return (Exit exs)
Exit _   <<+>> Hit       = throwError undefined -- TODO: "\nTXS2223: Exit sorts do not match\n"
Hit      <<+>> NoExit    = return Hit
Hit      <<+>> Exit _    = throwError undefined -- TODO: "\nTXS2224: Exit sorts do not match\n"
Hit      <<+>> Hit       = return Hit


-- | Combine exit sorts for parallel: min of exit sorts
(<<->>) :: ExitSort -> ExitSort -> CompilerM ExitSort
NoExit   <<->> NoExit    = return NoExit
NoExit   <<->> Exit _    = return NoExit
NoExit   <<->> Hit       = return NoExit
Exit _   <<->> NoExit    = return NoExit
Exit exs <<->> Exit exs' = do
    when (exs /= exs')
        (error "\nTXS2222: Exit sorts do not match\n")
--         (throwError undefined) -- TODO:"\nTXS2222: Exit sorts do not match\n"
    return (Exit exs)
Exit _   <<->> Hit       = throwError undefined -- TODO: "\nTXS2223: Exit sorts do not match\n"
Hit      <<->> NoExit    = return NoExit
Hit      <<->> Exit _    = throwError undefined -- TODO: "\nTXS2224: Exit sorts do not match\n"
Hit      <<->> Hit       = return Hit

offerSid :: ( MapsTo Text SortId mm
            , MapsTo (Loc VarDeclE) SortId mm
            , MapsTo (Loc FuncDeclE) FuncId mm
            , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm )
         => mm -> ChanOfferDecl -> CompilerM SortId
offerSid mm (QuestD vd) = case ivarDeclSort vd of
    Nothing -> error $ "No sort for offer variable" ++ show vd  -- TODO: throw the appropriate error message
    Just sr -> mm .@!! (sortRefName sr, sr)
offerSid mm (ExclD ex) = case inferExpTypes mm ex of
    Left err    -> throwError err
    Right []    -> error $ "No matching sort for " ++ show ex
    Right [sId] -> return sId
    Right xs    -> error $  "Found multiple matching sorts for " ++ show ex
                         ++ ": " ++ show xs -- TODO: throwError

instance HasTypedVars Transition where
    inferVarTypes mm (Transition _ ofr _ _) = inferVarTypes mm ofr

