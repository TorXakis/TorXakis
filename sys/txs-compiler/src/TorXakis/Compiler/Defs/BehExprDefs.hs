{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Defs.BehExprDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' behavior expressions.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Defs.BehExprDefs
    ( toBExpr
    , toOffer
    , toActOffer
    )
where

import           Control.Monad                     (foldM, when)
import           Control.Monad.Except              (liftEither, throwError)
import           Data.List                         (nub)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           GHC.Exts                          (toList)

import           BehExprDefs                       (chanIdExit, chanIdIstep)
import           ChanId                            (ChanId, chansorts)
import           Constant                          (Constant (Cbool))
import           FuncTable                         (Handler, Signature)
import           ProcId                            (ChanSort (ChanSort),
                                                    ExitSort (Exit), ProcId,
                                                    exitSortIds, procchans,
                                                    procvars)
import qualified ProcId
import           SortId                            (SortId, sortIdBool)
import           TxsDefs                           (ActOffer (ActOffer), BExpr,
                                                    ChanOffer (Exclam, Quest),
                                                    Offer (Offer), actionPref,
                                                    chanid, choice, disable,
                                                    enable, guard, hide,
                                                    interrupt, parallel,
                                                    procInst, stop, valueEnv)
import           ValExpr                           (ValExpr, cstrConst)
import           VarId                             (VarId, varsort)

import           TorXakis.Compiler.Data            (CompilerM, forCatch)
import           TorXakis.Compiler.Error           (Entity (Process, Channel),
                                                    Error (Error),
                                                    ErrorType (MultipleDefinitions, NoDefinition, ParseError, TypeMismatch),
                                                    ErrorLoc (NoErrorLoc),
                                                    getErrorLoc, _errorLoc,
                                                    _errorMsg, _errorType)
import           TorXakis.Compiler.Maps            (chRefsToIds, dropHandler,
                                                    lookupChId, usedChIdMap,
                                                    (.@@))
import           TorXakis.Compiler.Maps.VarRef     (varIdForRef)
import           TorXakis.Compiler.MapsTo          ((:&) ((:&)), Contents, In,
                                                    MapsTo, innerMap, keys)
import           TorXakis.Compiler.ValExpr.SortId  (exitSort, offerSid)
import           TorXakis.Compiler.ValExpr.ValExpr (expDeclToValExpr)
import           TorXakis.Parser.Data              (ActOfferDecl (ActOfferDecl), BExpDecl (Accept, ActPref, Choice, Disable, Enable, Guard, Hide, Interrupt, LetBExp, Pappl, Par, Stop),
                                                    ChanDeclE,
                                                    ChanOfferDecl (ExclD, QuestD),
                                                    chanRefOfOfferDecl,
                                                    ChanRefE, FuncDeclE,
                                                    LetVarDecl, Loc,
                                                    OfferDecl (OfferDecl),
                                                    SyncOn (All, OnlyOn),
                                                    VarDeclE, VarRefE,
                                                    asVarReflLoc, chanRefName,
                                                    getLoc, toText, varDeclExp)

-- | Compile a behavior expression declaration into a behavior expression.
toBExpr :: ( MapsTo Text SortId mm
           , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
           , MapsTo (Loc VarDeclE) SortId mm
           , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
           , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
           , MapsTo (Loc ChanDeclE) ChanId mm
           , MapsTo ProcId () mm
           , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False )
        => mm
        -> Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
        -> BExpDecl
        -> CompilerM BExpr
toBExpr _ _ Stop               = return stop
toBExpr mm vrvds (ActPref _ ao be) = actionPref <$> toActOffer mm vrvds ao <*> toBExpr mm vrvds be
toBExpr mm vrvds (LetBExp vss be)  = do
    be0 <- toBExpr mm vrvds be
    foldM letToBExpr be0 (toList <$> vss)
    where
      letToBExpr be' vs = do
          venv <- Map.fromList <$> traverse (vpair vrvds) vs
          return $ valueEnv venv be'
toBExpr mm vrvds (Pappl n l crs exs) = do
    chIds <- chRefsToIds mm crs
    let candidate :: ProcId -> Bool
        candidate pId =
               toText   n            == ProcId.name pId
            && procchans pId         == fmap (ChanSort . chansorts) chIds -- Compare the sort id's of the channels
            && length (procvars pId) == length exs
    -- Try to find a matching process definition:
    res <- forCatch (filter candidate $ keys @ProcId @() mm) $ \pId -> do
        let eSids = procvars pId
        vExps <- liftEither $
            traverse (uncurry $ expDeclToValExpr vrvds) $ zip eSids exs
        return (pId, procInst pId chIds vExps)
    case res of
        (_ , [(_, pInst)]) -> return pInst
        (_, r:rs )         -> throwError Error
            { _errorType = MultipleDefinitions Process
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "Multiple matching definitions for process instantiation: "
                         <> T.pack (show (r:rs))
            }
        (ls, _)             -> throwError Error
            { _errorType = NoDefinition
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "No matching process definition found for '" <> toText n <> "[..](..)': "
                         <> T.pack (show ls)
            }
toBExpr mm vrvds (Par _ sOn be0 be1) = do
    be0' <- toBExpr mm vrvds be0
    be1' <- toBExpr mm vrvds be1
    cIds <- case sOn of
            All         ->
                return $ nub . Map.elems $ usedChIdMap mm
            OnlyOn crfs ->
                traverse (lookupChId mm) (getLoc <$> crfs)
    return $ parallel (Set.fromList $ chanIdExit:cIds) [be0', be1']
toBExpr mm vrvds (Enable _ be0 (Accept _ ofrs be1)) = do
    be0'  <- toBExpr mm vrvds be0
    let fss = dropHandler (innerMap mm)
    eSids <- exitSortIds <$> exitSort (fss :& mm) be0
    ofrs' <- traverse (uncurry $ toChanOffer vrvds) $ zip eSids ofrs
    be1'  <- toBExpr mm vrvds be1
    return $ enable be0' ofrs' be1'
toBExpr mm vrvds (Enable l be0 be1) = do
    be0' <- toBExpr mm vrvds be0
    let fss = dropHandler (innerMap mm)
    es   <- exitSort (fss :& mm) be0
    when (es /= Exit [])
        (throwError Error
            { _errorType = TypeMismatch
            , _errorLoc = getErrorLoc l
            , _errorMsg = "The exit sort of an enable expression without accept must be Exit []."
            }
        )
    be1' <- toBExpr mm vrvds be1
    return $ enable be0' [] be1'
toBExpr _ _ (Accept l _ _ )      =
    throwError Error
    { _errorType = ParseError
    , _errorLoc  = getErrorLoc l
    , _errorMsg  = "ACCEPT cannot be used here."
    }
toBExpr mm vrvds (Disable _ be0 be1) = do
    be0' <- toBExpr mm vrvds be0
    be1' <- toBExpr mm vrvds be1
    return $ disable be0' be1'
toBExpr mm vrvds (Interrupt _ be0 be1) = do
    be0' <- toBExpr mm vrvds be0
    be1' <- toBExpr mm vrvds be1
    return $ interrupt be0' be1'
toBExpr mm vrvds (Choice _ be0 be1) = do
    be0' <- toBExpr mm vrvds be0
    be1' <- toBExpr mm vrvds be1
    return $ choice (Set.fromList [be0', be1'])
toBExpr mm vrvds (Guard g be) = do
    g'  <- liftEither $ expDeclToValExpr vrvds sortIdBool g
    be' <- toBExpr mm vrvds be
    return $ guard g' be'
toBExpr mm vrvds (Hide _ cds be) = do
    chNameChIds <- traverse (mm .@@) (getLoc <$> cds) :: CompilerM [ChanId]
    be' <- toBExpr mm vrvds be
    return $ hide (Set.fromList chNameChIds) be'

-- | Compile a let var declaration into a par of @VarId@ and a value
-- expression.
vpair :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
      -> LetVarDecl
      -> CompilerM (VarId, ValExpr VarId)
vpair vrvds vd = do
    vId <- liftEither (varIdForRef vrvds (asVarReflLoc (getLoc vd)))
    ex  <- liftEither $
        expDeclToValExpr vrvds (varsort vId) (varDeclExp vd)
    return (vId, ex)

-- | Compile an action offer declaration into an action offer.
toActOffer :: ( MapsTo Text SortId mm
              , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
              , MapsTo (Loc ChanDeclE) ChanId mm
              , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
              , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
              , MapsTo (Loc VarDeclE) SortId mm
              , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False )
           => mm
           -> Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
           -> ActOfferDecl
           -> CompilerM ActOffer
toActOffer mm vrvds (ActOfferDecl osd mc) =
    let chanRefList :: [Text]
        chanRefList = map (chanRefName . chanRefOfOfferDecl) osd
        chanRefSet :: Set.Set Text
        chanRefSet = Set.fromList chanRefList
    in
        if Set.size chanRefSet == length chanRefList
        then do
            os <- traverse (toOffer mm vrvds) osd
            c  <- maybe (return . cstrConst . Cbool $ True)
                        (liftEither . expDeclToValExpr vrvds sortIdBool)
                        mc
            -- Filter the internal actions (to comply with the current TorXakis compiler).
            let os' = filter ((chanIdIstep /=) . chanid) os
            return $ ActOffer (Set.fromList os') Set.empty c
        else
            throwError Error
                        { _errorType = MultipleDefinitions Channel
                        , _errorLoc  = NoErrorLoc
                        , _errorMsg  = "List of Channels that occur multiple times in ActOffer: " <> T.pack (show (findDuplicates chanRefSet chanRefList))
                        }
    where
        -- find the Duplicates
        -- alternative implementation, make multiset and use elements with occurrence > 1
        findDuplicates :: Ord a => Set.Set a -> [a] -> Set.Set a
        findDuplicates _ []                        = Set.empty
        findDuplicates s l      | Set.null s       = Set.fromList l
        findDuplicates s (x:xs) | x `Set.member` s = findDuplicates (Set.delete x s) xs
                                | otherwise        = Set.insert x (findDuplicates s xs)

-- | Compile a list offer declarations on a channel into a list of offer
-- declarations.
toOffer :: ( MapsTo Text SortId mm
           , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
           , MapsTo (Loc VarDeclE) SortId mm
           , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
           , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
           , MapsTo (Loc ChanDeclE) ChanId mm
           , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False )
        => mm
        -> Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
        -> OfferDecl
        -> CompilerM Offer
-- EXIT is a special channel that can be use anywhere in a behavior
-- expression and doesn't have to be declared. For instance:
--
-- >  X ? v >-> EXIT >>> EXIT ! "Boom" >>> ACCEPT ? str IN X ! str NI
--
-- so we have to treat this channel specially.
toOffer mm vrvds (OfferDecl cr cods) = case chanRefName cr of
    "EXIT" -> do
        let fshs :: Map (Loc FuncDeclE) (Signature, Handler VarId)
            fshs = innerMap mm
            fss = fst <$> fshs
        eSids <- traverse (offerSid (fss :& mm)) cods
        ofrs  <- traverse (uncurry (toChanOffer vrvds))
                     (zip eSids cods)
        return $ Offer chanIdExit ofrs
    _      -> do
        cId  <- lookupChId mm (getLoc cr)
        let definedLength = length (chansorts cId)
            actualLength = length cods
          in if definedLength == actualLength
                then Offer cId <$> traverse (uncurry (toChanOffer vrvds))
                                            (zip (chansorts cId) cods)
                else throwError Error
                        { _errorType = TypeMismatch
                        , _errorLoc  = getErrorLoc cr
                        , _errorMsg  = "Mismatch in defined (" <> T.pack (show definedLength) 
                                           <> ") and actual (" <> T.pack (show actualLength) <> ") channel parameters."
                        }

-- | Compile a channel offer declaration into a channel offer.
toChanOffer :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
            -> SortId -- ^ Expected sort id's the offer
            -> ChanOfferDecl
            -> CompilerM ChanOffer
toChanOffer vrvds _ (QuestD vd) =
    Quest  <$> liftEither (varIdForRef vrvds (asVarReflLoc (getLoc vd)))
toChanOffer vrvds eSid (ExclD ex) = liftEither $
    Exclam <$> expDeclToValExpr vrvds eSid ex
