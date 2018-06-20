{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module TorXakis.Compiler.Defs.BehExprDefs where

import           Control.Monad                     (foldM, when)
import           Control.Monad.Error.Class         (liftEither, throwError)
import           Data.List                         (nub)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Semigroup                    ((<>))
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Traversable                  (for)
import           GHC.Exts                          (toList)

import           ChanId                            (ChanId (ChanId), chansorts,
                                                    name, unid)
import           ConstDefs                         (Const (Cbool))
import           FuncDef                           (FuncDef)
import           FuncId                            (FuncId)
import           FuncTable                         (Handler,
                                                    Signature (Signature))
import           ProcId                            (ExitSort (Exit), ProcId,
                                                    exitSortIds, procchans,
                                                    procvars)
import qualified ProcId
import           SortId                            (SortId, sortIdBool)
import           StdTDefs                          (chanIdExit, chanIdIstep)
import           TxsDefs                           (ActOffer (ActOffer), BExpr,
                                                    ChanOffer (Exclam, Quest),
                                                    Offer (Offer), ProcDef,
                                                    actionPref, chanid, choice,
                                                    disable, enable, guard,
                                                    hide, interrupt, parallel,
                                                    procInst, stop, valueEnv)
import           ValExpr                           (ValExpr, cstrConst)
import           VarId                             (VarId, varsort)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.VarRef
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.Common
import           TorXakis.Compiler.ValExpr.FuncDef
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

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
toBExpr _ vrvds Stop             = return stop
toBExpr mm vrvds (ActPref ao be) = actionPref <$> toActOffer mm vrvds ao <*> toBExpr mm vrvds be
toBExpr mm vrvds (LetBExp vss be) = do
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
               toText   n                     == ProcId.name pId
            && fmap chansorts (procchans pId) == fmap chansorts chIds -- Compare the sort id's of the channels
            && length (procvars pId)          == length exs
    -- Try to find a matching process definition:
    res <- forCatch (filter candidate $ keys @ProcId @() mm) $ \pId -> do
        let eSids = varsort <$> procvars pId
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
            , _errorMsg  = "No matching process definition found: "
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
    let fshs :: Map (Loc FuncDeclE) (Signature, Handler VarId)
        fshs = innerMap mm
        fss = fst <$> fshs
    eSids <- exitSortIds <$> exitSort (fss :& mm) be0
    ofrs' <- traverse (uncurry $ toChanOffer vrvds) $ zip eSids ofrs
    be1'  <- toBExpr mm vrvds be1
    return $ enable be0' ofrs' be1'
toBExpr mm vrvds (Enable _ be0 be1) = do
    be0' <- toBExpr mm vrvds be0
    let fshs :: Map (Loc FuncDeclE) (Signature, Handler VarId)
        fshs = innerMap mm
        fss = fst <$> fshs
    es   <- exitSort (fss :& mm) be0
    when (es /= Exit [])
        (throwError undefined) -- TODO: give the appropriate error message.
    be1' <- toBExpr mm vrvds be1
    return $ enable be0' [] be1'
toBExpr _ vrvds (Accept l _ _ )      =
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

vpair :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
      -> LetVarDecl
      -> CompilerM (VarId, ValExpr VarId)
vpair vrvds vd = do
    vId <- liftEither (varIdForRef vrvds (asVarReflLoc (getLoc vd)))
    ex  <- liftEither $
        expDeclToValExpr vrvds (varsort vId) (varDeclExp vd)
    return (vId, ex)

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
toActOffer mm vrvds (ActOfferDecl osd mc) = do
    os <- traverse (toOffer mm vrvds) osd
    c  <- fromMaybe (return . cstrConst . Cbool $ True)
                    (liftEither . expDeclToValExpr vrvds sortIdBool <$> mc)
    -- Filter the internal actions (to comply with the current TorXakis compiler).
    let os' = filter ((chanIdIstep /=) . chanid) os
    return $ ActOffer (Set.fromList os') Set.empty c

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
        ofrs <- traverse (uncurry (toChanOffer vrvds))
                     (zip (chansorts cId) cods)
        return $ Offer cId ofrs

toChanOffer :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
            -> SortId -- ^ Expected sort id's the offer
            -> ChanOfferDecl
            -> CompilerM ChanOffer
toChanOffer vrvds _ (QuestD vd) =
    Quest  <$> liftEither (varIdForRef vrvds (asVarReflLoc (getLoc vd)))
toChanOffer vrvds eSid (ExclD ex) = liftEither $
    Exclam <$> expDeclToValExpr vrvds eSid ex

