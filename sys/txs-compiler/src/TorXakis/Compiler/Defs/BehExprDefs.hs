{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module TorXakis.Compiler.Defs.BehExprDefs where

import qualified Data.Map as Map
import           Control.Monad.Error.Class         (liftEither, throwError)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Set                          as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Data.Semigroup ((<>))
import           Control.Monad (when)
import           Debug.Trace 

import           StdTDefs (chanIdIstep, chanIdExit)
import           ConstDefs                         (Const (Cbool))
import           SortId                            (sortIdBool, SortId)
import           TxsDefs                           (ActOffer (ActOffer), BExpr, ChanOffer (Quest, Exclam),
                                                    Offer (Offer), chanid, actionPref, stop, valueEnv, procInst,
                                                    ProcDef, parallel, enable, disable, interrupt, guard,
                                                    choice, hide)
import           ChanId (ChanId (ChanId), chansorts, name, unid)
import           VarId (VarId, varsort)
import           FuncId (FuncId)
import           FuncDef (FuncDef)
import           ValExpr (ValExpr, cstrConst)
import           ProcId (ProcId, procvars, procchans, exitSortIds, ExitSort (Exit))
import qualified ProcId

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Parser.Data
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.Common

toBExpr :: ( MapsTo Text SortId mm
           , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
           , MapsTo (Loc ChanDeclE) ChanId mm
           , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
           , MapsTo (Loc VarDeclE) VarId mm
           , MapsTo FuncDefInfo FuncId mm
           , MapsTo FuncId (FuncDef VarId) mm
           , MapsTo ProcId () mm
           , MapsTo (Loc VarDeclE) SortId mm )
        => mm -> BExpDecl -> CompilerM BExpr
toBExpr _ Stop             = return stop
toBExpr mm (ActPref ao be) = actionPref <$> toActOffer mm ao <*> toBExpr mm be
toBExpr mm (LetBExp vs be) = valueEnv   <$> venv <*> toBExpr mm be
    where
      venv = Map.fromList <$> traverse vpair vs
      vpair :: LetVarDecl -> CompilerM (VarId, ValExpr VarId)
      vpair vd = do
          vId <- mm .@ getLoc vd
          ex  <- liftEither $
              expDeclToValExpr mm (varsort vId) (varDeclExp vd)
          return (vId, ex)
toBExpr mm (Pappl n l crs exs) = do
    chIds <- chRefsToIds mm crs
    let candidate :: ProcId -> Bool
        candidate pId =
               toText   n                     == ProcId.name pId
            && fmap chansorts (procchans pId) == fmap chansorts chIds -- Compare the sort id's of the channels
    -- Try to find a matching process definition:
    res <- forCatch (filter candidate $ keys @ProcId @() mm) $ \pId -> do
        let eSids = varsort <$> procvars pId
        vExps <- liftEither $
            traverse (uncurry $ expDeclToValExpr mm) $ zip eSids exs
        return (pId, procInst pId chIds vExps)
    case res of
        (_ , [(_, pInst)]) -> return pInst
        (_, r:rs )         -> throwError Error
            { _errorType = MultipleDefinitions
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
toBExpr mm (Par _ sOn be0 be1) = do
    be0' <- toBExpr mm be0
    be1' <- toBExpr mm be1
    cIds <- case sOn of
            All         ->
                return $ values @(Loc ChanDeclE) mm
            OnlyOn crfs ->
                traverse (lookupChId mm) (getLoc <$> crfs)
    return $ parallel (chanIdExit:cIds) [be0', be1']
toBExpr mm (Enable _ be0 (Accept _ ofrs be1)) = do
    be0'  <- toBExpr mm be0
    eSids <- exitSortIds <$> exitSort mm be0
    ofrs' <- traverse (uncurry $ toChanOffer mm) $ zip eSids ofrs
    be1'  <- toBExpr mm be1
    return $ enable be0' ofrs' be1'
toBExpr mm (Enable _ be0 be1) = do
    be0' <- toBExpr mm be0
    es   <- exitSort mm be0
    when (es /= Exit [])
        (throwError undefined) -- TODO: give the appropriate error message.
    be1' <- toBExpr mm be1
    return $ enable be0' [] be1'
toBExpr _ (Accept l _ _ )      =
    throwError Error
    { _errorType = ParseError
    , _errorLoc  = getErrorLoc l
    , _errorMsg  = "ACCEPT cannot be used here."
    }
toBExpr mm (Disable _ be0 be1) = do
    be0' <- toBExpr mm be0
    be1' <- toBExpr mm be1
    return $ disable be0' be1'
toBExpr mm (Interrupt _ be0 be1) = do
    be0' <- toBExpr mm be0
    be1' <- toBExpr mm be1
    return $ interrupt be0' be1'
toBExpr mm (Choice _ be0 be1) = do
    be0' <- toBExpr mm be0
    be1' <- toBExpr mm be1
    return $ choice [be0', be1']
toBExpr mm (Guard g be) = do
    g'  <- liftEither $ expDeclToValExpr mm sortIdBool g
    be' <- toBExpr mm be
    return $ guard g' be'
toBExpr mm (Hide _ cds be) = do
    chNameChIds <- traverse (mm .@) (getLoc <$> cds) :: CompilerM [ChanId]
    be' <- toBExpr mm be
    return $ hide chNameChIds be'
    
toActOffer :: ( MapsTo Text SortId mm
              , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
              , MapsTo (Loc ChanDeclE) ChanId mm
              , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
              , MapsTo (Loc VarDeclE) VarId mm
              , MapsTo FuncDefInfo FuncId mm
              , MapsTo FuncId (FuncDef VarId) mm
              , MapsTo (Loc VarDeclE) SortId mm )
           => mm -> ActOfferDecl -> CompilerM ActOffer
toActOffer mm (ActOfferDecl osd mc) = do
    os <- traverse (toOffer mm) osd
    c  <- fromMaybe (return . cstrConst . Cbool $ True)
                    (liftEither . expDeclToValExpr mm sortIdBool <$> mc)
    -- Filter the internal actions (to comply with the current TorXakis compiler).
    let os' = filter ((chanIdIstep /=) . chanid) os
    return $ ActOffer (Set.fromList os') Set.empty c

toOffer :: ( MapsTo Text SortId mm
           , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
           , MapsTo (Loc ChanDeclE) ChanId mm
           , MapsTo (Loc VarDeclE) VarId mm
           , MapsTo (Loc VarDeclE) SortId mm
           , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
           , MapsTo (Loc VarDeclE) VarId mm
           , MapsTo FuncDefInfo FuncId mm
           , MapsTo FuncId (FuncDef VarId) mm )
        => mm -> OfferDecl -> CompilerM Offer
-- EXIT is a special channel that can be use anywhere in a behavior
-- expression and doesn't have to be declared. For instance:
--
-- >  X ? v >-> EXIT >>> EXIT ! "Boom" >>> ACCEPT ? str IN X ! str NI
--
-- so we have to treat this channel specially.        
toOffer mm (OfferDecl cr cods) = case chanRefName cr of
    "EXIT" -> do
        eSids <- traverse (offerSid mm) cods
        ofrs  <- traverse (uncurry (toChanOffer mm))
                     (zip eSids cods)
        return $ Offer chanIdExit ofrs
    _      -> do
        cId  <- lookupChId mm (getLoc cr)
        ofrs <- traverse (uncurry (toChanOffer mm))
                     (zip (chansorts cId) cods)
        return $ Offer cId ofrs


toChanOffer :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
               , MapsTo (Loc VarDeclE) VarId mm
               , MapsTo FuncDefInfo FuncId mm
               , MapsTo (Loc VarDeclE) SortId mm
               , MapsTo FuncId (FuncDef VarId) mm )
            => mm
            -> SortId -- ^ Expected sort id's the offer
            -> ChanOfferDecl
            -> CompilerM ChanOffer
toChanOffer mm _ (QuestD vd) =
    Quest  <$> mm .@ getLoc vd
toChanOffer mm eSid (ExclD ex) = liftEither $
    Exclam <$> expDeclToValExpr mm eSid ex
