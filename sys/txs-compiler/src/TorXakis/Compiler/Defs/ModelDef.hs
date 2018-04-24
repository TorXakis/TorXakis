{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications          #-}
module TorXakis.Compiler.Defs.ModelDef where

import           Data.Text              (Text)
import qualified Data.Map               as Map
import           Data.Map               (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.List (sortBy)
import           Data.Ord (compare)

import           ChanId                 (ChanId, unid, name)
import           TxsDefs                            (ModelDef (ModelDef), ProcDef)
import           VarId (VarId)
import           FuncId (FuncId)
import           FuncDef (FuncDef)
import           ProcId (ProcId)
import           SortId (SortId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.BehExprDefs
import           TorXakis.Parser.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Defs.ChanId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Compiler.ValExpr.SortId

modelDeclToModelDef :: ( MapsTo Text SortId mm
                       , MapsTo Text ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                       , MapsTo FuncDefInfo FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , MapsTo ProcId ProcDef mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm )
                    => mm -> ModelDecl -> CompilerM ModelDef
modelDeclToModelDef mm md = do
    ins  <- Set.fromList <$> traverse (`lookupM` mm) (chanRefName <$> modelIns md)
    outs <- Set.fromList <$> traverse (`lookupM` mm) (chanRefName <$> modelOuts md)
    let
        allChIds :: [Set ChanId]
        allChIds = Set.singleton <$> sortByUnid (values @Text mm)
        -- Sort the channels by its id, since we have to comply with the current TorXakis compiler.
        sortByUnid :: [ChanId] -> [ChanId]
        sortByUnid = sortBy cmpChUnid
            where
              cmpChUnid c0 c1 = unid c0 `compare` unid c1
    syncs <- maybe (return allChIds)  
                   (traverse (chRefsToChIdSet mm))
                   (modelSyncs md)
    let
        insyncs  = filter (`Set.isSubsetOf` ins) syncs
        outsyncs = filter (`Set.isSubsetOf` outs) syncs
        -- TODO: construct this, once you know the exit sort of the behavior expression `be`.
        -- splsyncs = ...
        -- errsyncs = ...
    -- Infer the variable types of the expression:
    bvSids <- Map.fromList <$> inferVarTypes mm (modelBExp md)
    bTypes <- Map.fromList <$> inferVarTypes (bvSids <.+> mm) (modelBExp md)
    bvIds  <- Map.fromList <$> mkVarIds bTypes (modelBExp md)
    let
        chanIds :: Map Text ChanId
        chanIds = innerMap mm
        -- Only the model channels are accessible when constructing the behavior expression.
        modelChans = Set.union (Set.map name ins) (Set.map name outs)
        mm' = replaceInnerMap mm (Map.restrictKeys chanIds modelChans)
    be   <- toBExpr (bvSids <.+> (bvIds <.+> mm')) (modelBExp md)
    return $ ModelDef insyncs outsyncs [] be
