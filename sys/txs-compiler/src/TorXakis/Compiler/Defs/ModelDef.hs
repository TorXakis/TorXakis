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

import           ChanId                 (ChanId, unid)
import           TxsDefs                            (ModelDef (ModelDef), ProcDef)
import           VarId (VarId)
import           FuncId (FuncId)
import           FuncDef (FuncDef)
import           ProcId (ProcId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.BehExprDefs
import           TorXakis.Parser.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Defs.ChanId

modelDeclToModelDef :: ( MapsTo Text ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                       , MapsTo FuncDefInfo FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , MapsTo ProcId ProcDef mm
                       , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False )
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
    let mm' =  mm
            :& (Map.empty :: Map (Loc VarDeclE) VarId) -- No variables are declared in a model.
        insyncs  = filter (`Set.isSubsetOf` ins) syncs
        outsyncs = filter (`Set.isSubsetOf` outs) syncs
        -- TODO: construct this, once you know the exit sort of the behavior expression `be`.
        -- splsyncs = ...
        -- errsyncs = ...
    be   <- toBExpr mm' (modelBExp md)
    return $ ModelDef insyncs outsyncs [] be
