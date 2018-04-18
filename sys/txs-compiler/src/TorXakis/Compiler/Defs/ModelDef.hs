{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications          #-}
module TorXakis.Compiler.Defs.ModelDef where

import           Data.Text              (Text)
import qualified Data.Map               as Map
import           Data.Map               (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import           ChanId                 (ChanId)
import           TxsDefs                            (ModelDef (ModelDef))
import           VarId (VarId)
import           FuncId (FuncId)
import           FuncDef (FuncDef)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.BehExprDefs
import           TorXakis.Parser.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps

modelDeclToModelDef :: ( MapsTo Text ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                       , MapsTo FuncDefInfo FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False )
                    => mm -> ModelDecl -> CompilerM ModelDef
modelDeclToModelDef mm md = do
    ins  <- Set.fromList <$> traverse (`lookupM` mm) (chanRefName <$> modelIns md)
    outs <- Set.fromList <$> traverse (`lookupM` mm) (chanRefName <$> modelOuts md)
    let
        toChanIdSet :: Set ChanRef -> CompilerM (Set ChanId)
        toChanIdSet chs = Set.fromList <$>
            traverse (`lookupM` mm)
                     (chanRefName <$> Set.toList chs)
        allChIds :: [Set ChanId]
        allChIds = Set.singleton <$> values @Text mm
    syncs <- maybe (return allChIds)  
                   (traverse toChanIdSet)
                   (modelSyncs md)
    let mm' =  mm
            :& (Map.empty :: Map (Loc VarDeclE) VarId) -- No variables are declared in a model.
        insyncs  = filter (`Set.isSubsetOf` ins)  syncs
        outsyncs = filter (`Set.isSubsetOf` outs) syncs
        -- TODO: construct this, once you know the exit sort of the behavior expression `be`.
        -- splsyncs = ...
        -- errsyncs = ...
    be   <- toBExpr mm' (modelBExp md)
    return $ ModelDef insyncs outsyncs [] be
