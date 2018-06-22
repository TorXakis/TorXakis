{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Defs.ModelDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' model definitions.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Defs.ModelDef
    (modelDeclToModelDef, chRefsToChIdSet)
where

import           Control.Monad.Error.Class          (liftEither)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)

import           ChanId                             (ChanId)
import           FuncTable                          (Handler, Signature)
import           ProcId                             (ExitSort (Exit, NoExit),
                                                     ProcId)
import           SortId                             (SortId)
import           StdTDefs                           (chanIdExit)
import           TxsDefs                            (ModelDef (ModelDef))
import           VarId                              (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.BehExprDefs
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.Maps.VarRef
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Parser.Data

-- | Compile a model declaration into a model definition.
modelDeclToModelDef :: ( MapsTo Text SortId mm
                       , MapsTo Text (Loc ChanDeclE) mm -- Needed because channels are declared outside the model.
                       , MapsTo (Loc ChanDeclE) ChanId mm -- Also needed because channels are declared outside the model
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                       , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
                       , MapsTo ProcId () mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False
                       , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False )
                    => mm -> ModelDecl -> CompilerM ModelDef
modelDeclToModelDef mm md = do
    -- Map the channel references to the places in which they are declared.
    chDecls <- getMap mm md :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
    -- Add the channel declaration introduced by the hide operator.
    modelChIds <- getMap mm md :: CompilerM (Map (Loc ChanDeclE) ChanId)
    let mm' = chDecls :& (modelChIds <.+> mm)
    -- Infer the variable types of the expression:
    let fss = dropHandler (innerMap mm)
    bTypes <- Map.fromList <$> inferVarTypes (fss :& mm') (modelBExp md)
    bvIds  <- Map.fromList <$> mkVarIds bTypes (modelBExp md)
    let mm'' = bTypes <.+> (bvIds <.+> mm')
    evds <- liftEither $ varDefsFromExp mm'' md

    ins  <- Set.fromList <$> traverse (lookupChId mm') (getLoc <$> modelIns md)
    outs <- Set.fromList <$> traverse (lookupChId mm') (getLoc <$> modelOuts md)
    syncs <- maybe (return (usedChIds mm'))
                   (traverse (chRefsToChIdSet mm'))
                   (modelSyncs md)
    let
        insyncs  = filter (`Set.isSubsetOf` ins) syncs
        outsyncs = filter (`Set.isSubsetOf` outs) syncs
    be   <- toBExpr mm'' evds (modelBExp md)
    eSort <- exitSort (fss :& mm'') (modelBExp md)
    let
        splsyncs = case eSort of
            NoExit  -> []
            Exit [] -> [ Set.singleton chanIdExit ]
            _       -> [] -- NOTE: Ask jan, what should we return in this case? Error?

    return $ ModelDef insyncs outsyncs splsyncs be

-- | Compile a set of channel references to the set of channel id's they refer
-- to.
chRefsToChIdSet :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
                   , MapsTo (Loc ChanDeclE) ChanId mm )
                => mm -> Set ChanRef -> CompilerM (Set ChanId)
chRefsToChIdSet mm = fmap Set.fromList . chRefsToIds mm . Set.toList
