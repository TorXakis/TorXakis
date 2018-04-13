{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module TorXakis.Compiler.Defs.TxsDefs where

import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           SortDef                           (SortDef (SortDef))
import           SortId                            (SortId)
import           TxsDefs                           (TxsDefs, cstrDefs, empty,
                                                    modelDefs, sortDefs)
import           Data.Text              (Text)
import           ChanId                 (ChanId)
import           VarId (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ModelDef
import           TorXakis.Compiler.Defs.ModelId
import           TorXakis.Compiler.ValExpr.CstrDef
import           TorXakis.Parser.Data
import           TorXakis.Compiler.MapsTo

adtsToTxsDefs :: (HasCstrIds e, HasSortIds e)
              => e -> [ADTDecl] -> CompilerM TxsDefs
adtsToTxsDefs e ds = do
    lCstrDefs <- compileToCstrDefs e ds
    return $ empty
        { sortDefs = envToSortDefs e
        , cstrDefs = lCstrDefs
        }

envToSortDefs :: (HasCstrIds e, HasSortIds e)
              => e -> Map SortId SortDef
envToSortDefs e = Map.fromList $
    zip (allSortIds e) (repeat SortDef)

modelDeclsToTxsDefs :: ( MapsTo Text ChanId mm
                       , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                       )
                    => mm -> [ModelDecl] -> CompilerM TxsDefs
modelDeclsToTxsDefs mm mds = do
    mIds   <- traverse modelDeclToModelId  mds
    mDecls <- traverse (modelDeclToModelDef mm) mds
    return $ empty { modelDefs = Map.fromList $ zip mIds mDecls }
