{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications          #-}
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

adtsToTxsDefs :: ( MapsTo Text SortId mm
                 , HasCstrIds e)
              => mm -> e -> [ADTDecl] -> CompilerM TxsDefs
adtsToTxsDefs mm e ds = do
    lCstrDefs <- compileToCstrDefs mm e ds
    return $ empty
        { sortDefs = envToSortDefs mm
        , cstrDefs = lCstrDefs
        }

envToSortDefs :: ( MapsTo Text SortId mm )
              => mm -> Map SortId SortDef
envToSortDefs mm = Map.fromList $
    zip (values @Text mm) (repeat SortDef)

modelDeclsToTxsDefs :: ( MapsTo Text ChanId mm
                       , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                       )
                    => mm -> [ModelDecl] -> CompilerM TxsDefs
modelDeclsToTxsDefs mm mds = do
    mIds   <- traverse modelDeclToModelId  mds
    mDecls <- traverse (modelDeclToModelDef mm) mds
    return $ empty { modelDefs = Map.fromList $ zip mIds mDecls }
