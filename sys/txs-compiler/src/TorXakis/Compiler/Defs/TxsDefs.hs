{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module TorXakis.Compiler.Defs.TxsDefs where

import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Text                         (Text)

import           ChanId                            (ChanId)
import           CstrId                            (CstrId)
import           FuncDef                           (FuncDef)
import           FuncId                            (FuncId)
import           ProcId                            (ProcId)
import           SortDef                           (SortDef (SortDef))
import           SortId                            (SortId)
import           TxsDefs                           (ModelDef, ModelId, ProcDef,
                                                    TxsDefs, cstrDefs, empty,
                                                    modelDefs, sortDefs)
import           VarId                             (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ModelDef
import           TorXakis.Compiler.Defs.ModelId
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.CstrDef
import           TorXakis.Parser.Data

adtsToTxsDefs :: ( MapsTo Text SortId mm
                 , MapsTo (Loc CstrE) CstrId mm)
              => mm -> [ADTDecl] -> CompilerM TxsDefs
adtsToTxsDefs mm ds = do
    lCstrDefs <- compileToCstrDefs mm ds
    return $ empty
        { sortDefs = envToSortDefs mm
        , cstrDefs = lCstrDefs
        }

envToSortDefs :: ( MapsTo Text SortId mm )
              => mm -> Map SortId SortDef
envToSortDefs mm = Map.fromList $
    zip (values @Text mm) (repeat SortDef)

modelDeclsToTxsDefs :: ( MapsTo Text SortId mm
                       , MapsTo Text (Loc ChanDeclE) mm
                       -- , MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
                       , MapsTo (Loc ChanDeclE) ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [(Loc FuncDeclE)]) mm
                       , MapsTo (Loc FuncDeclE) FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , MapsTo ProcId ProcDef mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       -- , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                       , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                       , In (ProcId, ()) (Contents mm) ~ 'False )
                    => mm -> [ModelDecl] -> CompilerM (Map ModelId ModelDef)
modelDeclsToTxsDefs mm mds =
    Map.fromList <$> (zip <$> traverse modelDeclToModelId  mds
                          <*> traverse (modelDeclToModelDef mm') mds)
    where
      procIdsOnly :: Map ProcId ()
      procIdsOnly = Map.fromList $ zip (keys @ProcId @ProcDef mm) (repeat ())
      mm' = mm :& procIdsOnly
