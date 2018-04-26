{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications          #-}
module TorXakis.Compiler.Defs.TxsDefs where

import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Text              (Text)

import           SortDef                           (SortDef (SortDef))
import           SortId                            (SortId)
import           TxsDefs                           (TxsDefs, cstrDefs,
                                                    modelDefs, sortDefs, ProcDef, ModelId, ModelDef, empty)
import           ChanId                 (ChanId)
import           VarId (VarId)
import           CstrId (CstrId)
import           FuncId (FuncId)
import           FuncDef (FuncDef)
import           ProcId (ProcId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ModelDef
import           TorXakis.Compiler.Defs.ModelId
import           TorXakis.Compiler.ValExpr.CstrDef
import           TorXakis.Parser.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps

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
                       , MapsTo Text ChanId mm
                       , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
                       , MapsTo FuncDefInfo FuncId mm
                       , MapsTo FuncId (FuncDef VarId) mm
                       , MapsTo ProcId ProcDef mm
                       , MapsTo (Loc VarDeclE) SortId mm
                       , MapsTo (Loc VarDeclE) VarId mm
                       , In (ProcId, ()) (Contents mm) ~ 'False )
                    => mm -> [ModelDecl] -> CompilerM (Map ModelId ModelDef)
modelDeclsToTxsDefs mm mds =
    Map.fromList <$> (zip <$> traverse modelDeclToModelId  mds
                          <*> traverse (modelDeclToModelDef mm') mds)
    where
      procIdsOnly :: Map ProcId ()
      procIdsOnly = Map.fromList $ zip (keys @ProcId @ProcDef mm) (repeat ())
      mm' = mm :& procIdsOnly
