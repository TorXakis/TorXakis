-- |

module TorXakis.Compiler.Defs.TxsDefs where

import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           SortDef                           (SortDef (SortDef))
import           SortId                            (SortId)
import           TxsDefs                           (TxsDefs, cstrDefs, empty,
                                                    modelDefs, sortDefs)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Defs.ModelDef
import           TorXakis.Compiler.Defs.ModelId
import           TorXakis.Compiler.ValExpr.CstrDef
import           TorXakis.Parser.Data

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

modelDeclsToTxsDefs :: [ModelDecl] -> CompilerM TxsDefs
modelDeclsToTxsDefs mds = do
    mIds   <- traverse modelDeclToModelId  mds
    mDecls <- traverse modelDeclToModelDef mds
    return $ empty { modelDefs = Map.fromList $ zip mIds mDecls }
