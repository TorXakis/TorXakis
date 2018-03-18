-- | 

module TorXakis.Compiler.Defs.TxsDefs where

import           Data.Map (Map)
import qualified Data.Map as Map

import           TxsDefs (TxsDefs, empty, sortDefs, cstrDefs, funcDefs)
import           SortId (SortId)
import           SortDef (SortDef (SortDef))
    
import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.ValExpr.CstrDef
import           TorXakis.Compiler.ValExpr.FuncDef
    
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
