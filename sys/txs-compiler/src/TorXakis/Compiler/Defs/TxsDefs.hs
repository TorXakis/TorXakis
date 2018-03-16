-- | 

module TorXakis.Compiler.Defs.TxsDefs where

import           Data.Map (Map)
import qualified Data.Map as Map

import           TxsDefs (TxsDefs, empty, sortDefs, cstrDefs)
import           SortId (SortId)
import           SortDef (SortDef (SortDef))
    
import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.CstrDef

adtsToTxsDefs :: Env -> [ADTDecl] -> CompilerM TxsDefs
adtsToTxsDefs e ds = do
    lCstrDefs <- compileToCstrDefs e ds
    return $ empty
        { sortDefs = envToSortDefs e
        , cstrDefs = lCstrDefs
        }

-- TODO: put this in a sort package.
envToSortDefs :: Env -> Map SortId SortDef    
envToSortDefs e = Map.fromList $
    zip (Map.elems. sortsMap $ e) (repeat SortDef)
