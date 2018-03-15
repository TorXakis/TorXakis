-- | 

module TorXakis.Compiler.Sigs where

import           Sigs    (Sigs)
import           VarId   (VarId (VarId))
import           FuncTable (FuncTable (FuncTable))
import           TorXakis.Sort.ADTDefs (ADTDefs)

adtDefstoSigs :: ADTDefs -> Sigs VarId
-- > data Sigs v = Sigs  { chan :: [ChanId]
-- >                     , func :: FuncTable v
-- >                     , pro  :: [ProcId]
-- >                     , sort :: Map.Map Text SortId
-- >                     } 
-- >
adtDefstoSigs = undefined
