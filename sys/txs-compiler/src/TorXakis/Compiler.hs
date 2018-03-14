module TorXakis.Compiler where

import qualified Data.Text     as T
import           Control.Arrow       ( (|||), left, right )
import           Control.Monad.State ( State, get, put )
    
import           Id      (Id)
import           TxsDefs (TxsDefs)
import           Sigs    (Sigs)
import           VarId   (VarId)
import           SortId  (SortId (SortId))
import           Id  (Id (Id))
import           TorXakis.Sort.ADTDefs (ADTDefs, addADTDefs, emptyADTDefs, getDefs)
import           TorXakis.Sort.Name (toText, getName)


import           TorXakis.Compiler.Error (Error)
import           TorXakis.Parser (ParsedDefs, adts)

-- | Compile a string into a TorXakis model.
--
-- TODO: for now we use String's to be able to leverage on the 'haskell' token
-- parser, in the future we might want to change this to text, and benchmark
-- what is the performance gain.
compile :: String -> Either Error (Id, TxsDefs, Sigs VarId)
compile = undefined

toTxsDefs :: ParsedDefs -> Either Error TxsDefs
toTxsDefs pd = right adtDefsToTxsDefs as
    where
      as :: Either Error ADTDefs
      as = left (T.pack . show) $ addADTDefs (adts pd) emptyADTDefs


-- TODO: This function should be placed in a temporary module to allow
-- transforming all the new definitions into the old ones. After we have smart
-- constructors for all the layers of TorXakis we can get rid of this.
adtDefsToTxsDefs :: ADTDefs -> TxsDefs
--
-- data  TxsDefs  =  TxsDefs { sortDefs   :: Map.Map SortId SortDef
--                           , cstrDefs   :: Map.Map CstrId CstrDef
--                           , funcDefs   :: Map.Map FuncId (FuncDef VarId)
--                           , varDefs    :: Map.Map VarId ()             -- MAYBE?
--                           }
--
-- TODO: wtf?
-- data  SortDef        = SortDef
adtDefsToTxsDefs = undefined

sortIds :: ADTDefs -> State Int [SortId]
-- data SortId = SortId
--     { name :: Name            -- capid
--     , unid :: Id
--     }
sortIds adfs  = do
    i <- get
    let ns = toText . getName <$> getDefs adfs
    put (i + length ns)
    return $ uncurry SortId <$> zip ns [Id j | j <- [i..]]
    
-- | Legacy compile function, used to comply with the old interface. It should
-- be deprecated in favor of @compile@.
compileLegacy :: String -> (Id, TxsDefs, Sigs VarId)
compileLegacy = (throwOnLeft ||| id) . compile 
    where throwOnLeft = error . show
