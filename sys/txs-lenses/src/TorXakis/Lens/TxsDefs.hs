-- | 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
module TorXakis.Lens.TxsDefs where

import           Lens.Micro (SimpleGetter, Lens', to, (^.))
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.List  (find)

import           TxsDefs    (TxsDefs, ModelDef, ModelId (ModelId), PurpDef)
import qualified TxsDefs
import           Name       (Name)

-- | a can be indexed by b returning a c
class IndexedBy a b c where
    ix :: b -> SimpleGetter a (Maybe c)

modelDefs :: Lens' TxsDefs (Map ModelId ModelDef)
modelDefs h tdefs =
    (\mDefs' -> tdefs {TxsDefs.modelDefs = mDefs'}) <$> h (TxsDefs.modelDefs tdefs)

instance IndexedBy TxsDefs Name ModelDef where
    ix n = to lookupModelByName
        where
          lookupModelByName :: TxsDefs -> Maybe ModelDef
          lookupModelByName defs =
              snd <$> find (eqModelName . fst)
                           (Map.toList (defs ^. modelDefs ))
              where
                eqModelName :: ModelId -> Bool
                eqModelName (ModelId mn _) =  mn == n

instance IndexedBy TxsDefs Name PurpDef where
    ix = undefined
