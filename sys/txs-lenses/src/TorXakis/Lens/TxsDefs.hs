{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- |
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module TorXakis.Lens.TxsDefs where

import           Data.List  (find)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Lens.Micro (Lens', SimpleGetter, to, (^.))

import           Name       (Name)
import           TxsDefs    (CnectDef, CnectId (CnectId), MapperDef,
                             MapperId (MapperId), ModelDef, ModelId (ModelId),
                             PurpDef, PurpId (PurpId), TxsDefs)
import qualified TxsDefs

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

purpDefs :: Lens' TxsDefs (Map PurpId PurpDef)
purpDefs h tdefs =
    (\pDefs' -> tdefs {TxsDefs.purpDefs = pDefs'}) <$> h (TxsDefs.purpDefs tdefs)

instance IndexedBy TxsDefs Name PurpDef where
    ix n = to lookupPurpByName
      where
        lookupPurpByName :: TxsDefs -> Maybe PurpDef
        lookupPurpByName defs =
            snd <$> find (eqPurpName . fst)
                        (Map.toList (defs ^. purpDefs))
          where
            eqPurpName :: PurpId -> Bool
            eqPurpName (PurpId pn _) = pn == n

cnectDefs :: Lens' TxsDefs (Map CnectId CnectDef)
cnectDefs h tdefs =
    (\cDefs' -> tdefs {TxsDefs.cnectDefs = cDefs'}) <$> h (TxsDefs.cnectDefs tdefs)

instance IndexedBy TxsDefs Name CnectDef where
    ix n = to lookupCnectByName
      where
        lookupCnectByName :: TxsDefs -> Maybe CnectDef
        lookupCnectByName defs =
            snd <$> find (eqCnectName . fst)
                        (Map.toList (defs ^. cnectDefs))
          where
            eqCnectName :: CnectId -> Bool
            eqCnectName (CnectId cn _) = cn == n

mapperDefs :: Lens' TxsDefs (Map MapperId MapperDef)
mapperDefs h tdefs =
    (\mpDefs' -> tdefs {TxsDefs.mapperDefs = mpDefs'}) <$> h (TxsDefs.mapperDefs tdefs)

instance IndexedBy TxsDefs Name MapperDef where
    ix n = to lookupMapperByName
      where
        lookupMapperByName :: TxsDefs -> Maybe MapperDef
        lookupMapperByName defs =
            snd <$> find (eqMapperName . fst)
                        (Map.toList (defs ^. mapperDefs))
          where
            eqMapperName :: MapperId -> Bool
            eqMapperName (MapperId mpn _) = mpn == n
