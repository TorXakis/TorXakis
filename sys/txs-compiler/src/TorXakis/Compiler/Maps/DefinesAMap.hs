{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module TorXakis.Compiler.Maps.DefinesAMap where

import           Control.Lens                     ((^.))
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)

import           ChanId                           (ChanId (ChanId))
import           Id                               (Id (Id))
import           SortId                           (SortId)
import           StdTDefs                         (chanIdExit, chanIdHit,
                                                   chanIdHit, chanIdIstep,
                                                   chanIdMiss, chanIdQstep)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Parser.Data

-- | Expressions that define a map from 'k' to 'v', provided a map 'm' is
-- available.
class DefinesAMap k v e mm where
    getKVs :: mm -> e -> CompilerM [(k, v)]
    getMap :: Ord k => mm -> e -> CompilerM (Map k v)
    getMap mm = fmap Map.fromList . getKVs mm

instance (DefinesAMap k v e mm) => DefinesAMap k v [e] mm where
    getKVs mm es = concat <$> traverse (getKVs mm) es

instance (DefinesAMap k v e mm) => DefinesAMap k v (Maybe e) mm where
    getKVs mm = maybe (return []) (getKVs mm)

instance (DefinesAMap k v e mm) => DefinesAMap k v (Set e) mm where
    getKVs mm = getKVs mm . Set.toList

-- * Expressions that define a map from channel name to its declaration
instance DefinesAMap Text (Loc ChanDeclE) ChanDecl () where
    getKVs () cd = return [(chanDeclName cd, getLoc cd)]

-- | Predefined channels declarations.
predefChDecls :: Map Text (Loc ChanDeclE)
predefChDecls =  Map.fromList
    [ ("EXIT", exitChLoc)
    , ("ISTEP", istepChLoc)
    , ("QSTEP", qstepChLoc)
    , ("HIT", hitChLoc)
    , ("MISS", missChLoc)
    ]

exitChLoc :: Loc ChanDeclE
exitChLoc = PredefLoc "EXIT"

istepChLoc :: Loc ChanDeclE
istepChLoc = PredefLoc "ISTEP"

qstepChLoc :: Loc ChanDeclE
qstepChLoc = PredefLoc "QSTEP"

hitChLoc :: Loc ChanDeclE
hitChLoc = PredefLoc "HIT"

missChLoc :: Loc ChanDeclE
missChLoc = PredefLoc "MISS"


-- * Expressions that define a map from a channel reference to its declaration
instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ParsedDefs () where
    getKVs () pd = do
        cdMap <- getMap () (pd ^. chdecls) :: CompilerM (Map Text (Loc ChanDeclE))
        chDeclModels <- getKVs (predefChDecls <.+> cdMap) (pd ^. models)
        chDeclProcs <- getKVs () (pd ^. procs)
        return $  chDeclModels ++ chDeclProcs

-- | A model declaration relies on channels declared outside of its scope,
-- hence the need for a map from channel names to a location in which these
-- channels are declared.
instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ModelDecl mm where
    getKVs mm md = do
       -- We augment the map with the predefined channels.
       let mm' = predefChDecls <.+> mm
       ins   <- getKVs mm' (modelIns md)
       outs  <- getKVs mm' (modelOuts md)
       syncs <- getKVs mm' (modelSyncs md)
       bes   <- getKVs mm' (modelBExp md)
       return $ ins ++ outs ++ syncs ++ bes

instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ProcDecl () where
    getKVs () pd = do
        cdMap <- getMap () (procDeclChParams pd) :: CompilerM (Map Text (Loc ChanDeclE))
        getKVs (predefChDecls <.+> cdMap) (procDeclBody pd)

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) BExpDecl mm where
    getKVs _ Stop                = return []
    getKVs mm (ActPref aod be)   = (++) <$> getKVs mm aod <*> getKVs mm be
    getKVs mm (LetBExp _ be)     = getKVs mm be
    getKVs mm (Pappl _ _ crs _ ) = getKVs mm crs
    getKVs mm (Par _ son be0 be1) = (++) <$> getKVs mm son
                                         <*> ((++) <$> getKVs mm be0 <*> getKVs mm be1)
    getKVs mm (Enable _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Accept _ _ be) = getKVs mm be
    getKVs mm (Disable _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Interrupt _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Choice _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Guard _ be) = getKVs mm be
    getKVs mm (Hide _ cds be) = do
        cdMap <- getMap () cds :: CompilerM (Map Text (Loc ChanDeclE))
        getKVs (cdMap <.+> mm) be

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) =>  DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ActOfferDecl mm where
    getKVs mm (ActOfferDecl os _) = getKVs mm os

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) OfferDecl mm where
    getKVs mm (OfferDecl cr _) = getKVs mm cr

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ChanRef mm where
    getKVs mm cr = do
        loc <- mm .@!! (chanRefName cr, cr)
        return [(getLoc cr, loc)]

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) SyncOn mm where
    getKVs _ All           = return []
    getKVs mm (OnlyOn crs) = getKVs mm crs

-- * Expressions that introduce new channel id's.

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ParsedDefs mm where
    getKVs mm pd = do
        chIdsDecls <- getKVs mm (pd ^. chdecls)
        chIdsProcs <- getKVs mm (pd ^. procs)
        return $ chIdsDecls ++ chIdsProcs

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ChanDecl mm where
    getKVs mm cd = do
        chId   <- getNextId
        chSids <- traverse (mm .@!!) (chanDeclSorts cd)
        return [(getLoc cd, ChanId (chanDeclName cd) (Id chId) chSids)]

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ProcDecl mm where
    getKVs mm pd = do
        parChids <- getKVs mm (procDeclChParams pd)
        -- Add the exit sort.
        retChids <- getKVs mm (procDeclRetSort pd)
        -- Add the predefined channels.
        let predefChids = [ (exitChLoc, chanIdExit)
                          , (istepChLoc, chanIdIstep)
                          , (qstepChLoc, chanIdQstep)
                          ]
        -- Add the channels declared at the body.
        bodyChids <- getKVs mm (procDeclBody pd)
        return $ parChids ++ retChids ++ predefChids ++ bodyChids


instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ModelDecl mm where
    getKVs mm md = do
        -- Add the predefined channels.
        let predefChids = [ (exitChLoc, chanIdExit)
                          , (istepChLoc, chanIdIstep)
                          , (qstepChLoc, chanIdQstep)
                          ]
        -- Add the channels declared at the body.
        bodyChids <- getKVs mm (modelBExp md)
        return $ predefChids ++ bodyChids

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId BExpDecl mm where
    getKVs _ Stop                = return []
    getKVs mm (ActPref _ be)   = getKVs mm be
    getKVs mm (LetBExp _ be)     = getKVs mm be
    getKVs _ Pappl {} = return []
    getKVs mm (Par _ _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Enable _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Accept _ _ be) = getKVs mm be
    getKVs mm (Disable _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Interrupt _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Choice _ be0 be1) = (++) <$> getKVs mm be0 <*> getKVs mm be1
    getKVs mm (Guard _ be) = getKVs mm be
    getKVs mm (Hide _ cds be) = do
        cdMap <- getKVs mm cds :: CompilerM [(Loc ChanDeclE, ChanId)]
        beMap <- getKVs mm be
        return $ cdMap ++ beMap

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ExitSortDecl mm where
    getKVs _ NoExitD = return []
    getKVs mm (ExitD xs) = do
        chId  <- getNextId
        eSids <- sortIds mm xs
        return [(exitChLoc, ChanId "EXIT" (Id chId) eSids)]
    getKVs _ HitD = return [ (hitChLoc, chanIdHit)
                           , (missChLoc, chanIdMiss)
                           ]
