{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Maps.DefinesAMap
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Expressions that define a map.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Maps.DefinesAMap
    ( DefinesAMap
    , getKVs
    , getKs
    , getMap
    , uGetKVs
    , predefChDecls
    )
where

import           Control.Lens                     ((^.), (^..))
import           Control.Monad.Except             (throwError)
import           Data.Data                        (Data)
import           Data.Data.Lens                   (biplate)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)


import           BehExprDefs                      (chanIdExit, chanIdHit,
                                                   chanIdHit, chanIdIstep,
                                                   chanIdMiss, chanIdQstep)
import           ChanId                           (ChanId (ChanId))
import           Id                               (Id (Id))
import           SortId                           (SortId)

import           TorXakis.Compiler.Data           (CompilerM, getNextId)
import           TorXakis.Compiler.Error          (Error (Error),
                                                   ErrorLoc (NoErrorLoc),
                                                   ErrorType (CompilerPanic),
                                                   _errorLoc, _errorMsg,
                                                   _errorType)
import           TorXakis.Compiler.Maps           ((.@!!))
import           TorXakis.Compiler.MapsTo         (MapsTo, (<.+>))
import           TorXakis.Compiler.ValExpr.SortId (sortIds)
import           TorXakis.Parser.Data             (ActOfferDecl (ActOfferDecl), BExpDecl (Accept, ActPref, Choice, Disable, Enable, Guard, Hide, Interrupt, LetBExp, Pappl, Par, Stop),
                                                   ChanDecl, ChanDeclE, ChanRef,
                                                   ChanRefE, CnectDecl,
                                                   CnectItem,
                                                   CnectItem (CnectItem),
                                                   CodecItem (CodecItem),
                                                   ExitSortDecl (ExitD, HitD, NoExitD),
                                                   ExpDecl, Loc (PredefLoc),
                                                   MapperDecl, ModelDecl,
                                                   OfferDecl (OfferDecl),
                                                   ParLetVarDecl, ParsedDefs,
                                                   ProcDecl, PurpDecl,
                                                   StautDecl,
                                                   SyncOn (All, OnlyOn),
                                                   TestGoalDecl, VarDecl,
                                                   chanDeclName, chanDeclSorts,
                                                   chanRefName, chdecls,
                                                   cnectDeclCnectItems,
                                                   cnectDeclCodecs, getLoc,
                                                   mapperBExp, mapperIns,
                                                   mapperOuts, mapperSyncs,
                                                   modelBExp, modelIns,
                                                   modelOuts, modelSyncs,
                                                   models, procDeclBody,
                                                   procDeclChParams,
                                                   procDeclRetSort, procs,
                                                   purpDeclGoals, purpDeclIns,
                                                   purpDeclOuts, purpDeclSyncs,
                                                   stautDeclChParams,
                                                   stautDeclRetSort,
                                                   testGoalDeclBExp)

-- | Abstract syntax tree types that define a map.
--
-- 'DefinesAMap k v e mm' models the fact that an AST 'e' defines a map from
-- 'k' to 'v', provided a map 'mm' is available.
class (Show k, Ord k, Eq k) => DefinesAMap k v e mm where
    -- | Get the 'k' 'v' pairs defined by the ast 'e'.
    getKVs :: mm -> e -> CompilerM [(k, v)]
    getKVs mm e = do
        kvs <- uGetKVs mm e
        let ks = getKs @k @v @e @mm e
        -- The key value pairs 'kvs' might contain keys not found in 'e' in the
        -- case of pre-defined entities, hence the need for checking set
        -- inclusion instead of set equality.
        if Set.fromList ks `Set.isSubsetOf` Set.fromList (fmap fst kvs)
            then return kvs
            else throwError Error
                 { _errorType = CompilerPanic
                 , _errorLoc  = NoErrorLoc
                 , _errorMsg  = "Not all the keys where mapped: \n"
                                <> "    - expected: " <> T.pack (show ks) <> "\n"
                                <> "    - but got: " <> T.pack (show $ fmap fst kvs)

                 }
    -- | Get the dictionary of 'k' 'v' pairs defined by the ast 'e'.
    getMap :: Ord k => mm -> e -> CompilerM (Map k v)
    getMap mm = fmap Map.fromList . getKVs mm
    -- | Get the list of keys defined in 'e'. This is used for invariant
    -- checking.
    getKs :: e -> [k]
    default getKs :: (Data e, Typeable k) => e -> [k]
    getKs md = md ^.. biplate
    -- | Get the 'k' 'v' pairs defined by the ast 'e', without checking for the
    -- invariant. Unchecked version of 'getKVs'.
    uGetKVs :: mm -> e -> CompilerM [(k, v)]

instance (Data e, Typeable k, DefinesAMap k v e mm) => DefinesAMap k v [e] mm where
    uGetKVs mm es = concat <$> traverse (uGetKVs mm) es
    -- We don't want to use the default implementation here, since 'e' might
    -- have defined its own 'getKs' function.
    getKs = concatMap (getKs @k @v @e @mm)

instance (Data e, Typeable k, DefinesAMap k v e mm) => DefinesAMap k v (Maybe e) mm where
    uGetKVs mm = maybe (return []) (uGetKVs mm)
    -- We don't want to use the default implementation here, since 'e' might
    -- have defined its own 'getKs' function.
    getKs = maybe [] (getKs @k @v @e @mm)

instance (Data e, Typeable k, Ord e, DefinesAMap k v e mm) => DefinesAMap k v (Set e) mm where
    uGetKVs mm = uGetKVs mm . Set.toList
    getKs = concatMap (getKs @k @v @e @mm) . Set.toList

-- * Expressions that define a map from channel name to its declaration
instance DefinesAMap Text (Loc ChanDeclE) ChanDecl () where
    uGetKVs () cd = return [(chanDeclName cd, getLoc cd)]
    getKs cd = [chanDeclName cd]

-- | Predefined channels declarations.
predefChDecls :: Map Text (Loc ChanDeclE)
predefChDecls =  Map.fromList
    [ ("EXIT", exitChLoc)
    , ("ISTEP", istepChLoc)
    , ("QSTEP", qstepChLoc)
    , ("HIT", hitChLoc)
    , ("MISS", missChLoc)
    ]

-- | Predefined locations for channels start at 10000.
exitChLoc :: Loc ChanDeclE
exitChLoc = PredefLoc "EXIT" 10000

istepChLoc :: Loc ChanDeclE
istepChLoc = PredefLoc "ISTEP" 10001

qstepChLoc :: Loc ChanDeclE
qstepChLoc = PredefLoc "QSTEP" 10002

hitChLoc :: Loc ChanDeclE
hitChLoc = PredefLoc "HIT" 10003

missChLoc :: Loc ChanDeclE
missChLoc = PredefLoc "MISS" 10004

-- * Expressions that define a map from a channel reference to its declaration
instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ParsedDefs () where
    uGetKVs () pd = do
        cdMap <- getMap () (pd ^. chdecls) :: CompilerM (Map Text (Loc ChanDeclE))
        chDeclModels <- uGetKVs (predefChDecls <.+> cdMap) (pd ^. models)
        chDeclProcs <- uGetKVs () (pd ^. procs)
        return $  chDeclModels ++ chDeclProcs

-- | A model declaration relies on channels declared outside of its scope,
-- hence the need for a map from channel names to a location in which these
-- channels are declared.
instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ModelDecl mm where
    uGetKVs mm md = do
       -- We augment the map with the predefined channels.
       let mm' = predefChDecls <.+> mm
       ins   <- uGetKVs mm' (modelIns md)
       outs  <- uGetKVs mm' (modelOuts md)
       syncs <- uGetKVs mm' (modelSyncs md)
       bes   <- uGetKVs mm' (modelBExp md)
       return $ ins ++ outs ++ syncs ++ bes

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) PurpDecl mm where
    uGetKVs mm pd = do
       -- We augment the map with the predefined channels.
       let mm' = predefChDecls <.+> mm
       ins   <- uGetKVs mm' (purpDeclIns pd)
       outs  <- uGetKVs mm' (purpDeclOuts pd)
       syncs <- uGetKVs mm' (purpDeclSyncs pd)
       gls   <- uGetKVs mm' (purpDeclGoals pd)
       return $ ins ++ outs ++ syncs ++ gls

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) MapperDecl mm where
    uGetKVs mm md = do
       -- We augment the map with the predefined channels.
       let mm' = predefChDecls <.+> mm
       ins   <- uGetKVs mm' (mapperIns md)
       outs  <- uGetKVs mm' (mapperOuts md)
       syncs <- uGetKVs mm' (mapperSyncs md)
       bes   <- uGetKVs mm' (mapperBExp md)
       return $ ins ++ outs ++ syncs ++ bes

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap  (Loc ChanRefE) (Loc ChanDeclE) CnectDecl mm where
    uGetKVs mm cd = (++) <$> uGetKVs mm (cnectDeclCnectItems cd)
                         <*> uGetKVs mm (cnectDeclCodecs cd)

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap  (Loc ChanRefE) (Loc ChanDeclE) CnectItem mm where
    uGetKVs mm (CnectItem cr _ _ _) = uGetKVs mm cr

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap  (Loc ChanRefE) (Loc ChanDeclE) CodecItem mm where
    uGetKVs mm (CodecItem offr _ _) = uGetKVs mm offr

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) TestGoalDecl mm where
    uGetKVs mm gd = uGetKVs mm (testGoalDeclBExp gd)

instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ProcDecl () where
    uGetKVs () pd = do
        cdMap <- getMap () (procDeclChParams pd) :: CompilerM (Map Text (Loc ChanDeclE))
        uGetKVs (predefChDecls <.+> cdMap) (procDeclBody pd)

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) BExpDecl mm where
    uGetKVs _ Stop                = return []
    uGetKVs mm (ActPref _ aod be) = (++) <$> uGetKVs mm aod <*> uGetKVs mm be
    uGetKVs mm (LetBExp _ be)     = uGetKVs mm be
    uGetKVs mm (Pappl _ _ crs _ ) = uGetKVs mm crs
    uGetKVs mm (Par _ son be0 be1) = (++) <$> uGetKVs mm son
                                         <*> ((++) <$> uGetKVs mm be0 <*> uGetKVs mm be1)
    uGetKVs mm (Enable _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Accept _ _ be) = uGetKVs mm be
    uGetKVs mm (Disable _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Interrupt _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Choice _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Guard _ be) = uGetKVs mm be
    uGetKVs mm (Hide _ cds be) = do
        cdMap <- getMap () cds :: CompilerM (Map Text (Loc ChanDeclE))
        uGetKVs (cdMap <.+> mm) be

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) =>  DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ActOfferDecl mm where
    uGetKVs mm (ActOfferDecl os _) = uGetKVs mm os

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) OfferDecl mm where
    uGetKVs mm (OfferDecl cr _) = uGetKVs mm cr

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ChanRef mm where
    uGetKVs mm cr = do
        loc <- mm .@!! (chanRefName cr, cr)
        return [(getLoc cr, loc)]

instance ( MapsTo Text (Loc ChanDeclE) mm
         ) => DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) SyncOn mm where
    uGetKVs _ All           = return []
    uGetKVs mm (OnlyOn crs) = uGetKVs mm crs

-- * Expressions that introduce new channel id's.
instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ChanDecl mm where
    uGetKVs mm cd = do
        chId   <- getNextId
        chSids <- traverse (mm .@!!) (chanDeclSorts cd)
        return [(getLoc cd, ChanId (chanDeclName cd) (Id chId) chSids)]

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ProcDecl mm where
    uGetKVs mm pd = do
        parChids <- uGetKVs mm (procDeclChParams pd)
        -- Add the exit sort.
        retChids <- uGetKVs mm (procDeclRetSort pd)
        -- Add the predefined channels.
        let predefChids = [ (exitChLoc, chanIdExit)
                          , (istepChLoc, chanIdIstep)
                          , (qstepChLoc, chanIdQstep)
                          ]
        -- Add the channels declared at the body.
        bodyChids <- uGetKVs mm (procDeclBody pd)
        return $ parChids ++ retChids ++ predefChids ++ bodyChids

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId StautDecl mm where
    uGetKVs mm pd = do
        parChids <- uGetKVs mm (stautDeclChParams pd)
        -- Add the exit sort.
        retChids <- uGetKVs mm (stautDeclRetSort pd)
        -- Add the predefined channels.
        let predefChids = [ (exitChLoc, chanIdExit)
                          , (istepChLoc, chanIdIstep)
                          , (qstepChLoc, chanIdQstep)
                          ]
        -- Add the channels declared at the body.
        -- A state-automaton does not declare channels in its body.
        return $ parChids ++ retChids ++ predefChids

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ModelDecl mm where
    uGetKVs mm md = do
        bodyChIds <- uGetKVs mm (modelBExp md)
        return $ predefChIds ++ bodyChIds

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId MapperDecl mm where
    uGetKVs mm md = do
        bodyChIds <- uGetKVs mm (mapperBExp md)
        return $ predefChIds ++ bodyChIds

-- Predefined channels.
predefChIds :: [(Loc ChanDeclE, ChanId)]
predefChIds = [ (exitChLoc, chanIdExit)
              , (istepChLoc, chanIdIstep)
              , (qstepChLoc, chanIdQstep)
              , (hitChLoc, chanIdHit)
              , (missChLoc, chanIdMiss)
              ]

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId PurpDecl mm where
    uGetKVs mm pd = do
        goalsChIds <- uGetKVs mm (purpDeclGoals pd)
        return $ predefChIds ++ goalsChIds

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId TestGoalDecl mm where
    uGetKVs mm gd = uGetKVs mm (testGoalDeclBExp gd)

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId BExpDecl mm where
    uGetKVs _ Stop                = return []
    uGetKVs mm (ActPref _ _ be)   = uGetKVs mm be
    uGetKVs mm (LetBExp _ be)     = uGetKVs mm be
    uGetKVs _ Pappl {} = return []
    uGetKVs mm (Par _ _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Enable _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Accept _ _ be) = uGetKVs mm be
    uGetKVs mm (Disable _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Interrupt _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Choice _ be0 be1) = (++) <$> uGetKVs mm be0 <*> uGetKVs mm be1
    uGetKVs mm (Guard _ be) = uGetKVs mm be
    uGetKVs mm (Hide _ cds be) = do
        cdMap <- uGetKVs mm cds :: CompilerM [(Loc ChanDeclE, ChanId)]
        beMap <- uGetKVs mm be
        return $ cdMap ++ beMap

instance ( MapsTo Text SortId mm
         ) => DefinesAMap (Loc ChanDeclE) ChanId ExitSortDecl mm where
    uGetKVs _ NoExitD = return []
    uGetKVs mm (ExitD xs) = do
        chId  <- getNextId
        eSids <- sortIds mm xs
        return [(exitChLoc, ChanId "EXIT" (Id chId) eSids)]
    uGetKVs _ HitD = return [ (hitChLoc, chanIdHit)
                           , (missChLoc, chanIdMiss)
                           ]

-- | Bogus instance for @ExpDecl@ to be able to use @subCompile@.
--
-- A value expression declaration @ExpDecl@ has no channels, so @uGetKVs@
-- returns the empty list.
instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ExpDecl mm where
    uGetKVs _ _ = return []

-- | Bogus instance for @ParLetVarDecl@ to be able to use @subCompile@.
--
-- A value expression declaration @ParLetVarDecl@ has no channels.
instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ParLetVarDecl mm where
    uGetKVs _ _ = return []

-- | Bogus instance for @VarDecl@ to be able to use @subCompile@.
--
instance DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) VarDecl mm where
    uGetKVs _ _ = return []
