{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module Mapper

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Mapper
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( mapperMap   -- :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
              -- maps actions to actions
              -- if no mapper or error then idenity
              -- quiescence etc is always identity
, mapperMenu  -- menu of current mapper state
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State

import qualified Data.Set  as Set

import CoreUtils

import qualified EnvCore   as IOC
import qualified EnvData
import qualified Behave
import qualified BTree

import qualified TxsDefs
import qualified TxsDDefs

-- ----------------------------------------------------------------------------------------- --
-- mapper :  may only be called when in Testing or Simuling mode

mapperMap :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
mapperMap act@(TxsDDefs.Act acts)  =  do
     maybeMapperDef <- gets (IOC.mapperdef . IOC.state)
     mapSts         <- gets (IOC.mapsts . IOC.state)
     case (maybeMapperDef, mapSts) of
       ( Nothing, _  ) -> return act
       ( _      , [] ) -> return act
       ( Just (TxsDefs.MapperDef chins chouts syncs _), mtree) -> do
           let actchids = Set.map fst acts
               inchids  = Set.fromList chins
               outchids = Set.fromList chouts
               allmenu  = Behave.behMayMenu syncs mtree
               mapmenu  = [ ( btoffs
                            , hidvars
                            , TxsDefs.cstrAnd (Set.fromList (pred
                                                            : [ TxsDefs.cstrEqual (TxsDefs.cstrVar ivar)
                                                                                  (TxsDefs.cstrConst wal)
                                                              | BTree.CToffer chan choffs <- Set.toList btoffs
                                                              , (chid, wals)              <- Set.toList acts
                                                              , (ivar, wal)               <- zip choffs wals
                                                              , chan == chid
                                                              ]
                                                            )
                                               )
                            )
                          | (btoffs, hidvars, pred) <- allmenu
                          , actchids ==
                            Set.filter (`Set.member` inchids) (Set.map BTree.ctchan btoffs)
                          ]
           mact    <- randMenu mapmenu
           case mact of
             Nothing              -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Mapper: no mapper mapping" ]
                                        return act
             Just TxsDDefs.ActQui -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Mapper: quiescence in mapper" ]
                                        return act
             Just (TxsDDefs.Act macts) -> do
               envb           <- filterEnvCtoEnvB
               (maymt',envb') <- lift $ runStateT (Behave.behAfterAct syncs mtree macts) envb
               writeEnvBtoEnvC envb'
               case maymt' of
                Nothing  -> do
                   IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Mapper: no mapper after mapping" ]
                   return act
                Just mt' -> do
                   modify $ \env -> env { IOC.state = (IOC.state env) { IOC.mapsts = mt' } }
                   case ( filter ((`Set.member` inchids ).fst) (Set.toList macts)
                        , filter ((`Set.member` outchids).fst) (Set.toList macts)
                        ) of
                     ( []  , []    ) -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Mapper: something wrong" ]
                                           return act
                     ( []  , mouts ) -> return $ TxsDDefs.Act (Set.fromList mouts)
                     ( _, []    )    -> mapperMap $ TxsDDefs.Act Set.empty
                     ( _, mouts )    -> return $ TxsDDefs.Act (Set.fromList mouts)

mapperMap act@TxsDDefs.ActQui = return act


-- ----------------------------------------------------------------------------------------- --
-- mapperMenu :  menu of current mapper state

mapperMenu :: IOC.IOC BTree.Menu
mapperMenu = do
  maybeMapperDef <- gets (IOC.mapperdef . IOC.state)
  mapSts         <- gets (IOC.mapsts . IOC.state)
  case (maybeMapperDef, mapSts) of
    ( Nothing, _  ) -> return []
    ( _      , [] ) -> return []
    ( Just (TxsDefs.MapperDef _chins _chouts syncs _), mtree) ->
        return $ Behave.behMayMenu syncs mtree


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

