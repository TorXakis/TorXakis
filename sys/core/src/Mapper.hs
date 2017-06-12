{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Mapper

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Mapper
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( mapperInit  -- :: IOC.IOC ()
, mapperMap   -- :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
              -- maps actions to actions
              -- if no mapper or error then idenity
              -- quiescence etc is always identity
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State

import qualified Data.Set  as Set
import qualified Data.Map  as Map

import CoreUtils

import qualified EnvCore   as IOC
import qualified EnvData   as EnvData
import qualified Behave    as Behave
import qualified BTree     as BTree

import qualified TxsDefs   as TxsDefs
import qualified TxsDDefs  as TxsDDefs
import qualified TxsShow   as TxsShow


-- ----------------------------------------------------------------------------------------- --
-- mapper initialization


mapperInit :: IOC.IOC ()

mapperInit  =  do
     mapperDef <- gets IOC.mapperdef
     case mapperDef of
     { TxsDefs.DefMapper (TxsDefs.MapperDef achins achouts asyncsets abexp)
         -> do envb           <- filterEnvCtoEnvB
               (maymt',envb') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb
               writeEnvBtoEnvC envb'
               modify $ \envc -> envc { IOC.mapsts = case maymt' of
                                                     { Nothing  -> Map.empty
                                                     ; Just mt' -> Map.singleton 0 mt'
                                      }              }
     ; _ -> do modify $ \envc -> envc { IOC.mapsts = Map.empty }
     }


-- ----------------------------------------------------------------------------------------- --
-- mapper


mapperMap :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action

mapperMap (TxsDDefs.Act acts)  =  do
     mapperDef <- gets IOC.mapperdef
     curState  <- gets IOC.curstate
     nexState  <- gets IOC.nexstate
     mapSts    <- gets IOC.mapsts
     case (mapperDef, Map.lookup curState mapSts) of
     { ( TxsDefs.DefNo                                             , _         ) -> do return $ TxsDDefs.Act acts
     ; (_                                                          , Nothing   ) -> do return $ TxsDDefs.Act acts
     ; ( TxsDefs.DefMapper (TxsDefs.MapperDef chins chouts syncs _), Just mtree) -> do
          let actchids = Set.map fst acts
          let inchids  = Set.fromList chins
          let outchids = Set.fromList chouts

          allmenu <- return $ Behave.behMayMenu syncs mtree
          mapmenu <- return $ [ ( btoffs
                                , hidvars
                                , preds ++
                                  [ TxsDefs.cstrEqual (TxsDefs.cstrVar ivar) (TxsDefs.cstrConst wal)
                                  | BTree.CToffer chan choffs <- Set.toList btoffs
                                  , (chid, wals)        <- Set.toList acts
                                  , (ivar, wal)         <- zip choffs wals
                                  , chan == chid
                                  ]
                                )
                              | (btoffs, hidvars, preds) <- allmenu
                              , actchids ==
                                Set.filter (`Set.member` inchids) (Set.map BTree.ctchan btoffs)
                              ]
          mact  <- randMenu mapmenu
          case mact of
          { Nothing              -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                     $ "Mapper: no mapper mapping" ]
                                       return $ TxsDDefs.Act acts
          ; Just TxsDDefs.ActQui -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                     $ "Mapper: quiescence in mapper" ]
                                       return $ TxsDDefs.Act acts
          ; Just (TxsDDefs.Act macts) -> do
              envb           <- filterEnvCtoEnvB
              (maymt',envb') <- lift $ runStateT (Behave.behAfterAct syncs mtree macts) envb
              writeEnvBtoEnvC envb'
              case maymt' of
              { Nothing  -> do
                  IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                $ "Mapper: no mapper after mapping" ]
                  return $ TxsDDefs.Act acts
              ; Just mt' -> do
                  modify $ \env -> env { IOC.mapsts = Map.insert nexState mt' mapSts }
                  case ( filter ((`Set.member` inchids ).fst) (Set.toList macts)
                       , filter ((`Set.member` outchids).fst) (Set.toList macts)
                       ) of
                  { ( []  , []    ) -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                        $ "Mapper: something wrong" ]
                                          return $ TxsDDefs.Act acts
                  ; ( []  , mouts ) -> do return $ TxsDDefs.Act (Set.fromList mouts)
                  ; ( mins, []    ) -> do mapperMap $ TxsDDefs.Act Set.empty
                  ; ( mins, mouts ) -> do return $ TxsDDefs.Act (Set.fromList mouts)
                  }
     }    }   }

mapperMap TxsDDefs.ActQui  =  do
     return $ TxsDDefs.ActQui
     

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

