{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Lib.SocketWorld
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Connections with the SUT using sockets.
--------------------------------------------------------------------------------
module TorXakis.Lib.SocketWorld
    ( initSocketWorld
    , closeSockets
    )
where

import           Debug.Trace

import           Control.Concurrent           (forkIO, killThread)
import           Control.Concurrent.Async     (async, mapConcurrently, wait)
import           Control.Concurrent.STM.TChan (TChan, writeTChan)
import           Control.Monad                (forever)
import           Control.Monad.State          (liftIO)
import           Control.Monad.STM            (atomically)
import           Data.Either                  (partitionEithers)
import           Data.List                    (nub)
import           Data.Map                     (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromMaybe)
import           Data.Semigroup               ((<>))
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Network.TextViaSockets       (Connection)
import qualified Network.TextViaSockets       as TVS

import           ConstDefs                    (Const (Cstring))
import qualified TxsCore                      as Core
import           TxsDDefs                     (Action (Act), ConnHandle (..))
import           TxsDefs                      (CnectDef (..), CnectType (..),
                                               ConnDef (..))
import qualified TxsDefs
import           ValExpr                      (cstrConst, subst)
import qualified VarId

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session

-- | Initialize the connections with the SUT.
initSocketWorld :: Session
                -> TChan Action -- ^ Channel where the actions received from the SUT will be placed.
                -> CnectDef     -- ^ Description about how to connect with the SUT.
                -> IO WorldConnDef
initSocketWorld s fWCh cdef@(CnectDef _ cds) = do
    hp2conn <- mkConnections cdef
    let towhdls  = map (connDefToConnHandle hp2conn) (filter isConnDtoW cds)
        frowhdls = map (connDefToConnHandle hp2conn) (filter isConnDfroW cds)
    -- Start the threads that listen for messages from the SUT.
    frowThreads         <- traverse (forkIO . fromWorldThread s fWCh) frowhdls
    let wcdPairs = zip (map TxsDDefs.chan towhdls)
                       $ map (ToWorldMapping . sendToSocket s) towhdls
    return $ WorldConnDef towhdls
                          (Map.fromList wcdPairs)
                          frowhdls
                          frowThreads
    where
      -- | Connect to all the hosts and ports specified in the given connect def, and
      -- return a mapping from (host, port) to the connection associated to it.
      mkConnections :: CnectDef -> IO (Map (Host, Port) Connection)
      mkConnections (CnectDef t ds) =  Map.fromList . zip hps <$>
          mapConcurrently (mkConnection t) hps
          where hp c = (Host . hostname $ c, Port . portnr $ c)
                hps = nub $ fmap hp ds

      mkConnection :: CnectType -> (Host, Port) -> IO Connection
      mkConnection ClientSocket (Host h, Port p) =
          TVS.connectTo (T.unpack h) (show p)
      mkConnection ServerSocket (_, Port p) =
          TVS.acceptOn (fromInteger p)

      isConnDtoW :: ConnDef -> Bool
      isConnDtoW ConnDtoW {}  = True
      isConnDtoW ConnDfroW {} = False

      isConnDfroW :: ConnDef -> Bool
      isConnDfroW ConnDtoW {}  = False
      isConnDfroW ConnDfroW {} = True

      -- | Convert a connection definition to a connection handle, given the map from
      -- (host, port) to their corresponding connection.
      connDefToConnHandle :: Map (Host, Port) Connection -> ConnDef -> ConnHandle
      connDefToConnHandle hp2c cd =
          case cd of
              ConnDtoW  chid h p vs e  -> ConnHtoW  chid (c h p) vs e
              ConnDfroW chid h p v  es -> ConnHfroW chid (c h p) v  es
          where c h p = fromMaybe (err h p)
                    $ Map.lookup (Host h, Port p) hp2c
                -- A connection should be always found. If not, then there's a
                -- bug in this module!
                err h p =  error $ "Panic: could not find a connection for ("
                    ++ show h ++ ", " ++ show p ++ ")"

newtype Host = Host Text deriving (Eq, Ord)
newtype Port = Port Integer deriving (Eq, Ord)

sendToSocket :: Session -> ConnHandle -> [Const] -> IO (Response (Maybe Action))
sendToSocket _ ConnHfroW{} _ = return $ Left "Shouldn't send to socket FROM world."
sendToSocket s h@ConnHtoW{connection = c} consts = do
    rTxt <- encode s h consts
    case rTxt of
        Right txt -> do
            liftIO $ TVS.putLineTo c txt
            return $ Right Nothing
        Left  err -> return $ Left err

encode ::  Session -> ConnHandle -> [Const] -> IO (Response Text)
encode _ ConnHfroW{} _ = error "Shouldn't try to encode for socket connection FROM world."
encode s (ConnHtoW _ _ vIds vExpr) consts = do
    let wenv = Map.fromList $ zip vIds consts
    fDfs <- runReadOnlyIOC s $ TxsDefs.funcDefs <$> Core.txsGetTDefs
    let substRes = subst (Map.map cstrConst wenv) fDfs vExpr
    mval <- runResponse $ runReadOnlyIOCE s $ Core.txsEval substRes
    return $ case mval of
        Right (Cstring txt) -> Right txt
        Right _             -> Left "Encode 3: No encoding to String\n"
        Left  err           -> Left $ "Encode 3: No encoding to String\n" <> err

fromWorldThread :: Session -> TChan Action -> ConnHandle -> IO ()
fromWorldThread _ _ ConnHtoW{} = error "Shouldn't try to listen to socket connection TO world."
fromWorldThread s frowchan h@ConnHfroW{connection = c} = forever $ do
    t <- TVS.getLineFrom c
    rAction <- decode s h t
    case rAction of
        Right action -> atomically $ writeTChan frowchan action
        Left  e      -> print e

decode :: Session -> ConnHandle -> Text -> IO (Response Action)
decode _ ConnHtoW{} _ = error "Shouldn't try to decode from socket connection TO world."
decode s (ConnHfroW cId _ vId vExprs) txt = do
    let senv = Map.fromList [ (vId, cstrConst (Cstring txt)) ]
    fDfs <- runReadOnlyIOC s $ TxsDefs.funcDefs <$> Core.txsGetTDefs
    mwals <- mapM (runResponse . runReadOnlyIOCE s . Core.txsEval . subst senv fDfs) vExprs
    return $ case partitionEithers mwals of
        ([], wals) -> Right $ Act ( Set.singleton (cId,wals) )
        (es, _)    -> Left $ T.intercalate "\n" $ "Decode: eval failed":es

closeSockets :: WorldConnDef -> IO ()
closeSockets (WorldConnDef toWConnHdls _ froWConnHdls froWThreadIds) = do
    mapM_ (TVS.close . connection) (toWConnHdls ++ froWConnHdls)
    mapM_ killThread froWThreadIds
