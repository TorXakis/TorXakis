{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Tester where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.Async      (async, mapConcurrently, wait)
import           Control.Concurrent.STM.TChan  (TChan, writeTChan)
import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar   (readTVarIO)
import           Control.Exception             (try)
import           Control.Monad                 (void)
import           Control.Monad.State           (lift)
import           Control.Monad.STM             (atomically)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Name                          (Name)
import           Network.TextViaSockets        (Connection)
import qualified Network.TextViaSockets        as TVS

import qualified TxsCore                       as Core
import           TxsDDefs                      (Action, ConnHandle (..),
                                                SAction (..))
import           TxsDefs                       (CnectDef (..), CnectType (..),
                                                ConnDef (..))
import qualified TxsDefs

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Internal
import           TorXakis.Lib.Session

-- | Start the tester
setTest :: Session
        -> Name
        -> Name
        -> IO (Response ())
setTest s mdlNm cnctNm = runResponse $ do
    mDef <- lookupModel s mdlNm
    st <- lift $ readTVarIO (s ^. sessionState)
    let fWCh = s ^. fromWorldChan
        prms = st ^. sessionParams
        Just (deltaString,_) = Map.lookup "param_Sut_deltaTime" prms
        deltaTime = read deltaString
    cDefsMap <- lift $ runIOC s (TxsDefs.cnectDefs <$> Core.txsGetTDefs)
    let cdefs = [ cdef
                | (TxsDefs.CnectId nm _, cdef) <- Map.toList cDefsMap
                , nm == cnctNm
                ]
    -- if null cdefs
    --     then return $ Left $ "No CnectDef found with name: " <> cnctNm
    --     else do
    lift $ initWorld s fWCh $ head cdefs
    wcd <- lift $ readTVarIO (s ^. wConnDef)
    lift $ runIOC s $
        Core.txsSetTest
            (lift <$> putToW deltaTime fWCh (wcd ^. toWorldMappings))
            (lift $ getFromW deltaTime fWCh)
            mDef Nothing Nothing

-- | Test for n-steps or actions
test :: Session -> StepType -> IO (Response ())
test s (NumberOfSteps n) = do
    void $ forkIO $ do
        verdict <- try $ runIOC s $ Core.txsTestN n
        atomically $ writeTQueue (s ^. verdicts) verdict
    return success
test s (AnAction a) = undefined s a

initWorld :: Session -> TChan Action -> CnectDef -> IO ()
initWorld _s fWCh cdef = do
    (towhdls, frowhdls) <- connectToSockets cdef
    frowThreads <- sequence [ forkIO $ fromWorldThread c fWCh
        | ConnHfroW _ c _ _ <- frowhdls
        ]
    undefined towhdls frowhdls frowThreads

connectToSockets :: CnectDef -> IO ([ConnHandle],[ConnHandle])
connectToSockets (CnectDef sType conndefs) = do
    -- let (towhdlInfo1, frowhdlInfo1, tofroInfo) = [ ((ctow, vars', vexp), (cfrow, var', vexps), (htow, ptow))
    --                                              | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
    --                                              , ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
    --                                              , htow == hfrow , ptow == pfrow
    --                                              ]
    --     (towhdlInfo2, toInfo)   = [ ((ctow, vars', vexp), (htow, ptow))
    --                               | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
    --                               , (htow,ptow) `notElem` tofroInfo
    --                               ]
    --     (frowhdlInfo2, froInfo) = [ ((cfrow, var', vexps), (hfrow, pfrow))
    --                               | ConnDfroW cfrow hfrow pfrow var' vexps <- conndefs
    --                               , (hfrow,pfrow) `notElem` tofroInfo
    --                               ]
    let tofrosocks = [ (ctow, vars', vexp, cfrow, var', vexps, htow, ptow)
                   | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                   , ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
                   , htow == hfrow , ptow == pfrow
                   ]
        tosocks =  [ (ctow, vars', vexp, htow, ptow)
                   | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                   , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p) <- tofrosocks]
                   ]
        frosocks = [ (cfrow, var', vexps, hfrow, pfrow)
                   | ConnDfroW cfrow hfrow pfrow var' vexps <- conndefs
                   , (hfrow,pfrow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                   ]
        tofroInfo = [ (hst,prt)
                    | (_, _, _, _, _, _, hst, prt) <- tofrosocks
                    ]
        toInfo    = [ (hst,prt)
                    | (_, _, _, hst, prt) <- tosocks
                    ]
        froInfo   = [ (hst,prt)
                    | (_, _, _, hst, prt) <- frosocks
                    ]
    (tofroConns, toConns, froConns) <- case sType of
        ClientSocket -> openCnectClientSockets tofroInfo toInfo froInfo
        ServerSocket -> openCnectServerSockets tofroInfo toInfo froInfo
    -- return $ zipHandles (towhdlInfo1  ++ towhdlInfo2)
    --                     (frowhdlInfo1 ++ frowhdlInfo2)
    --                     (tofroConns ++ toConns)
    --                     (tofroConns ++ froConns)
    return $ zipHandles tofrosocks tofroConns tosocks toConns frosocks froConns

openCnectClientSockets :: [(Text, Integer)]
                       -> [(Text, Integer)]
                       -> [(Text, Integer)]
                       -> IO ([Connection],[Connection],[Connection])
openCnectClientSockets tofroInfo toInfo froInfo = do
     tofroConns <- sequence [ TVS.connectTo (T.unpack hst) (show prt)
                             | (hst, prt) <- tofroInfo
                             ]
     toConns    <- sequence [ TVS.connectTo (T.unpack hst) (show prt)
                             | (hst, prt) <- toInfo
                             ]
     froConns   <- sequence [ TVS.connectTo (T.unpack hst) (show prt)
                             | (hst, prt) <- froInfo
                             ]
     return (tofroConns, toConns, froConns)

openCnectServerSockets :: [(Text, Integer)]
                       -> [(Text, Integer)]
                       -> [(Text, Integer)]
                       -> IO ([Connection],[Connection],[Connection])
openCnectServerSockets tofroInfo toInfo froInfo = do
     tofroConnsA <- async $ mapConcurrently TVS.acceptOn
                                   [fromInteger prt
                                   | (_, prt) <- tofroInfo
                                   ]
     toConnsA    <- async $ mapConcurrently TVS.acceptOn
                                   [fromInteger prt
                                   | (_, prt) <- toInfo
                                   ]
     froConnsA   <- async $ mapConcurrently TVS.acceptOn
                                   [fromInteger prt
                                   | (_, prt) <- froInfo
                                   ]
     tofroConns <- wait tofroConnsA
     toConns    <- wait toConnsA
     froConns   <- wait froConnsA
     return (tofroConns, toConns, froConns)

-- zipHandles towhdlInfos frowhdlInfos toWConns froWConns =
    -- let towhdls  = [ ConnHtoW ctow c vars' vexp
    --                | (ctow, vars', vexp) <- towhdlInfos
    --                , c <- toWConns
    --                ]

    --     frowhdls = [ ConnHfroW cfrow c var' vexps
    --                | (cfrow, var', vexps) <- frowhdlInfos
    --                , c <- froWConns
    --                ]
    --  in ( towhdls, frowhdls )

zipHandles tofrosocks tofroConns tosocks toConns frosocks froConns =
    let towhdls  = [ ConnHtoW ctow c vars' vexp
                   | ( (ctow, vars', vexp, _, _, _, _, _), c ) <- zip tofrosocks tofroConns
                   ] ++
                   [ ConnHtoW ctow c vars' vexp
                   | ( (ctow, vars', vexp, _, _), c ) <- zip tosocks toConns
                   ]

        frowhdls = [ ConnHfroW cfrow c var' vexps
                   | ( (_, _, _, cfrow, var', vexps, _, _), c ) <- zip tofrosocks tofroConns
                   ]++
                   [ ConnHfroW cfrow c var' vexps
                   | ( (cfrow, var', vexps, _, _), c ) <- zip frosocks froConns
                   ]
     in ( towhdls, frowhdls )


fromWorldThread :: Connection -> TChan Action -> IO ()
fromWorldThread c frowchan = do
     s <- TVS.getLineFrom c
     atomically $ writeTChan frowchan $ decode (SAct c s)
     fromWorldThread c frowchan

decode :: SAction -> Action
decode = undefined
