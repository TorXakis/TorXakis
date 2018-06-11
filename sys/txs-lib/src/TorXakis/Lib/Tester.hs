{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module TorXakis.Lib.Tester where

import           Control.Concurrent           (ThreadId, forkIO)
import           Control.Concurrent.Async     (async, mapConcurrently, wait)
import           Control.Concurrent.STM.TChan (TChan, writeTChan)
import           Control.Concurrent.STM.TVar  (readTVarIO, writeTVar)
import           Control.Monad.Except         (throwError)
import           Control.Monad.State          (lift, liftIO)
import           Control.Monad.STM            (atomically)
import           Data.Either                  (partitionEithers)
import qualified Data.Map.Strict              as Map
import           Data.Semigroup               ((<>))
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lens.Micro                   ((^.))
import           Name                         (Name)
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
import           TorXakis.Lib.Internal
import           TorXakis.Lib.Session

-- | Start the tester
setTest :: Name
        -> Name
        -> Name
        -> Session
        -> IO (Response ())
setTest mdlNm cnctNm purpMappNms s = runResponse $ do
    mDef <- lookupModel s mdlNm
    st <- lift $ readTVarIO (s ^. sessionState)
    let fWCh = s ^. fromWorldChan
        prms = st ^. sessionParams
        Just (deltaString,_) = Map.lookup "param_Sut_deltaTime" prms
        deltaTime = read deltaString
    cDefsMap    <- lift $ runReadOnlyIOC s $ TxsDefs.cnectDefs <$> Core.txsGetTDefs
    mappDefsMap <- lift $ runReadOnlyIOC s $ TxsDefs.mapperDefs <$> Core.txsGetTDefs
    purpDefsMap <- lift $ runReadOnlyIOC s $ TxsDefs.purpDefs <$> Core.txsGetTDefs
    let cdefs = [ cdef
                | (TxsDefs.CnectId nm _, cdef) <- Map.toList cDefsMap
                , nm == cnctNm
                ]
        purpMapps = T.words purpMappNms
        adefs = [ adef
                | (TxsDefs.MapperId nm _, adef) <- Map.toList mappDefsMap
                , nm `elem` purpMapps
                ]
        pdefs =  [ pdef
                 | (TxsDefs.PurpId  nm _, pdef) <- Map.toList purpDefsMap
                 , nm `elem` purpMapps
                 ]
    mADef <- case adefs of
                 [a] -> return $ Just a
                 []  -> return Nothing
                 _   -> throwError "Wrong or inconsistent parameters"
    mPDef <- case pdefs of
                 [p] -> return $ Just p
                 []  -> return Nothing
                 _   -> throwError "Wrong or inconsistent parameters"
    case cdefs of
        [] -> throwError $ "No CnectDef found with name: " <> cnctNm
        [c]
            | isConsistent mDef mADef mPDef c -> lift $ do
                    (wcd,tids) <- initSocketWorld s fWCh c
                    atomically $ do
                        writeTVar (s ^. wConnDef) wcd
                        writeTVar (s ^. worldListeners) tids
                    runIOC s $
                        Core.txsSetTest
                            (lift <$> putToW deltaTime fWCh (wcd ^. toWorldMappings))
                            (lift $ getFromW deltaTime fWCh)
                            mDef mADef mPDef
            | otherwise   -> throwError "Wrong or inconsistent parameters"
        _   -> throwError "Wrong or inconsistent parameters"
      where
        isConsistent :: TxsDefs.ModelDef
                     -> Maybe TxsDefs.MapperDef
                     -> Maybe TxsDefs.PurpDef
                     -> TxsDefs.CnectDef
                     -> Bool
        isConsistent (TxsDefs.ModelDef minsyncs moutsyncs _ _)
                     Nothing _
                     (TxsDefs.CnectDef _ conndefs) =
            let { mins   = Set.fromList minsyncs
                ; mouts  = Set.fromList moutsyncs
                ; ctows  = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDtoW  chn _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDfroW chn _ _ _ _ <- conndefs ]
                }
            in  mins == ctows && cfrows == mouts
        isConsistent _
                     (Just (TxsDefs.MapperDef achins achouts asyncsets _)) _
                     (TxsDefs.CnectDef _ conndefs) =
            let { ctows  = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDtoW  chn _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDfroW chn _ _ _ _ <- conndefs ]
                ; ains   = Set.fromList $ filter (not . Set.null)
                                [ sync `Set.intersection` Set.fromList achins  | sync <- asyncsets ]
                ; aouts  = Set.fromList $ filter (not . Set.null)
                                [ sync `Set.intersection` Set.fromList achouts | sync <- asyncsets ]
                }
            in  cfrows `Set.isSubsetOf` ains && ctows `Set.isSubsetOf` aouts

-- | Test for n-steps or an action
test :: Session -> StepType -> IO (Response ())
test s (NumberOfSteps n) = runForVerdict s (Core.txsTestN n)
test s (AnAction a)      = runForVerdict s (Core.txsTestIn a)

-- | Test step by observing output
testO :: Session -> IO (Response ())
testO s = runForVerdict s Core.txsTestOut

initSocketWorld :: Session -> TChan Action -> CnectDef -> IO (WorldConnDef,[ThreadId])
initSocketWorld s fWCh cdef = do
    (towhdls, frowhdls) <- connectToSockets cdef
    frowThreads <- sequence [ forkIO $ fromWorldThread s h fWCh
        | h <- frowhdls
        ]
    let wcdPairs = zip (map TxsDDefs.chan towhdls)
                       $ map (ToWorldMapping . sendToSocket s) towhdls
    return (WorldConnDef $ Map.fromList wcdPairs, frowThreads)

sendToSocket :: Session -> ConnHandle -> [Const] -> IO (Response (Maybe Action))
sendToSocket _ ConnHfroW{} _ = return $ Left "Shouldn't send to socket FROM world."
sendToSocket s h@ConnHtoW{connection = c} consts = do
    rTxt <- encode s h consts
    case rTxt of
        Right txt -> do liftIO $ TVS.putLineTo c txt
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

connectToSockets :: CnectDef -> IO ([ConnHandle],[ConnHandle])
connectToSockets (CnectDef sType conndefs) = do
    let toFroTriplets = [ ((ctow, vars', vexp), (cfrow, var', vexps), (htow, ptow))
                        | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                        , ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
                        , htow == hfrow , ptow == pfrow
                        ]
        (towhdlInfo1, frowhdlInfo1, tofroInfo) =
            foldr (\(t, f, i) (acct, accf, acci) -> (t:acct,f:accf,i:acci))
                  ([],[],[])
                  toFroTriplets
        toTuples      = [ ((ctow, vars', vexp), (htow, ptow))
                        | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                        , (htow,ptow) `notElem` tofroInfo
                        ]
        (towhdlInfo2, toInfo) =
            foldr (\(t, i) (acct, acci) -> (t:acct,i:acci))
                  ([],[])
                  toTuples
        froTuples     = [ ((cfrow, var', vexps), (hfrow, pfrow))
                        | ConnDfroW cfrow hfrow pfrow var' vexps <- conndefs
                        , (hfrow,pfrow) `notElem` tofroInfo
                        ]
        (frowhdlInfo2, froInfo) =
            foldr (\(f, i) (accf, acci) -> (f:accf,i:acci))
                  ([],[])
                  froTuples
    (tofroConns, toConns, froConns) <- case sType of
        ClientSocket -> openCnectClientSockets tofroInfo toInfo froInfo
        ServerSocket -> openCnectServerSockets tofroInfo toInfo froInfo
    return $ zipHandles (towhdlInfo1  ++ towhdlInfo2)
                        (frowhdlInfo1 ++ frowhdlInfo2)
                        (tofroConns ++ toConns)
                        (tofroConns ++ froConns)

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

zipHandles :: [(TxsDefs.ChanId, [VarId.VarId], TxsDefs.VExpr)]
           -> [(TxsDefs.ChanId, VarId.VarId, [TxsDefs.VExpr])]
           -> [Connection]
           -> [Connection]
           -> ([ConnHandle], [ConnHandle])
zipHandles towhdlInfos frowhdlInfos toWConns froWConns =
    let towhdls  = [ ConnHtoW ctow c vars' vexp
                   | (ctow, vars', vexp) <- towhdlInfos
                   , c <- toWConns
                   ]
        frowhdls = [ ConnHfroW cfrow c var' vexps
                   | (cfrow, var', vexps) <- frowhdlInfos
                   , c <- froWConns
                   ]
    in  (towhdls, frowhdls)

fromWorldThread :: Session -> ConnHandle -> TChan Action -> IO ()
fromWorldThread _ ConnHtoW{} _ = error "Shouldn't try to listen to socket connection TO world."
fromWorldThread s h@ConnHfroW{connection = c} frowchan = do
    t <- TVS.getLineFrom c
    rAction <- decode s h t
    case rAction of
        Right action -> atomically $ writeTChan frowchan action
        Left  e      -> print e
    fromWorldThread s h frowchan

decode :: Session -> ConnHandle -> Text -> IO (Response Action)
decode _ ConnHtoW{} _ = error "Shouldn't try to decode from socket connection TO world."
decode s (ConnHfroW cId _ vId vExprs) txt = do
    let senv = Map.fromList [ (vId, cstrConst (Cstring txt)) ]
    fDfs <- runReadOnlyIOC s $ TxsDefs.funcDefs <$> Core.txsGetTDefs
    mwals <- mapM (runResponse . runReadOnlyIOCE s . Core.txsEval . subst senv fDfs) vExprs
    return $ case partitionEithers mwals of
        ([], wals) -> Right $ Act ( Set.singleton (cId,wals) )
        (es, _)    -> Left $ T.intercalate "\n" $ "Decode: eval failed":es
