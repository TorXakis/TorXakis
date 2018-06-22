{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
-- |
module TorXakis.Lib
( module TorXakis.Lib
, module TorXakis.Lib.Common
, module TorXakis.Lib.CommonCore
, module TorXakis.Lib.Eval
, module TorXakis.Lib.Params
, module TorXakis.Lib.Path
, module TorXakis.Lib.Session
, module TorXakis.Lib.Simulator
, module TorXakis.Lib.Solvers
, module TorXakis.Lib.Stepper
, module TorXakis.Lib.Timer
, module TorXakis.Lib.Tester
, module TorXakis.Lib.Vals
, module TorXakis.Lib.Vars
)
where

import           Control.Arrow                 (left)
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TQueue (TQueue, readTQueue, writeTQueue)
import           Control.Concurrent.STM.TVar   (readTVarIO, writeTVar)
import           Control.DeepSeq               (force)
import           Control.Exception             (ErrorCall, Exception,
                                                SomeException, evaluate, try)
import           Control.Monad                 (void)
import           Control.Monad.Except          (ExceptT, liftEither, runExceptT,
                                                throwError)
import           Control.Monad.State           (lift)
import           Control.Monad.STM             (atomically)
import           Data.Aeson                    (ToJSON)
import           Data.Either.Utils             (maybeToEither)
import           Data.Foldable                 (traverse_)
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import           Lens.Micro                    ((^.))
import           System.Random                 (mkStdGen, setStdGen)

import qualified BuildInfo
import qualified VersionInfo

import           BehExprDefs                   (ChanOffer (Exclam, Quest),
                                                Offer (Offer))
import           ChanId                        (ChanId)
import           ConstDefs                     (Const)
import           EnvCore                       (IOC)
import           EnvData                       (Msg (TXS_CORE_SYSTEM_INFO, TXS_CORE_USER_INFO, TXS_CORE_USER_WARNING),
                                                StateNr)
import           TxsAlex                       (Token (Cchanenv, Csigs, Cunid, Cvarenv),
                                                txsLexer)
import qualified TxsCore                       as Core
import           TxsDDefs                      (Action (Act), Verdict)
import           TxsDefs                       (ModelDef (ModelDef), modelDefs,
                                                stop)
import           TxsHappy                      (bexprParser, prefoffsParser,
                                                txsParser)
import           TxsShow                       (fshow)

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Eval
import           TorXakis.Lib.Params
import           TorXakis.Lib.Path
import           TorXakis.Lib.Session
import           TorXakis.Lib.Simulator
import           TorXakis.Lib.SocketWorld      (closeSockets)
import           TorXakis.Lib.Solvers
import           TorXakis.Lib.Stepper
import           TorXakis.Lib.Tester
import           TorXakis.Lib.Timer
import           TorXakis.Lib.Vals
import           TorXakis.Lib.Vars

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

newtype LibException = TxsError { errMsg :: Msg } deriving Show

instance Exception LibException

data TorXakisInfo = Info { version :: String, buildTime :: String }
    deriving (Generic)

instance ToJSON TorXakisInfo

info :: TorXakisInfo
info = Info VersionInfo.version BuildInfo.buildTime

setSeed :: Session -> Int -> IO ()
setSeed s seed = do
    setStdGen $ mkStdGen seed
    atomically $ writeTQueue (s ^. sessionMsgs) (TXS_CORE_SYSTEM_INFO $ "Global seed set to " ++ show seed)

-- | Load a TorXakis file, compile it, and return the response.
--
-- The absolute path to the file is used to associate the parsed structure
-- (using the file-contents) with the file name, so that by calling `unload` we
-- know what to unload.
--
-- In the future we might want to make loading of 'TorXakis' models
-- incremental, to give better error messages, and support more modularity.
load :: Session -> FileContents -> IO (Response ())
load s xs = do
    r <- try $ do -- Since the 'TorXakis' parser currently just calls 'error'
                  -- we have to catch a generic 'ErrorCall' exception.
        (_, ts, is) <- evaluate . force . txsParser . txsLexer $ xs
        return (ts, is)
    case r of
        Left err -> return $ Left $ T.pack $ show  (err :: ErrorCall)
        Right (ts, is) ->
            Right <$> runIOC s (Core.txsInit ts is (msgHandler (_sessionMsgs s)))
      where
        msgHandler :: TQueue Msg -> [Msg] -> IOC ()
        msgHandler q = lift . atomically . traverse_ (writeTQueue q)

-- | Get the menu.
getMenu :: Session
        -> Text
        -> IO String
getMenu s args =
    let (kind,what) =
            case T.words args of
                ["in"]       -> ( "mod", "in" )
                ["out"]      -> ( "mod", "out" )
                ["map"]      -> ( "map", "" )
                ["purp",gnm] -> ( "purp", T.unpack gnm )
                _            -> ( "mod", "all" )
   in fshow <$> runIOC s (Core.txsMenu kind what)

-- | Wait for a verdict to be reached.
waitForVerdict :: Session -> IO (Either SomeException Verdict)
waitForVerdict s = atomically $ readTQueue (s ^. verdicts)

-- | Start the stepper with the given model.
stopTxs :: Session -> IO (Response ())
stopTxs s = do
    _ <- closeSockets <$> readTVarIO (s ^. wConnDef)
    atomically $ writeTVar (s ^. wConnDef) $ WorldConnDef [] Map.empty [] []
    runResponse $ lift $ runIOC s Core.txsStop

-- | Parse a String into a set of offers.
parseAction :: Session -> Text -> IO (Response Action)
parseAction s act = do
    let strAct = T.unpack act
    sigs <- runIOC s Core.txsGetSigs
    --
    -- IOS.Tested (TxsDefs.CnectDef _ conndefs) <- gets IOS.modus
    -- We don't have the connection definition here! Can we get this from `TxsDefs`?
    -- let chids = [ chan | TxsDefs.ConnDtoW chan _ _ _ _ <- conndefs ]
    --
    -- We don't have a local value in the session. Do we need one?
    --  vals <- gets IOS.locvals
    --                 , locvals :: TxsDefs.VEnv              -- ^ local value environment (EnvSever)
    --
    runExceptT $ do
        -- TODO: What is the correct way to get the `chids`?
        let
            cannotParse :: String
            cannotParse =  "There is no current model set, "
                        ++ "which is required for parsing an action"
        ModelDef is os _ _ <- runIOCE s $
            maybeToEither cannotParse <$> Core.txsGetCurrentModel
        let chids = concatMap Set.toList is ++ concatMap Set.toList os
        parseRes <- fmap (left showEx) $
            lift $ try $ evaluate . force . prefoffsParser $
            ( Csigs    sigs
            : Cchanenv chids
            : Cvarenv  []
            : Cunid    0 -- Do we need to keep track of the UID in the session?
            : txsLexer strAct
            )
        (_, offs) <- liftEither parseRes
        cstOfs <- traverse offerToAction (Set.toList offs)
        return $ Act (Set.fromList cstOfs)
    where
      offerToAction :: Offer -> ExceptT Text IO (ChanId, [Const])
      offerToAction (Offer cid offrs) = do
          csts <- traverse evalExclam  offrs
          return (cid, csts)

      evalExclam :: ChanOffer -> ExceptT Text IO Const
      evalExclam (Quest _) = throwError "No ? offer allowed as input."
      evalExclam (Exclam choff) = do
          res <- lift (runIOC s (Core.txsEval choff))
          liftEither $ left T.pack res

lpe :: Session -> Text -> IO (Response ())
lpe s args = do
    let mn = args
    mModelDef <- runExceptT $ lookupModel s mn
    _ <- forkIO $ case mModelDef of
        Right mDef -> do
                mModelId <- runIOC s (Core.txsLPEForModelDef mDef mn)
                atomically $ writeTQueue (s ^. sessionMsgs) $
                    case mModelId of
                        Just mId -> TXS_CORE_USER_INFO $
                                        "LPE modeldef generated: \n" ++ fshow mId
                        _         -> TXS_CORE_USER_WARNING "Could not generate LPE"
        _          -> do
            tdefs <- runIOC s Core.txsGetTDefs
            let mdefs = modelDefs tdefs
                chids = Set.toList $ Set.unions [ Set.unions (chins ++ chouts ++ spls)
                                                | (_, ModelDef chins chouts spls _)
                                                  <- Map.toList mdefs
                                                ]
            sigs <- runIOC s Core.txsGetSigs
            vals <- readTVarIO $ s ^. locValEnv
            parseRes <- fmap (left showEx) $
                            try $ evaluate . force . bexprParser $
                                    ( TxsAlex.Csigs    sigs
                                    : TxsAlex.Cchanenv chids
                                    : TxsAlex.Cvarenv  (Map.keys vals)
                                    : TxsAlex.Cunid    0
                                    : TxsAlex.txsLexer (T.unpack args)
                                    )
            bexpr <- case parseRes of
                Left  err       ->  do  atomically $
                                            writeTQueue (s ^. sessionMsgs)
                                                        (TXS_CORE_USER_WARNING $
                                                            "Incorrect behaviour expression: "
                                                            ++ T.unpack err)
                                        return TxsDefs.stop
                Right (_, bxpr) ->  return bxpr
            mayBexpr' <- runIOC s $ Core.txsLPE (Left bexpr)
            atomically $ writeTQueue (s ^. sessionMsgs) $
                case mayBexpr' of
                    Just (Left bexpr') -> TXS_CORE_USER_INFO $
                                            "LPE behaviour generated: \n" ++ fshow bexpr'
                    _                  -> TXS_CORE_USER_WARNING "Could not generate LPE"
    return success

ncomp :: Session -> Text -> IO (Response ())
ncomp s mn = do
    mModelDef <- runExceptT $ lookupModel s mn
    case mModelDef of
        Right mDef -> do
            void $ forkIO $ do
                mPurpId <- runIOC s $ Core.txsNComp mDef
                atomically $ writeTQueue (s ^. sessionMsgs) $
                    case mPurpId of
                        Just pId -> TXS_CORE_USER_INFO $
                                        "Test Purpose generated: \n" ++ fshow pId
                        _         -> TXS_CORE_USER_WARNING "Could not generate test purpose"
            return success
        Left err   -> return $ Left err

showItem :: Session -> String -> String -> IO String
showItem s item nm =
    case (item,nm) of
        ("tdefs"    ,""      ) -> runIOC s $ Core.txsShow "tdefs"     ""
        ("state"    ,"nr"    ) -> runIOC s $ Core.txsShow "state"     ""
        ("state"    ,"model" ) -> runIOC s $ Core.txsShow "model"     ""
        ("state"    ,"mapper") -> runIOC s $ Core.txsShow "mapper"    ""
        ("state"    ,"purp"  ) -> runIOC s $ Core.txsShow "purp"      ""
        ("modeldef" ,_       ) -> runIOC s $ Core.txsShow "modeldef"  nm
        ("mapperdef",_       ) -> runIOC s $ Core.txsShow "mapperdef" nm
        ("purpdef"  ,_       ) -> runIOC s $ Core.txsShow "purpdef"   nm
        ("procdef"  ,_       ) -> runIOC s $ Core.txsShow "procdef"   nm
        ("funcdef"  ,_       ) -> runIOC s $ Core.txsShow "funcdef"   nm
        ("cnect"    ,""      ) -> do WorldConnDef towhdls _ frowhdls _ <- readTVarIO (s ^. wConnDef)
                                     return $ fshow (towhdls ++ frowhdls)
        ("var"      ,""      ) -> fshow <$> readTVarIO (s ^. locVars)
        ("val"      ,""      ) -> fshow <$> readTVarIO (s ^. locValEnv)
        _                      -> return "Nothing to be shown"

-- | Go to a state.
gotoState :: Session -> StateNr -> IO (Response ())
gotoState s stNr = runResponse $ lift $ runIOC s $ Core.txsGoTo stNr

-- | Back a number of states.
backStates :: Session -> Int -> IO (Response ())
backStates s steps = gotoState s (-steps)
