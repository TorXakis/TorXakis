-- |
module TorXakis.Lib where

import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, modifyTVar')
import           Control.Monad.STM           (atomically)
import           Control.Exception           (try, ErrorCall)
import           Control.Monad.State         (runStateT, lift)
import           Lens.Micro                  ((.~), (^.))
    
import           TxsAlex  (txsLexer)
import           TxsHappy (txsParser)
import           EnvCore  (IOC, initState)
import           TxsCore  (txsInit, txsSetStep)
import           EnvData  (Msg)
import           TorXakis.Lens.TxsDefs (ix)
import           Name     (Name)

import           TorXakis.Session

data Response = Success | Error { msg :: String } deriving (Show)

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

-- | Create a new session.
newSession :: IO Session
newSession = Session <$> newTVarIO emptySessionState

-- | Load a TorXakis file, compile it, and return the response.
--
-- The absolute path to the file is used to associate the parsed structure
-- (using the file-contents) with the file name, so that by calling `unload` we
-- know what to unload.
--
-- In the future we might want to make loading of 'TorXakis' models
-- incremental, to give better error messages, and support more modularity.
load :: Session -> FileContents -> IO Response
load s xs = do
    r <- try $ do -- Since the 'TorXakis' parser currently just calls 'error'
                  -- we have to catch a generic 'ErrorCall' exception.
        let (_, ts, is) = txsParser . txsLexer $ xs
        atomically $ modifyTVar' (_sessionState s) ((tdefs .~ ts) . (sigs .~ is))
        return ()
    case r of
        Left err -> return $ Error $ show (err :: ErrorCall)
        Right _  -> return Success


-- | Start the stepper with the given model.
stepper :: Session
        -> Name        -- ^ Model name
        -> IO Response
stepper s mn = do
    st <- readTVarIO (_sessionState s)
    (res, nextState) <- flip runStateT initState $ do
        -- TODO: catch possible errors
        txsInit (st ^. tdefs) (st ^. sigs) dummyHandler
        -- Lookup the model definition that matches the name.
        let mMDef = st ^. tdefs . ix mn
        case mMDef of
            Nothing ->
                return $ Error $ "No model named " ++ show mn
            Just mDef -> do
                -- TODO: this is a bit twisted. All the torxakis errors will be
                -- reported to the dummy handler, so we have to force the dummy
                -- handler to put the errors it receives onto some shared
                -- channel, and read them back from there here. Unless we want
                -- to change the way TorXakis deals with errors.
                txsSetStep mDef
                return Success
    atomically $ modifyTVar' (_sessionState s) (envCore .~ nextState)
    return res

-- | For now we define a dummy handler for the information, warning, and error
-- messages that come from TorXakis.
--
-- TODO: put these messages somewhere so that they can be retrieved whenever
-- needed.
dummyHandler :: [Msg] -> IOC ()
dummyHandler = lift . putStrLn . show

-- | How a step is described
data StepType = Action ActionName
              | Run Steps
              | GoTo StateNumber
              | Reset -- ^ Go to the initial state.
              | Rewind Steps

-- TODO: discuss with Jan: do we need a `Tree` step here?

data StateNumber
data ActionName
-- | Number of steps
data Steps

-- | step for n-steps
step :: Session -> StepType -> IO Response -- | Or a conduit?
step = undefined
