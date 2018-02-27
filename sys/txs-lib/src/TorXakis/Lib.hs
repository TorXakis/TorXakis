-- |
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
module TorXakis.Lib where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar')
import           Control.Monad.STM (atomically)
import           Control.DeepSeq (($!!), NFData)
import           GHC.Generics    (Generic)
import           Control.Exception (try, ErrorCall)
-- See TODO below.
-- import           Path

import           Sigs     (Sigs, empty)
import           VarId    (VarId)
import           TxsDefs  (TxsDefs, empty)
import           TxsAlex  (txsLexer)
import           TxsHappy (txsParser)

data Response = Success | Error { msg :: String } deriving (Show)

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

data ModelName

-- | The session, which maintains the state of a TorXakis model.
newtype Session = Session
    { _sessionState :: TVar SessionSt }

data SessionSt = SessionSt
    { _tdefs   :: TxsDefs
    , _sigs    :: Sigs VarId
    } deriving (Generic, NFData)

emptySessionState :: SessionSt
emptySessionState = SessionSt TxsDefs.empty Sigs.empty 

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
        let (_, ts', is') = txsParser . txsLexer $ xs 
        atomically $ modifyTVar' (_sessionState s) (const $!! SessionSt ts' is')
        return ()
    case r of
        Left err -> return $ Error $ show (err :: ErrorCall)
        Right _  -> return Success
    

-- | Start the stepper with the current model.
stepper :: Session -> ModelName -> IO Response
stepper = undefined

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
