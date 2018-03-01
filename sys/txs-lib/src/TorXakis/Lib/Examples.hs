-- | Examples of the usage of 'TorXakis.Lib'.

module TorXakis.Lib.Examples where

import           Prelude hiding (take)

import           Lens.Micro                    ((^.))
import           Data.Conduit                  (runConduit,(.|))
import           Data.Conduit.Combinators      (take, sinkList)
import           Data.Conduit.TQueue           (sourceTQueue)
import           Data.Foldable                 (traverse_)
import           Control.Monad                 (void)

import           EnvData  (Msg)

import           TorXakis.Lib
import           TorXakis.Session


-- | Get the next message in the session.
getNextMsg :: Session -> IO [Msg]
getNextMsg s = getNextNMsgs s 1

-- | Get the next N messages in the session.
--
-- If no messages are available this call will block waiting for new messages.
--
getNextNMsgs :: Session -> Int -> IO [Msg]
getNextNMsgs s n =
    runConduit $ sourceTQueue (s ^. sessionMsgs) .| take n .| sinkList

-- | TODO: this is also used for debugging purposes. Put this into an
-- `Examples` or `Test` folder.
--
-- In such an example folder we might want to include a conduit version of it.
printNextNMsgs :: Session -> Int -> IO ()
printNextNMsgs s n = getNextNMsgs s n >>= traverse_ print

testEcho :: IO ()
testEcho = do
    cs <- readFile "examps/Echo/Echo.txs"
    s <- newSession 
    r <- load s cs
    putStrLn $ "Result of `load`: " ++ show r
    void $ stepper s "Model"
    m <- getNextMsg s
    print m
    r' <- step s (NumberOfSteps 1)
    putStrLn $ "Result of `step`: " ++ show r'    
    m' <- getNextMsg s
    print m'

-- TODO: put an example that uses conduits.
