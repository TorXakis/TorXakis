{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module CLI
(
  startCLI
)
where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           System.Console.Haskeline

startCLI :: IO ()
startCLI = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "% "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do replicateM_ 5 $ do outputStrLn $ "Input was: " ++ input
                                                liftIO $ threadDelay (10 ^ (6 :: Int))
                             loop
