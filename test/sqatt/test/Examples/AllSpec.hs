{-# LANGUAGE OverloadedStrings #-}
module Examples.AllSpec (spec) where

import           Examples.All
import           Filesystem.Path
import           Sqatt
import           System.IO
import           Test.Hspec
import           Turtle

-- | For now the root directory where the logs are stored is not configurable.
sqattLogsRoot :: Turtle.FilePath
sqattLogsRoot = "sqatt-logs"

spec :: Spec
spec = beforeAll
         ( do checkSMTSolvers
              checkCompilers
              checkTxsInstall
              hSetBuffering System.IO.stdout NoBuffering
         )
         ( do
             dir <- runIO $ do
               cd $ ".." </> ".."
               currDate <- date
               let logDir =
                     sqattLogsRoot </> fromString ("test-" ++ show currDate)
               mktree logDir
               return logDir
             testExampleSets dir allExamples
         )
