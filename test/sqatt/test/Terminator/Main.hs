{-# LANGUAGE OverloadedStrings #-}
-- import           Control.Concurrent.Async
import qualified System.Process as Process
import           Turtle         (sleep)

main :: IO ()
main = runAllWithProcess

runAllWithProcess :: IO ()
runAllWithProcess = do
    putStrLn ""
    let cp = Process.proc "ticl" ["-s", "http://localhost:9831/"]
    (_, _, _, phTicl) <- Process.createProcess cp {Process.std_in = Process.CreatePipe}
    phWS <- Process.spawnProcess "txs-webserver" ["-p", "9831"]
    putStrLn "Give them 5s"
    suddenDeath phTicl phWS
    putStrLn "Exiting!"

-- runAllConcurrently :: IO ()
-- runAllConcurrently = do
--     putStrLn ""
--     runConcurrently $
--         Concurrently runTicl <|> Concurrently runTxsWebserver <|> Concurrently timeout

-- runTicl ::IO ()
-- runTicl = do
--     sleep 1
--     view $ inproc "ticl" ["-s", "http://localhost:9831/"] Turtle.empty
--     putStrLn "CLI done!"

-- runTxsWebserver :: IO ()
-- runTxsWebserver = do
--     view $ inproc "txs-webserver" ["-p", "9831"] Turtle.empty
--     putStrLn "Web-server done!"

-- timeout :: IO ()
-- timeout = sleep 5 >> putStrLn "Time's up!"

suddenDeath :: Process.ProcessHandle -> Process.ProcessHandle ->  IO ()
suddenDeath ph1 ph2 = sleep 5 >> putStrLn "Time's up!"
                        >> Process.terminateProcess ph1
                        >> Process.terminateProcess ph2
