{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent.Async
import           Turtle

main :: IO ()
main = runAllConcurrently

runAllConcurrently :: IO ()
runAllConcurrently = do
    putStrLn ""
    runConcurrently $
        Concurrently runTicl <|> Concurrently runTxsWebserver <|> Concurrently timeout

runTicl ::IO ()
runTicl = do
    sleep 1
    view $ inproc "ticl" ["-s", "http://localhost:9831/"] Turtle.empty
    putStrLn "CLI done!"

runTxsWebserver :: IO ()
runTxsWebserver = do
    view $ inproc "txs-webserver" ["-p", "9831"] Turtle.empty
    putStrLn "Web-server done!"

timeout :: IO ()
timeout = sleep 5 >> putStrLn "Time's up!"
