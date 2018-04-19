{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Main (main) where

import           Control.Exception            (catch, throwIO)
import           Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   as BSL
import           Lens.Micro                   ((^.))
import           System.Process               (StdStream (NoStream), proc,
                                               std_out, withCreateProcess)

import qualified Network.HTTP.Client          as C
import qualified Network.HTTP.Client.Internal as CI
import           Network.Wreq                 as W

import           Test.Hspec

main :: IO ()
main = withCreateProcess (proc "txs-webserver-exe" []) {std_out = NoStream} $ \_stdin _stdout _stderr _ph -> do
    s <- spec
    hspec s

spec :: IO Spec
spec = return $ do
        describe "Get TorXakis info" $
            it "Info responds with 200 and info" $ do
                r <- get "http://localhost:8080/info"
                r ^. responseStatus . statusCode `shouldBe` 200
                let bodyStr = BSL.unpack $ r ^. responseBody
                bodyStr `shouldContain` "\"version\""
                bodyStr `shouldContain` "\"buildTime\""
        describe "Create new TorXakis session" $
            it "Creates 2 sessions" $ do
                r <- post "http://localhost:8080/sessions/new" [partText "" ""]
                r ^. responseStatus . statusCode `shouldBe` 201
                r ^. responseBody `shouldBe` "{\"sessionId\":1}"
                r2 <- post "http://localhost:8080/sessions/new" [partText "" ""]
                r2 ^. responseStatus . statusCode `shouldBe` 201
                r2 ^. responseBody `shouldBe` "{\"sessionId\":2}"
        describe "Upload files to a session" $ do
            it "Uploads valid files" $ do
                _ <- post "http://localhost:8080/sessions/new" [partText "" ""]
                r <- post "http://localhost:8080/sessions/1/load" [partFile "Point.txs" "../../examps/Point/Point.txs"]
                r ^. responseStatus . statusCode `shouldBe` 202
                r ^. responseBody `shouldBe` "{\"fileName\": \"Point.txs\"}"
            it "Fails for parse error" $ do
                let handler (CI.HttpExceptionRequest _ (C.StatusCodeException r body)) = do
                        BS.unpack body `shouldStartWith` "{\"msg\": \"Error in wrong.txt: \nParse Error:"
                        let s = r ^. responseStatus
                        return CI.Response{CI.responseStatus = s}
                    handler e = throwIO e
                _ <- post "http://localhost:8080/sessions/new" [partText "" ""]
                r <- post "http://localhost:8080/sessions/1/load" [partFile "wrong.txt" "../../sys/txs-lib/test/data/wrong.txt"]
                        `catch` handler
                r ^. responseStatus . statusCode `shouldBe` 400
            it "Starts stepper and takes 3 steps" $ do
                _ <- post "http://localhost:8080/sessions/new" [partText "" ""]
                _ <- put "http://localhost:8080/sessions/1/load" [partFile "Point.txs" "../../examps/Point/Point.txs"]
                post "http://localhost:8080/sessions/1/models/Model/stepper" [partText "" ""] >>= checkSuccess
                post "http://localhost:8080/sessions/1/stepper/3" [partText "" ""] >>= checkSuccess
                get "http://localhost:8080/sessions/1/messages" >>= checkJSON
            -- it "Starts tester and tests 3 steps" $ do
            --     _ <- post "http://localhost:8080/sessions/new" [partText "" ""]
            --     _ <- post "http://localhost:8080/sessions/1/model" [partFile "Point.txs" "../../examps/Point/Point.txs"]
            --     _ <- checkSuccess <$> post "http://localhost:8080/tester/start/1/Model" [partText "" ""]
            --     _ <- checkSuccess <$> post "http://localhost:8080/tester/test/1/3" [partText "" ""]
            --     checkJSON    <$> get "http://localhost:8080/sessions/sse/1/messages"

checkSuccess :: Response BSL.ByteString -> IO ()
checkSuccess r =
    r ^. responseStatus . statusCode `shouldBe` 204

checkJSON :: Response BSL.ByteString -> IO ()
checkJSON r = do
    r ^. responseStatus . statusCode `shouldBe` 200
    BSL.unpack (r ^. responseBody) `shouldStartWith` "data:{\"tag\":\""
