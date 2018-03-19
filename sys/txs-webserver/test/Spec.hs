-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Main (main) where

import           Control.Exception           (catch, throwIO)
import           Control.Lens                ((^.))
import           Data.ByteString.Char8       (unpack)
import           System.Process              (withCreateProcess, proc, std_out, StdStream(NoStream))

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
        describe "GET /users" $
            it "responds with 200 and [Users]" $ do
                r <- get "http://localhost:8080/users"
                r ^. responseStatus . statusCode `shouldBe` 200
                let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
                r ^. responseBody `shouldBe` users
        describe "Create new TorXakis session" $
            it "Creates 2 sessions" $ do
                r <- post "http://localhost:8080/session/new" [partText "" ""]
                r ^. responseStatus . statusCode `shouldBe` 201
                r ^. responseBody `shouldBe` "1"
                r2 <- post "http://localhost:8080/session/new" [partText "" ""]
                r2 ^. responseStatus . statusCode `shouldBe` 201
                r2 ^. responseBody `shouldBe` "2"
        describe "Upload files to a session" $ do
            it "Uploads valid files" $ do
                _ <- post "http://localhost:8080/session/new" [partText "" ""]
                r <- post "http://localhost:8080/session/1/model" [partFile "Point.txs" "c:/Repos/TorXakis/examps/Point/Point.txs"]
                r ^. responseStatus . statusCode `shouldBe` 201
                r ^. responseBody `shouldBe` "\"\\nLoaded: Point.txs\""
            it "Fails for parse error" $ do
                let handler (CI.HttpExceptionRequest _ (C.StatusCodeException r body)) = do
                        unpack body `shouldStartWith` "\nError in wrong.txt: \nParse Error:"
                        let s = r ^. responseStatus
                        return CI.Response{CI.responseStatus = s}
                    handler e = throwIO e
                _ <- post "http://localhost:8080/session/new" [partText "" ""]
                r <- post "http://localhost:8080/session/1/model" [partFile "wrong.txt" "C:/Repos/TorXakis/sys/txs-lib/test/data/wrong.txt"]
                        `catch` handler
                r ^. responseStatus . statusCode `shouldBe` 400
            it "Starts stepper and takes 3 steps" $ do
                _ <- post "http://localhost:8080/session/new" [partText "" ""]
                _ <- post "http://localhost:8080/session/1/model" [partFile "Point.txs" "c:/Repos/TorXakis/examps/Point/Point.txs"]
                r <- post "http://localhost:8080/stepper/start/1/Model" [partText "" ""]
                r ^. responseStatus . statusCode `shouldBe` 200
                r ^. responseBody `shouldBe` "\"Success\""
                r2 <- post "http://localhost:8080/stepper/step/1/3" [partText "" ""]
                r2 ^. responseStatus . statusCode `shouldBe` 200
                r2 ^. responseBody `shouldBe` "\"Success\""
