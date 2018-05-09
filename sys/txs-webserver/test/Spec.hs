{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Main (main) where

import           Control.Exception            (catch, throwIO)
import           Data.Aeson                   (decode, decodeStrict)
import           Data.Aeson.Types             (Object, Parser, parseMaybe, (.:))
import           Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   as BSL
import           Data.Maybe                   (mapMaybe)
import           Lens.Micro                   ((^.), (^?))
import           Lens.Micro.Aeson             (key, _Integer)
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
                r ^. responseStatus . statusCode `shouldBe` 200 -- OK
                let bodyStr = BSL.unpack $ r ^. responseBody
                bodyStr `shouldContain` "\"version\""
                bodyStr `shouldContain` "\"buildTime\""
        describe "Create new TorXakis session" $
            it "Creates 2 sessions" $ do
                r <- post "http://localhost:8080/sessions/new" [partText "" ""]
                r ^. responseStatus . statusCode `shouldBe` 201 -- Created
                r ^. responseBody `shouldBe` "{\"sessionId\":1}"
                r2 <- post "http://localhost:8080/sessions/new" [partText "" ""]
                r2 ^. responseStatus . statusCode `shouldBe` 201 -- Created
                r2 ^. responseBody `shouldBe` "{\"sessionId\":2}"
        describe "Upload files to a session" $ do
            it "Uploads valid file" $ do
                _ <- post "http://localhost:8080/sessions/new" [partText "" ""]
                r <- put "http://localhost:8080/sessions/1/model" [partFile "Point.txs" "../../examps/Point/Point.txs"]
                r ^. responseStatus . statusCode `shouldBe` 202 -- Accepted
                let res = do
                        [result] <- decode $ r ^. responseBody
                        parseMaybe parseFileUploadResult result
                case res of
                    Nothing -> expectationFailure $ "Can't parse: " ++ show (r ^. responseBody)
                    Just (fileName, loaded) -> do
                        fileName `shouldBe` "Point.txs"
                        loaded `shouldBe` True
            it "Uploads multiple files" $ do
                sId <- mkNewSession
                r <- put (newSessionUrl sId)
                    [ partFile "LuckyPeople.txs" "../../examps/LuckyPeople/spec/LuckyPeople.txs"
                    , partFile "PurposeExamples.txs" "../../examps/LuckyPeople/spec/PurposeExamples.txs"
                    ]
                r ^. responseStatus . statusCode `shouldBe` 202 -- Accepted
                let res = do
                        results <- decode $ r ^. responseBody
                        return $ mapMaybe (parseMaybe parseFileUploadResult) results
                case res of
                    Nothing -> expectationFailure $ "Can't parse: " ++ show (r ^. responseBody)
                    Just arr -> do
                        arr `shouldContain` [("LuckyPeople.txs",True)]
                        arr `shouldContain` [("PurposeExamples.txs",True)]
            it "Fails for parse error" $ do
                let handler (CI.HttpExceptionRequest _ (C.StatusCodeException r body)) = do
                        BS.unpack body `shouldStartWith` "\nParse Error:"
                        let s = r ^. responseStatus
                        return CI.Response{CI.responseStatus = s}
                    handler e = throwIO e
                _ <- post "http://localhost:8080/sessions/new" [partText "" ""]
                sId <- mkNewSession
                r <- put (newSessionUrl sId) [partFile "wrong.txt" "../../sys/txs-lib/test/data/wrong.txt"]
                        `catch` handler
                r ^. responseStatus . statusCode `shouldBe` 400 -- Bad Request
                r2 <- put (newSessionUrl sId) [ partFile "Point.txs" "../../examps/Point/Point.txs"
                                                                   , partFile "wrong.txt" "../../sys/txs-lib/test/data/wrong.txt"
                                                                   ]
                        `catch` handler
                r2 ^. responseStatus . statusCode `shouldBe` 400 -- Bad Request
        describe "Stepper" $
            it "Starts stepper and takes 3 steps" $ do
                sId <- mkNewSession
                _ <- put (newSessionUrl sId) [partFile "Point.txs" "../../examps/Point/Point.txs"]
                post "http://localhost:8080/sessions/1/set-step/Model" [partText "" ""] >>= check204NoContent
                post "http://localhost:8080/sessions/1/start-step/" [partText "" ""] >>= check204NoContent
                post "http://localhost:8080/sessions/1/stepper/3" [partText "" ""] >>= check204NoContent
                totalSteps <- foldGet checkActions 0 "http://localhost:8080/sessions/1/messages"
                totalSteps `shouldBe` 3
            -- it "Starts tester and tests 3 steps" $ do
            --     _ <- post "http://localhost:8080/sessions/new" [partText "" ""]
            --     _ <- put "http://localhost:8080/sessions/1/model" [partFile "Point.txs" "../../examps/Point/Point.txs"]
            --     _ <- checkSuccess <$> post "http://localhost:8080/tester/start/1/Model" [partText "" ""]
            --     _ <- checkSuccess <$> post "http://localhost:8080/tester/test/1/3" [partText "" ""]
            --     checkJSON    <$> get "http://localhost:8080/sessions/sse/1/messages"

check204NoContent :: Response BSL.ByteString -> IO ()
check204NoContent r = r ^. responseStatus . statusCode `shouldBe` 204

parseFileUploadResult :: Object -> Parser (String, Bool)
parseFileUploadResult o = do
    fn <- o .: "fileName" :: Parser String
    l  <- o .: "loaded"   :: Parser Bool
    return (fn,l)

checkActions :: Int -> BS.ByteString -> IO Int
checkActions steps bs = do
    let Just jsonBs  = BS.stripPrefix "data:" bs
        Just jsonObj = decodeStrict jsonBs
    case parseMaybe parseTag jsonObj of
        Just "AnAction" -> return $ steps + 1
        _               -> return steps

parseTag :: Object -> Parser String
parseTag o = o .: "tag"

mkNewSession :: IO Integer
mkNewSession = do
    sr <- post "http://localhost:8080/sessions/new" [partText "" ""]
    sr ^. responseStatus . statusCode `shouldBe` 201 -- Created
    let Just sId = sr ^? responseBody . key "sessionId" . _Integer
    return sId

newSessionUrl :: Integer -> String
newSessionUrl sId = "http://localhost:8080/sessions/" ++ show sId ++ "/model"
