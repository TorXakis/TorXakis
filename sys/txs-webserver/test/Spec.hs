{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Main (main) where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, wait)
import           Control.Exception            (catch, throwIO)
import           Data.Aeson                   (decode, decodeStrict)
import           Data.Aeson.Types             (Object, Parser, parseMaybe,
                                               toJSON, (.:))
import           Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy.Char8   as BSL
import           Data.Maybe                   (mapMaybe)
import           GHC.Stack                    (HasCallStack)
import           Lens.Micro                   ((^.), (^?))
import           Lens.Micro.Aeson             (key, _Integer)
import           System.Process               (StdStream (NoStream), proc,
                                               std_out, withCreateProcess)

import qualified Network.HTTP.Client          as C
import qualified Network.HTTP.Client.Internal as CI
import           Network.Wreq                 as W

import TorXakis.Lib (StepType (NumberOfSteps))

import           Test.Hspec

main :: IO ()
-- TODO: a problem with this is that it won't recompile the 'txs-webserver'
-- program when it changes, so this is not that useful for unit testing. We
-- should just call the 'main' function at 'txs-webserver/app/Main.hs'.
main = withCreateProcess (proc "txs-webserver" []) {std_out = NoStream} $
    \_stdin _stdout _stderr _ph -> do
        s <- spec
        hspec s

spec :: IO Spec
spec = return $ do
        describe "Get TorXakis info" $
            it "Info responds with 200 and info" $ do
                r <- get (host ++ "/info")
                r ^. responseStatus . statusCode `shouldBe` 200 -- OK
                let bodyStr = BSL.unpack $ r ^. responseBody
                bodyStr `shouldContain` "\"version\""
                bodyStr `shouldContain` "\"buildTime\""
        describe "Create new TorXakis session" $
            it "Creates 2 sessions" $ do
                r <- post (host ++ "/sessions/new") emptyP
                r ^. responseStatus . statusCode `shouldBe` 201 -- Created
                r ^. responseBody `shouldBe` "{\"sessionId\":1}"
                r2 <- post (host ++ "/sessions/new") emptyP
                r2 ^. responseStatus . statusCode `shouldBe` 201 -- Created
                r2 ^. responseBody `shouldBe` "{\"sessionId\":2}"
        describe "Upload files to a session" $ do
            it "Uploads valid file" $ do
                _ <- post (host ++ "/sessions/new") emptyP
                r <- put (host ++ "/sessions/1/model") [partFile "Point.txs" "../../examps/Point/Point.txs"]
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
                _ <- post (host ++ "/sessions/new") emptyP
                sId <- mkNewSession
                r <- put (newSessionUrl sId) [partFile "wrong.txt" "../../sys/txs-lib/test/data/wrong.txt"]
                        `catch` handler
                r ^. responseStatus . statusCode `shouldBe` 400 -- Bad Request
                r2 <- put (newSessionUrl sId) [ partFile "Point.txs" "../../examps/Point/Point.txs"
                                                                   , partFile "wrong.txt" "../../sys/txs-lib/test/data/wrong.txt"
                                                                   ]
                        `catch` handler
                r2 ^. responseStatus . statusCode `shouldBe` 400 -- Bad Request
        describe "Stepper" $ do
            it "Starts stepper and takes 3 steps" $ do
                sId <- mkNewSession
                _ <- put (newSessionUrl sId) [partFile "Point.txs" "../../examps/Point/Point.txs"]
                post (setStepUrl sId) emptyP >>= check204NoContent
                post (startStepUrl sId) emptyP >>= check204NoContent
                let threeSteps = toJSON (NumberOfSteps 3)
                post (stepUrl sId) threeSteps >>= check204NoContent
                post (openMessagesUrl sId) emptyP >>= check204NoContent
                a <- async $ foldGet checkActions 0 (messagesUrl sId)
                threadDelay (10 ^ (6 :: Int)) -- We have to wait a bit till all
                                              -- the messages are put in the
                                              -- queue by the stepper.
                post (closeMessagesUrl sId) emptyP >>= check204NoContent
                totalSteps <- wait a
                totalSteps `shouldBe` 3
            it "Starts stepper and takes 6 steps in two different step commands" $ do
                sId <- mkNewSession
                _ <- put (newSessionUrl sId) [partFile "Point.txs" "../../examps/Point/Point.txs"]
                post (setStepUrl sId) emptyP >>= check204NoContent
                post (startStepUrl sId) emptyP >>= check204NoContent
                let threeSteps = toJSON (NumberOfSteps 3)
                post (stepUrl sId) threeSteps >>= check204NoContent
                post (openMessagesUrl sId) emptyP >>= check204NoContent
                a <- async $ foldGet checkActions 0 (messagesUrl sId)
                post (stepUrl sId) threeSteps >>= check204NoContent
                threadDelay (10 ^ (6 :: Int)) -- We have to wait a bit till all
                                              -- the messages are put in the
                                              -- queue by the stepper.
                post (closeMessagesUrl sId) emptyP >>= check204NoContent
                totalSteps <- wait a
                totalSteps `shouldBe` 6
            -- it "Starts tester and tests 3 steps" $ do
            --     _ <- post host ++ "/sessions/new" emptyP
            --     _ <- put host ++ "/sessions/1/model" [partFile "Point.txs" "../../examps/Point/Point.txs"]
            --     _ <- checkSuccess <$> post host ++ "/tester/start/1/Model" emptyP
            --     _ <- checkSuccess <$> post host ++ "/tester/test/1/3" emptyP
            --     checkJSON    <$> get host ++ "/sessions/sse/1/messages"

check204NoContent :: HasCallStack => Response BSL.ByteString -> IO ()
check204NoContent r = r ^. responseStatus . statusCode `shouldBe` 204

parseFileUploadResult :: Object -> Parser (String, Bool)
parseFileUploadResult o = do
    fn <- o .: "fileName" :: Parser String
    l  <- o .: "loaded"   :: Parser Bool
    return (fn, l)

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
    sr <- post (host ++ "/sessions/new") emptyP
    sr ^. responseStatus . statusCode `shouldBe` 201 -- Created
    let Just sId = sr ^? responseBody . key "sessionId" . _Integer
    return sId

host :: String
host = "http://localhost:8080"

newSessionUrl :: Integer -> String
newSessionUrl sId = host ++ "/sessions/" ++ show sId ++ "/model"

setStepUrl :: Integer -> String
setStepUrl sId = host ++ "/sessions/" ++ show sId ++ "/set-step/Model"

startStepUrl :: Integer -> String
startStepUrl sId = host ++ "/sessions/" ++ show sId ++ "/start-step"

stepUrl :: Integer -> String
stepUrl sId = host ++ "/sessions/" ++ show sId ++ "/step/"

messagesUrl :: Integer -> String
messagesUrl sId = host ++ "/sessions/" ++ show sId ++ "/messages"

closeMessagesUrl :: Integer -> String
closeMessagesUrl sId = host ++ "/sessions/" ++ show sId ++ "/messages/close"

openMessagesUrl :: Integer -> String
openMessagesUrl sId = host ++ "/sessions/" ++ show sId ++ "/messages/open"

emptyP :: [Part]
emptyP = [partText "" ""]
