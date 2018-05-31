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

import           TorXakis.Lib                 (StepType (NumberOfSteps))

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
                let threeSteps = toJSON (NumberOfSteps 3)
                post (stepUrl sId) threeSteps >>= check204NoContent
                post (openMessagesUrl sId) emptyP >>= check204NoContent
                a <- async $ foldGet checkActions 0 (messagesUrl sId)
                threadDelay (3 * 10 ^ (6 :: Int)) -- We have to wait a bit till all
                                              -- the messages are put in the
                                              -- queue by the stepper.
                post (closeMessagesUrl sId) emptyP >>= check204NoContent
                totalSteps <- wait a
                totalSteps `shouldBe` 3
            it "Starts stepper and takes 6 steps in two different step commands" $ do
                sId <- mkNewSession
                _ <- put (newSessionUrl sId) [partFile "Point.txs" "../../examps/Point/Point.txs"]
                post (setStepUrl sId) emptyP >>= check204NoContent
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
        describe "Timer" $ do
            it "Starts timer" $ do
                sId <- mkNewSession
                r <- post (timerUrl sId "testTimer") emptyP
                r ^. responseStatus . statusCode `shouldBe` 200 -- OK
                let res = do timer <- decode $ r ^. responseBody :: Maybe Object
                             parseMaybe parseTimerResult timer
                case res of
                    Nothing -> expectationFailure $  "Can't parse: " ++ show (r ^. responseBody)
                    Just (tn,stt,stp,d) -> do tn `shouldBe` "testTimer"
                                              stt `shouldNotSatisfy` Prelude.null
                                              stp `shouldSatisfy` Prelude.null
                                              d   `shouldSatisfy` Prelude.null
            it "Reads timer" $ do
                sId <- mkNewSession
                r <- post (timerUrl sId "testTimer") emptyP
                let Just (_,startTime,_,_) =
                        do  timer <- decode $ r ^. responseBody :: Maybe Object
                            parseMaybe parseTimerResult timer
                r2 <- post (timerUrl sId "testTimer") emptyP
                r2 ^. responseStatus . statusCode `shouldBe` 200 -- OK
                let res2 = do timer <- decode $ r2 ^. responseBody :: Maybe Object
                              parseMaybe parseTimerResult timer
                case res2 of
                    Nothing -> expectationFailure $ "Can't parse: " ++ show (r2 ^. responseBody)
                    Just (tn,stt,stp,d) -> do tn `shouldBe` "testTimer"
                                              stt `shouldBe` startTime
                                              stp `shouldNotSatisfy` Prelude.null
                                              d   `shouldNotSatisfy` Prelude.null
            it "Restarts timer" $ do
                sId <- mkNewSession
                _ <- post (timerUrl sId "testTimer") emptyP
                _ <- post (timerUrl sId "testTimer") emptyP
                r <- post (timerUrl sId "testTimer") emptyP
                let res = do timer <- decode $ r ^. responseBody :: Maybe Object
                             parseMaybe parseTimerResult timer
                case res of
                    Nothing -> expectationFailure $ "Can't parse: " ++ show (r ^. responseBody)
                    Just (tn,stt,stp,d) -> do tn `shouldBe` "testTimer"
                                              stt `shouldNotSatisfy` Prelude.null
                                              stp `shouldSatisfy` Prelude.null
                                              d   `shouldSatisfy` Prelude.null
            it "Multiple timers" $ do
                sId <- mkNewSession
                r1_start <- post (timerUrl sId "testTimer1") emptyP
                let Just (_,startTime1,_,_) =
                        do  timer <- decode $ r1_start ^. responseBody :: Maybe Object
                            parseMaybe parseTimerResult timer
                r2_start <- post (timerUrl sId "testTimer2") emptyP
                let Just (_,startTime2,_,_) =
                        do  timer <- decode $ r2_start ^. responseBody :: Maybe Object
                            parseMaybe parseTimerResult timer

                r1_read <- post (timerUrl sId "testTimer1") emptyP
                let res1 = do timer <- decode $ r1_read ^. responseBody :: Maybe Object
                              parseMaybe parseTimerResult timer
                case res1 of
                    Nothing -> expectationFailure $ "Can't parse: " ++ show (r1_read ^. responseBody)
                    Just (tn,stt,stp,d) -> do tn `shouldBe` "testTimer1"
                                              stt `shouldBe` startTime1
                                              stp `shouldNotSatisfy` Prelude.null
                                              d   `shouldNotSatisfy` Prelude.null
                r2_read <- post (timerUrl sId "testTimer2") emptyP
                let res2 = do timer <- decode $ r2_read ^. responseBody :: Maybe Object
                              parseMaybe parseTimerResult timer
                case res2 of
                    Nothing -> expectationFailure $ "Can't parse: " ++ show (r2_read ^. responseBody)
                    Just (tn,stt,stp,d) -> do tn `shouldBe` "testTimer2"
                                              stt `shouldBe` startTime2
                                              stp `shouldNotSatisfy` Prelude.null
                                              d   `shouldNotSatisfy` Prelude.null
        describe "Eval" $
            it "Evaluates correctly" $ do
                sId <- mkNewSession
                _ <- put (newSessionUrl sId) [partFile "Point.txs" "../../examps/Point/Point.txs"]
                _ <- post (valsUrl sId) [partText "i" "x = 42"]
                _ <- post (valsUrl sId) [partText "i" "y = 5"]
                r <- post (evalUrl sId) [partText "expr" "LET z = 7 IN x*y+5+z NI"]
                r ^. responseStatus . statusCode `shouldBe` 200 -- OK
                r ^. responseBody `shouldBe` "222"
                _ <- post (varsUrl sId) [partText "i" "t :: Int"]
                r2 <- post (evalUrl sId) [partText "expr" "IF t == t THEN 4 ELSE t FI"]
                r2 ^. responseStatus . statusCode `shouldBe` 200 -- OK
                r2 ^. responseBody `shouldBe` "4"
                _ <- post (varsUrl sId) [partText "i" "s :: String"]
                r3 <- post (evalUrl sId) [partText "expr" "len (s) < 0"]
                r3 ^. responseStatus . statusCode `shouldBe` 200 -- OK
                r3 ^. responseBody `shouldBe` "False"

check204NoContent :: HasCallStack => Response BSL.ByteString -> IO ()
check204NoContent r = r ^. responseStatus . statusCode `shouldBe` 204

parseFileUploadResult :: Object -> Parser (String, Bool)
parseFileUploadResult o = do
    fn <- o .: "fileName" :: Parser String
    l  <- o .: "loaded"   :: Parser Bool
    return (fn, l)

parseTimerResult :: Object -> Parser (String, String, String, String)
parseTimerResult o = do
    tn  <- o .: "timerName" :: Parser String
    stt <- o .: "startTime" :: Parser String
    stp <- o .: "stopTime" :: Parser String
    d   <- o .: "duration"  :: Parser String
    return (tn,stt,stp,d)

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

stepUrl :: Integer -> String
stepUrl sId = host ++ "/sessions/" ++ show sId ++ "/step/"

messagesUrl :: Integer -> String
messagesUrl sId = host ++ "/sessions/" ++ show sId ++ "/messages"

closeMessagesUrl :: Integer -> String
closeMessagesUrl sId = host ++ "/sessions/" ++ show sId ++ "/messages/close"

openMessagesUrl :: Integer -> String
openMessagesUrl sId = host ++ "/sessions/" ++ show sId ++ "/messages/open"

timerUrl :: Integer -> String -> String
timerUrl sId nm = Prelude.concat [host, "/sessions/", show sId, "/timers/", nm]

valsUrl :: Integer -> String
valsUrl sId = Prelude.concat [host, "/sessions/", show sId, "/vals"]

varsUrl :: Integer -> String
varsUrl sId = Prelude.concat [host, "/sessions/", show sId, "/vars"]

evalUrl :: Integer -> String
evalUrl sId = Prelude.concat [host, "/sessions/", show sId, "/eval"]

emptyP :: [Part]
emptyP = [partText "" ""]
