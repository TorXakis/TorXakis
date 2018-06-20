{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Main (main) where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, wait)
import           Control.Exception            (catch, throwIO)
import           Data.Aeson                   (decode, decodeStrict)
import           Data.Aeson.Types             (Object, Parser, parseMaybe,
                                               toJSON, (.:))
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.List                    (isInfixOf)
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
                r ^. responseStatus . statusCode `shouldBe` 200 -- OK
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
                r ^. responseStatus . statusCode `shouldBe` 200 -- OK
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
        describe "Tester" $ do
            it "Starts SUT & tester and takes 20 steps" $
                withCreateProcess (proc "java" ["-cp","../../examps/LuckyPeople/sut","LuckyPeople"])
                                  {std_out = NoStream} $ \_stdin _stdout _stderr _ph -> do
                    sId <- mkNewSession
                    _ <- put (newSessionUrl sId) [partFile "model.txs" "../../examps/LuckyPeople/spec/LuckyPeople.txs"]
                    post (setTestUrl sId) [ partString "model" "Model"
                                          , partString "cnect" "Sut"
                                          , partString "purp&map" ""
                                          ] >>= check204NoContent
                    threadDelay (10 ^ (4 :: Int))
                    let twentySteps = toJSON (NumberOfSteps 20)
                    post (testUrl sId) twentySteps >>= check204NoContent
                    post (openMessagesUrl sId) emptyP >>= check204NoContent
                    a <- async $ foldGet checkActions 0 (messagesUrl sId)
                    threadDelay (2 * 10 ^ (6 :: Int))
                    post (closeMessagesUrl sId) emptyP >>= check204NoContent
                    totalSteps <- wait a
                    totalSteps `shouldBe` 20
            it "Tests with purpose" $
                withCreateProcess (proc "java" ["-cp","../../examps/LuckyPeople/sut","LuckyPeople"])
                                  {std_out = NoStream} $ \_stdin _stdout _stderr _ph -> do
                    sId <- mkNewSession
                    _ <- put (newSessionUrl sId) [ partFile "model.txs" "../../examps/LuckyPeople/spec/LuckyPeople.txs"
                                                 , partFile "purp.txs" "../../examps/LuckyPeople/spec/PurposeExamples.txs"
                                                 ]
                    post (openMessagesUrl sId) emptyP >>= check204NoContent
                    a <- async $ foldGet checkActions 0 (messagesUrl sId)
                    post (setTestUrl sId) [ partString "model" "Model"
                                          , partString "cnect" "Sut"
                                          , partString "purp&map" "PurposeExamples"
                                          ] >>= check204NoContent

                    let fiftySteps = toJSON (NumberOfSteps 50)
                    post (testUrl sId) fiftySteps >>= check204NoContent
                    threadDelay (2 * 10 ^ (6 :: Int))

                    post (closeMessagesUrl sId) emptyP >>= check204NoContent
                    totalSteps <- wait a
                    totalSteps `shouldBe` 36
        describe "Simulator" $
            it "Starts simulator & tester and tests 10 steps, then stops" $ do
                sSimId <- mkNewSession
                _ <- put (newSessionUrl sSimId) [partFile "model.txs" "../../examps/Echo/Echo.txs"]
                post (openMessagesUrl sSimId) emptyP >>= check204NoContent
                aSim <- async $ foldGet checkActions 0 (messagesUrl sSimId)
                _ <- put (paramUrl sSimId) [ partString "param_Sim_deltaTime" "100" ]
                aSetSim <- async $ post (setSimUrl sSimId) [ partString "model" "Model"
                                        , partString "cnect" "Sim"
                                        , partString "map"   ""
                                        ] >>= check204NoContent

                sTestId <- mkNewSession
                _ <- put (newSessionUrl sTestId) [partFile "model.txs" "../../examps/Echo/Echo.txs"]
                post (openMessagesUrl sTestId) emptyP >>= check204NoContent
                aTest <- async $ foldGet checkActions 0 (messagesUrl sTestId)
                _ <- put (paramUrl sSimId) [ partString "param_Sut_deltaTime" "10000" ]
                post (setTestUrl sTestId) [ partString "model" "Model"
                                            , partString "cnect" "Sut"
                                            , partString "map"   ""
                                            ] >>= check204NoContent
                _ <- wait aSetSim

                post (testUrl sTestId) (toJSON $ NumberOfSteps 10) >>= check204NoContent
                post (simUrl sSimId) (toJSON $ NumberOfSteps 15) >>= check204NoContent
                threadDelay (5 * 10 ^ (6 :: Int))

                post (closeMessagesUrl sTestId) emptyP >>= check204NoContent
                post (closeMessagesUrl sSimId) emptyP >>= check204NoContent
                testSteps <- wait aTest
                simSteps <- wait aSim
                testSteps `shouldBe` 10
                simSteps `shouldBe` 15

                post (openMessagesUrl sSimId) emptyP >>= check204NoContent
                aSimStop <- async $ foldGet checkStops 0 (messagesUrl sSimId)
                post (openMessagesUrl sTestId) emptyP >>= check204NoContent
                aTestStop <- async $ foldGet checkStops 0 (messagesUrl sTestId)

                post (stopUrl sSimId) emptyP >>= check204NoContent
                post (stopUrl sTestId) emptyP >>= check204NoContent

                post (setStepUrl sTestId) emptyP >>= check204NoContent
                post (stopUrl sTestId) emptyP >>= check204NoContent

                post (closeMessagesUrl sTestId) emptyP >>= check204NoContent
                post (closeMessagesUrl sSimId) emptyP >>= check204NoContent
                testStops <- wait aTestStop
                simStops <- wait aSimStop
                testStops `shouldBe` 2
                simStops `shouldBe` 1
        describe "Menu" $
            it "returns menu for different arguments" $
                withCreateProcess (proc "java" ["-cp","../../examps/LuckyPeople/sut","LuckyPeople"])
                                  {std_out = NoStream} $ \_stdin _stdout _stderr _ph -> do
                    sId <- mkNewSession
                    _ <- put (newSessionUrl sId) [ partFile "model.txs" "../../examps/LuckyPeople/spec/LuckyPeople.txs"
                                                 , partFile "purp.txs" "../../examps/LuckyPeople/spec/PurposeExamples.txs"
                                                 ]
                    r1 <- post (menuUrl sId) [ partString "args" "" ]
                    let m1 = BSL.unpack $ r1 ^. responseBody
                    Prelude.putStrLn $ "Empty args: " ++ m1
                    r2 <- post (menuUrl sId) ([ ] :: [Part])
                    let m2 = BSL.unpack $ r2 ^. responseBody
                    Prelude.putStrLn $ "No args: " ++ m2
                    r3 <- post (menuUrl sId) [ partString "args" "in" ]
                    let m3 = BSL.unpack $ r3 ^. responseBody
                    Prelude.putStrLn $ "in: " ++ m3
                    r4 <- post (menuUrl sId) [ partString "args" "out" ]
                    let m4 = BSL.unpack $ r4 ^. responseBody
                    Prelude.putStrLn $ "out: " ++ m4
                    r5 <- post (menuUrl sId) [ partString "args" "map" ]
                    let m5 = BSL.unpack $ r5 ^. responseBody
                    Prelude.putStrLn $ "map: " ++ m5
                    r6 <- post (menuUrl sId) [ partString "args" "purp PurposeExamples" ]
                    let m6 = BSL.unpack $ r6 ^. responseBody
                    Prelude.putStrLn $ "purp PurposeExamples: " ++ m6
                    m1 `shouldNotSatisfy` null
                    m2 `shouldNotSatisfy` null
                    m3 `shouldNotSatisfy` null
                    m4 `shouldNotSatisfy` null
                    m5 `shouldNotSatisfy` null
                    m6 `shouldNotSatisfy` null
        describe "LPE" $
            it "Calculate for Echo" $ do
                sId <- mkNewSession
                _ <- put (newSessionUrl sId) [ partFile "model.txs" "../../examps/Echo/Echo.txs" ]

                post (lpeUrl sId) [ partString "args" "proc [ In, Out ] ()" ] >>= check202Accepted
                rP <- get (showUrl sId "procdef" "LPE_proc")
                rP ^. responseStatus . statusCode `shouldBe` 200 -- OK
                let lpeProcStr = BSL.unpack $ rP ^. responseBody
                lpeProcStr `shouldStartWith` "( LPE_proc,"

                post (lpeUrl sId) [ partString "args" "Model" ] >>= check202Accepted
                rM <- get (showUrl sId "modeldef" "LPE_Model")
                rM ^. responseStatus . statusCode `shouldBe` 200 -- OK
                let lpeModelStr = BSL.unpack $ rM ^. responseBody
                lpeModelStr `shouldStartWith` "( LPE_Model,"

                -- Currently fails due to bug #735 in LPE
                -- rP2 <- get (showUrl sId "procdef" "LPE_proc")
                -- rP2 ^. responseStatus . statusCode `shouldBe` 200 -- OK
                -- let lpeProcStr2 = BSL.unpack $ rP2 ^. responseBody
                -- lpeProcStr2 `shouldStartWith` "( LPE_proc,"
        describe "Solve" $
            it "Solves normal, uni and ran" $ do
                sId <- mkNewSession
                _ <- put (newSessionUrl sId) [partFile "Point.txs" "../../examps/Point/Point.txs"]
                _ <- post (varsUrl sId) [partText "i" "x :: Int"]
                _ <- post (varsUrl sId) [partText "i" "y :: Int"]
                _ <- post (valsUrl sId) [partText "i" "z = 4"]
                rSol <- post (solveUrl sId "sol") [partText "expr" "x+y==z"]
                rSol ^. responseStatus . statusCode `shouldBe` 200 -- OK
                rSol ^. responseBody `shouldBe` "[ ( x, 0 ) ( y, 4 ) ]"
                rUni <- post (solveUrl sId "uni") [partText "expr" "(x+y==z) /\\ (x == 1)"]
                rUni ^. responseStatus . statusCode `shouldBe` 200 -- OK
                rUni ^. responseBody `shouldBe` "[ ( x, 1 ) ( y, 3 ) ]"
                rRan <- post (solveUrl sId "ran") [partText "expr" "x+y==z"]
                rRan ^. responseStatus . statusCode `shouldBe` 200 -- OK
                let ranRes = BSL.unpack $ rRan ^. responseBody
                ranRes `shouldContain` "( x,"
                ranRes `shouldContain` "( y,"
check204NoContent :: HasCallStack => Response BSL.ByteString -> IO ()
check204NoContent r = r ^. responseStatus . statusCode `shouldBe` 204

check202Accepted :: HasCallStack => Response BSL.ByteString -> IO ()
check202Accepted r = r ^. responseStatus . statusCode `shouldBe` 202

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
    -- print bs
    case parseMaybe parseTag jsonObj of
        Just "AnAction" -> return $ steps + 1
        _               -> return steps

checkStops :: Int -> BS.ByteString -> IO Int
checkStops stops bs = do
    let Just jsonBs  = BS.stripPrefix "data:" bs
        Just jsonObj = decodeStrict jsonBs
    -- print bs
    case parseMaybe parseTag jsonObj of
        Just "TXS_CORE_USER_INFO" ->
            let Just str = parseMaybe parseS jsonObj
            in  if "stopped" `isInfixOf` str
                    then return $ stops + 1
                    else return stops
        _ -> return stops

parseTag :: Object -> Parser String
parseTag o = o .: "tag"

parseS :: Object -> Parser String
parseS o = o .: "s"

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

setTestUrl :: Integer -> String
setTestUrl sId = Prelude.concat [host, "/sessions/", show sId, "/set-test"]

testUrl :: Integer -> String
testUrl sId = host ++ "/sessions/" ++ show sId ++ "/test/"

setSimUrl :: Integer -> String
setSimUrl sId = Prelude.concat [host, "/sessions/", show sId, "/set-sim"]

simUrl :: Integer -> String
simUrl sId = host ++ "/sessions/" ++ show sId ++ "/sim/"

stopUrl :: Integer -> String
stopUrl sId = host ++ "/sessions/" ++ show sId ++ "/stop"

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

solveUrl :: Integer -> String -> String
solveUrl sId kind = Prelude.concat [host, "/sessions/", show sId, "/solve/", kind]

paramUrl :: Integer -> String
paramUrl sId = Prelude.concat [host, "/sessions/", show sId, "/params"]

menuUrl :: Integer -> String
menuUrl sId = Prelude.concat [host, "/sessions/", show sId, "/menu"]

lpeUrl :: Integer -> String
lpeUrl sId = Prelude.concat [host, "/sessions/", show sId, "/lpe"]

showUrl :: Integer -> String -> String -> String
showUrl sId item nm= Prelude.concat [host, "/sessions/", show sId, "/show/", item, "/", nm]

emptyP :: [Part]
emptyP = [partText "" ""]
