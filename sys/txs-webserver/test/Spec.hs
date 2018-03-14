-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Lens                ((^.))
-- -- import           Data.ByteString
-- import qualified Data.IntMap.Strict          as Map
-- import           Servant                     (Application)

-- import API (app)
-- import Common (Env (..))

-- import Network.HTTP.Types (hContentType, hContentLength, methodPost)
-- import Network.Wai.Test   (SResponse)
import Test.Hspec
-- import Test.Hspec.Core.Hooks
-- import Test.Hspec.Wai
-- import Test.Hspec.Wai.JSON
import Network.Wreq

main :: IO ()
main = do
    s <- spec
    hspec s

spec :: IO Spec
spec = return $ do -- before getApp $ do
            describe "GET /users" $ -- do
                it "responds with 200 and [Users]" $ do
                    -- get "/users" `shouldRespondWith` 200
                    r <- get "http://localhost:8080/users"
                    r ^. responseStatus . statusCode `shouldBe` 200
                -- it "responds with [User]" $ do
                    let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
                    -- get "/users" `shouldRespondWith` users
                    -- r <- get "http://localhost:8080/users"
                    r ^. responseBody `shouldBe` users
            describe "Create new TorXakis session" $
                it "Creates 2 sessions" $ do
                    -- post "/session/new" "" `shouldRespondWith` "1" {matchStatus = 201}
                    -- post "/session/new" "" `shouldRespondWith` "2" {matchStatus = 201}
                    r <- post "http://localhost:8080/session/new" [partText "" ""]
                    r ^. responseStatus . statusCode `shouldBe` 201
                    r ^. responseBody `shouldBe` "1"
                    r2 <- post "http://localhost:8080/session/new" [partText "" ""]
                    r2 ^. responseStatus . statusCode `shouldBe` 201
                    r2 ^. responseBody `shouldBe` "2"
            describe "Upload files to a session" $ do
                it "Uploads valid files" $ do
                    -- post "/session/new" "" `shouldRespondWith` "1" {matchStatus = 201}
                    -- postPointTxs "/session/1/model" `shouldRespondWith` "\"Success\"" {matchStatus = 201}
                    _ <- post "http://localhost:8080/session/new" [partText "" ""]
                    r <- post "http://localhost:8080/session/1/model" [partFile "Point.txs" "c:/Repos/TorXakis/examps/Point/Point.txs"]
                    r ^. responseStatus . statusCode `shouldBe` 201
                    r ^. responseBody `shouldBe` "\"\\nLoaded: Point.txs\""
                it "Fails for parse error" $ do
                    -- post "/session/new" "" `shouldRespondWith` "1" {matchStatus = 201}
                    -- postWrongTxt "/session/1/model" `shouldRespondWith` "Something went wrong" {matchStatus = 500}
                    _ <- post "http://localhost:8080/session/new" [partText "" ""]
                    r <- post "http://localhost:8080/session/1/model" [partFile "wrong.txt" "C:/Repos/TorXakis/sys/txs-lib/test/data/wrong.txt"]
                    r ^. responseStatus . statusCode `shouldBe` 400
                    r ^. responseBody `shouldBe` "\"\\nLoaded: Point.txs\""

-- getApp :: IO Application
-- getApp = do
--     e <- getEnv
--     return $ app e

-- getEnv :: IO Env
-- getEnv = do
--     sessionsMap <- newTVarIO Map.empty
--     zeroId      <- newTVarIO 0
--     return $ Env sessionsMap zeroId

{-
postPointTxs :: ByteString -> WaiSession SResponse
postPointTxs path = request methodPost
                        path
                        [ (hContentType,   "multipart/form-data; boundary=---------------------------18666290479101")
                        , (hContentLength, "1705")
                        ]
                        "-----------------------------18666290479101\n\
                        \ Content-Disposition: form-data; name=\"foo\"; filename=\"Point.txs\"\n\
                        \ Content-Type: application/octet-stream\n\
                        \\n\
                        \ {-\n\
                        \ TorXakis - Model Based Testing\n\
                        \ Copyright (c) 2015-2017 TNO and Radboud University\n\
                        \ See LICENSE at root directory of this repository.\n\
                        \ -}\n\
                        \\n\
                        \ TYPEDEF Point ::= Point { x, y :: Int } ENDDEF\n\
                        \\n\
                        \ FUNCDEF validPoint (p :: Point) :: Bool ::=\n\
                        \     ( x(p) <> 0 ) /\\ ( y(p) <> 0 )\n\
                        \ ENDDEF\n\
                        \\n\
                        \ FUNCDEF inRange (p1, p2 :: Point) :: Bool ::=\n\
                        \     (x(p1) == x(p2)) \\/ (y(p1) == y(p2))\n\
                        \ ENDDEF\n\
                        \\n\
                        \ PROCDEF  allowedBehaviour  [ In, Out :: Point ]  ( ) ::=\n\
                        \         In ? p1 [[ validPoint(p1) ]]\n\
                        \     >-> Out ? p2 [[ validPoint(p2) /\\ inRange (p1,p2) ]]\n\
                        \     >-> allowedBehaviour [In,Out] ()\n\
                        \ ENDDEF\n\
                        \\n\
                        \ CHANDEF Channels ::=\n\
                        \     In, Out :: Point\n\
                        \ ENDDEF\n\
                        \\n\
                        \ MODELDEF  Model ::=\n\
                        \     CHAN IN     In\n\
                        \     CHAN OUT    Out\n\
                        \\n\
                        \     BEHAVIOUR   allowedBehaviour [ In, Out ] ( )\n\
                        \ ENDDEF\n\
                        \\n\
                        \ -- ----------------------------------------------------------------------------------------- --\n\
                        \\n\
                        \ CNECTDEF  Sut ::=\n\
                        \         CLIENTSOCK\n\
                        \\n\
                        \         CHAN OUT  In                        HOST \"localhost\"  PORT 9999\n\
                        \         ENCODE    In ? s                    ->  ! toString(s)\n\
                        \\n\
                        \         CHAN IN   Out                       HOST \"localhost\"  PORT 9999\n\
                        \         DECODE    Out ! fromString(s)       <-   ? s\n\
                        \ ENDDEF\n\
                        \\n\
                        \ CNECTDEF  Sim ::=\n\
                        \         SERVERSOCK\n\
                        \\n\
                        \         CHAN OUT  Out                       HOST \"localhost\"  PORT 9999\n\
                        \         ENCODE    Out ? s                   ->  ! toString(s)\n\
                        \       \n\
                        \         CHAN IN   In                        HOST \"localhost\"  PORT 9999\n\
                        \         DECODE    In ! fromString(s)        <-   ? s\n\
                        \ ENDDEF\n\
                        \\n\
                        \ -----------------------------18666290479101--\n"

postWrongTxt :: ByteString -> WaiSession SResponse
postWrongTxt path = request methodPost
                        path
                        [ (hContentType,   "multipart/form-data; boundary=---------------------------18666290479101")
                        , (hContentLength, "253")
                        ]
                         "-----------------------------18666290479101\n\
                        \ Content-Disposition: form-data; name=\"foo\"; filename=\"wrong.txt\"\n\
                        \ Content-Type: application/octet-stream\n\
                        \\n\
                        \ This file contains text that can't be parsed by TorXakis.\n\
                        \ -----------------------------18666290479101--\n"
-}