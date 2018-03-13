-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent.STM.TVar (newTVarIO)
import qualified Data.IntMap.Strict          as Map

import API (app)
import Common (Env (..))

import Test.Hspec
import Test.Hspec.Wai
-- import Test.Hspec.Wai.JSON

main :: IO ()
main = do
    s <- spec
    hspec s

spec :: IO Spec
spec = do
    e <- getEnv
    return $ with (return $ app e ) $
        describe "GET /users" $ do
            it "responds with 200" $
                get "/users" `shouldRespondWith` 200
            it "responds with [User]" $ do
                let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
                get "/users" `shouldRespondWith` users

getEnv :: IO Env
getEnv = do
    sessionsMap <- newTVarIO Map.empty
    zeroId      <- newTVarIO 0
    return $ Env sessionsMap zeroId