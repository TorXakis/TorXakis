-- {-# LANGUAGE OverloadedStrings  #-}
module Endpoints.Stepper
( StartStepperEP
, startStepper
) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text)
import           Servant

import           TorXakis.Lib                (stepper, Response (..))

import           Common (SessionId, TxsHandler, getSession)

type StartStepperEP = "stepper" :> "start" :> Capture "sid" SessionId :> Capture "model" Text :> Post '[JSON] String

startStepper :: SessionId -> Text -> TxsHandler String
startStepper sid model =  do
    s <- getSession sid
    r <- liftIO $ do
            putStrLn $ "Starting stepper for model " ++ show model ++ " in session " ++ show sid 
            r <- stepper s model
            print r
            return r
    case r of
        Success -> return $ show Success
        Error m -> error m
