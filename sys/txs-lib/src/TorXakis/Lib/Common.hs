{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Common where

import           Control.Exception    (SomeException)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Data.Text            (Text)
import qualified Data.Text            as T

type Error = Text
type Response a = Either Error a

success :: Response ()
success = Right ()

runResponse :: ExceptT Text IO a -> IO (Response a)
runResponse = runExceptT

-- | Show exception as `Text`
showEx :: SomeException -> Text
showEx = T.pack . show
