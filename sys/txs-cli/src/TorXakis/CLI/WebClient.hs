{-# LANGUAGE ScopedTypeVariables #-}
-- | Web client for `txs-webserver`.
module TorXakis.CLI.WebClient
    (load, stepper)
where

--import           Data.Aeson             (FromJSON, decode)

--import           Network.Wreq
import           Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Data.List                  (intercalate)
-- import           Data.Text                  (Text)
-- import           GHC.Generics               (Generic)
import           Control.Arrow          ((+++))
import           Control.Exception      (try)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Text              as T
import           Network.HTTP.Client    (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException))
import           Network.Wreq
import           System.FilePath        (takeFileName)

import           TorXakis.CLI.Env

-- | Load a list of files using the given environment.
load :: MonadIO m => Env -> [FilePath] -> m (Either String ())
load env files = liftIO $
    fmap (mapException +++ const ())  (try (put url pfs))
    where fns = map takeFileName files
          pfs  = zipWith partFile (map T.pack fns) files
          host = txsHost env
          sid  = sessionId env
          url  = concat [host, "sessions/", show sid, "/model"]

mapException :: HttpException -> String
mapException (HttpExceptionRequest _ (StatusCodeException _ msg)) = BS.unpack msg
mapException e                          = show e -- TODO: give a more informative error.

stepper :: MonadIO m => Env -> String -> m (Either String ())
stepper = undefined
