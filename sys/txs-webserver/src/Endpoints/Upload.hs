module Endpoints.Upload
( UploadEP
, upload
) where

import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Text                   as T
import           Data.Text.Lazy              (unpack)
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Servant
import           Servant.Multipart           (MultipartForm, MultipartData, Mem, FileData, files, fdFileName, fdPayload) --  iName, iValue, inputs

import           TorXakis.Lib                (load, Response (..))
import           TorXakis.Session            (Session)

import           Common (SessionId, TxsHandler, getSession)

type UploadEP = "session" :> Capture "sid" SessionId :> "model" :> MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] String

upload :: SessionId -> MultipartData Mem -> TxsHandler String
upload sid multipartData =  do
    s <- getSession sid
    liftIO $ do
        rs <- mapM (loadFile s) $ files multipartData
        let errMsg = concatErrorMsgs rs
        case errMsg of
            []   -> return $ show Success
            eMsg -> error eMsg

loadFile :: Session -> FileData Mem -> IO Response
loadFile s f = do
    let fnTxt = fdFileName f
    if fnTxt == T.empty
        then return Success
        else do
            putStrLn $ "Loading file: " ++ show fnTxt
            let contentTxt = decodeUtf8 $ fdPayload f
            r <- load s $ unpack contentTxt
            print r
            return r

concatErrorMsgs :: [Response] -> String
concatErrorMsgs []               = []
concatErrorMsgs (Error m : rs) = m ++ "\n" ++ concatErrorMsgs rs
concatErrorMsgs (Success : rs)   = concatErrorMsgs rs