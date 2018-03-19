{-# LANGUAGE OverloadedStrings  #-}
module Endpoints.Upload
( UploadEP
, upload
) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString.Lazy.Char8  (pack)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
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
    res <- liftIO $
        case files multipartData of
            [] -> return $ Left "No files received"
            fs -> do
                rs <- mapM (loadFile s) fs
                let (errMsg,succMsg) = concatErrorMsgs rs T.empty T.empty
                case T.unpack errMsg of
                    []   -> return $ Right $ T.unpack succMsg
                    eMsg -> return $ Left eMsg
    case res of
        Left  e -> throwError err400 { errBody = pack e }
        Right m -> return m

loadFile :: Session -> FileData Mem -> IO (Text,Response)
loadFile s f = do
    let fnTxt = fdFileName f
        contentTxt = decodeUtf8 $ fdPayload f
    if contentTxt == LT.empty
        then return (fnTxt, Error "Empty file")
        else do
            putStrLn $ "Loading file: " ++ show fnTxt
            r <- load s $ LT.unpack contentTxt
            print r
            return (fnTxt,r)

concatErrorMsgs :: [(Text, Response)] -> Text -> Text -> (Text, Text)
concatErrorMsgs []                   em sm = (em,sm)
concatErrorMsgs ( (fn,Error m) : rs) em sm = concatErrorMsgs rs (em <> "\nError in " <> fn <> ": " <> T.pack m) sm
concatErrorMsgs ( (fn,Success) : rs) em sm = concatErrorMsgs rs em (sm <> "\nLoaded: " <> fn)