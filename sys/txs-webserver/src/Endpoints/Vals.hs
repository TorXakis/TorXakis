-- | End-points for the parsing of TorXakis entities.
module Endpoints.Vals
    ( ValsAPI
    , valsServer
    )
where

import           Servant
import           Servant.Multipart      (Mem, MultipartData, MultipartForm,
                                         iValue, inputs)

import           Common       (Env, SessionId, liftLib)
import qualified TorXakis.Lib as Lib

type ValsAPI =  "sessions"
            :> Capture "sid" SessionId
            :> "vals"
            :>
            (
                MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] String
            :<|>
                Get '[JSON] String
            )


valsServer :: Env -> Server ValsAPI
valsServer env sId = createVal :<|> getVals
    where
        createVal :: MultipartData Mem -> Handler String
        createVal mpData =
            case inputs mpData of
                (tIn:_rest) -> liftLib env sId (`Lib.createVal` iValue tIn)
                []          -> return "No input received"
        getVals :: Handler String
        getVals = liftLib env sId Lib.getVals

