{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ConnectionDefs
where

import           Control.DeepSeq
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           ChanId
import           ValExprDefs
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- Connection Definitions

data  CnectType     =  ClientSocket
                     | ServerSocket
     deriving (Eq,Ord,Read,Show, Generic, NFData)



data  ConnDef       =  ConnDtoW  { chan     :: ChanId
                                 , hostname :: Text
                                 , portnr   :: Integer
                                 , vars     :: [VarId]         -- encoding domain
                                 , vexpr    :: VExpr           -- encoding range of String
                                 }
                     | ConnDfroW { chan     :: ChanId
                                 , hostname :: Text
                                 , portnr   :: Integer
                                 , var      :: VarId           -- decoding domain of String
                                 , vexprs   :: [VExpr]         -- decoding range
                                 }
     deriving (Eq,Ord,Read,Show, Generic, NFData)


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
