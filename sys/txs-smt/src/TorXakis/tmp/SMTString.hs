{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.SMTString

where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T



import SMTStringAlex
import SMTStringHappy


        
-- | Decode SMT to String.
stringFromSMT :: Text -> Text
stringFromSMT = T.pack . smtStringParser . smtStringLexer . T.unpack

