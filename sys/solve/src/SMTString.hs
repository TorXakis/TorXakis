{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module SMTString

where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf


import SMTStringAlex
import SMTStringHappy

-- | Encode String to SMT.
--
--   According to smt-lib-version 2.5 standard (http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf),
--   quote and escape characters are escaped.
--   
--   Furthermore, prevent CVC4 Parse Error "Extended/unprintable characters are not part of SMT-LIB, and they must be encoded as escape sequences"
stringToSMT :: Text -> Text
stringToSMT = T.concatMap toSMTChar
    where
        toSMTChar :: Char -> Text
        toSMTChar '"' = "\"\""
        toSMTChar '\\' = "\\\\"
        toSMTChar c  | ord c < 32 || ord c >= 127     = T.pack $ printf "\\x%02x" (ord c)
        toSMTChar c                                   = T.singleton c
        
-- | Decode SMT to String.
stringFromSMT :: Text -> Text
stringFromSMT = T.pack . smtStringParser . smtStringLexer . T.unpack

