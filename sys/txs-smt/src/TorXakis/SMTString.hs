{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.SMTString
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- decode SMT String.
-----------------------------------------------------------------------------
module TorXakis.SMTString

where

import Data.Text (Text)
import qualified Data.Text as T

import TorXakis.SMTStringAlex
import TorXakis.SMTStringHappy

-- | Decode SMT to String.
stringFromSMT :: Text -> Text
stringFromSMT = T.pack . smtStringParser . smtStringLexer . T.unpack

