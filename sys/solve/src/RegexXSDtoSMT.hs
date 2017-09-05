{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module RegexXSDtoSMT
where

import           Data.Text     (Text)
import qualified Data.Text     as T
import           RegexAlex
import           RegexSMTHappy

parseRegex :: Text -> Text
parseRegex = regexSMTParser . regexLexer . T.unpack
