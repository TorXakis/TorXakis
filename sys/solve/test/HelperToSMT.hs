{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module HelperToSMT

where

import           Data.Char
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Numeric (showHex)

escape :: Text -> Text
escape t
    | t == T.empty = T.empty
    | otherwise    =
        T.pack (case T.head t of
                    '"'                            -> "\"\""
                    '\\'                           -> "\\\\"
                    x | ord x < 16                 -> "\\x0" ++ showHex (ord x) ""
                    x | ord x < 32 || ord x >= 127 -> "\\x"  ++ showHex (ord x) ""
                    x                              -> [x]
                )
        <> escape (T.tail t)