{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module RegexXSDtoSMT
where
import RegexAlex
import RegexSMTHappy

parseRegex :: String -> String
parseRegex = regexSMTParser . regexLexer