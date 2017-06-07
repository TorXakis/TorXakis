{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module RegexXSDtoSMT
where
import RegexAlex
import RegexSMTHappy

parseRegex :: String -> String
parseRegex = regexSMTParser . regexLexer