{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
module HelperToSMT

where
import Data.Char
import Numeric (showHex)

escape :: String -> String
escape [] = []
escape (x:xs) 
    | x == '"'                      = "\"\"" ++ escape xs
    | x == '\\'                     = "\\\\" ++ escape xs
    | ord x < 16                    = "\\x0" ++ showHex (ord x) (escape xs)
    | ord x < 32 || ord x >= 127    = "\\x"  ++ showHex (ord x) (escape xs)
    | otherwise = x:escape xs