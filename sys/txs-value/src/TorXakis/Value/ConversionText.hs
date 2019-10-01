{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConversionText
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Text conversions of values.
-----------------------------------------------------------------------------
module TorXakis.Value.ConversionText
( 
-- * Conversion from and to 'Data.Text.Text'
  valueToText
, valueFromText
)
where
import           Data.Char              (chr, ord)
import           Data.Either            (partitionEithers)
import           Data.List
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Regex.TDFA

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext
import           TorXakis.Value.Value
import           TorXakis.Value.ValueAlex
import           TorXakis.Value.ValueHappy

-- | Is Char a control charachter?
controlChar :: Char -> Bool
controlChar c = ord c == 127 || ord c < 32

-- | encode character to canconical representation: &#...; for non-printable characters, terminator char ('), and normal otherwise
-- encodeChar :: Char -> Char -> String
-- encodeChar terminator c | c == terminator || controlChar c = "&#" ++ show (ord c) ++ ";"
-- encodeChar _          c                                    = [c]

-- | decode character from canconical representation: &#...; for non-printable characters, terminator char ('), and normal otherwise
-- decodeChar :: String -> Char
-- decodeChar [c]                            = c
-- decodeChar s | s =~ "\\`&#[0-9]{1,3};\\'" = (chr . read . init . drop 2) s
-- decodeChar e                              = error ("decodeChar - unexpected string sequence " ++ show e)

-- | encode String to canconical representation: &#...; for non-printable characters, terminator char, escape symbol (&), and normal otherwise
encodeString :: Char -> String -> String
encodeString terminator = concatMap encodeStringChar
    where 
        encodeStringChar :: Char -> String
        encodeStringChar c | c == terminator || c == '&' || controlChar c = "&#" ++ show (ord c) ++ ";"
        encodeStringChar c                                                = [c]

-- | decode String from canconical representation: &#...; for non-printable characters, terminator char, escape symbol (&), and normal otherwise
decodeString :: String -> String
decodeString s = replaceMatches (getAllMatches (s =~ "&#[0-9]{1,3};")) escapedCharToString 0 s
    where
        replaceMatches :: [(MatchOffset, MatchLength)]
                       -> (String -> String)
                       -> Int 
                       -> String
                       -> String
        replaceMatches [] _ _ s' = s'
        replaceMatches ((off,len):ms) f pos s' =
            let (pre, rest)            = splitAt (off-pos) s'
                (matchText, remaining) = splitAt len rest
              in
                pre ++ f matchText ++ replaceMatches ms f (off+len) remaining

        escapedCharToString :: String -> String
        escapedCharToString = (: []) . chr . read . init . drop 2

-- | 'TorXakis.Value.Value' to 'Data.Text.Text' conversion.
valueToText :: SortContext c => c -> Value -> Text
valueToText ctx = valueViewToText ctx . view

valueViewToText :: SortContext c => c -> ValueView -> Text
valueViewToText _   (Cbool True)   = T.pack "True"
valueViewToText _   (Cbool False)  = T.pack "False"
valueViewToText _   (Cint i)       = T.pack (show i)
--valueViewToText _   (Cchar c)      = T.pack ("'" ++ encodeChar '\'' c ++ "'")
valueViewToText _   (Cstring s)    = T.pack ("\"" ++ encodeString '"' (T.unpack s) ++ "\"")
valueViewToText _   (Cadt _ c [])  = TorXakis.Name.toText (toName c)
valueViewToText ctx (Cadt _ c as)  =
        let cNode = (TorXakis.Name.toText . toName) c
          in
            cNode
            <> T.pack "("
            <> T.intercalate (T.pack ",") (map (valueToText ctx) as) 
            <> T.pack ")"
valueViewToText _   (Cany _)       = error "ANY not supported"

-- | 'TorXakis.Value.Value' from 'Data.Text.Text' conversion.
-- Expected 'TorXakis.Sort' of 'TorXakis.Value.Value' must be provided.
valueFromText :: SortContext c => c -> Sort -> Text -> Either Error Value
valueFromText ctx s t = 
    let p :: ParseValue
        p = (valueParser . valueLexer . T.unpack) t
      in
        fromParseValue s p
    where
        fromParseValue :: Sort -> ParseValue -> Either Error Value
        fromParseValue SortBool    (Pbool b)    = Right $ mkBool b
        fromParseValue SortInt     (Pint b)     = Right $ mkInt b
        fromParseValue SortString  (Pstring s') = Right $ mkString (T.pack (decodeString s'))
        fromParseValue (SortADT a) (Pcstr ctext ps) =
            case mkName ctext of
                Left e      -> Left $ Error ("Illegal name " ++ show ctext ++ "\n" ++ show e)
                Right cname -> let adtDef = fromMaybe (error ("ADTDef "++ show a ++ " not in context"))
                                                      (lookupADT (toName a) ctx)
                                in case lookupConstructor cname adtDef of
                                        Nothing   -> Left $ Error ("Constructor " ++ show cname ++  " not defined for ADT " ++ show a)
                                        Just cDef -> let fs = elemsField cDef
                                                         actual = length fs
                                                         expected = length ps
                                                      in if actual == expected
                                                            then case partitionEithers (zipWith fromParseValue (map TorXakis.Sort.sort fs) ps) of
                                                                      ([], vs) -> mkADT ctx a (RefByName cname) vs
                                                                      (es, _)  -> Left $ Error $ intercalate "\n" (map show es)
                                                            else Left $ Error ("Fields mismatch - expected " ++ show expected ++ " yet actual " ++ show actual)
        fromParseValue s' p                     = Left $ Error ("Sort " ++ show s' ++ " mismatch with parsed value " ++ show p ++ "\nNote ANY is not supported")