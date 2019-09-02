{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConversionXML
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- XML conversions of values.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Value.ConversionXML
( 
-- * Conversion from and to XML
  valueToXML
, valueFromXML
)
where
import           Data.ByteString          (pack)
import           Data.ByteString.Internal (c2w)
import           Data.Char                (chr, ord)
import           Data.Either              (partitionEithers)
import           Data.List
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Text.XML.Expat.Tree

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Regex
import           TorXakis.Sort
import           TorXakis.SortContext
import           TorXakis.Value.Value


-- TODO: See original file for optimization that call T.concat only once
-- solution is to use foldl to make a list of Text that only finally is concated once

rootNodeName :: Text
rootNodeName = "TorXakis"

-- | char node Name
-- for escaping not supported characted in Xml by using their value in a special node.
charNodeName :: Text
charNodeName = "char"

-- | node Text To Xml
nodeTextToXML :: Text -> Text -> Text
nodeTextToXML n t = T.concat ["<", n, ">", t, "</", n, ">"]

-- | valid Char
-- Assumptions : Char in extended ASCII (i.e. ord c in [0..255]  { thus [0x0..0xFF] } )
--               Use xml 1.0
-- see http://www.w3.org/TR/xml/#charsets
-- note: the discouraged characters [#x7F-#x84] and [#x86-#x9F] are valid
validChar :: Char -> Bool
validChar c = let i = ord c in
                    i >= 0x20
                 || case i of
                        0x9 -> True
                        -- 0xA -> True   -- enable TorXakis' line based communication
                        -- 0xD -> True   -- enable round tripping (\r changed in \n)
                        _   -> False

-- | encode Char to Xml
encodeChar :: Char -> Text
encodeChar '&' = "&amp;"
encodeChar '<' = "&lt;"
encodeChar  c  =
  if validChar c
    then T.singleton c
    else nodeTextToXML charNodeName (T.pack (show (ord c)))

-- | encode String to Xml
encodeString :: Text -> Text
encodeString = T.concatMap encodeChar

-- | 'TorXakis.Value.Value' to XML conversion.
valueToXML :: SortContext c => c -> Value -> Text
valueToXML ctx = pairToXML rootNodeName
    where
        pairToXML :: Text -> Value -> Text
        pairToXML node (Cbool True)   = nodeTextToXML node "true"
        pairToXML node (Cbool False)  = nodeTextToXML node "false"
        pairToXML node (Cint i)       = nodeTextToXML node (T.pack (show i))
        pairToXML node (Cchar c)      = nodeTextToXML node (encodeChar c)
        pairToXML node (Cstring s)    = nodeTextToXML node (encodeString s)
        pairToXML node (Cregex r)     = nodeTextToXML node (encodeString (toXsd r))
        pairToXML node (Ccstr a c as) = 
            let adtDef = fromMaybe (error ("ADTDef " ++ show a ++ " not in context"))
                                   (lookupADT (toName a) ctx)
                cstrDef = fromMaybe (error ("cstrDef " ++ show c ++ " not in ADTDef " ++ show a))
                                    (lookupConstructor (toName c) adtDef)
                cNode = (TorXakis.Name.toText . constructorName) cstrDef
                fieldTexts = map (TorXakis.Name.toText . fieldName) ( elemsField cstrDef )
                txt = nodeTextToXML cNode (T.concat (zipWith pairToXML fieldTexts as))
              in nodeTextToXML node txt
        pairToXML _   (Cany _)       = error "ANY not supported"


stringFromList :: [Node Text Text] -> Text
stringFromList = T.concat . go
  where
    go :: [Node Text Text] -> [Text]
    go [] = [""]
    go (Text a : xs) =
      a:go xs
    go (Element n [] [Text nrString] : xs ) | n == charNodeName =
      T.singleton (chr (read (T.unpack nrString))) : go xs
    go (x:_) =
      error ("ConversionXML - stringFromList : unexpected item " ++ show x)

-- | 'TorXakis.Value.Value' from XML conversion.
-- Expected 'TorXakis.Sort' of 'TorXakis.Value.Value' must be provided.
valueFromXML :: SortContext c => c -> Sort -> Text -> Either Error Value
valueFromXML ctx s t =
    case parse' defaultParseOptions{ overrideEncoding = Just ISO88591 } (pack $ map c2w (T.unpack t)) of
        Left e     -> Left $ Error ("Parse error " ++ show e)
        Right tree -> fromXML s rootNodeName tree
    where
        fromXML :: Sort -> Text -> Node Text Text -> Either Error Value
        fromXML SortBool   n (Element nt [] list) | nt == n
                = Right $ Cbool ("true" == stringFromList list)
        fromXML SortInt    n (Element nt [] list) | nt == n
                = Right $ Cint (read (T.unpack (stringFromList list)))
        fromXML SortChar   n (Element nt [] list) | nt == n
                = let str = T.unpack (stringFromList list) in
                    case str of
                        [c] -> Right $ Cchar c
                        _   -> Left $ Error (T.pack ("Char should have length 1, yet is " ++ show str))
        fromXML SortString n (Element nt [] list) | nt == n
                = Right $ Cstring (stringFromList list)
        fromXML SortRegex  n (Element nt [] list) | nt == n
                = let str = stringFromList list in
                    case fromXsd str of
                        Left e -> Left $ Error ("Unable to parse regex `" ++ show str ++ "` with error "++ show e)
                        Right r -> Right $ Cregex r
        fromXML (SortADT a) n (Element nt [] [Element ctext [] list]) | nt == n
                = case mkName ctext of
                    Left e      -> Left $ Error ("Illegal name " ++ show ctext ++ "\n" ++ show e)
                    Right cname -> let adtDef = fromMaybe (error ("ADTDef "++ show a ++ " not in context"))
                                                          (lookupADT (toName a) ctx)
                                     in case lookupConstructor cname adtDef of
                                            Nothing   -> Left $ Error ("Constructor " ++ show cname ++ " not defined for ADT " ++ show a)
                                            Just cDef -> let fs = elemsField cDef
                                                             actual = length fs
                                                             expected = length list
                                                          in if actual == expected
                                                            then case partitionEithers (zipWith3 fromXML (map TorXakis.Sort.sort fs) (map (TorXakis.Name.toText . fieldName) fs) list) of
                                                                      ([], vs) -> Right $ Ccstr a (RefByName cname) vs
                                                                      (es, _)  -> Left $ Error $ intercalate "\n" (map show es)
                                                            else Left $ Error ("Fields mismatch - expected " ++ show expected ++ " yet actual " ++ show actual)
        fromXML s' n l = Left $ Error ("Sort " ++ show s' ++ " of node " ++ show n ++ " mismatch with XML value " ++ show l)