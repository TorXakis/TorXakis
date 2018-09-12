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
import           Data.Either     (partitionEithers)
import qualified Data.HashMap    as Map
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.XML.Expat.Tree

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Value.Value


-- TODO: See original file for optimization that call T.concat only once
-- solution is to use foldl to make a list of Text that only finally is concated once

rootNodeName :: Text
rootNodeName = "TorXakis"

charNodeName :: Text
charNodeName = "char"

anyValue :: Text
anyValue = "ANY"

nodeTextToXML :: Text -> Text -> Text
nodeTextToXML n t = T.concat ["<", n, ">", t, "</", n, ">"]

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

encodeChar :: Char -> Text
encodeChar '&' = "&amp;"
encodeChar '<' = "&lt;"
encodeChar  c  =
  if validChar c
    then T.singleton c
    else nodeTextToXML charNodeName (T.pack (show (ord c)))

encodeString :: Text -> Text
encodeString = T.concatMap encodeChar

-- | 'TorXakis.Value.Value' to XML conversion.
valueToXML :: SortContext a => a -> Value -> Text
valueToXML ctx = pairToXML rootNodeName
    where
        pairToXML :: Text -> Value -> Text
        pairToXML node (Cbool True)   = nodeTextToXML node "true"
        pairToXML node (Cbool False)  = nodeTextToXML node "false"
        pairToXML node (Cint i)       = nodeTextToXML node (T.pack (show i))
        pairToXML node (Cchar c)      = nodeTextToXML node (encodeChar c)
        pairToXML node (Cstring s)    = nodeTextToXML node (encodeString s)
        pairToXML node (Cregex r)     = nodeTextToXML node (encodeString r)
        pairToXML node (Ccstr a c as) = 
            let adtDef = fromMaybe (error ("ADTDef " ++ show a ++ " not in context"))
                                   (Map.lookup a (adtDefs ctx))
                cstrDef = fromMaybe (error ("cstrDef " ++ show c ++ " not in ADTDef " ++ show a))
                                    (Map.lookup c ((constructors . viewADTDef) adtDef))
                fieldTexts = map (TorXakis.Name.toText . fieldName) ( (fields . viewConstructorDef) cstrDef )
                cNode = (TorXakis.Name.toText . toName) c
                txt = nodeTextToXML cNode (T.concat (zipWith pairToXML fieldTexts as))
              in nodeTextToXML node txt
        pairToXML node (Cany _)       = nodeTextToXML node anyValue


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
valueFromXML :: SortContext a => a -> Sort -> Text -> Either MinError Value
valueFromXML ctx s t =
    case parse' defaultParseOptions{ overrideEncoding = Just ISO88591 } (pack $ map c2w (T.unpack t)) of
        Left e     -> Left $ MinError (T.pack ("Parse error " ++ show e))
        Right tree -> fromXML s rootNodeName tree
    where
        fromXML :: Sort -> Text -> Node Text Text -> Either MinError Value
        fromXML s'         n (Element nt [] list) | nt == n && anyValue == stringFromList list
                = Right $ Cany s'
        fromXML SortBool   n (Element nt [] list) | nt == n
                = Right $ Cbool ("true" == stringFromList list)
        fromXML SortInt    n (Element nt [] list) | nt == n
                = Right $ Cint (read (T.unpack (stringFromList list)))
        fromXML SortChar   n (Element nt [] list) | nt == n
                = let str = T.unpack (stringFromList list) in
                    case str of
                        [c] -> Right $ Cchar c
                        _   -> Left $ MinError (T.pack ("Char should have length 1, yet is " ++ show str))
        fromXML SortString n (Element nt [] list) | nt == n
                = Right $ Cstring (stringFromList list)
        fromXML SortRegex  n (Element nt [] list) | nt == n
                = Right $ Cregex (stringFromList list)
        fromXML (SortADT a) n (Element nt [] [Element cname [] list]) | nt == n
                = case mkName cname of
                    Left e   -> Left $ MinError (T.pack "Illegal name " <> n <> T.pack "\n" <> TorXakis.Error.toText e)
                    Right n' -> let adtDef = fromMaybe (error ("ADTDef "++ show a ++ " not in context"))
                                                       (Map.lookup a (adtDefs ctx))
                                    c = RefByName n'
                                  in case Map.lookup c ( (constructors . viewADTDef) adtDef ) of
                                        Nothing   -> Left $ MinError (T.pack "Constructor " <> n <> T.pack " not defined for ADT " <> (TorXakis.Name.toText . toName) a)
                                        Just cDef -> let fs = (fields . viewConstructorDef) cDef
                                                         actual = length fs
                                                         expected = length list
                                                      in if actual == expected
                                                        then case partitionEithers (zipWith3 fromXML (map sort fs) (map (TorXakis.Name.toText . fieldName) fs) list) of
                                                                  ([], vs) -> Right $ Ccstr a c vs
                                                                  (es, _)  -> Left $ MinError $ T.intercalate (T.pack "\n") (map TorXakis.Error.toText es)
                                                        else Left $ MinError (T.pack ("Fields mismatch - expected " ++ show expected ++ " yet actual " ++ show actual))
        fromXML s' n l = Left $ MinError (T.pack ("Sort " ++ show s' ++ " of node " ++ show n ++ " mismatch with XML value " ++ show l))