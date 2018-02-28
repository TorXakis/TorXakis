{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module XmlFormat
  ( constToXml
  , constFromXml
  )
where

import           Control.Monad.State
import           Data.ByteString          (pack)
import           Data.ByteString.Internal (c2w)
import           Data.Char                (chr, ord)
import qualified Data.HashMap.Strict      as HMap
import           Data.Maybe
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Text.XML.Expat.Tree

import           ConstDefs
import           Name
import           Sort
import           TxsDefs


-- | Simple rose tree to represent the XML information that will be converted
-- to string. This is meant to serve as an intermediate representation of the
-- XML text string, so that `T.concat` is called only once.
data XMLTree = XLeaf Text | XNode Text [XMLTree]

xmlTreeToText :: XMLTree -> Text
xmlTreeToText tree = T.concat $ reverse $ execState (xmlTreeToList tree) []
  where
    xmlTreeToList :: XMLTree -> State [Text] ()
    xmlTreeToList (XLeaf text) = modify (text:)
    xmlTreeToList (XNode text ts) = do
      modify (T.concat ["<", text, ">" ]:)
      _ <- traverse xmlTreeToList ts
      modify (T.concat ["</", text, ">" ]:)

instance IsString XMLTree where
  fromString = XLeaf . T.pack

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

encodeString :: Text -> Text
encodeString = T.concatMap encodeChar

encodeChar :: Char -> Text
encodeChar '&' = "&amp;"
encodeChar '<' = "&lt;"
encodeChar  c  =
  if validChar c
  then T.singleton c
  else T.concat ["<char>", T.pack (show (ord c)), "</char>"]

-- | Make an XML node.
--
-- | Infix and right associative version of XNode.
(~>) :: Text -> [XMLTree] -> XMLTree
(~>) = XNode

infixr 6 ~>

lookupConstructorName :: TxsDefs -> Ref (ADTDef Sort) -> Text -> (Ref (ConstructorDef Sort), ConstructorDef Sort)
lookupConstructorName txsdefs aRef cNmTxt  =
    case HMap.lookup aRef (adtDefsToMap $ adtDefs txsdefs) of
        Nothing -> error $ "ADT " ++ show aRef ++ " not found in mapping"
        Just ad ->
            fromMaybe
                (error $ "Constructor " ++ T.unpack cNmTxt ++ " not found in ADT " ++ show aRef)
                (findConstructor cNm ad)
            where
                Right cNm = name cNmTxt

lookupConstructorDef :: TxsDefs -> Ref (ADTDef Sort) -> Ref (ConstructorDef Sort) -> ConstructorDef Sort
lookupConstructorDef txsdefs aRef cRef =
    case HMap.lookup aRef (adtDefsToMap $ adtDefs txsdefs) of
        Nothing -> error $ "ADT " ++ show aRef ++ " not found in mapping"
        Just ad ->
            fromMaybe
                (error $ "Constructor " ++ show cRef ++ " not found in ADT " ++ show aRef)
                (HMap.lookup cRef (cDefsToMap $ constructors ad))
-- ----------------------------------------------------------------------------------------- --
rootName :: Text
rootName = "TorXakisMsg"

constToXml :: TxsDefs -> Const -> Text
constToXml tdefs w =
  xmlTreeToText $ pairNameConstToXml tdefs (rootName, w)

pairNameConstToXml :: TxsDefs -> (Text, Const)  -> XMLTree
pairNameConstToXml _ (n, Cbool True) =
  n ~> ["true"]
pairNameConstToXml _ (n, Cbool False) =
  n ~> ["false"]
pairNameConstToXml _ (n, Cint i) =
  n ~> [XLeaf (T.pack (show i))]
pairNameConstToXml _ (n, Cstring s) =
  n ~> [XLeaf (encodeString s)]
pairNameConstToXml tdefs (n, Cstr aRef cRef cArgs) =
  let cDef = lookupConstructorDef tdefs aRef cRef
      nodes = map (pairNameConstToXml tdefs)
                  (zip (map toText $ getFieldNames cDef) cArgs)
      cName = toText $ constructorName cDef
  in
    n ~> [cName ~> nodes]
pairNameConstToXml _ (n, w) =
  error (   "XmlFormat - constToXml: " ++ show w
         ++ " - Const for name " ++ show n
         ++ " can not be translated to Xml"
        )

stringFromList :: [Node Text Text] -> Text
stringFromList = T.concat . go
  where
    go :: [Node Text Text] -> [Text]
    go [] = [""]
    go (Text a : xs) =
      a:go xs
    go (Element "char" [] [Text nrString] : xs ) =
      T.singleton (chr (read (T.unpack nrString))) : go xs
    go (x:_) =
      error ("XmlFormat - stringFromList : unexpected item " ++ show x)

constFromXml :: TxsDefs -> Sort -> Text -> Const
constFromXml tdefs sid s =
  case parse' defaultParseOptions{ overrideEncoding = Just ISO88591 } (pack $ map c2w (T.unpack s)) of
    Left err   -> error ("constFromXml parse error " ++ show err)
    Right tree -> pairNameConstFromXml tdefs sid tree rootName

pairNameConstFromXml :: TxsDefs -> Sort -> Node Text Text -> Text -> Const
pairNameConstFromXml _ SortBool (Element nt [] list) n
  | n == nt = Cbool   ("true" == stringFromList list)
pairNameConstFromXml _ SortInt     (Element nt [] list) n
  | n == nt = Cint    (read (T.unpack (stringFromList list)))
pairNameConstFromXml _ SortString (Element nt [] list) n
  | n == nt = Cstring (stringFromList list)
pairNameConstFromXml tdefs s@(SortADT aRef) (Element nt [] [Element cname [] list]) n
  | n == nt =
    let (cRef, cDef) = lookupConstructorName tdefs aRef cname
        sorts = getFieldSorts cDef
        vexprArgs =
          map (uncurry3 (pairNameConstFromXml tdefs))
              (zip3 sorts list (map toText $ getFieldNames cDef))
        uncurry3 f (a, b, c) = f a b c
    in
      if length sorts == length list
      then Cstr aRef cRef vexprArgs
      else error $  "XmlFormat - constFromXml: Number of arguments mismatch "
                 ++ "in constructor " ++ show cname
                 ++ " of sort " ++ show s
                 ++ " : definition " ++ show (length sorts)
                 ++ " vs actual " ++ show (length list) ++ "\n"
pairNameConstFromXml tdefs s i n =
    error $  "XmlFormat - constFromXml: Unexpected item "
          ++ show i
          ++ " for name " ++ show n
          ++ " with sort " ++ show s
          ++ " using tdefs " ++ show tdefs
          ++ "\n"
