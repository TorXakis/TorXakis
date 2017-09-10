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
import           CstrId
import           Data.ByteString          (pack)
import           Data.ByteString.Internal (c2w)
import           Data.Char                (chr, ord)
import           Data.Foldable
import qualified Data.Map                 as Map
import           Data.Monoid
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as T
import           FuncId
import           SortId
import           StdTDefs
import           Text.XML.Expat.Tree
import           TextShow
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
      traverse xmlTreeToList ts
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

-- ----------------------------------------------------------------------------------------- --
-- lookup a constructor definition given its constructor id in the given TorXakis definitions

lookupConstructorDef :: TxsDefs -> CstrId -> TxsDef
lookupConstructorDef txsdefs cId  =
    case Map.lookup cId (cstrDefs txsdefs) of
        Nothing -> error $ "ConstructorId " ++ show cId ++ " not found in mapping"
        Just d  -> DefCstr d

-- ----------------------------------------------------------------------------------------- --
-- lookup a constructor given its sort and constructor name in the given TorXakis definitions

lookupConstructor :: TxsDefs -> SortId -> Text -> CstrId
lookupConstructor tdefs sid n
  =  case [ cstr
          | cstr@CstrId{ CstrId.name = n', cstrsort = sid' } <- Map.keys (cstrDefs tdefs)
          , n == n'
          , sid == sid'
          ] of
        [c] -> c
        _   -> error $ "TXS SMT2TXS lookupConstructor: No (unique) constructor for sort " ++
                      show sid ++ " and name " ++ T.unpack n ++ "\n"
-- ----------------------------------------------------------------------------------------- --
rootName :: Text
rootName = "TorXakisMsg"

constToXml :: TxsDefs -> Const -> Text
constToXml tdefs w =
  xmlTreeToText $ pairNameConstToXml tdefs (rootName, w)

getFieldNames :: TxsDef -> [Text]
getFieldNames (DefCstr (CstrDef _ funcIds))   = map FuncId.name funcIds
getFieldNames _                               = error "getFieldNames: unexpected input" -- TODO: give a more informative error message.

pairNameConstToXml :: TxsDefs -> (Text, Const)  -> XMLTree
pairNameConstToXml _ (n, Cbool True) =
  n ~> ["true"]
pairNameConstToXml _ (n, Cbool False) =
  n ~> ["false"]
pairNameConstToXml _ (n, Cint i) =
  n ~> [XLeaf (showt i)]
pairNameConstToXml _ (n, Cstring s) =
  n ~> [XLeaf (encodeString s)]
pairNameConstToXml tdefs (n, Cstr cid wals) =
  let cName = CstrId.name cid
      cDef = lookupConstructorDef tdefs cid
      nodes = map (pairNameConstToXml tdefs)
                  (zip (getFieldNames cDef) wals)
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

constFromXml :: TxsDefs -> SortId -> Text -> Const
constFromXml tdefs sid s =
  case parse' defaultParseOptions{ overrideEncoding = Just ISO88591 } (pack $ map c2w (T.unpack s)) of
    Left err   -> error ("constFromXml parse error " ++ show err)
    Right tree -> pairNameConstFromXml tdefs sid tree rootName

pairNameConstFromXml :: TxsDefs -> SortId -> Node Text Text -> Text -> Const
pairNameConstFromXml _ sid (Element nt [] list) n
  | n == nt, sid == sortId_Bool = Cbool ("true" == stringFromList list)
pairNameConstFromXml _     sid     (Element nt [] list) n
  | n == nt, sid == sortId_Int = Cint (read (T.unpack (stringFromList list)))
pairNameConstFromXml _ sid (Element nt [] list) n
  | n == nt, sid == sortId_String = Cstring (stringFromList list)
pairNameConstFromXml tdefs sid (Element nt [] [Element cname [] list]) n
  | n == nt =
    let cstrid = lookupConstructor tdefs sid cname
        cDef = lookupConstructorDef tdefs cstrid
        vexprArgs =
          map (uncurry3 (pairNameConstFromXml tdefs))
              (zip3 (cstrargs cstrid) list (getFieldNames cDef))
        uncurry3 f (a, b, c) = f a b c
    in
      if length (cstrargs cstrid) == length list
      then Cstr cstrid vexprArgs
      else error $  "XmlFormat - constFromXml: Number of arguments mismatch "
                 ++ "in constructor " ++ show cname
                 ++ " of sort " ++ show (SortId.name sid)
                 ++ " : definition " ++ show (length (cstrargs cstrid))
                 ++ " vs actual " ++ show (length list) ++ "\n"
pairNameConstFromXml tdefs sid i n =
    error $  "XmlFormat - constFromXml: Unexpected item "
          ++ show i
          ++ " for name " ++ show n
          ++ " with sort " ++ show (SortId.name sid)
          ++ " using tdefs " ++ show tdefs
          ++ "\n"
