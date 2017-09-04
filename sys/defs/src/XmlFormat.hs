{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- ----------------------------------------------------------------------------------------- --
module XmlFormat
(
      constToXml
    , constFromXml
)
-- ----------------------------------------------------------------------------------------- --
where

import           CstrId
import           Data.ByteString          (pack)
import           Data.ByteString.Internal (c2w)
import           Data.Char                (chr, ord)
import qualified Data.Map                 as Map
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import           FuncId
import           SortId
import           StdTDefs
import           Text.XML.Expat.Tree
import           TxsDefs

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
encodeChar '&'    = "&amp;"
encodeChar '<'    = "&lt;"
encodeChar  c     = if validChar c
                        then T.singleton c
                        else "<char>" <> T.pack (show (ord c)) <> "</char>"

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
                      show sid ++ " and name " ++ n ++ "\n"
-- ----------------------------------------------------------------------------------------- --
rootName :: Text
rootName = "TorXakisMsg"

constToXml :: TxsDefs -> Const -> Text
constToXml tdefs w = pairNameConstToXml tdefs (rootName, w)

getFieldNames :: TxsDef -> [Text]
getFieldNames (DefCstr (CstrDef _ funcIds))   = map FuncId.name funcIds
getFieldNames _                               = error "getFieldNames: unexpected input"

pairNameConstToXml :: TxsDefs -> (Text, Const)  -> Text
pairNameConstToXml _        (n,Cbool True)      =   "<" ++ n ++ ">true</" ++ n ++ ">"
pairNameConstToXml _        (n,Cbool False)     =   "<" ++ n ++ ">false</" ++ n ++ ">"
pairNameConstToXml _        (n,Cint i)          =   "<" ++ n ++ ">" ++ show i ++ "</" ++ n ++ ">"
pairNameConstToXml _        (n,Cstring s)       =   "<" ++ n ++ ">" ++ T.unpack (encodeString s) ++ "</" ++ n ++ ">"
pairNameConstToXml tdefs    (n,Cstr cid wals)   =   let cName = CstrId.name cid in
                                                        let cDef = lookupConstructorDef tdefs cid in
                                                            "<" ++ n ++ "><" ++ cName ++ ">" ++
                                                                concatMap (pairNameConstToXml tdefs) (zip (getFieldNames cDef) wals) ++
                                                            "</" ++ cName ++ "></" ++ n ++ ">"
pairNameConstToXml _ (n,w)                      = error ("XmlFormat - constToXml: " ++ show w ++ " - Const for name " ++ n ++ " can not be translated to Xml")


stringFromList :: [Node Text Text] -> Text
stringFromList []                                           = ""
stringFromList (Text a: xs)                                 = a <> stringFromList xs
stringFromList (Element "char" [] [Text nrString] : xs )    = chr(read nrString) : stringFromList xs
stringFromList (x:_)                                        = error ("XmlFormat - stringFromList : unexpected item " ++ show x)

constFromXml :: TxsDefs -> SortId -> Text -> Const
constFromXml tdefs sid s = case parse' defaultParseOptions{ overrideEncoding = Just ISO88591 } (pack $ map c2w s) of
                                Left err    -> error ("constFromXml parse error " ++ show err)
                                Right tree  -> pairNameConstFromXml tdefs sid tree rootName

pairNameConstFromXml :: TxsDefs -> SortId -> Node Text Text -> Text -> Const
pairNameConstFromXml = undefined
-- pairNameConstFromXml _     sid     (Element nt [] list) n                      | n == nt, sid == sortId_Bool       = Cbool ("true" == stringFromList list)
-- pairNameConstFromXml _     sid     (Element nt [] list) n                      | n == nt, sid == sortId_Int        = Cint (read (stringFromList list))
-- pairNameConstFromXml _     sid     (Element nt [] list) n                      | n == nt, sid == sortId_String     = Cstring (stringFromList list)
-- pairNameConstFromXml tdefs sid     (Element nt [] [Element cname [] list]) n   | n == nt                           =
--     let cstrid = lookupConstructor tdefs sid cname in
--         if length (cstrargs cstrid) == length list
--             then let cDef = lookupConstructorDef tdefs cstrid in
--                     let vexprArgs = map (\(sid',argValue,n') -> pairNameConstFromXml tdefs sid' argValue n') (zip3 (cstrargs cstrid) list (getFieldNames cDef)) in
--                         Cstr cstrid vexprArgs
--             else error $ "XmlFormat - constFromXml: Number of arguments mismatch " ++
--                          "in constructor " ++ cname ++ " of sort " ++ SortId.name sid ++
--                          " : definition " ++ show (length (cstrargs cstrid)) ++
--                          " vs actual " ++ show (length list) ++ "\n"
-- pairNameConstFromXml _ sid i n                                                                                     =
--     error $ "XmlFormat - constFromXml: Unexpected item " ++ show i ++ " for name " ++ n ++ " with sort " ++ SortId.name sid ++ "\n"
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
