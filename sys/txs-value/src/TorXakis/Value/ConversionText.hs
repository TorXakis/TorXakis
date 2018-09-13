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
import           Data.Either     (partitionEithers)
import qualified Data.HashMap    as Map
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as T

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Value.Value
import           TorXakis.Value.ValueAlex
import           TorXakis.Value.ValueHappy


-- | 'TorXakis.Value.Value' to 'Data.Text.Text' conversion.
valueToText :: SortContext a => a -> Value -> Text       -- Usage of SortContext is prevented since reference by Name is exploited
valueToText _   (Cbool True)   = T.pack "True"
valueToText _   (Cbool False)  = T.pack "False"
valueToText _   (Cint i)       = T.pack (show i)
valueToText _   (Cchar c)      = T.pack (show c)
valueToText _   (Cstring s)    = T.pack (show s)
valueToText _   (Cregex r)     = T.pack (show r)
-- valueToText _   (Ccstr _ c []) = TorXakis.Name.toText (toName c)  -- TODO: desired? - parser need to be changed as well!
valueToText ctx (Ccstr _ c as) = TorXakis.Name.toText (toName c) 
                                    <> T.pack "("
                                    <> T.intercalate (T.pack ",") (map (valueToText ctx) as) 
                                    <> T.pack ")"
valueToText _   (Cany _)       = error "ANY not supported"

-- | 'TorXakis.Value.Value' from 'Data.Text.Text' conversion.
-- Expected 'TorXakis.Sort' of 'TorXakis.Value.Value' must be provided.
valueFromText :: SortContext a => a -> Sort -> Text -> Either MinError Value
valueFromText ctx s t = 
    let p :: ParseValue
        p = (valueParser . valueLexer . T.unpack) t
      in
        fromParseValue s p
    where
        fromParseValue :: Sort -> ParseValue -> Either MinError Value
        fromParseValue SortBool    (Pbool b)    = Right $ Cbool b
        fromParseValue SortInt     (Pint b)     = Right $ Cint b
        fromParseValue SortChar    (Pchar c)    = Right $ Cchar c
        fromParseValue SortString  (Pstring s') = Right $ Cstring s'
        fromParseValue SortRegex   (Pstring r)  = Right $ Cregex r
        fromParseValue (SortADT a) (Pcstr n ps) =
            case mkName n of
                Left e   -> Left $ MinError (T.pack "Illegal name " <> n <> T.pack "\n" <> TorXakis.Error.toText e)
                Right n' -> let adtDef = fromMaybe (error ("ADTDef "++ show a ++ " not in context"))
                                                   (Map.lookup a (adtDefs ctx))
                                c = RefByName n'
                            in case Map.lookup c ( (constructors . viewADTDef) adtDef ) of
                                    Nothing   -> Left $ MinError (T.pack "Constructor " <> n <> T.pack " not defined for ADT " <> (TorXakis.Name.toText . toName) a)
                                    Just cDef -> let fs = (fields . viewConstructorDef) cDef
                                                     actual = length fs
                                                     expected = length ps
                                                  in if actual == expected
                                                        then case partitionEithers (zipWith fromParseValue (map sort fs) ps) of
                                                                  ([], vs) -> Right $ Ccstr a c vs
                                                                  (es, _)  -> Left $ MinError $ T.intercalate (T.pack "\n") (map TorXakis.Error.toText es)
                                                        else Left $ MinError (T.pack ("Fields mismatch - expected " ++ show expected ++ " yet actual " ++ show actual))
        fromParseValue s' p                     = Left $ MinError (T.pack ("Sort " ++ show s' ++ " mismatch with parsed value " ++ show p ++ "\nNote ANY is not supported"))