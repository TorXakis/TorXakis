{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValueSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'SortGenContext'.
-----------------------------------------------------------------------------
module TorXakis.ValueSpec
(spec
)
where
import           Debug.Trace
import           Data.Char (chr)
import qualified Data.Text as T
import           Test.Hspec

import           TorXakis.ContextSort
import           TorXakis.Sort
import           TorXakis.Value

-- | ConversionText for String is Identity
prop_ConversionText_StringId :: Bool
prop_ConversionText_StringId = 
    let ctx = empty :: ContextSort
        val = mkString (T.pack (map chr [0..255]))
        txt = valueToText ctx val
        actual = valueFromText ctx SortString txt
      in
        case actual of
            Left e     -> trace ("\nParse error " ++ show e) False
            Right val' -> val == val'

-- | ConversionXML for String is Identity
prop_ConversionXML_StringId :: Bool
prop_ConversionXML_StringId = 
    let ctx = empty :: ContextSort
        val = mkString (T.pack (map chr [0..255]))
        xml = valueToXML ctx val
        actual = valueFromXML ctx SortString xml
      in
        case actual of
            Left e     -> trace ("\nParse error " ++ show e) False
            Right val' -> val == val'

-- | from Text with all characters escaped
prop_fromText_String :: Bool
prop_fromText_String = 
    let range = [0..255]
        txt = T.pack ("\"" ++ concatMap (\i -> "&#" ++ show i ++ ";") range ++ "\"")
        
        ctx = empty :: ContextSort
        actual = valueFromText ctx SortString txt
      in
        
        case actual of
            Left e     -> trace ("\nParse error " ++ show e) False
            Right val -> val == mkString (T.pack (map chr range))

-- | ConversionText for Int is Identity
prop_ConversionText_IntId ::  Bool
prop_ConversionText_IntId = all check [-255..255]
    where check :: Integer -> Bool
          check i = 
                let ctx = empty :: ContextSort
                    v = mkInt i
                    txt = valueToText ctx v
                    actual = valueFromText ctx SortInt txt
                  in
                    case actual of
                        Left e   -> trace ("\nParse error " ++ show e) False
                        Right v' -> v' == v

-- | ConversionXML for Int is Identity
prop_ConversionXML_IntId ::  Bool
prop_ConversionXML_IntId = all check [-255..255]
    where check :: Integer -> Bool
          check i = 
                let ctx = empty :: ContextSort
                    v = mkInt i
                    xml = valueToText ctx v
                    actual = valueFromText ctx SortInt xml
                  in
                    case actual of
                        Left e   -> trace ("\nParse error " ++ show e) False
                        Right v' -> v' == v

spec :: Spec
spec = do
  describe "String conversion" $ do
        it "fromText . toText == id" prop_ConversionText_StringId
        it "fromXML . toXML == id" prop_ConversionXML_StringId
        it "fromText with all characters escaped" prop_fromText_String
  describe "Int conversion" $ do
        it "fromText . toText == id" prop_ConversionText_IntId
        it "fromXML . toXML == id" prop_ConversionXML_IntId