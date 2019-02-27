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

import           TorXakis.ContextTestSort
import           TorXakis.Sort
import           TorXakis.Value

-- | ConversionText for String is Identity
prop_ConversionText_StringId :: Bool
prop_ConversionText_StringId = 
    let ctx = empty :: ContextTestSort
        val = Cstring (T.pack (map chr [0..255]))
        txt = valueToText ctx val
        actual = valueFromText ctx SortString txt
      in
        case actual of
            Left e     -> trace ("\nParse error " ++ show e) False
            Right val' -> val == val'

-- | ConversionXML for String is Identity
prop_ConversionXML_StringId :: Bool
prop_ConversionXML_StringId = 
    let ctx = empty :: ContextTestSort
        val = Cstring (T.pack (map chr [0..255]))
        xml = valueToXML ctx val
        actual = valueFromXML ctx SortString xml
      in
        case actual of
            Left e     -> trace ("\nParse error " ++ show e) False
            Right val' -> val == val'

prop_fromText_String :: Bool
prop_fromText_String = 
    let range = [0..255]
        txt = T.pack ("\"" ++ concatMap (\i -> "&#" ++ show i ++ ";") range ++ "\"")
        
        ctx = empty :: ContextTestSort
        actual = valueFromText ctx SortString txt
      in
        
        case actual of
            Left e     -> trace ("\nParse error " ++ show e) False
            Right val -> val == Cstring (T.pack (map chr range))

-- | ConversionText for Char is Identity
prop_ConversionText_CharId ::  Bool
prop_ConversionText_CharId =
    let vals = map chr [0..255]
      in 
        all check vals
    where check :: Char -> Bool
          check v = 
                let ctx = empty :: ContextTestSort
                    txt = valueToText ctx (Cchar v)
                    actual = valueFromText ctx SortChar txt
                  in
                    case actual of
                        Left e   -> trace ("\nParse error " ++ show e) False
                        Right v' -> v' == Cchar v

-- | ConversionXML for Char is Identity
prop_ConversionXML_CharId ::  Bool
prop_ConversionXML_CharId =
    let vals = map chr [0..255]
      in
        all check vals
    where check :: Char -> Bool
          check v = 
                let ctx = empty :: ContextTestSort
                    xml = valueToText ctx (Cchar v)
                    actual = valueFromText ctx SortChar xml
                  in
                    case actual of
                        Left e   -> trace ("\nParse error " ++ show e) False
                        Right v' -> v' == Cchar v

prop_fromText_Char :: Bool
prop_fromText_Char = 
    let vals = [0..255]
      in
        all check vals
    where check :: Int -> Bool
          check i =
            let txt = T.pack ("'&#" ++ show i ++ ";'")
                ctx = empty :: ContextTestSort
                actual = valueFromText ctx SortChar txt
              in
                case actual of
                    Left e     -> trace ("\nParse error " ++ show e) False
                    Right val -> val == Cchar (chr i)

-- | ConversionText for Int is Identity
prop_ConversionText_IntId ::  Bool
prop_ConversionText_IntId = all check [-255..255]
    where check :: Integer -> Bool
          check v = 
                let ctx = empty :: ContextTestSort
                    txt = valueToText ctx (Cint v)
                    actual = valueFromText ctx SortInt txt
                  in
                    case actual of
                        Left e   -> trace ("\nParse error " ++ show e) False
                        Right v' -> v' == Cint v

-- | ConversionXML for Int is Identity
prop_ConversionXML_IntId ::  Bool
prop_ConversionXML_IntId = all check [-255..255]
    where check :: Integer -> Bool
          check v = 
                let ctx = empty :: ContextTestSort
                    xml = valueToText ctx (Cint v)
                    actual = valueFromText ctx SortInt xml
                  in
                    case actual of
                        Left e   -> trace ("\nParse error " ++ show e) False
                        Right v' -> v' == Cint v

spec :: Spec
spec = do
  describe "String conversion" $ do
        it "fromText . toText == id" prop_ConversionText_StringId
        it "fromXML . toXML == id" prop_ConversionXML_StringId
        it "fromText with all characters escaped" prop_fromText_String
  describe "Char conversion" $ do
        it "fromText . toText == id" prop_ConversionText_CharId
        it "fromXML . toXML == id" prop_ConversionXML_CharId
        it "fromText with all characters escaped" prop_fromText_Char
  describe "Int conversion" $ do
        it "fromText . toText == id" prop_ConversionText_IntId
        it "fromXML . toXML == id" prop_ConversionXML_IntId