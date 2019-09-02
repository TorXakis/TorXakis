{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TestXSD2SMT
(
testXSD2SMTList
)
where
-- general Haskell imports
import           Data.Char
import qualified Data.Text     as T
import           Numeric (showHex)
import           Test.HUnit

import           RegexXSD2SMT


-- | escape non-printable characters
-- see http://cvc4.cs.stanford.edu/wiki/Strings
--  0x20 == 32
--  0x7e == 126
escape :: String -> String
escape [] = []
escape (x:xs) 
    | x == '"'                      = "\"\"" ++ escape xs
    | x == '\\'                     = "\\\\" ++ escape xs
    | ord x < 16                    = "\\x0" ++ showHex (ord x) (escape xs)
    | ord x < 32 || ord x >= 127    = "\\x"  ++ showHex (ord x) (escape xs)
    | otherwise                     = x:escape xs

-- ----------------------------------------------------------------------------
testXSD2SMTList :: Test
testXSD2SMTList = TestList [
        TestLabel "Empty"                    testEmpty,
        TestLabel "FromChar"                 testFromChar,
        TestLabel "From Digit Char"          testFromDigitChar,
        TestLabel "From Format Char"         testFromFormatChar,
        TestLabel "Quotes"                   testFromCharQuotes,
        TestLabel "From Non Printable Char"  testFromNonPrintableChar,
        TestLabel "Union"                    testUnion,
        TestLabel "Union Multiple 1"         testUnionMultiple1,
        TestLabel "Union Multiple 2"         testUnionMultiple2,
        TestLabel "Union Multiple 3"         testUnionMultiple3,
        TestLabel "Concat"                   testConcat,
        TestLabel "Concat Multiple 1"        testConcatMultiple1,
        TestLabel "Concat Multiple 2"        testConcatMultiple2,
        TestLabel "Concat Multiple 3"        testConcatMultiple3,
        TestLabel "Optional"                 testOptional,
        TestLabel "OptionalMultiple"         testOptionalMultiple,
        TestLabel "Plus"                     testPlus,
        TestLabel "PlusMultiple"             testPlusMultiple,
        TestLabel "Star"                     testStar,
        TestLabel "StarMultiple"             testStarMultiple,
        TestLabel "Precedence"               testPrecedence,
        TestLabel "Quantity Range"           testQuantityRange,
        TestLabel "Quantity Range Multiple"  testQuantityRangeMultiple,
        TestLabel "Quantity Min"             testQuantityMin,
        TestLabel "Quantity Min Multiple"    testQuantityMinMultiple,
        TestLabel "Quantity Exact"           testQuantityExact,
        TestLabel "Quantity Exact Multiple"  testQuantityExactMultiple,
        TestLabel "Concat Union 1"           testConcatUnion1,
        TestLabel "Concat Union 2"           testConcatUnion2,
        TestLabel "Union Concat 1"           testUnionConcat1,
        TestLabel "Union Concat 2"           testUnionConcat2,
        TestLabel "Union Of Unions"          testUnionOfUnions,
        TestLabel "Union Of Concats"         testUnionOfConcats,
        TestLabel "Concat Of Concats"        testConcatOfConcats,
        TestLabel "Concat Of Unions"         testConcatOfUnions,
        TestLabel "CharGroup Range"          testCharGroupRange,
        TestLabel "CharGroup Single Char 1"  testCharGroupChar1,
        TestLabel "CharGroup Single Char 2"  testCharGroupChar2,
        TestLabel "CharGroup Single Char 3"  testCharGroupChar3,
        TestLabel "CharGroup Single Char 4"  testCharGroupChar4,
        TestLabel "CharGroup Chars 1"        testCharGroupChars1,
        TestLabel "CharGroup Chars 2"        testCharGroupChars2,
        TestLabel "CharGroup Chars 3"        testCharGroupChars3,
        TestLabel "CharGroup Chars 4"        testCharGroupChars4,
        TestLabel "CharGroup Esc Char 1"     testCharGroupEscChar1,
        TestLabel "CharGroup Esc Char 2"     testCharGroupEscChar2,
        TestLabel "CharGroup Esc Char 3"     testCharGroupEscChar3,
        TestLabel "CharGroup Esc Chars"      testCharGroupEscChars,
        TestLabel "CharGroup Range Chars 1"  testCharGroupRangeChars1,
        TestLabel "CharGroup Range Chars 2"  testCharGroupRangeChars2,
        TestLabel "CharGroup Range Chars 3"  testCharGroupRangeChars3,
        TestLabel "CharGroup Range Chars 4"  testCharGroupRangeChars4,
        TestLabel "CharGroup Range Chars 5"  testCharGroupRangeChars5,
        TestLabel "Dot"                      testDot
    ]

data TestObject = TestObject { input    :: String
                             , expected :: String
                             }


---------------------------------------------------------------------------
-- TestObject constructors
---------------------------------------------------------------------------
regexEmptyString :: TestObject
regexEmptyString = TestObject "" "(str.to.re \"\") "

regexFromChar :: Char -> TestObject
regexFromChar c = TestObject [c] ("(str.to.re \"" ++ escape [c] ++ "\") ")

regexDot :: TestObject
regexDot = TestObject ['.'] "(re.union (re.range \"\\x00\" \"\\x09\") (re.range \"\\x0B\" \"\\x0C\") (re.range \"\\x0E\" \"\\xFF\") ) "

concatRegex :: [TestObject] -> TestObject
concatRegex [] = regexEmptyString
concatRegex [t] = t
concatRegex (hd:tl)  = finalize (foldl addRegex hd tl)
    where   addRegex :: TestObject -> TestObject -> TestObject
            addRegex (TestObject input1 expected1) (TestObject input2 expected2) = TestObject (input1 ++ input2) (expected1 ++ expected2)
            finalize :: TestObject -> TestObject
            finalize (TestObject i e) = TestObject i ("(re.++ " ++ e ++ ") ")

unionRegex :: [TestObject] -> TestObject
unionRegex [] =  regexEmptyString
unionRegex [t] = t
unionRegex (hd:tl) =  finalize (foldl joinRegex hd tl)
    where   joinRegex :: TestObject -> TestObject -> TestObject
            joinRegex (TestObject input1 expected1) (TestObject input2 expected2) = TestObject (input1 ++ "|" ++ input2) (expected1 ++ expected2)
            finalize :: TestObject -> TestObject
            finalize (TestObject inp expct) = TestObject inp ("(re.union " ++ expct ++ ") ")

precedenceRegex :: TestObject ->  TestObject
precedenceRegex (TestObject inp expct)     = TestObject ("(" ++ inp ++ ")") expct

optionalRegex :: TestObject -> TestObject
optionalRegex (TestObject inp expct) = TestObject ( inp ++  "?") ("(re.opt " ++ expct ++ ") ")

plusRegex :: TestObject -> TestObject
plusRegex (TestObject inp expct) = TestObject ( inp ++  "+") ("(re.+ " ++ expct ++ ") ")

starRegex :: TestObject -> TestObject
starRegex (TestObject inp expct) = TestObject ( inp ++  "*") ("(re.* " ++ expct ++ ") ")

quantityRangeRegex :: TestObject -> Int -> Int -> TestObject
quantityRangeRegex (TestObject inp expct) low high = TestObject ( inp ++  "{" ++ show low ++ "," ++ show high ++ "}") ("(re.loop " ++ expct ++ show low ++ " " ++ show high ++ ") ")

quantityMinRegex :: TestObject -> Int -> TestObject
quantityMinRegex (TestObject inp expct) low = TestObject ( inp ++  "{" ++ show low ++ ",}") ("(re.loop " ++ expct ++ show low ++ ") ")

quantityExactRegex :: TestObject -> Int -> TestObject
quantityExactRegex rt val = quantityRangeRegex rt val val

charGroupPartRange :: Char -> Char -> TestObject
charGroupPartRange c1 c2 = TestObject ([c1] ++ "-" ++ [c2]) ("(re.range \"" ++ escape [c1] ++ "\" \"" ++ escape [c2] ++ "\") ")

charGroupPartChar :: Char -> TestObject
charGroupPartChar c = TestObject [c] ("(str.to.re \"" ++ escape [c] ++ "\") ")

charGroupPartEscChar :: Char -> TestObject
charGroupPartEscChar 't' = TestObject "\\t" "(str.to.re \"\\t\") "
charGroupPartEscChar 'n' = TestObject "\\n" "(str.to.re \"\\n\") "
charGroupPartEscChar 'r' = TestObject "\\r" "(str.to.re \"\\r\") "
charGroupPartEscChar c = TestObject ("\\"++[c]) ("(str.to.re \"" ++ [c] ++ "\") ")

charGroupRegex :: [TestObject] -> TestObject
charGroupRegex [TestObject inp expct] = TestObject ("["++inp++"]") expct
charGroupRegex list = toTestObject (foldl addCharGroupPart ("","") list)
    where
        addCharGroupPart :: (String,String) -> TestObject -> (String,String)
        addCharGroupPart (concatIn, concatExp) (TestObject inp expct) = (concatIn ++ inp, concatExp ++ expct)
        toTestObject :: (String, String) -> TestObject
        toTestObject (inp,expct) = TestObject ( "[" ++ inp ++ "]" ) ( "(re.union " ++ expct ++ ") ")

---------------------------------------------------------------------------
-- Test Template
---------------------------------------------------------------------------
testTestObject :: String -> TestObject -> Test
testTestObject s rt = TestCase $
    -- Trace.trace ( (input rt) ++ " => " ++ (expected rt) ) $ do
    assertEqual s (expected rt) (T.unpack (xsd2smt (T.pack (input rt) ) ) )

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testEmpty :: Test
testEmpty = testTestObject "empty" regexEmptyString

testFromChar :: Test
testFromChar = testTestObject "from Char" $ regexFromChar 'a'

testFromDigitChar :: Test
testFromDigitChar = testTestObject "from Digit Char" $ regexFromChar '2'

testFromFormatChar :: Test
testFromFormatChar = testTestObject "from Format Char" $ regexFromChar 'n'

testFromCharQuotes :: Test
testFromCharQuotes = testTestObject "from Char Quotes" $ regexFromChar '"'

testFromNonPrintableChar :: Test
testFromNonPrintableChar = testTestObject "from Non Printable Char" $ regexFromChar '\x01'

testUnion :: Test
testUnion = testTestObject "union" $ unionRegex [regexFromChar 'a',regexFromChar 'b']

testUnionMultiple1 :: Test
testUnionMultiple1 = testTestObject "union multiple 1" $ unionRegex [precedenceRegex $ unionRegex [regexFromChar 'a', regexFromChar 'b'], regexFromChar 'c']

testUnionMultiple2 :: Test
testUnionMultiple2 = testTestObject "union multiple 2" $ unionRegex [regexFromChar 'a', precedenceRegex $ unionRegex [regexFromChar 'b', regexFromChar 'c']]

testUnionMultiple3 :: Test
testUnionMultiple3 = testTestObject "union multiple 3" $ unionRegex [regexFromChar 'a', regexFromChar 'b', regexFromChar 'c']

testConcat :: Test
testConcat = testTestObject "concat" $ concatRegex [regexFromChar 'a', regexFromChar 'b']

testConcatMultiple1 :: Test
testConcatMultiple1 = testTestObject "concat multiple 1" $ concatRegex [precedenceRegex $ concatRegex [regexFromChar 'a', regexFromChar 'b'], regexFromChar 'c']

testConcatMultiple2 :: Test
testConcatMultiple2 = testTestObject "concat multiple 2" $ concatRegex [regexFromChar 'a', precedenceRegex $ concatRegex [regexFromChar 'b', regexFromChar 'c']]

testConcatMultiple3 :: Test
testConcatMultiple3 = testTestObject "concat multiple 3" $ concatRegex [regexFromChar 'a', regexFromChar 'b', regexFromChar 'c']

testOptional :: Test
testOptional = testTestObject "optional" $ optionalRegex (regexFromChar 'a')

testOptionalMultiple :: Test
testOptionalMultiple = testTestObject "optional multiple" $ optionalRegex ( precedenceRegex $ concatRegex [regexFromChar 'a', regexFromChar 'b'] )

testPlus :: Test
testPlus = testTestObject "Plus" $ plusRegex (regexFromChar 'a')

testPlusMultiple :: Test
testPlusMultiple = testTestObject "Plus Multiple" $ plusRegex ( precedenceRegex $ concatRegex [regexFromChar 'a', regexFromChar 'b'] )

testStar :: Test
testStar = testTestObject "Star" $ starRegex (regexFromChar 'a')

testStarMultiple :: Test
testStarMultiple = testTestObject "Star Multiple" $ starRegex ( precedenceRegex $ concatRegex [regexFromChar 'a', regexFromChar 'b'] )

testPrecedence :: Test
testPrecedence = testTestObject "Precedence" $ precedenceRegex (regexFromChar 'a')

testQuantityRange :: Test
testQuantityRange = testTestObject "Quantity Range" $ quantityRangeRegex (regexFromChar 'a') 3 6

testQuantityRangeMultiple :: Test
testQuantityRangeMultiple = testTestObject "Quantity Range Multiple" $ quantityRangeRegex (precedenceRegex $ unionRegex [regexFromChar 'a', regexFromChar 'b']) 9 123

testQuantityMin :: Test
testQuantityMin = testTestObject "Quantity Min" $ quantityMinRegex (regexFromChar 'a') 5

testQuantityMinMultiple :: Test
testQuantityMinMultiple = testTestObject "Quantity Min Multiple" $ quantityMinRegex (precedenceRegex $  unionRegex [regexFromChar 'a', regexFromChar 'b']) 234

testQuantityExact :: Test
testQuantityExact = testTestObject "Quantity Exact" $ quantityExactRegex (regexFromChar 'a') 7

testQuantityExactMultiple :: Test
testQuantityExactMultiple = testTestObject "Quantity Exact Multiple" $ quantityExactRegex (precedenceRegex $ unionRegex [regexFromChar 'a', regexFromChar 'b']) 256

testConcatUnion1 :: Test
testConcatUnion1 = testTestObject "concat union 1" $ concatRegex [precedenceRegex $ unionRegex [regexFromChar 'a', regexFromChar 'b'], regexFromChar 'c']

testConcatUnion2 :: Test
testConcatUnion2 = testTestObject "concat union 2" $ concatRegex [regexFromChar 'a', precedenceRegex $ unionRegex [regexFromChar 'b', regexFromChar 'c']]

testUnionConcat1 :: Test
testUnionConcat1 = testTestObject "union concat 1" $ unionRegex [concatRegex [regexFromChar 'a', regexFromChar 'b'], regexFromChar 'c']

testUnionConcat2 :: Test
testUnionConcat2 = testTestObject "union concat 2" $ unionRegex [regexFromChar 'a', concatRegex [regexFromChar 'b', regexFromChar 'c']]

testUnionOfUnions :: Test
testUnionOfUnions = testTestObject "union of unions" $ unionRegex [precedenceRegex $ unionRegex [regexFromChar 'a', regexFromChar 'b'], precedenceRegex $ unionRegex [regexFromChar 'c', regexFromChar 'd']]

testUnionOfConcats :: Test
testUnionOfConcats = testTestObject "union of concats" $ unionRegex [concatRegex [regexFromChar 'a', regexFromChar 'b'], concatRegex [regexFromChar 'c', regexFromChar 'd']]

testConcatOfConcats :: Test
testConcatOfConcats = testTestObject "concat of concats" $ concatRegex [precedenceRegex $ concatRegex [regexFromChar 'a', regexFromChar 'b'], precedenceRegex $ concatRegex [regexFromChar 'c', regexFromChar 'd']]

testConcatOfUnions :: Test
testConcatOfUnions = testTestObject "concat of unions" $ concatRegex [precedenceRegex $ unionRegex [regexFromChar 'a', regexFromChar 'b'], precedenceRegex $ unionRegex [regexFromChar 'c', regexFromChar 'd']]

testCharGroupRange :: Test
testCharGroupRange = testTestObject "char group range" $ charGroupRegex [charGroupPartRange 'a' 'n']

testCharGroupChar1 :: Test
testCharGroupChar1 = testTestObject "char group single char 1" $ charGroupRegex [charGroupPartChar 't']

testCharGroupChar2 :: Test
testCharGroupChar2 = testTestObject "char group single char 2" $ charGroupRegex [charGroupPartChar '*']

testCharGroupChar3 :: Test
testCharGroupChar3 = testTestObject "char group single char 3" $ charGroupRegex [charGroupPartChar '-']

testCharGroupChar4 :: Test
testCharGroupChar4 = testTestObject "char group single char 4" $ charGroupRegex [charGroupPartChar '\t']

testCharGroupChars1 :: Test
testCharGroupChars1 = testTestObject "char group chars 1" $ charGroupRegex [charGroupPartChar 'a', charGroupPartChar 'b', charGroupPartChar 'c']

testCharGroupChars2 :: Test
testCharGroupChars2 = testTestObject "char group chars 2" $ charGroupRegex [charGroupPartChar 'a', charGroupPartChar '-']

testCharGroupChars3 :: Test
testCharGroupChars3 = testTestObject "char group chars 3" $ charGroupRegex [charGroupPartChar '-', charGroupPartChar 'a']

testCharGroupChars4 :: Test
testCharGroupChars4 = testTestObject "char group chars 4" $ charGroupRegex [charGroupPartChar '\x03', charGroupPartChar '\x7F']

testCharGroupEscChar1 :: Test
testCharGroupEscChar1 = testTestObject "char group esc char 1" $ charGroupRegex [charGroupPartEscChar 't']

testCharGroupEscChar2 :: Test
testCharGroupEscChar2 = testTestObject "char group esc char 2" $ charGroupRegex [charGroupPartEscChar '[']

testCharGroupEscChar3 :: Test
testCharGroupEscChar3 = testTestObject "char group esc char 3" $ charGroupRegex [charGroupPartEscChar '*']

testCharGroupEscChars :: Test
testCharGroupEscChars = testTestObject "char group esc chars" $ charGroupRegex [charGroupPartEscChar '[', charGroupPartEscChar ']']

testCharGroupRangeChars1 :: Test
testCharGroupRangeChars1 = testTestObject "char group range chars 1" $ charGroupRegex [charGroupPartRange 'a' 'k', charGroupPartChar '-', charGroupPartChar 'z']

testCharGroupRangeChars2 :: Test
testCharGroupRangeChars2 = testTestObject "char group range chars 2" $ charGroupRegex [charGroupPartChar 'a', charGroupPartRange '0' '9', charGroupPartEscChar '|']

testCharGroupRangeChars3 :: Test
testCharGroupRangeChars3 = testTestObject "char group range chars 3" $ charGroupRegex [charGroupPartRange 'a' 'k', charGroupPartChar '-']

testCharGroupRangeChars4 :: Test
testCharGroupRangeChars4 = testTestObject "char group range chars 4" $ charGroupRegex [charGroupPartRange '"' 'z', charGroupPartChar '\'']

testCharGroupRangeChars5 :: Test
testCharGroupRangeChars5 = testTestObject "char group range chars 5" $ charGroupRegex [charGroupPartRange '\x06' '\x7F', charGroupPartChar '\x0A']

testDot :: Test
testDot = testTestObject "dot" regexDot
