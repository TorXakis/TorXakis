{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestPreludeRead
(
testPreludeReadList
)
where
import Test.HUnit

testEmpty :: Test
testEmpty = TestCase $
    let s = read "\"\"" :: String in
        assertEqual "check empty" "" s

testQuote :: Test
testQuote = TestCase $
    let s = read "\"\\\"\"" :: String in
        assertEqual "check quote" "\"" s

testBackslash :: Test
testBackslash = TestCase $
    let s = read "\"\\\\\"" :: String in
        assertEqual "check backslash" "\\" s

testBackslashSpecial :: Test
testBackslashSpecial = TestCase $
    let s = read "\"\\a\"" :: String in
        assertEqual "check backslash special" "\a" s

testSpecialChar :: Test
testSpecialChar = TestCase $
    let s = read "\"\\x07\"" :: String in
        assertEqual "check special char" "\a" s        -- 007 Bell

testHex :: Test
testHex = TestCase $
    let s = read "\"\\xAa\"" :: String in
        assertEqual "check hex" "\xaa" s      -- not case sensitive in hexadecimal characters

testHexX :: Test
testHexX = TestCase $
    let s = read "\"\\XfB\"" :: String in
        assertEqual "check HEX" "\xfb" s      -- not case sensitive in x

testControlCodeAbbreviation :: Test
testControlCodeAbbreviation = TestCase $
    let s = read "\"\\ENQ\"" :: String in
        assertEqual "check special char" "\ENQ" s        -- 005 enquiry - is case sensitive!
        
----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testPreludeReadList = TestList [
      TestLabel "empty"                             testEmpty
    , TestLabel "quote"                             testQuote
    , TestLabel "backslash"                         testBackslash
    , TestLabel "backslash non special"             testBackslashSpecial
    , TestLabel "special char"                      testSpecialChar
    , TestLabel "Hex case sensitive"                testHex
    , TestLabel "HEX case sensitive"                testHexX
    , TestLabel "Control Code Abbreviation"         testControlCodeAbbreviation
    ]