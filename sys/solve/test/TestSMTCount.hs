{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestSMTCount
(
testSMTCountList
)
where
-- general Haskell imports
import Test.HUnit

-- specific SMT imports
import SMTInternal

testSMTCountList :: Test
testSMTCountList = TestList [
        smtCountTestCase "" 0,      -- length 0
        
        smtCountTestCase "(" 1,     -- length 1
        smtCountTestCase ")" (-1),
        smtCountTestCase "\"" 0,
        smtCountTestCase "x" 0,
        
        smtCountTestCase "((" 2,    -- length 2
        smtCountTestCase "()" 0,
        smtCountTestCase "(\"" 1,
        smtCountTestCase "(x" 1,

        smtCountTestCase ")(" 0,
        smtCountTestCase "))" (-2),
        smtCountTestCase ")\"" (-1),
        smtCountTestCase ")x" (-1),

        smtCountTestCase "\"(" 0,
        smtCountTestCase "\")" 0,
        smtCountTestCase "\"\"" 0,
        smtCountTestCase "\"x" 0,

        smtCountTestCase "x(" 1,
        smtCountTestCase "x)" (-1),
        smtCountTestCase "x\"" 0,
        smtCountTestCase "xx" 0,
        
        smtCountTestCase "\"\"(" 1,     -- selection length 3
        smtCountTestCase "\"\")" (-1),
        
        smtCountTestCase "(\"\"" 1,
        smtCountTestCase ")\"\"" (-1),
        
        smtCountTestCase "\"(\"" 0,
        smtCountTestCase "\")\"" 0,
        smtCountTestCase "\"x\"" 0
    ]

smtCountTestCase :: String -> Integer -> Test
smtCountTestCase s expected = TestCase $
    -- Trace.trace ("String : " ++ s) $ 
    -- Trace.trace ("Expected : " ++ show(expected)) $
    assertEqual s expected (countBracket s)