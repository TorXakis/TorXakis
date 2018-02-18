{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | Example usage of the Sort/ADT package. These examples can be incorporated
-- into tests later.

module Alt.Examples where

import           Alt.Error
import           Alt.ADT
import           Alt.Cstr
import           Alt.Field
import           Alt.LookupTable
import           Alt.ADTDefs

-- ** Some good weather tests.

tPerson :: Either Error ADTD
tPerson = "Person" .::= ["Person" .= ["name" .: "String", "age" .: "Int"]]

tOperation :: Either Error ADTD
tOperation = "Operation" .::= [ "Plus"  .= ["p0" .: "Int", "p1" .: "Int"]
                              , "Minus" .= ["m0" .: "Int", "m1" .: "Int"]
                              ]

tStudent :: Either Error ADTD
tStudent = "Student" .::= [ "Student" .= ["who" .: "Person", "grade" .: "Int"]]

mADTDefs :: Either Error (LookupTable ADT)
mADTDefs = mkADTs' [tPerson, tOperation, tStudent] 

-- * Tests for duplicated fields
tDupF = "DupF" .::= [ "DupF" .= ["foo" .: "bar", "foo" .: "baz"]]

tDupC = "DupC" .::= [ "DupC" .= [], "DupC" .= []] 


tDupADTs = mkADTs' [tPerson, tPerson]

-- ** Tests for constructability.
tA = "A" .::= [ "A" .= ["b" .: "B"]]

tB = "B" .::= [ "B" .= ["a" .: "A"]]

-- This should give a "non-construtible ADT" error.
mCircularADTDefs = mkADTs' [tA, tB]

-- But this should be constructible
tA' = "A" .::= [ "A" .= ["b" .: "B"], "C" .= []]

tB' = "B" .::= [ "B" .= ["a" .: "A"]]

mNonCircular = mkADTs' [tA', tB']
