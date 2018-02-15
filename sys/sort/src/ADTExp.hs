{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- | An alternative definition of ADT's.
module ADTExp
(
  mkADTDecl    
, mkConstructorDecl  
, mkFieldDecl
, mkADTs
, mkSort
) where

import           Data.Text        (Text)
import           Data.Map.Strict  (Map)

newtype Sort = Sort { sortName :: Text }

-- | Smart constructor for sort:
--
-- Precondition:
--
-- * Sort names should be not empty.
--
mkSort :: Text -> Either Error Sort
mkSort = undefined

predefSorts :: [Sort]
predefSorts = [Sort "Bool", Sort "Int", Sort "Char", Sort "Regex", Sort "String"]

data ADTDecl = ADTDecl
    { adtDeclName :: Text
    , adtDeclConstructors :: [ConstructorDecl]
    }

-- | Smart constructor for an ADT declaration:
--
-- Preconditions:
--
-- * The ADT name should be non-empty.
--
-- * All the constructor names should be unique.
--
-- * All the field names should be unique across all constructor declarations.
--
mkADTDecl :: Text -- ^ Constructor name
          -> [ConstructorDecl]
          -> Either Error ADTDecl
mkADTDecl = undefined

data ConstructorDecl = ConstructorDecl
    { constrDeclName :: Text
    , constrFields :: [FieldDecl]
    }

-- | Smart constructor for a constructor declaration.
--
-- Preconditions:
--
-- * The constructor name should be non-empty
--
-- * All the field names should be unique
--
mkConstructorDecl :: Text        -- ^ Constructor name.
                  -> [FieldDecl] -- ^ Fields
                  -> Either Error ConstructorDecl
mkConstructorDecl = undefined

data FieldDecl = FieldDecl
    { fieldDeclName :: Text
    , fieldDeclSort :: Text
    }

-- | Smart constructor for a field declaration:
--
-- Preconditions:
--
-- * The field name should be non-empty.
--
-- * The field type should be non-empty.
--
mkFieldDecl :: Text -- ^ Field name.
            -> Text -- ^ Field type.
            -> Either Error FieldDecl
mkFieldDecl = undefined

type Error = String

data ADT = ADT
    { adtName :: Text
    , constructors :: LookupTable Constructor
    }

-- | Smart constructor for an ADT:
--
--
mkADTDefs :: [ADTDecl] -> Either Error (LookupTable ADT)
mkADTDefs = undefined

-- type LookupTable v = Map (Ref v) v I think 'Ref's are not needed.
type LookupTable v = Map Text v

-- newtype Ref v = Ref { refName :: Text }

data Constructor = Constructor
    { constructorName :: Text
    , fields :: LookupTable Field
    }

data Field = Field
    { fieldName :: Text
    , fieldSort :: Sort
    }

-- | Smart constructor for ADT's.
--
-- Preconditions:
--
-- * All the names of the ADT's should be unique.
--
-- * All the referenced sorts should exists in the lookup table of available
--   sorts, or as a declared ADT.
--
-- * All ADT's should be constructible.
--
mkADTs :: [ADTDecl]
       -> [Sort] -- ^ List of available sorts.
       -> Either Error (LookupTable ADT)
mkADTs = undefined

-- | A more convenient version of the 'mkADTs' smart constructor that uses the
-- predefined sorts by default.
mkADTs' :: [Either Error ADTDecl]
        -> Either Error (LookupTable ADT)
mkADTs' eDecls = sequence eDecls >>= (`mkADTs` predefSorts)

-----------------------------------------------------------------------------
-- Examples
-----------------------------------------------------------------------------

(.::=) :: Text -> [Either Error ConstructorDecl] -> Either Error ADTDecl
(.::=) = undefined

(.=) :: Text -> [Either Error FieldDecl] -> Either Error ConstructorDecl
(.=) = undefined

-- "MyType" .::= 
--     [ "Sum"  .= [ "name" .: "String", "age" .: "Int"]
--     , "Prod" .= [ "p0" .: "Int"]
--     ]

(.:) :: Text -> Text -> Either Error FieldDecl
(.:) = mkFieldDecl

tPerson :: Either Error ADTDecl
tPerson = "Person" .::= ["Person" .= ["name" .: "String", "age" .: "Int"]]

tOperation :: Either Error ADTDecl
tOperation = "Operation" .::= [ "Plus"  .= ["p0" .: "Int", "p1" .: "Int"]
                              , "Minus" .= ["m0" .: "Int", "m1" .: "Int"]
                              ]

tStudent :: Either Error ADTDecl
tStudent = "Student" .::= [ "Student" .= ["who" .: "Person", "grade" .: "Int"]]

mADTDefs :: Either Error (LookupTable ADT)
mADTDefs = mkADTs' [tPerson, tOperation, tStudent] 

-- ** Example of not-constructible data.

tA :: Either Error ADTDecl
tA = "A" .::= [ "A" .= ["b" .: "B"]]

tB :: Either Error ADTDecl
tB = "B" .::= [ "B" .= ["a" .: "A"]]

mCircularADTDefs :: Either Error (LookupTable ADT)
mCircularADTDefs = mkADTs' [tA, tB]
    
-- This should give a circular dependency problem.
