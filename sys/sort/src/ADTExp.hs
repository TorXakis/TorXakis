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

import           Data.Text       (Text)
import qualified Data.Text        as T
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.List.Unique
    
newtype Sort = Sort { sortName :: Text } deriving (Show, Eq)

-- | Smart constructor for sort:
--
-- Precondition:
--
-- * Sort names should be not empty.
--
mkSort :: Text -- ^ SortName
       -> Either Error Sort
mkSort sName =
    if T.null sName
    then Left  $ Error EmptyName "Sort name cannot be empty"
    else Right $ Sort sName

-- | Predefined sorts.
predefSorts :: [Sort]
predefSorts =
    [ Sort "Bool"
    , Sort "Int"
    , Sort "Char"
    , Sort "Regex"
    , Sort "String"
    ]

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
mkADTDecl adtName cs
    | T.null adtName    = Left $ Error EmptyName
                          "ADT name cannot be empty"
    | null cs           = Left $ Error EmptyConstructorDefs
                          "ADT must have at least one constructor"
    | not (null nuCtrs) = Left $ Error (DuplicatedConstructors nuCtrs)
                          ""
    | not (null nuFlds) = Left $ Error (DuplicatedFields nuFlds)
                          ""
    | otherwise         = Right $ ADTDecl adtName cs
    where
      nuCtrs = repeated $ name <$> cs
      nuFlds = repeated $ name <$> concatMap constrFields cs

data ConstructorDecl = ConstructorDecl
    { constrDeclName :: Text
    , constrFields :: [FieldDecl]
    } deriving (Show)

instance HasName ConstructorDecl where
    name (ConstructorDecl n _) = n

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
mkConstructorDecl cName fs
    | T.null cName        = Left $ Error EmptyName
                                         "Constructor name cannot be empty"
    | not (null nuFields) = Left $ Error (DuplicatedNames nuFields) ""
    | otherwise      = Right $ ConstructorDecl cName fs
    where
      nuFields :: [Text]
      nuFields = repeated $ name <$> fs

data FieldDecl = FieldDecl
    { fieldDeclName :: Text
    , fieldDeclSort :: Text
    } deriving (Show, Eq)

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
mkFieldDecl fName fType
    | T.null fName = Left $ Error EmptyName "Field name cannot be empty"
    | T.null fType = Left $ Error EmptyName "Field type cannot be empty"
    | otherwise    = Right $ FieldDecl fName fType

class HasName a where
    name :: a -> Text

instance HasName FieldDecl where
    name (FieldDecl n _) = n

data Error = Error
    { errorType :: ErrorType
    , errorMessage :: Text
    } deriving (Show)

data ErrorType = EmptyName
               | DuplicatedNames [Text]
               | DuplicatedConstructors [Text]
               | DuplicatedFields [Text]
               | UndefinedReference
               | EmptyConstructorDefs
               deriving (Show, Eq)

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
