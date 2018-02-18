{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | Utility functions for compiling collections of ADT declarations to ADT's.

module Alt.ADTDefs where

import           Data.Text (Text)
import           Data.List
import           Data.List.Unique

import           Alt.ADT
import           Alt.Cstr
import           Alt.Field
import           Alt.ChecksTo
import           Alt.Error
import           Alt.LookupTable
import           Alt.Name
import           Alt.Sort

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
mkADTs :: [ADTD]
       -> [Sort] -- ^ List of available sorts.
       -> Either Error (LookupTable ADT)
mkADTs as ts
    | not (null nuADTs) = errorWithTexts NotUniqueADTs nuADTs
    | not (null ndRefs) = errorWithTexts NotDefinedRefs (name <$> ndRefs)
    | not (null ncADTs) = errorWithTexts NonCstrADT (name <$> ncADTs)
    | otherwise         = Right $ checkTo as
    where
      -- 
      adtNames = name <$> as
      sortNames = name <$> ts
      nuADTs = repeated $ adtNames ++ sortNames
      -- 
      ndRefs = filter isUndefined allFields
      isUndefined f = fdSort f `notElem` allSorts
      allSorts = adtNames ++ sortNames
      allFields :: [FieldD]
      allFields = concatMap cdFields (concatMap adCstrs as)
      --
      ncADTs = getNonConstructibleADTs as sortNames

-- | A more convenient version of the 'mkADTs' smart constructor that uses the
-- predefined sorts by default.
mkADTs' :: [Either Error ADTD]
        -> Either Error (LookupTable ADT)
mkADTs' eDecls = sequence eDecls >>= (`mkADTs` predefSorts)

-- | Return all the ADT's that are non-constructible.
getNonConstructibleADTs :: [ADTD] -- ^ ADT declarations
                        -> [Text] -- ^ Names of the available sorts
                        -> [ADTD]
getNonConstructibleADTs decls aSorts = 
    let (cs, ncs) = partition (isConstructible aSorts) decls in
        if null cs
        then ncs
        else getNonConstructibleADTs ncs (aSorts ++ map name cs)

-- | Determine whether an ADT is constructible using the available sorts.
isConstructible :: [Text] -> ADTD -> Bool
isConstructible aSorts decls =
    any isConstructibleField (adCstrs decls)
    where
      isConstructibleField :: CstrD -> Bool
      isConstructibleField c = 
          all (`elem` aSorts) (fdSort <$> cdFields c)
