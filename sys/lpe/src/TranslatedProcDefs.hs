{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module TranslatedProcDefs

where

import ProcId


-- | TranslatedProcDefs
-- keeping track of already translated ProcDefs to avoid double work and loops
data TranslatedProcDefs = TranslatedProcDefs
  { lPreGNF  :: [ProcId]
  , lGNF     :: [ProcId]
  , lLPE     :: [ProcId]
  } deriving (Eq, Ord, Read, Show)

emptyTranslatedProcDefs :: TranslatedProcDefs
emptyTranslatedProcDefs = TranslatedProcDefs { TranslatedProcDefs.lPreGNF = []
                                             , TranslatedProcDefs.lGNF = []
                                             , TranslatedProcDefs.lLPE = [] }
