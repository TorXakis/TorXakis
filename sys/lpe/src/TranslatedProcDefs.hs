{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module TranslatedProcDefs

where

import ChanId
import ProcId


-- | TranslatedProcDefs
-- keeping track of already translated ProcDefs to avoid double work and loops
data TranslatedProcDefs = TranslatedProcDefs
  { lPreGNF  :: [ProcId]
  , lGNF     :: [ProcId]          -- translated
  , lGNFdirectcalls :: [ProcId]   -- loop detection: current chain of direct calls without progress
  , lGNFinTranslation :: [ProcId] -- currently being translated
  , lLPE     :: [(ProcId, [ChanId])]
  } deriving (Eq, Ord, Read, Show)

emptyTranslatedProcDefs :: TranslatedProcDefs
emptyTranslatedProcDefs = TranslatedProcDefs { TranslatedProcDefs.lPreGNF = []
                                             , TranslatedProcDefs.lGNF = []
                                             , TranslatedProcDefs.lGNFdirectcalls = []
                                             , TranslatedProcDefs.lGNFinTranslation = []
                                             , TranslatedProcDefs.lLPE = [] }
