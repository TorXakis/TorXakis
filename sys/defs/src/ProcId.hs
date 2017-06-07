{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --

module ProcId

where
import Name
import ChanId
import SortId
import VarId

data  ExitSort      =  NoExit
                     | Exit [SortId]
                     | Hit
     deriving (Eq,Ord,Read,Show)

data ProcId         = ProcId    { name       :: Name
                                , unid       :: Int
                                , procchans  :: [ChanId]
                                , procvars   :: [VarId]
                                , procexit   :: ExitSort
                                }
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

