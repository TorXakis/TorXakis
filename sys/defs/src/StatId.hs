{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --

module StatId

where
import Name
import ProcId

data StatId         = StatId    { name       :: Name
                                , unid       :: Int
                                , procid     :: ProcId
                                }
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

