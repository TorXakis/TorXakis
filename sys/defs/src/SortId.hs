{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --

module SortId

where
import Name

data  SortId        =  SortId   { name       :: Name            -- capid
                                , unid       :: Int
                                }
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

