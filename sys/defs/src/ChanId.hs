{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --

module ChanId

where
import Name
import SortId

data ChanId         = ChanId    { name       :: Name
                                , unid       :: Int
                                , chansorts  :: [SortId]
                                }
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

