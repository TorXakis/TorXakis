{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --

module NoId

where
import Name

data  NoId          =  NoId     { name       :: Name
                                , unid       :: Int
                                }
     deriving (Eq,Ord,Read,Show)


noId :: NoId
noId  =  NoId "" (-1)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

