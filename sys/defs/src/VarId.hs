{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --

module VarId

where
import Name
import SortId
import Variable

data VarId          = VarId     { name       :: Name             --smallid
                                , unid       :: Int
                                , varsort    :: SortId
                                }
     deriving (Eq,Ord,Read,Show)

instance Variable VarId
  where
    vname v            = VarId.name v ++ "$$" ++ show (VarId.unid v)
    vunid              = VarId.unid
    vsort              = VarId.varsort
    cstrVariable s i t = VarId s i t
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

