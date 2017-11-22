{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- ----------------------------------------------------------------------------------------- --
--
-- Value Expressions
-- ----------------------------------------------------------------------------------------- --
module ValExpr
( ValExpr
, view
, eval
, ValExprView(..)
, PredefKind(..)
, module ValExprImpls
, module ValExprImplsExtension
)
where

import           ValExprDefs
import           ValExprImpls
import           ValExprImplsExtension
