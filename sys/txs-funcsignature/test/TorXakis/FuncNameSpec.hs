{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncNameSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'FuncName'.
-----------------------------------------------------------------------------
module TorXakis.FuncNameSpec
(spec
)
where
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Name
import           TorXakis.NameGen
import           TorXakis.OperatorName
import           TorXakis.OperatorNameGen

prop_OperatorNotFuncName :: OperatorNameGen -> Bool
prop_OperatorNotFuncName (OperatorNameGen n) =
    case mkName (TorXakis.OperatorName.toText n) of
      Left _ -> True
      Right _ -> False

prop_FuncNotOperatorName :: NameGen -> Bool
prop_FuncNotOperatorName (NameGen n) =
    case mkOperatorName (TorXakis.Name.toText n) of
      Left _ -> True
      Right _ -> False

spec :: Spec
spec = 
  describe "A FuncName" $ do
    it "can't confuse the name of a prefix function for an infix operator" $ property prop_FuncNotOperatorName
    it "can't confuse the name of an infix operator for a prefix function" $ property prop_OperatorNotFuncName
