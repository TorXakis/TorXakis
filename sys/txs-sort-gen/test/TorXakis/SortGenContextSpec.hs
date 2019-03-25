{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortGenContextSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'SortGenContext'.
-----------------------------------------------------------------------------
module TorXakis.SortGenContextSpec
(spec
)
where
import qualified Data.Text              as T
import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.ContextTestSort
import           TorXakis.Name
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.Sort
import           TorXakis.SortGenContext

unsafeName :: String -> Name
unsafeName n = case mkName (T.pack n) of
                    Right x -> x
                    Left e -> error ("unexpected error with name " ++ n ++ "\n" ++ show e)

unsafeConstructorDef :: Name -> [FieldDef] -> ConstructorDef
unsafeConstructorDef n fs = case mkConstructorDef n fs of
                                Right x -> x
                                Left e -> error ("unexpected error with constructor " ++ show n ++ "\n" ++ show e)

-- Two constructors to ensure that error can only be detected at ADT creation.
prop_SignatureClashes :: Gen Bool
prop_SignatureClashes =
    let cName = unsafeName "Cstr"
        cDef = unsafeConstructorDef cName []
        isCstrName = unsafeName (TorXakis.PrettyPrint.TorXakis.toString (txsFunctionNameIsConstructor cDef)) 
      in do
        ctx <- arbitraryTestSortContext
        s <- arbitrarySort ctx
        let field = FieldDef isCstrName s
            c2Name = unsafeName "AnotherCstr"
            c2Def  = unsafeConstructorDef c2Name [field]
            aName  = unsafeName "ADT"
          in
            case mkADTDef aName [cDef, c2Def] of
                Left _  -> return $ s == SortBool
                Right _ -> return $ s /= SortBool

-- | Increments can be combined
prop_Increments :: Gen Bool
prop_Increments = 
    let c0 = empty :: ContextTestSort in do
        incr1 <- arbitraryADTDefs c0
        case addADTs incr1 c0 of
            Left e1  -> error ("Invalid generator 1 - " ++ show e1)
            Right c1 -> do
                            incr2 <- arbitraryADTDefs c1
                            case addADTs incr2 c1 of
                                Left e2  -> error ("Invalid generator 2 - " ++ show e2)
                                Right c2 -> return $ case addADTs (incr2++incr1) c0 of
                                                        Left e    -> trace ("error = " ++ show e) False
                                                        Right c12 -> c12 == c2 || trace ("incr1 = " ++ show incr1 ++ "\nincr2 = " ++ show incr2) False

spec :: Spec
spec = do
  describe "A sort gen context" $
        it "incr2 after incr1 == incr2 ++ incr1" $ property prop_Increments
  describe "An ADT definition" $ 
        it "has name constraints" $ property prop_SignatureClashes