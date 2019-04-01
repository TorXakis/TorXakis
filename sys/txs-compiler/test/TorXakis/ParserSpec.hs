{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ParserSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for the 'TorXakis' parser.
--------------------------------------------------------------------------------
module TorXakis.ParserSpec
    (spec)
where

import           Control.Lens ((^?))
import           Data.Either          (isRight)
import           Data.Foldable        (traverse_)
import           System.FilePath      ((</>))
import           Test.Hspec           (Spec, describe, it, parallel,
                                       shouldSatisfy, shouldBe)

import           Common                  (onAllFilesIn)
import           TorXakis.Compiler.Error (errorLoc, ErrorLoc(ErrorLoc))
import           TorXakis.Parser         (parseFile)



spec :: Spec
spec = do
    describe "Parses the single-concept examples" $
      testParser `onAllFilesIn` ("test" </> "data" </> "success")

    describe "Parses the examples in the 'examps' folder" $
      testParser `onAllFilesIn` ("test" </> "data" </> "examps")

    describe "Parses large models" $
      testParser `onAllFilesIn` ("test" </> "data" </> "large")

    describe "Parses the regression tests" $
      testParser `onAllFilesIn` ("test" </> "data" </> "regression")

    describe "Gives the expected errors" $
      parallel $ traverse_ checkError errors

    where
        testParser fp = it (show fp) $ do
            r <- parseFile fp
            r `shouldSatisfy` isRight

        checkError (fp, expErr) = it (show fp) $ do
            Left err <- parseFile fp
            err ^? errorLoc `shouldBe` Just expErr

        errors
          = [( prefix </> "UnmatchedParenthesis.txs", ErrorLoc 15 5)
            ,( prefix </> "ReservedWordAsIdentifier.txs", ErrorLoc 1 30)
            ]
          where
            prefix = "test" </> "data" </> "wrong"
