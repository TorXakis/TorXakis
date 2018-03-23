-- |

module TorXakis.ParserSpec where

import           Data.Either          (isRight)
import           Data.Foldable        (traverse_)
import           System.FilePath      ((</>))
import           System.FilePath.Find (extension, find, (==?))
import           Test.Hspec           (Spec, describe, it, parallel, runIO,
                                       shouldSatisfy)

import           TorXakis.Parser      (parseFile)


spec :: Spec
spec = do
    describe "Correctly parses the incremental examples" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "success")
        parallel $ traverse_ testParser fs

    describe "Correctly parses complete TorXakis models" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "parser" </> "success")
        parallel $ traverse_ testParser fs

    where
        testParser fp = it (show fp) $ do
            r <- parseFile fp
            r `shouldSatisfy` isRight
