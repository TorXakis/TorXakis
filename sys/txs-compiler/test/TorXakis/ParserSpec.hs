-- |

module TorXakis.ParserSpec where

import           Test.Hspec (Spec, it, shouldSatisfy, runIO, describe, parallel)
import           System.FilePath.Find (find, extension, (==?))
import           System.FilePath ((</>))
import           Data.Either (isRight)
import           Data.Foldable (traverse_)

import           TorXakis.Parser (parseFile)


spec :: Spec
spec = 
    describe "Correctly parses the examples" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "success")
        parallel $ traverse_ testParser fs

    where
        testParser fp = it (show fp) $ do
            r <- parseFile fp
            r `shouldSatisfy` isRight
