module TorXakis.CompilerSpec where

import           Test.Hspec (Spec, it, shouldSatisfy, runIO, describe, parallel)
import           System.FilePath.Find (find, extension, (==?))
import           System.FilePath ((</>))
import           Data.Either (isRight)
import           Data.Foldable (traverse_)

import           TorXakis.Compiler (compileFile)

spec :: Spec
spec = 
    describe "Correctly compiles the examples" $ do
        fs <- runIO $ find (return True) (extension ==? ".txs")
              ("test" </> "data" </> "success")
        parallel $ traverse_ testParser fs

    where
        testParser fp = it (show fp) $ do
            r <- compileFile fp
            r `shouldSatisfy` isRight
        
