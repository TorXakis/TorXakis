module TorXakis.CompilerSpec where

import           Test.Hspec ( Spec, it, shouldSatisfy, runIO
                            , describe, parallel, shouldBe, Expectation)
import           System.FilePath.Find (find, extension, (==?))
import           System.FilePath ((</>))
import           Data.Either (isRight)
import           Data.Foldable (traverse_)

import           TxsAlex (txsLexer)
import           TxsHappy (txsParser)
import           Id (Id, Resettable, reset)
import           VarId (VarId)
import           Sigs (Sigs)
import           TxsDefs (TxsDefs, sortDefs, cstrDefs, funcDefs, varDefs)

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
            -- First sanity check, the models are successfully compiled.
            r `shouldSatisfy` isRight
            let Right (_, tdefs, _sigs) = r
            (_, tdefs', _sigs') <- txsCompile fp
            sortDefs tdefs ~==~ sortDefs tdefs'
            cstrDefs tdefs ~==~ cstrDefs tdefs'
            funcDefs tdefs ~==~ funcDefs tdefs'
            varDefs  tdefs ~==~ varDefs  tdefs'
            

-- | Equality modulo unique id's.
(~==~) :: (Resettable e, Show e, Eq e) => e -> e -> Expectation
e0 ~==~ e1 = reset e0 `shouldBe` reset e1

txsCompile :: FilePath -> IO (Id, TxsDefs, Sigs VarId)
txsCompile = (txsParser . txsLexer <$>) . readFile
        
