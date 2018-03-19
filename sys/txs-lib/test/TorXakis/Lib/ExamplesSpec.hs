module TorXakis.Lib.ExamplesSpec where

import           Test.Hspec (Spec, it)
import           TorXakis.Lib.Examples

spec :: Spec
spec = do
    it "testEchoReactive" testEchoReactive
    it "testWrongFile"    testWrongFile
