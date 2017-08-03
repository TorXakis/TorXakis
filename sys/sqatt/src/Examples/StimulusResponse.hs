{-# LANGUAGE OverloadedStrings #-}
module Examples.StimulusResponse (examples) where

import           Sqatt

test1 :: TxsExample
test1 = TxsExample
  "Stimulus-Response Test 1"
  "examps/stimulusresponse/StimulusResponse.txs"
  "test/examps/stimulusresponse/StimulusResponse.txscmd"
  (Just "examps/stimulusresponse/StimulusResponse.java")
  Pass

examples :: [TxsExample]
examples = [test1]
