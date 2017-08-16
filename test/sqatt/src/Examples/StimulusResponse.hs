{-# LANGUAGE OverloadedStrings #-}
module Examples.StimulusResponse (test1, exampleSet) where

import           Filesystem.Path
import           Sqatt

test0 :: TxsExample
test0 = TxsExample
  "Stimulus-Response Test 0"
  ("examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txs")
  ("test"</>"examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txscmd")
  (Just $ "examps"</>"stimulusresponse"</>"StimulusResponse"<.>"java")
  Pass

test1 :: TxsExample
test1 = TxsExample
  "Stimulus-Response Test 1"
  ("examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txs")
  ("test"</>"examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txscmd")
  (Just $ "examps"</>"stimulusresponse"</>"StimulusNoResponse"<.>"java")
  Fail


test2 :: TxsExample
test2 = TxsExample
  "Stimulus-Response Test 2"
  ("examps"</>"stimulusresponse"</>"StimulusResponseLoop"<.>"txs")
  ("test"</>"examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txscmd")
  (Just $ "examps"</>"stimulusresponse"</>"StimulusResponseLoop"<.>"java")
  Pass

test3 :: TxsExample
test3 = TxsExample
  "Stimulus-Response Test 3"
  ("examps"</>"stimulusresponse"</>"StimulusResponseLoop"<.>"txs")
  ("test"</>"examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txscmd")
  (Just $ "examps"</>"stimulusresponse"</>"StimulusResponse"<.>"java")
  Fail

test4 :: TxsExample
test4 = TxsExample
  "Stimulus-Response Test 4"
  ("examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txs")
  ("test"</>"examps"</>"stimulusresponse"</>"StimulusResponse"<.>"txscmd")
  (Just $ "examps"</>"stimulusresponse"</>"StimulusResponseLoop"<.>"java")
  Pass

examples :: [TxsExample]
examples = [test0, test1, test2, test3, test4]

exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "Stimulus Response" examples
