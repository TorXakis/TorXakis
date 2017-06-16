echo BENCHMARK TEST   $> result.txt
param param_ImpRel IOCO
param param_InputCompletion ANGELIC
param param_RandSolve_IntHalf 1000
param param_RandSolve_IntNum 5
param param_RandSolve_adtWidth 5
param param_Randomization IncrementChoice
param param_Sim_deltaTime 200
param param_Sim_inputEager 0
param param_Sut_deltaTime 2000
param param_Sut_ioTime 10
param param_Test_inputEager 3
param param_TrueBins_Next Linear
param param_TrueBins_NrOfBins 10
param param_TrueBins_StringLength 6
param param_TrueBins_StringMode Regex
param param_max_rand_depth 4
echo                  $>> result.txt
info                  $>> result.txt
echo                  $>> result.txt
param                 $>> result.txt
time                  $>> result.txt
seed 12345            $>> result.txt
echo                  $>> result.txt
timer all
echo stepper SpecSequence
echo timer ts1
echo step 100
echo timer ts1             $>> result.txt
echo stop
echo stepper SpecSequenceIStep
echo timer ts2
echo step 100
echo timer ts2             $>> result.txt
echo stop
echo stepper SpecSequenceData
echo timer ts3
echo step 100
echo timer ts3             $>> result.txt
echo stop
echo stepper SpecSequenceAlternate
echo timer ts4
echo step 100
echo timer ts4             $>> result.txt
echo stop
echo stepper SpecChoice
echo timer tc1
echo step 100
echo timer tc1             $>> result.txt
echo stop
echo stepper SpecSynchronized
echo timer ty1
echo step 100
echo timer ty1             $>> result.txt
echo stop
echo stepper SpecSynchronizedIStep
echo timer ty2
echo step 100
echo timer ty2             $>> result.txt
echo stop
echo stepper SpecSynchronizedAlternate
echo timer ty3
echo step 100
echo timer ty3             $>> result.txt
echo stop
echo stepper SpecSynchronizedAB
echo timer ty4
echo step 100
echo timer ty4             $>> result.txt
echo stop
echo stepper SpecSynchronizedMany
echo timer ty5
echo step 100
echo timer ty5             $>> result.txt
echo stop
echo stepper SpecSynchronizedManyPairs
echo timer ty6
echo step 100
echo timer ty6             $>> result.txt
echo stop
echo stepper SpecParallel
echo timer tp1
echo step 100
echo timer tp1             $>> result.txt
echo stop
echo stepper SpecParallelIStep
echo timer tp2
echo step 100
echo timer tp2             $>> result.txt
echo stop
echo stepper SpecParallelAlternate
echo timer tp3
echo step 100
echo timer tp3             $>> result.txt
echo stop
echo stepper SpecParallelAB
echo timer tp4
echo step 100
echo timer tp4             $>> result.txt
echo stop
echo stepper SpecParallelData
echo timer tp5
echo step 100
echo timer tp5             $>> result.txt
echo stop
echo stepper SpecHideC_synchC_Par_Alternate_C_Xi
echo timer th1
echo step 100
echo timer th1             $>> result.txt
echo stop
echo stepper SpecHideC_synchC_Par_Alternate_C_X
echo timer th2
echo step 100
echo timer th2             $>> result.txt
echo stop
echo stepper SpecHideC_synchX_Par_Alternate_C_X
echo timer th3
echo step 100
echo timer th3             $>> result.txt
echo stop
echo stepper SpecHide_Bag_Match
echo timer th4
echo step 100
echo timer th4             $>> result.txt
echo stop
echo stepper SpecHide_Bag_MatchInt
echo timer th5
echo step 100
echo timer th5             $>> result.txt
echo stop
echo stepper SpecParallelSynchronous
echo timer tps
echo step 100
echo timer tps             $>> result.txt
echo stop
echo stepper SpecNested
echo timer tn1
echo step 100
echo timer tn1             $>> result.txt
echo stop
echo stepper SpecSequenceEnable
echo timer te1
echo step 100
echo timer te1             $>> result.txt
echo stop
echo stepper SpecSequenceEnableInt
echo timer te2
echo step 100
echo timer te2             $>> result.txt
echo stop
echo stepper SpecSequenceEnableIntTwice
echo timer te3
echo step 100
echo timer te3             $>> result.txt
echo stop
stepper SpecSequence10Ints
timer ta1
step 100
timer ta1             $>> result.txt
stop
stepper SpecSequence10Ints_b
timer ta_b
step 100
timer ta_b             $>> result.txt
stop
echo                  $>> result.txt
timer all             $>> result.txt
echo                  $>> result.txt
echo End of Test      $>> result.txt
exit
