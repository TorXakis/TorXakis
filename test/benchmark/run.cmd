echo BENCHMARK TEST   $> benchmarkresult.log
param param_ImpRel IOCO
param param_InputCompletion ANGELIC
param param_RandSolve_IntHalf 1000
param param_RandSolve_IntNum 5
param param_RandSolve_adtWidth 5
param param_Randomization TrueBins
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
echo                  $>> benchmarkresult.log
info                  $>> benchmarkresult.log
echo                  $>> benchmarkresult.log
param                 $>> benchmarkresult.log
time                  $>> benchmarkresult.log
seed 12345            $>> benchmarkresult.log
echo                  $>> benchmarkresult.log
timer all
stepper SpecSequence
timer ts1
step 100
timer ts1             $>> benchmarkresult.log
stop
stepper SpecSequenceIStep
timer ts2
step 100
timer ts2             $>> benchmarkresult.log
stop
stepper SpecSequenceData
timer ts3
step 100
timer ts3             $>> benchmarkresult.log
stop
stepper SpecSequenceAlternate
timer ts4
step 100
timer ts4             $>> benchmarkresult.log
stop
stepper SpecChoice
timer tc1
step 100
timer tc1             $>> benchmarkresult.log
stop
stepper SpecSynchronized
timer ty1
step 100
timer ty1             $>> benchmarkresult.log
stop
stepper SpecSynchronizedIStep
timer ty2
step 100
timer ty2             $>> benchmarkresult.log
stop
stepper SpecSynchronizedAlternate
timer ty3
step 100
timer ty3             $>> benchmarkresult.log
stop
stepper SpecSynchronizedAB
timer ty4
step 100
timer ty4             $>> benchmarkresult.log
stop
stepper SpecSynchronizedMany
timer ty5
step 100
timer ty5             $>> benchmarkresult.log
stop
stepper SpecSynchronizedManyPairs
timer ty6
step 100
timer ty6             $>> benchmarkresult.log
stop
stepper SpecParallel
timer tp1
step 100
timer tp1             $>> benchmarkresult.log
stop
stepper SpecParallelIStep
timer tp2
step 100
timer tp2             $>> benchmarkresult.log
stop
stepper SpecParallelAlternate
timer tp3
step 100
timer tp3             $>> benchmarkresult.log
stop
stepper SpecParallelAB
timer tp4
step 100
timer tp4             $>> benchmarkresult.log
stop
stepper SpecParallelData
timer tp5
step 100
timer tp5             $>> benchmarkresult.log
stop
stepper SpecHideC_synchC_Par_Alternate_C_Xi
timer th1
step 100
timer th1             $>> benchmarkresult.log
stop
stepper SpecHideC_synchC_Par_Alternate_C_X
timer th2
step 100
timer th2             $>> benchmarkresult.log
stop
stepper SpecHideC_synchX_Par_Alternate_C_X
timer th3
step 100
timer th3             $>> benchmarkresult.log
stop
stepper SpecHide_Bag_Match
timer th4
step 100
timer th4             $>> benchmarkresult.log
stop
stepper SpecHide_Bag_MatchInt
timer th5
step 100
timer th5             $>> benchmarkresult.log
stop
stepper SpecParallelSynchronous
timer tps
step 100
timer tps             $>> benchmarkresult.log
stop
stepper SpecNested
timer tn1
step 100
timer tn1             $>> benchmarkresult.log
stop
stepper SpecSequenceEnable
timer te1
step 100
timer te1             $>> benchmarkresult.log
stop
stepper SpecSequenceEnableInt
timer te2
step 100
timer te2             $>> benchmarkresult.log
stop
stepper SpecSequenceEnableIntTwice
timer te3
step 100
timer te3             $>> benchmarkresult.log
stop
stepper SpecSequence10Ints
timer ta1
step 100
timer ta1             $>> benchmarkresult.log
stop
stepper SpecSequence10Ints_b
timer ta_b
step 100
timer ta_b            $>> benchmarkresult.log
stop
echo                  $>> benchmarkresult.log
timer all             $>> benchmarkresult.log
echo                  $>> benchmarkresult.log
echo End of Test      $>> benchmarkresult.log
exit
