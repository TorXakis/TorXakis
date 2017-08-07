REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET REL=examps\ControlLoop
SET EXAMPS=%1\%REL%\
SET EXAMPSUTS=%1\%REL%\
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start ControlLoopModel Test
call %TORXAKIS% %EXAMPS%\ControlLoopModel.txs < %TEST%\ControlLoop.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.ControlLoopModel.Sut.ControlLoop.log
echo ------- End ControlLoopModel Test


echo ------- Start MultipleControlLoops_SpecProduce Test
call %TORXAKIS% %EXAMPS%\MultipleControlLoops.txs < %TEST%\MultipleControlLoops_SpecProduce.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MultipleControlLoops.Sut.SpecProduce.log
echo ------- End MultipleControlLoops_SpecProduce Test


echo ------- Start MultipleControlLoops_SpecMeasure Test
call %TORXAKIS% %EXAMPS%\MultipleControlLoops.txs < %TEST%\MultipleControlLoops_SpecMeasure.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MultipleControlLoops.Sut.SpecMeasure.log
echo ------- End MultipleControlLoops_SpecMeasure Test


echo ------- Start MultipleControlLoops_SpecCorrect Test
call %TORXAKIS% %EXAMPS%\MultipleControlLoops.txs < %TEST%\MultipleControlLoops_SpecCorrect.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MultipleControlLoops.Sut.SpecCorrect.log
echo ------- End MultipleControlLoops_SpecCorrect Test


echo ------- Start MultipleControlLoops_Spec Test
call %TORXAKIS% %EXAMPS%\MultipleControlLoops.txs < %TEST%\MultipleControlLoops_Spec.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MultipleControlLoops.Sut.Spec.log
echo ------- End MultipleControlLoops_Spec Test

