REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET REL=examps\Adder
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start Adder Test 1 - Basic
start /min java -cp %EXAMPS% Adder 7890
call %TORXAKIS% %EXAMPS%\Adder.txs < %TEST%\Adder_Tester.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Adder.Sut.Adder.log
echo ------- End Adder Test 1

echo ------- Start Adder Test 2 - State Automation
start /min java -cp %EXAMPS% Adder 7890
call %TORXAKIS% %EXAMPS%\AdderStAut.txs < %TEST%\Adder_Tester.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.AdderStAut.Sut.Adder.log
echo ------- End Adder Test 2

echo ------- Start Adder Test 3 - Parallel Adders
start /min java -cp %EXAMPS% Adder 7891 7892 7893
call %TORXAKIS% %EXAMPS%\Adder.txs < %TEST%\Adder3_Tester.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Adder3.Sut.Adder3.log
echo ------- End Adder Test 3
