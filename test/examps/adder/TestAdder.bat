@ECHO OFF
SET TORXAKIS=torxakis.bat
SET REL=examps\adder
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start Adder Test 1 - Basic
start /min java -cp %EXAMPS% Adder 7890
echo call %TORXAKIS% %EXAMPS%\Adder.txs < %TEST%\Adder.txscmd
call %TORXAKIS% %EXAMPS%\Adder.txs < %TEST%\Adder.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Adder.Sut.Adder.log
echo ------- End Adder Test 1

echo ------- Start Adder Test 2 - State Automation
start /min java -cp %EXAMPS% Adder 7890
call %TORXAKIS% %EXAMPS%\AdderStAut.txs < %TEST%\Adder.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.AdderStAut.Sut.Adder.log
echo ------- End Adder Test 2

echo ------- Start Adder Test 3 - Parallel Adders
start /min java -cp %EXAMPS% Adder 7891
start /min java -cp %EXAMPS% Adder 7892
start /min java -cp %EXAMPS% Adder 7893
call %TORXAKIS% %EXAMPS%\Adder.txs < %TEST%\Adder3.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Adder3.Sut.Adder3.log
echo ------- End Adder Test 3

REM echo ------- Start Adder Test 4
REM start /min java -cp %EXAMPS% Adder
REM call %TORXAKIS% %EXAMPS%\AdderLoop.txs < %TEST%\Adder.txscmd
REM TIMEOUT /T 10 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.AdderLoop.Sut.Adder.log
REM echo ------- End Adder Test 4

REM echo ------- Start Adder Test 5
REM start /min java -cp %EXAMPS% AdderLoop
REM call %TORXAKIS% %EXAMPS%\Adder.txs < %TEST%\Adder.txscmd
REM TIMEOUT /T 10 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Adder.Sut.AdderLoop.log
REM echo ------- End Adder Test 5
