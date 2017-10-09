@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET REL=examps\DispatchProcess
SET EXAMPS=%1\%REL%\spec
SET EXAMPSUTS=%1\%REL%\sut
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo EXAMPSUTS %EXAMPSUTS%
echo TEST %TEST%


echo ------- Start DisPro01-processor Test
call %TORXAKIS% %EXAMPS%\DisPro01-processor.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro01-processor.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro01-processor Test


echo ------- Start DisPro02-dispatch Test
call %TORXAKIS% %EXAMPS%\DisPro02-dispatch.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro02-dispatch.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro02-dispatch Test


echo ------- Start DisPro03-processors Test
call %TORXAKIS% %EXAMPS%\DisPro03-processors.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro03-processors.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro03-processors Test


echo ------- Start DisPro04-hide Test
call %TORXAKIS% %EXAMPS%\DisPro04-hide.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro04-hide.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro04-hide Test


echo ------- Start DisPro05-data Test
call %TORXAKIS% %EXAMPS%\DisPro05-data.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro05-data.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro05-data Test


echo ------- Start DisPro05a-data-nohide Test
call %TORXAKIS% %EXAMPS%\DisPro05a-data-nohide.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro05a-data-nohide.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro05a-data-nohide Test


echo ------- Start DisPro06-datapos Test
call %TORXAKIS% %EXAMPS%\DisPro06-datapos.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro06-datapos.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro06-datapos Test


echo ------- Start DisPro06a-datapos-nohide Test
call %TORXAKIS% %EXAMPS%\DisPro06a-datapos-nohide.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro06a-datapos-nohide.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro06a-datapos-nohide Test


echo ------- Start DisPro07-gcd Test
call %TORXAKIS% %EXAMPS%\DisPro07-gcd.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro07-gcd.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro07-gcd Test


echo ------- Start DisPro08-gcdpurp Test
call %TORXAKIS% %EXAMPS%\DisPro08-gcdpurp.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro08-gcdpurp.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro08-gcdpurp Test


echo ------- Start DisPro09-multiproc Test
call %TORXAKIS% %EXAMPS%\DisPro09-multiproc.txs < %TEST%\DispatchProcess_Stepper.txscmd
TIMEOUT /T 2 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro09-multiproc.Sut.DispatchProcess_Stepper.log
echo ------- End DisPro09-multiproc Test


echo ------- Start DisPro10-data Test
start /min java -cp %EXAMPSUTS% DispatchProcess
call %TORXAKIS% %EXAMPS%\DisPro10-data.txs < %TEST%\DispatchProcess_Tester.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro10-data.Sut.DispatchProcess.log
echo ------- End DisPro10-data Test


REM echo ------- Start DisPro11-robust Test
REM call %TORXAKIS% %EXAMPS%\DisPro11-robust.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro11-robust.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro11-robust Test


echo ------- Start DisPro12-unique-id Test
start /min java -cp %EXAMPSUTS% DispatchProcess
call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Tester_12.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcess.log
echo ------- End DisPro12-unique-id Test


echo ------- Start DisPro12-unique-id_Wrong Test
start /min java -cp %EXAMPSUTS% DispatchProcess
call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Stepper_12_Wrong.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcess.log
echo ------- End DisPro12-unique-id_Wrong Test


echo ------- Start DisPro12-unique-id_Right Test
start /min java -cp %EXAMPSUTS% DispatchProcess
call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Stepper_12_Right.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcess.log
echo ------- End DisPro12-unique-id_Right Test
