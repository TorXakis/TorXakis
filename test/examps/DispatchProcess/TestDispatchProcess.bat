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


REM echo ------- Start DisPro01-processor Test
REM call %TORXAKIS% %EXAMPS%\DisPro01-processor.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro01-processor.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro01-processor Test


REM echo ------- Start DisPro02-dispatch Test
REM call %TORXAKIS% %EXAMPS%\DisPro02-dispatch.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro02-dispatch.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro02-dispatch Test


REM echo ------- Start DisPro03-processors Test
REM call %TORXAKIS% %EXAMPS%\DisPro03-processors.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro03-processors.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro03-processors Test


REM echo ------- Start DisPro04-hide Test
REM call %TORXAKIS% %EXAMPS%\DisPro04-hide.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro04-hide.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro04-hide Test


REM echo ------- Start DisPro05-data Test
REM call %TORXAKIS% %EXAMPS%\DisPro05-data.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro05-data.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro05-data Test


REM echo ------- Start DisPro05a-data-nohide Test
REM call %TORXAKIS% %EXAMPS%\DisPro05a-data-nohide.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro05a-data-nohide.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro05a-data-nohide Test


REM echo ------- Start DisPro06-datapos Test
REM call %TORXAKIS% %EXAMPS%\DisPro06-datapos.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro06-datapos.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro06-datapos Test


REM echo ------- Start DisPro06a-datapos-nohide Test
REM call %TORXAKIS% %EXAMPS%\DisPro06a-datapos-nohide.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro06a-datapos-nohide.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro06a-datapos-nohide Test


REM echo ------- Start DisPro07-gcd Test
REM call %TORXAKIS% %EXAMPS%\DisPro07-gcd.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro07-gcd.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro07-gcd Test


REM echo ------- Start DisPro08-gcdpurp Test
REM call %TORXAKIS% %EXAMPS%\DisPro08-gcdpurp.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro08-gcdpurp.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro08-gcdpurp Test


REM echo ------- Start DisPro09-multiproc Test
REM call %TORXAKIS% %EXAMPS%\DisPro09-multiproc.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM TIMEOUT /T 2 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro09-multiproc.Sut.DispatchProcess_Stepper.log
REM echo ------- End DisPro09-multiproc Test


REM echo ------- Start DisPro10-data Test
REM start /min java -cp %EXAMPSUTS% DispatchProcess
REM call %TORXAKIS% %EXAMPS%\DisPro10-data.txs < %TEST%\DispatchProcess_Tester.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro10-data.Sut.DispatchProcess.log
REM echo ------- End DisPro10-data Test


REM REM echo ------- Start DisPro11-robust Test
REM REM call %TORXAKIS% %EXAMPS%\DisPro11-robust.txs < %TEST%\DispatchProcess_Stepper.txscmd
REM REM TIMEOUT /T 2 /NOBREAK
REM REM move /Y testTrace.log testTrace.Spec.DisPro11-robust.Sut.DispatchProcess_Stepper.log
REM REM echo ------- End DisPro11-robust Test


REM echo ------- Start DisPro12-unique-id Test
REM start /min java -cp %EXAMPSUTS% DispatchProcess
REM call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Tester_12.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcess.log
REM echo ------- End DisPro12-unique-id Test


REM echo ------- Start DisPro12-unique-id_Wrong Test
REM start /min java -cp %EXAMPSUTS% DispatchProcess
REM call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Tester_12_Wrong.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcess.log
REM echo ------- End DisPro12-unique-id_Wrong Test




REM echo ------- Start DisPro12-unique-id_Right Test
REM start /min java -cp %EXAMPSUTS% DispatchProcess
REM call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Tester_12_Right.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcessSut_12_Right.log
REM echo ------- End DisPro12-unique-id_Right Test

echo ------- Start DisPro12-unique-id_PurposeWrong Test
start /min java -cp %EXAMPSUTS% DispatchProcess
call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Tester_12_Wrong.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcessSut_12_PurposeWrong.log
echo ------- End DisPro12-unique-id_PurposeWrong Test

echo ------- Start DisPro12-unique-id_PurposeRight Test
start /min java -cp %EXAMPSUTS% DispatchProcess
call %TORXAKIS% %EXAMPS%\DisPro12-unique-id.txs < %TEST%\DispatchProcess_Tester_12_Right.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.DisPro12-unique-id.Sut.DispatchProcessSut_12_PurposeRight.log
echo ------- End DisPro12-unique-id_PurposeRight Test
