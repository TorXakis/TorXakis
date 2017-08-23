@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET TORXAKISPORT=torxakisport.bat
SET REL=examps\Queue
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start Queue Stepper Test
call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Stepper.log
echo ------- End Queue Stepper Test

REM echo ------- Start Queue Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Queue.txs < %TEST%\Queue_Simulator.txscmd
REM call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Tester.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Simulator.log
REM echo ------- End Queue Simulator Test

echo ------- Start Queue_Lossy Stepper Test
call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Lossy_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Lossy_Stepper.log
echo ------- End Queue_Lossy Stepper Test

REM echo ------- Start Queue_Lossy Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Queue.txs < %TEST%\Queue_Lossy_Simulator.txscmd
REM call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Lossy_Tester.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Lossy_Simulator.log
REM echo ------- End Queue_Lossy Simulator Test

echo ------- Start Queue SUT Test
start /min java -cp %EXAMPS% Main 7890
call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Tester.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Tester.log
echo ------- End Queue SUT Test

REM Test StAut implementation
echo ------- Start QueueStAut Stepper Test
call %TORXAKIS% %EXAMPS%\QueueStAut.txs < %TEST%\Queue_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.QueueStAut.Sut.Queue_Stepper.log
echo ------- End QueueStAut Stepper Test

echo ------- Start QueueStAut_Lossy Stepper Test
call %TORXAKIS% %EXAMPS%\QueueStAut.txs < %TEST%\Queue_Lossy_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.QueueStAut.Sut.Queue_Lossy_Stepper.log
echo ------- End QueueStAut_Lossy Stepper Test

REM echo ------- Start QueueStAut SUT Test
REM start /min java -cp %EXAMPS% Main 7890
REM call %TORXAKIS% %EXAMPS%\QueueStAut.txs < %TEST%\Queue_Tester.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.QueueStAut.Sut.Queue_Tester.log
REM echo ------- End QueueStAut SUT Test