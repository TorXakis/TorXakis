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
call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Step.log
echo ------- End Queue Stepper Test

REM echo ------- Start Queue Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Queue.txs < %TEST%\Queue_Sim.txscmd
REM call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Test.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Sim.log
REM echo ------- End Queue Simulator Test

echo ------- Start Queue_Lossy Stepper Test
call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Lossy_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Lossy_Step.log
echo ------- End Queue_Lossy Stepper Test

REM echo ------- Start Queue_Lossy Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Queue.txs < %TEST%\Queue_Lossy_Sim.txscmd
REM call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Lossy_Test.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Lossy_Sim.log
REM echo ------- End Queue_Lossy Simulator Test

echo ------- Start Queue SUT Test
start /min java -cp %EXAMPS% Main 7890
call %TORXAKIS% %EXAMPS%\Queue.txs < %TEST%\Queue_Test.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Queue.Sut.Queue_Test.log
echo ------- End Queue SUT Test

REM Test StAut implementation
echo ------- Start QueueStAut Stepper Test
call %TORXAKIS% %EXAMPS%\QueueStAut.txs < %TEST%\Queue_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.QueueStAut.Sut.Queue_Step.log
echo ------- End QueueStAut Stepper Test

echo ------- Start QueueStAut_Lossy Stepper Test
call %TORXAKIS% %EXAMPS%\QueueStAut.txs < %TEST%\Queue_Lossy_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.QueueStAut.Sut.Queue_Lossy_Step.log
echo ------- End QueueStAut_Lossy Stepper Test

REM echo ------- Start QueueStAut SUT Test
REM start /min java -cp %EXAMPS% Main 7890
REM call %TORXAKIS% %EXAMPS%\QueueStAut.txs < %TEST%\Queue_Test.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.QueueStAut.Sut.Queue_Test.log
REM echo ------- End QueueStAut SUT Test
