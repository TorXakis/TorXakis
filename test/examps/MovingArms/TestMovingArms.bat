@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET TORXAKISPORT=torxakisport.bat
SET REL=examps\MovingArms
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


REM echo ------- Start MovingArms Stepper Test
call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Step.txscmd
REM TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Step.log
REM echo ------- End MovingArms Stepper Test

REM REM echo ------- Start MovingArms Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Sim.txscmd
REM call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Test.txscmd
REM REM TIMEOUT /T 4 /NOBREAK
REM REM move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Sim.log
REM REM echo ------- End MovingArms Simulator Test

REM echo ------- Start MovingArms_Purpose Stepper Test
REM call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Purpose_Stepper.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Purpose_Stepper.log
REM echo ------- End MovingArms_Purpose Stepper Test

REM REM echo ------- Start MovingArms Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Purpose_Simulator.txscmd
REM REM call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Purpose_Tester.txscmd
REM REM TIMEOUT /T 4 /NOBREAK
REM REM move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Purpose_Simulator.log
REM REM echo ------- End MovingArms Simulator Test

echo ------- Start MovingArms SingleAxisTestPurpose Test
start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Purpose_Simulator.txscmd
call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_SingleAxisTestPurpose_Test.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MovingArms_SingleAxisTestPurpose.Sut.MovingArms_Purpose_Simulator.log
echo ------- End MovingArms SingleAxisTestPurpose Test
