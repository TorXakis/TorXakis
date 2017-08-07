REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

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


echo ------- Start MovingArms Stepper Test
call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Step.log
echo ------- End MovingArms Stepper Test

REM echo ------- Start MovingArms Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Sim.txscmd
REM call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Test.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Sim.log
REM echo ------- End MovingArms Simulator Test

echo ------- Start MovingArms_Purpose Stepper Test
call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Purpose_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Purpose_Step.log
echo ------- End MovingArms_Purpose Stepper Test

REM echo ------- Start MovingArms Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Purpose_Sim.txscmd
REM call %TORXAKIS% %EXAMPS%\MovingArms.txs < %TEST%\MovingArms_Purpose_Test.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.MovingArms.Sut.MovingArms_Purpose_Sim.log
REM echo ------- End MovingArms Simulator Test
