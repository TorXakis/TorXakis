@ECHO OFF
setlocal 
set CALLLOC=%cd%

echo ----- Start StimulusResponse Tests
cd %1\test\examps\stimulusresponse
call TestStimulusResponse.bat %1
echo ----- End StimulusResponse Tests

echo ----- Start CustomersOrders Tests
cd %1\test\examps\CustomersOrders
call TestCustomersOrders.bat %1
echo ----- End CustomersOrders Tests
TIMEOUT /T 10 /NOBREAK

echo ----- Start LuckyPeople Tests
cd %1\test\examps\LuckyPeople
call TestLuckyPeople.bat %1
echo ----- End LuckyPeople Tests

echo ----- Start Adder Tests
cd %1\test\examps\Adder
call TestAdder.bat %1
echo ----- End Adder Tests

echo ----- Start DispatchProcess Tests
cd %1\test\examps\DispatchProcess
call TestDispatchProcess.bat %1
echo ----- End DispatchProcess Tests

cd %CALLLOC%
endlocal