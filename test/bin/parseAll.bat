@echo off
for /f "usebackq" %%A in (`dir /S /B ^| findstr /E "\.txs"`) do (
    torxakis.bat %%A  < exit.txscmd 
    TIMEOUT /T 10 /NOBREAK > nul
)