@ECHO OFF

REM Run SWBM
SWBM.exe

REM Update SFR parameters
Rscript Update_SFR_Parameters_Sensitivity.R

REM Run MODFLOW
OneWater SVHSM.nam


REM Delay execution so files are not read simultaneously
set /a num=%random% %%30+1
timeout %num%

REM Copy listing file for each sensitivity run for troubleshooting
find /I "PERCENT DISCREPANCY" SVHSM.lst >> ..\Mass_Balance.rec