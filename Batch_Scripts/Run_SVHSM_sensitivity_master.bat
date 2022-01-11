@ECHO OFF

SWBM.exe

REM Update SFR parameters
Rscript Update_SFR_Parameters_Sensitivity.R

OneWater SVHSM.nam

find /I "PERCENT DISCREPANCY" SVHSM.lst >> Mass_Balance.rec