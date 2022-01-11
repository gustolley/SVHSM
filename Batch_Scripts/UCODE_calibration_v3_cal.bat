@echo off

set run_name=v3_cal

set MF_dir=MODFLOW_v3
set SWBM_dir=input_150m_grid
set UCODE_dir=UCODE_v3

set UCODE_in_file=SVHSM_v3_cal.in
set UCODE_out_file=SVHSM_v3_cal_out
set UCODE_param_file=SVHSM_v3_cal.param

cd ..\SVHSM_runs\
IF EXIST %run_name% (
set INPUT=
set /P INPUT="All data will be overwritten. Continue? (y/n):"
echo %INPUT%
)
If "%INPUT%"=="n" EXIT
If "%INPUT%"=="N" EXIT

RMDIR %run_name%/S /Q  
MKDIR %run_name%
  
REM copy only necessary files for %run_name% analysis run  
for /L %%i in (1,1,10) do (  
  MKDIR %run_name%\Runner%%i  
  copy ..\SWBM\bin\SWBM.exe %run_name%\Runner%%i\SWBM.exe
  xcopy ..\%MF_dir% %run_name%\Runner%%i /E /Y
  xcopy ..\SWBM\%SWBM_dir% %run_name%\Runner%%i /E /Y
  xcopy ..\%UCODE_dir%\UCODE_Instruction_Files %run_name%\Runner%%i /E /Y  
  xcopy ..\%UCODE_dir%\UCODE_Template_Files %run_name%\Runner%%i /E /Y
  copy ..\%UCODE_dir%\UCODE_Input_Files\%UCODE_param_file% %run_name%\Runner%%i\%UCODE_param_file%
  copy ..\%UCODE_dir%\UCODE_Input_Files\SVHSM.pumpingobs %run_name%\Runner%%i\SVHSM.pumpingobs
  copy ..\%UCODE_dir%\UCODE_Input_Files\SVHSM.headobs %run_name%\Runner%%i\SVHSM.headobs
  copy ..\%UCODE_dir%\UCODE_Input_Files\SVHSM.flowobs %run_name%\Runner%%i\SVHSM.flowobs
  copy ..\%UCODE_dir%\UCODE_Input_Files\SVHSM.obsgrp %run_name%\Runner%%i\SVHSM.obsgrp
  copy ..\%UCODE_dir%\UCODE_Input_Files\SVHSM.pargrp %run_name%\Runner%%i\SVHSM.pargrp 
  copy ..\%UCODE_dir%\runner.exe %run_name%\Runner%%i\runner.exe  
  copy "..\R Scripts\UCODE Parameter Updates\Update_SFR_Parameters_Sensitivity.R" %run_name%\Runner%%i\Update_SFR_Parameters_Sensitivity.R
  copy ..\Batch_Scripts\Run_SVHSM_calibration_runner.bat %run_name%\Runner%%i\Run_SVHSM.bat
)
xcopy %run_name%\Runner1 %run_name% /E /Y 
copy ..\Batch_Scripts\Run_SVHSM_calibration_master.bat %run_name%\Run_SVHSM.bat
copy ..\%UCODE_dir%\%UCODE_in_file% %run_name%\%UCODE_in_file%

cd %run_name%  
for /L %%i in (1,1,10) do ( 
cd Runner%%i
start runner.exe  
cd .. 
)  
REM Sensitivity
echo Output now being written to record file UCODE.log
echo Model Running...
ucode_2014  %UCODE_in_file% %UCODE_out_file%_out echo>UCODE.log
echo Model Run Complete
pause
