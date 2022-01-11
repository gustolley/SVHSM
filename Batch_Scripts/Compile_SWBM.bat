@ECHO OFF

cd ..\SWBM\src

gfortran -c -fbounds-check define_fields.f90
gfortran -c -fbounds-check irrigation.f90
gfortran -c -fbounds-check outputmodule.f90
gfortran -c -fbounds-check SWBM.f90
gfortran *.o -o ..\bin\SWBM.exe

del /S *.o
del /S *.mod0
del /S *.mod

if exist ..\bin\SWBM.exe Echo SWBM successfully compiled

pause


