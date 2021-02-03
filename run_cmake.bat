@echo off

REM -----------------------------------------
REM WIN64 build file for MYSTRAN with gcc for win
REM by ceanwang@gmail.com
REM -----------------------------------------

MKDIR build_win10
CD build_win10
cmake -DCMAKE_MAKE_PROGRAM=make -DCMAKE_Fortran_COMPILER=gFortran -G "Unix Makefiles" ..
rem make
