@ECHO OFF
SET job=%1
SET SYSTEMTYPE=dos
rem
rem
  ECHO  *
  ECHO  *
  ECHO  ************************************************************
  ECHO  *    Copyright 2019 National Technology and Engineering    *
  ECHO  *             Solutions of Sandia, LLC (NTESS)             *
  ECHO  *                                                          *
  ECHO  * Under the terms of Contract DE-NA0003525 with NTESS, the *
  ECHO  * U.S. Government retains certain rights in this software. *
  ECHO  *                                                          *
  ECHO  ************************************************************
  ECHO  *
  ECHO  *
rem
rem
rem
rem   Copy the executable file to working directory
rem
  COPY bin\MatMCNP.exe  MatMCNP.exe
rem
rem
rem   Run the job
rem
  matmcnp-PC.pl %1

set saveerr=%ERRORLEVEL%

if %saveerr% GTR 0 goto ERROR

rem
rem  Delete the executable 
rem
  del MatMCNP.exe
rem
rem
rem
  ECHO MatMCNP Calculation Complete

goto END
:USAGE
ECHO Usage: MatMCNP jobname
goto END
:ERROR
ECHO Batch file error occurred
:END