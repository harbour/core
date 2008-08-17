@echo off
rem
rem $Id$
rem

if "%1" =="/OS2" goto OS2
if "%1" =="/NGI" goto NG
if "%1" =="/RTF" goto RTF
if "%1" =="/HTM" goto HTM
ECHO Assembling input files
:NG
..\..\..\bin\hbdoc /ngi rddads.lnk rddads.rsp
REM Compile the sources
Echo Compiling the sources
Processing Input Files
Copy ngi\overview.ngi .
Compiling Sources
ngxc overview.ngi
Linking the Guide
ngxl rddads.lnk
del *.ngi
del *.ngo
del ngi\*.txt
del ngi\*.ngi
GOTO END
:OS2
   ..\..\..\bin\hbdoc /OS2 rddads.lnk rddads.rsp
GOTO END
:RTF
   ..\..\..\bin\hbdoc /RTF rddads.lnk rddads.rsp
   hcw harbour.hpj
GOTO END
:HTM
   ..\..\..\bin\hbdoc /HTM rddads.lnk rddads.rsp
   GOTO END
:END
del ass*.bat
