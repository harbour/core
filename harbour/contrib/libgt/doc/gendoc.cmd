@echo off
rem 
rem $Id$
rem 

if "%1" =="/OS2" goto OS2
if "%1" =="/os2" goto OS2
if "%1" =="/NGI" goto NG
if "%1" =="/ngi" goto NG
if "%1" =="/RTF" goto RTF
if "%1" =="/rtf" goto RTF
if "%1" =="/HTM" goto HTM
if "%1" =="/htm" goto HTM
ECHO Assembling input files
:help
    echo.
    echo. Usage gendoc type
    echo. where type is:
    echo. /rtf for Winhelp output
    echo. /os2 for Os/2 help output
    echo. /ngi for Norton Guide output
    echo. /htm for HTML output
    goto END

:NG
hbdoc /ngi libgt.lnk libgt.rsp
REM Compile the sources
Echo Compiling the sources
Echo Processing Input Files
Copy ngi\funcam.txt+ngi\funcn_.txt overview.ngi
Echo Compiling Sources
ngxc overview.ngi
Echo Linking the Guide
ngxl libgt.lnk
del *.ngi
del *.ngo
del ngi\*.txt
del ngi\*.ngi
GOTO END
:OS2
   hbdoc /OS2 libgt.lnk libgt.rsp
GOTO END
:RTF
   hbdoc /RTF libgt.lnk libgt.rsp
   HCW HARBOUR.HPJ
GOTO END
:HTM
    hbdoc /HTM libgt.lnk libgt.rsp
   GOTO END
:END
del ass*.bat

del libgt.lnk
ren libgt.old libgt.lnk
