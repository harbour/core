@echo off
rem
rem $Id$
rem

set DBU_DIR=%CLIPPER_DIR%\SOURCE\DBU
if exist "%DBU_DIR%\DBU.PRG" goto BUILD
set DBU_DIR=.
if exist "%DBU_DIR%\DBU.PRG" goto BUILD

:HELP

echo ---------------------------------------------------------------
echo IMPORTANT: You'll either need to copy the original CA-Cl*pper 
echo            DBU sources to this directory, or set the location of 
echo            your original CA-Cl*pper installation (with DBU sources 
echo            in SOURCE\DBU):
echo            set CLIPPER_DIR=C:\CLIPPER5
echo ---------------------------------------------------------------

goto EXIT

:BUILD

..\..\..\bin\harbour /n %DBU_DIR%\DBU.PRG     /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUCOPY.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUEDIT.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUHELP.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUINDX.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUNET.PRG  /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUSTRU.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUUTIL.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %DBU_DIR%\DBUVIEW.PRG /i..\..\..\include\

echo -O2 -I..\..\..\include -L..\..\..\lib > build.tmp
echo -edbu.exe >> build.tmp
echo dbu.c     >> build.tmp
echo dbucopy.c >> build.tmp
echo dbuedit.c >> build.tmp
echo dbuhelp.c >> build.tmp
echo dbuindx.c >> build.tmp
echo dbunet.c  >> build.tmp
echo dbustru.c >> build.tmp
echo dbuutil.c >> build.tmp
echo dbuview.c >> build.tmp
echo debug.lib >> build.tmp
echo vm.lib >> build.tmp
echo rtl.lib >> build.tmp
echo gtwin.lib >> build.tmp
echo lang.lib >> build.tmp
echo rdd.lib >> build.tmp
echo macro.lib >> build.tmp
echo pp.lib >> build.tmp
echo dbfntx.lib >> build.tmp
echo dbfcdx.lib >> build.tmp
echo dbffpt.lib >> build.tmp
echo hbsix.lib >> build.tmp
echo common.lib >> build.tmp
bcc32 @build.tmp
del build.tmp

dbu

:EXIT
