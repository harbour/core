@echo off
rem
rem $Id$
rem

set HB_DIR_DBU=%CLIPPER_DIR%\SOURCE\DBU
if exist "%HB_DIR_DBU%\DBU.PRG" goto DIR_OK
set HB_DIR_DBU=.
if exist "%HB_DIR_DBU%\DBU.PRG" goto DIR_OK_LOCAL

echo ---------------------------------------------------------------
echo IMPORTANT: You'll either need to copy the original CA-Cl*pper 
echo            DBU sources to this directory, or set the location of 
echo            your original CA-Cl*pper installation (with DBU sources 
echo            in SOURCE\DBU):
echo            set CLIPPER_DIR=C:\CLIPPER5
echo ---------------------------------------------------------------
goto EXIT

:DIR_OK_LOCAL

if exist hb_dbu.dif patch -N -i hb_dbu.dif

:DIR_OK

..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBU.PRG    
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUCOPY.PRG
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUEDIT.PRG
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUHELP.PRG
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUINDX.PRG
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUNET.PRG 
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUSTRU.PRG
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUUTIL.PRG
..\..\..\bin\harbour -q -n -l -i..\..\..\include %HB_DIR_DBU%\DBUVIEW.PRG

echo -O2 -I..\..\..\include -L..\..\..\lib > build.tmp

echo dbu.c     >> build.tmp
echo dbucopy.c >> build.tmp
echo dbuedit.c >> build.tmp
echo dbuhelp.c >> build.tmp
echo dbuindx.c >> build.tmp
echo dbunet.c  >> build.tmp
echo dbustru.c >> build.tmp
echo dbuutil.c >> build.tmp
echo dbuview.c >> build.tmp

echo hbdebug.lib >> build.tmp
echo hbvm.lib >> build.tmp
echo hbrtl.lib >> build.tmp
echo gtwin.lib >> build.tmp
echo hblang.lib >> build.tmp
echo hbrdd.lib >> build.tmp
echo hbmacro.lib >> build.tmp
echo hbpp.lib >> build.tmp
echo rddntx.lib >> build.tmp
echo rddcdx.lib >> build.tmp
echo rddfpt.lib >> build.tmp
echo hbsix.lib >> build.tmp
echo hbcommon.lib >> build.tmp

bcc32 @build.tmp
del build.tmp

del *.obj

del dbu.c
del dbucopy.c
del dbuedit.c
del dbuhelp.c
del dbuindx.c
del dbunet.c
del dbustru.c
del dbuutil.c
del dbuview.c

dbu

:EXIT
