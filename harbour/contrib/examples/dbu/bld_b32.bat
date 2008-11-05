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

bcc32 -O2 -I..\..\..\include -L..\..\..\lib dbu.c dbucopy.c dbuedit.c dbuhelp.c dbuindx.c dbunet.c dbustru.c dbuutil.c dbuview.c hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib

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
