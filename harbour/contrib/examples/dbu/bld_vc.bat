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

..\..\..\bin\harbour /n %HB_DIR_DBU%\DBU.PRG     /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUCOPY.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUEDIT.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUHELP.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUINDX.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUNET.PRG  /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUSTRU.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUUTIL.PRG /i..\..\..\include\
..\..\..\bin\harbour /n %HB_DIR_DBU%\DBUVIEW.PRG /i..\..\..\include\

cl -D_CRT_SECURE_NO_DEPRECATE -nologo -O2 -W3 -I..\..\..\include dbu.c dbucopy.c dbuedit.c dbuhelp.c dbuindx.c dbunet.c dbustru.c dbuutil.c dbuview.c /link /subsystem:CONSOLE /LIBPATH:..\..\..\lib hbcpage.lib hbdebug.lib hbvm.lib hbrtl.lib gtwin.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbpcre.lib hbzlib.lib user32.lib gdi32.lib

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
