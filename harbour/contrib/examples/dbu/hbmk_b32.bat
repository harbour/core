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

..\..\..\bin\hbmk_b32 %HB_DIR_DBU%\dbu.prg %HB_DIR_DBU%\dbucopy.prg %HB_DIR_DBU%\dbuedit.prg %HB_DIR_DBU%\dbuhelp.prg %HB_DIR_DBU%\dbuindx.prg %HB_DIR_DBU%\dbunet.prg %HB_DIR_DBU%\dbustru.prg %HB_DIR_DBU%\dbuutil.prg %HB_DIR_DBU%\dbuview.prg

dbu

:EXIT
