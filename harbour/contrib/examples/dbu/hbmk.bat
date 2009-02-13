@rem
@rem $Id$
@rem

@echo off

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

..\..\..\bin\hbmk %* %HB_DIR_DBU%\DBU.PRG %HB_DIR_DBU%\DBUCOPY.PRG %HB_DIR_DBU%\DBUEDIT.PRG %HB_DIR_DBU%\DBUHELP.PRG %HB_DIR_DBU%\DBUINDX.PRG %HB_DIR_DBU%\DBUNET.PRG %HB_DIR_DBU%\DBUSTRU.PRG %HB_DIR_DBU%\DBUUTIL.PRG %HB_DIR_DBU%\DBUVIEW.PRG

:EXIT
