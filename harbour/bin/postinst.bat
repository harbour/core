@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem Copyright 2009 Viktor Szakats (viktor.szakats@syenar.hu)
rem Copyright 2003 Przemyslaw Czerpak (druzus / at / priv.onet.pl)
rem simple script run after Harbour make install to finish install
rem process
rem
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

goto inst_%HB_ARCHITECTURE%

:inst_win
rem Windows post install part

if "%OS%" == "Windows_NT" if "%HB_DLL%" == "yes" call %~dp0hb-mkdyn.bat

echo @"%%~dp0hbmk2.exe" -hbcc  %%*> %HB_BIN_INSTALL%\hbcc.bat
echo @"%%~dp0hbmk2.exe" -hbcmp %%*> %HB_BIN_INSTALL%\hbcmp.bat
echo @"%%~dp0hbmk2.exe" -hblnk %%*> %HB_BIN_INSTALL%\hblnk.bat

goto end


:inst_dos
rem DOS post install part

echo @hbmk2.exe -hbcc  %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9> %HB_BIN_INSTALL%\hbcc.bat
echo @hbmk2.exe -hbcmp %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9> %HB_BIN_INSTALL%\hbcmp.bat
echo @hbmk2.exe -hblnk %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9> %HB_BIN_INSTALL%\hblnk.bat

goto end


:inst_
:end
