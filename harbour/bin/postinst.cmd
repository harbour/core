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

:inst_os2
rem OS/2 post install part

echo @hbmk2.exe -hbcc  %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9> %HB_BIN_INSTALL%\hbcc.cmd
echo @hbmk2.exe -hbcmp %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9> %HB_BIN_INSTALL%\hbcmp.cmd
echo @hbmk2.exe -hblnk %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9> %HB_BIN_INSTALL%\hblnk.cmd

goto end


:inst_
:end
