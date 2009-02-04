@echo off
rem
rem $Id$
rem

rem ---------------------------------------------------------------
rem Copyright 2003 Przemyslaw Czerpak (druzus / at / priv.onet.pl)
rem simple script run after Harbour make install to finish install
rem process
rem
rem See doc/license.txt for licensing terms.
rem ---------------------------------------------------------------

goto inst_%HB_ARCHITECTURE%

:inst_dos
rem DOS post install part

goto end


:inst_win
rem Windows post install part

goto end


:inst_
:end
