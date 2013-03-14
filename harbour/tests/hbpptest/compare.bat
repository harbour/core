@echo off
rem
rem $Id$
rem

copy pp_test.prg _pp_hb.prg
copy pp_test.prg _pp_c5x.prg

..\..\bin\harbour -p -s -n _pp_hb.prg
clipper.exe _pp_c5x.prg /p /s /n

del _pp_hb.prg
del _pp_c5x.prg

diff -u -w _pp_hb.ppo _pp_c5x.ppo > pp_test.dif

del _pp_hb.ppo
del _pp_c5x.ppo
