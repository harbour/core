@echo off
rem
rem $Id$
rem

clipper rt_main.prg  /w /n
clipper rt_array.prg /w /n
clipper rt_date.prg  /w /n
clipper rt_file.prg  /w /n
clipper rt_hvm.prg   /w /n
clipper rt_math.prg  /w /n
clipper rt_misc.prg  /w /n
clipper rt_str.prg   /w /n
clipper rt_trans.prg /w /n

if "%1"=="" set _=rtlink 
if not "%1"=="" set _=exospace

%_% out rt_main fi rt_main, rt_array, rt_date, rt_file, rt_hvm, rt_math, rt_misc, rt_str, rt_trans

del *.obj
