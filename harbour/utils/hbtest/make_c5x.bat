@echo off
rem
rem $Id$
rem

clipper hbtest.prg   /w /n
clipper rt_array.prg /w /n
clipper rt_date.prg  /w /n
clipper rt_file.prg  /w /n
clipper rt_hvm.prg   /w /n
clipper rt_math.prg  /w /n
clipper rt_misc.prg  /w /n
clipper rt_str.prg   /w /n
clipper rt_trans.prg /w /n

if "%1"=="" set hb_linker=rtlink 
if not "%1"=="" set hb_linker=exospace

%hb_linker% out hbtest5x fi hbtest, rt_array, rt_date, rt_file, rt_hvm, rt_math, rt_misc, rt_str, rt_trans

del *.obj
