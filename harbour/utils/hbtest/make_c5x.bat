rem @echo off
rem
rem $Id$
rem

rem ; TODO: Use /FPi when HB_COMPAT_C53=1
%HB_MSC_DIR%\cl.exe /c /AL /Zl /Oalt /Gs /FPa /W3 /G2 rt_miscc.c

if not exist rt_miscc.obj set HB_CLIPOPT=%HB_CLIPOPT% /DRT_NO_C
if     exist rt_miscc.obj set HB_LINKOPT=%HB_LINKOPT% fi rt_miscc

:NO_C

if not "%HB_COMPAT_C53%" == "" set HB_CLIPOPT=%HB_CLIPOPT% /DHB_COMPAT_C53

clipper hbtest.prg   /w /n %HB_CLIPOPT%
clipper rt_array.prg /w /n %HB_CLIPOPT%
clipper rt_date.prg  /w /n %HB_CLIPOPT%
clipper rt_file.prg  /w /n %HB_CLIPOPT%
clipper rt_hvm.prg   /w /n %HB_CLIPOPT%
clipper rt_hvma.prg  /w /n %HB_CLIPOPT%
clipper rt_math.prg  /w /n %HB_CLIPOPT%
clipper rt_misc.prg  /w /n %HB_CLIPOPT%
clipper rt_str.prg   /w /n %HB_CLIPOPT%
clipper rt_stra.prg  /w /n %HB_CLIPOPT%
clipper rt_trans.prg /w /n %HB_CLIPOPT%

if     "%1" == "" set HB_LINKER=rtlink
if not "%1" == "" set HB_LINKER=exospace

%HB_LINKER% out hbtest5x fi hbtest,rt_array,rt_date,rt_file,rt_hvm,rt_hvma,rt_math,rt_misc,rt_str,rt_stra,rt_trans %HB_LINKOPT%

set HB_CLIPOPT=
set HB_LINKOPT=

del *.obj
