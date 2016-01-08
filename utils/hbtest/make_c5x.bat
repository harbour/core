@echo off

:: Copyright 1999-2008 Viktor Szakats (vszakats.net/harbour)

:: NOTE: To compile for CA-Cl*pper 5.3,
::       run with command-line parameter "53" (without quotes).

:: for 5.3
if     "%1" == "53" set _MSCOPT=/FPi
if     "%1" == "53" set _CLPOPT=/D_COMPAT_C53
if     "%1" == "53" set _HBTEST=hbtest53
if     "%1" == "53" set _LINKER=exospace

:: for 5.2
if not "%1" == "53" set _MSCOPT=/FPa
if not "%1" == "53" set _CLPOPT=
if not "%1" == "53" set _HBTEST=hbtest52
if not "%1" == "53" set _LINKER=rtlink

::

cl.exe /c /AL /Zl /Oalt /Gs /W3 /G2 %_MSCOPT% rt_miscc.c

if     exist rt_miscc.obj set _LINKOPT=fi rt_miscc
if     exist rt_miscc.obj set _CLPOPT=
if not exist rt_miscc.obj set _LINKOPT=
if not exist rt_miscc.obj set _CLPOPT=%_CLPOPT% /DRT_NO_C

::

clipper hbtest   /w /n %_CLPOPT%
clipper rt_array /w /n %_CLPOPT%
clipper rt_date  /w /n %_CLPOPT%
clipper rt_file  /w /n %_CLPOPT%
clipper rt_hvm   /w /n %_CLPOPT%
clipper rt_hvma  /w /n %_CLPOPT%
clipper rt_math  /w /n %_CLPOPT%
clipper rt_misc  /w /n %_CLPOPT%
clipper rt_str   /w /n %_CLPOPT%
clipper rt_stra  /w /n %_CLPOPT%
clipper rt_tran2 /w /n %_CLPOPT%
clipper rt_trans /w /n %_CLPOPT%

%_LINKER% out %_HBTEST% fi hbtest,rt_array,rt_date,rt_file,rt_hvm,rt_hvma,rt_math,rt_misc,rt_str,rt_stra,rt_tran2,rt_trans %_LINKOPT%

del *.obj
