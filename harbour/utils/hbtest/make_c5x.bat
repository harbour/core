@rem
@rem $Id$
@rem

@echo off

rem ---------------------------------------------------------------
rem ; NOTE: To compile for CA-Cl*pper 5.3, please use the
rem         command line parameter "53" (without quotes).
rem ---------------------------------------------------------------

rem ---------------------------------------------------------------
rem Copyright 1999-2008 Viktor Szakats (harbour syenar.hu)
rem See COPYING for licensing terms.
rem ---------------------------------------------------------------

rem ; Settings for CA-Cl*pper 5.3
if     "%1" == "53" set HB_MSCOPT=%HB_MSCOPT% /FPi
if     "%1" == "53" set HB_CLIPOPT=%HB_CLIPOPT% /DHB_COMPAT_C53
if     "%1" == "53" set HB_HBTEST=hbtest53
if     "%1" == "53" set HB_LINKER=exospace

rem ; Settings for CA-Cl*pper 5.2
if not "%1" == "53" set HB_MSCOPT=%HB_MSCOPT% /FPa
if not "%1" == "53" set HB_HBTEST=hbtest52
if not "%1" == "53" set HB_LINKER=rtlink

rem ---------------------------------------------------------------

cl.exe /c /AL /Zl /Oalt /Gs /W3 /G2 %HB_MSCOPT% rt_miscc.c

if     exist rt_miscc.obj set HB_LINKOPT=%HB_LINKOPT% fi rt_miscc
if not exist rt_miscc.obj set HB_CLIPOPT=%HB_CLIPOPT% /DRT_NO_C

rem ---------------------------------------------------------------

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

%HB_LINKER% out %HB_HBTEST% fi hbtest,rt_array,rt_date,rt_file,rt_hvm,rt_hvma,rt_math,rt_misc,rt_str,rt_stra,rt_trans %HB_LINKOPT%

rem ---------------------------------------------------------------

del *.obj

set HB_MSCOPT=
set HB_CLIPOPT=
set HB_LINKOPT=
set HB_LINKER=
set HB_HBTEST=
