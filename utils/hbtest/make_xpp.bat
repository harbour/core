@echo off

rem ; TOFIX: Add compilation for C sources
set HB_CLIPOPT=%HB_CLIPOPT% /DRT_NO_C

xpp hbtest.prg   /w /n %HB_CLIPOPT%
xpp rt_array.prg /w /n %HB_CLIPOPT%
xpp rt_date.prg  /w /n %HB_CLIPOPT%
xpp rt_file.prg  /w /n %HB_CLIPOPT%
xpp rt_hvm.prg   /w /n %HB_CLIPOPT%
xpp rt_hvma.prg  /w /n %HB_CLIPOPT%
xpp rt_math.prg  /w /n %HB_CLIPOPT%
xpp rt_misc.prg  /w /n %HB_CLIPOPT%
xpp rt_str.prg   /w /n %HB_CLIPOPT%
xpp rt_stra.prg  /w /n %HB_CLIPOPT%
xpp rt_trans.prg /w /n %HB_CLIPOPT%

alink hbtest rt_array rt_date rt_file rt_hvm rt_hvma rt_math rt_misc rt_str rt_stra rt_trans

del *.obj
