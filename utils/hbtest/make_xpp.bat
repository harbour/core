@echo off

set _CLPOPT=/DRT_NO_C

xpp hbtest   /w /n %_CLPOPT%
xpp rt_array /w /n %_CLPOPT%
xpp rt_date  /w /n %_CLPOPT%
xpp rt_file  /w /n %_CLPOPT%
xpp rt_hvm   /w /n %_CLPOPT%
xpp rt_hvma  /w /n %_CLPOPT%
xpp rt_math  /w /n %_CLPOPT%
xpp rt_misc  /w /n %_CLPOPT%
xpp rt_str   /w /n %_CLPOPT%
xpp rt_stra  /w /n %_CLPOPT%
xpp rt_tran2 /w /n %_CLPOPT%
xpp rt_trans /w /n %_CLPOPT%

alink hbtest rt_array rt_date rt_file rt_hvm rt_hvma rt_math rt_misc rt_str rt_stra rt_tran2 rt_trans

del *.obj
