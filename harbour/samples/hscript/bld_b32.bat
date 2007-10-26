@echo off
rem
rem $Id$
rem

rem NOTE: This sample program need nf.lib from contrib/libnf

..\..\bin\harbour hscript /n /i..\..\include
bcc32 -O2 -I..\..\include -L..\..\lib -ehscript.exe hscript.c debug.lib vm.lib rtl.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib dbffpt.lib codepage.lib hbsix.lib common.lib hbpcre.lib hsx.lib nf.lib
del hscript.c
