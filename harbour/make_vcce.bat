@echo off
rem
rem $Id$
rem

rem ; This batch file will be removed shortly, please use 
rem   following method in your build environment to create 
rem   an MSVC WinCE build.

set HB_BUILD_WINCE=yes
set HB_CC_NAME=vcce

call make_vc.bat %1 %2 %3
