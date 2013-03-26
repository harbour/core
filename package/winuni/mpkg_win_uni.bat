@echo off

rem ---------------------------------------------------------------
rem Copyright 2009-2013 Viktor Szakats (harbour syenar.net)
rem See COPYING.txt for licensing terms.
rem ---------------------------------------------------------------

rem - Adjust target dir, mingw dirs, set HB_DIR_UPX, HB_DIR_7Z, HB_DIR_MINGW,
rem   create required packages beforehand.
rem - Requires BCC in PATH or HB_DIR_BCC_IMPLIB (for implib).
rem - Run this from vanilla official source tree only.
rem - Requires GNU sed tool in PATH

echo ! Self: %0

if "%HB_VS%" == "" set HB_VS=32
if "%HB_VL%" == "" set HB_VL=320
if "%HB_VM%" == "" set HB_VM=3.2
if "%HB_VF%" == "" set HB_VF=3.2.0dev
if "%HB_RT%" == "" set HB_RT=C:\hb\

set HB_DR=hb%HB_VS%\
set HB_ABSROOT=%HB_RT%%HB_DR%

rem ; Assemble unified package from per-target builds

if exist %HB_ABSROOT% rd /q /s %HB_ABSROOT%

xcopy /y       %~dp0RELNOTES.txt                                                          %HB_ABSROOT%
xcopy /y /s    %~dp0..\..\extras\*.*                                                      %HB_ABSROOT%extras\
xcopy /y /s    %~dp0..\..\tests\*.*                                                       %HB_ABSROOT%tests\
xcopy /y       %~dp0HARBOUR_README_ADDONS.txt                                             %HB_ABSROOT%addons\
xcopy /y       %~dp0HARBOUR_README_DJGPP.txt                                              %HB_ABSROOT%comp\djgpp\
xcopy /y       %~dp0HARBOUR_README_MINGW.txt                                              %HB_ABSROOT%comp\mingw\
xcopy /y       %~dp0HARBOUR_README_MINGW64.txt                                            %HB_ABSROOT%comp\mingw64\
xcopy /y       %~dp0HARBOUR_README_MINGWARM.txt                                           %HB_ABSROOT%comp\mingwarm\
xcopy /y       %~dp0HARBOUR_README_POCC.txt                                               %HB_ABSROOT%comp\pocc\
xcopy /y       %~dp0HARBOUR_README_WATCOM.txt                                             %HB_ABSROOT%comp\watcom\

xcopy /y /s    %~dp0..\..\pkg\win\mingw\harbour-%HB_VF%-win-mingw                         %HB_ABSROOT%

xcopy /y /s    %~dp0..\..\pkg\linux\watcom\harbour-%HB_VF%-linux-watcom\lib               %HB_ABSROOT%lib\linux\watcom\
xcopy /y /s    %~dp0..\..\pkg\dos\watcom\hb%HB_VL%wa\lib                                  %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\os2\watcom\harbour-%HB_VF%-os2-watcom\lib                   %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\wce\mingwarm\harbour-%HB_VF%-wce-mingwarm\lib               %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\win\bcc\harbour-%HB_VF%-win-bcc\lib                         %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\win\bcc64\harbour-%HB_VF%-win-bcc64\lib                     %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\win\mingw\harbour-%HB_VF%-win-mingw\lib                     %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\lib                 %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\win\msvc\harbour-%HB_VF%-win-msvc\lib                       %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\win\msvc64\harbour-%HB_VF%-win-msvc64\lib                   %HB_ABSROOT%lib\
xcopy /y /s    %~dp0..\..\pkg\win\watcom\harbour-%HB_VF%-win-watcom\lib                   %HB_ABSROOT%lib\

xcopy /y       %~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\*.dll           %HB_ABSROOT%bin\
xcopy /y       %~dp0..\..\pkg\wce\mingwarm\harbour-%HB_VF%-wce-mingwarm\bin\*.dll         %HB_ABSROOT%bin\

rem ; Create special implibs for Borland (requires BCC in PATH)
rem   NOTE: Using intermediate .def files, because direct .dll to .lib conversion
rem         is buggy in BCC55 and BCC58 (no other versions tested), leaving off
rem         leading underscore from certain ("random") symbols, resulting in
rem         unresolved externals, when trying to use it. [vszakats]
for %%a in ( %HB_ABSROOT%bin\*-%HB_VS%.dll ) do (
   "%HB_DIR_BCC_IMPLIB%impdef.exe" -a "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw" "%%a"
   echo s/LIBRARY     %%~na.DLL/LIBRARY     "%%~na.dll"/Ig> _hbtemp.sed
   sed -f _hbtemp.sed < "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw" > "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
   "%HB_DIR_BCC_IMPLIB%implib.exe" -c -a "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.lib" "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
   del "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.defraw"
   del "%HB_ABSROOT%lib\win\bcc\%%~na-bcc.def"
)
del _hbtemp.sed

 copy /y       %~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbmk2.exe       %HB_ABSROOT%bin\hbmk2-x64.exe
 copy /y       %~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbnetio.exe     %HB_ABSROOT%bin\hbnetio-x64.exe
 copy /y       %~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbrun.exe       %HB_ABSROOT%bin\hbrun-x64.exe
 copy /y       %~dp0..\..\pkg\win\mingw64\harbour-%HB_VF%-win-mingw64\bin\hbtest.exe      %HB_ABSROOT%bin\hbtest-x64.exe

xcopy /y       "%HB_DIR_UPX%upx.exe"                                                      %HB_ABSROOT%bin\
 copy /y       "%HB_DIR_UPX%LICENSE"                                                      %HB_ABSROOT%bin\upx_LICENSE.txt

xcopy /y /s /e "%HB_DIR_MINGW%"                                                           %HB_ABSROOT%comp\mingw\
rem del %HB_ABSROOT%comp\mingw\tdm-mingw-1.908.0-4.4.1-2.exe

xcopy /y       "%HB_DIR_MINGW%\bin\libgcc_s_seh-1.dll"                                    %HB_ABSROOT%bin\
xcopy /y       "%HB_DIR_MINGW%\bin\libgcc_s_sjlj-1.dll"                                   %HB_ABSROOT%bin\
xcopy /y       "%HB_DIR_MINGW%\bin\libgcc_s_dw2-1.dll"                                    %HB_ABSROOT%bin\
xcopy /y       "%HB_DIR_MINGW%\bin\mingwm10.dll"                                          %HB_ABSROOT%bin\

rem ; Create unified installer

pushd

cd %HB_RT%

if exist %HB_RT%harbour-%HB_VF%-win-log.txt del %HB_RT%harbour-%HB_VF%-win-log.txt
if exist %HB_RT%harbour-%HB_VF%-win.exe del %HB_RT%harbour-%HB_VF%-win.exe

"%HB_DIR_NSIS%makensis.exe" %HB_OPT_NSIS% %~dp0mpkg_win_uni.nsi >> %HB_RT%harbour-%HB_VF%-win-log.txt 2>&1

rem ; Create unified archive

echo.> _hbfiles
echo "%HB_DR%RELNOTES.txt"                          >> _hbfiles
echo "%HB_DR%README.md"                             >> _hbfiles
echo "%HB_DR%COPYING.txt"                           >> _hbfiles
echo "%HB_DR%ChangeLog*.txt"                        >> _hbfiles
echo "%HB_DR%bin\*-%HB_VS%.dll"                     >> _hbfiles
echo "%HB_DR%bin\harbour.exe"                       >> _hbfiles
echo "%HB_DR%bin\hbformat.exe"                      >> _hbfiles
echo "%HB_DR%bin\hbi18n.exe"                        >> _hbfiles
echo "%HB_DR%bin\hbmk2.exe"                         >> _hbfiles
echo "%HB_DR%bin\hbmk2.*.hbl"                       >> _hbfiles
echo "%HB_DR%bin\hbnetio.exe"                       >> _hbfiles
echo "%HB_DR%bin\hbpp.exe"                          >> _hbfiles
echo "%HB_DR%bin\hbrun.exe"                         >> _hbfiles
echo "%HB_DR%bin\hbtest.exe"                        >> _hbfiles
if exist "%HB_DR%bin\*.hb"                          echo "%HB_DR%bin\*.hb"                          >> _hbfiles
if exist "%HB_DR%bin\*.hbr"                         echo "%HB_DR%bin\*.hbr"                         >> _hbfiles
if exist "%HB_DR%bin\*.ucf"                         echo "%HB_DR%bin\*.ucf"                         >> _hbfiles
if exist "%HB_DR%bin\hbmk.hbc"                      echo "%HB_DR%bin\hbmk.hbc"                      >> _hbfiles
echo "%HB_DR%bin\upx*.*"                            >> _hbfiles
echo "%HB_DR%include\*.*"                           >> _hbfiles
echo "%HB_DR%bin\hbmk2-x64.exe"                     >> _hbfiles
echo "%HB_DR%bin\hbnetio-x64.exe"                   >> _hbfiles
echo "%HB_DR%bin\hbrun-x64.exe"                     >> _hbfiles
echo "%HB_DR%bin\hbtest-x64.exe"                    >> _hbfiles
echo "%HB_DR%lib\win\mingw\*.*"                     >> _hbfiles
echo "%HB_DR%lib\win\mingw64\*.*"                   >> _hbfiles
echo "%HB_DR%lib\wce\mingwarm\*.*"                  >> _hbfiles
echo "%HB_DR%addons\HARBOUR_README_ADDONS.txt"          >> _hbfiles
rem echo "%HB_DR%comp\djgpp\HARBOUR_README_DJGPP.txt"       >> _hbfiles
echo "%HB_DR%comp\watcom\HARBOUR_README_WATCOM.txt"     >> _hbfiles
echo "%HB_DR%comp\pocc\HARBOUR_README_POCC.txt"         >> _hbfiles
echo "%HB_DR%comp\mingw\HARBOUR_README_MINGW.txt"       >> _hbfiles
echo "%HB_DR%comp\mingw64\HARBOUR_README_MINGW64.txt"   >> _hbfiles
echo "%HB_DR%comp\mingwarm\HARBOUR_README_MINGWARM.txt" >> _hbfiles
rem echo "%HB_DR%lib\dos\djgpp\*.*"                     >> _hbfiles
echo "%HB_DR%lib\dos\watcom\*.*"                    >> _hbfiles
echo "%HB_DR%lib\linux\watcom\*.*"                  >> _hbfiles
echo "%HB_DR%lib\os2\watcom\*.*"                    >> _hbfiles
echo "%HB_DR%lib\win\msvc\*.*"                      >> _hbfiles
echo "%HB_DR%lib\win\msvc64\*.*"                    >> _hbfiles
rem echo "%HB_DR%bin\harbour-%HB_VS%-bcc.dll"           >> _hbfiles
echo "%HB_DR%lib\win\bcc\*.*"                       >> _hbfiles
echo "%HB_DR%lib\win\bcc64\*.*"                     >> _hbfiles
echo "%HB_DR%lib\win\watcom\*.*"                    >> _hbfiles
rem echo "%HB_DR%lib\win\pocc\*.*"                      >> _hbfiles
rem echo "%HB_DR%lib\win\pocc64\*.*"                    >> _hbfiles
rem echo "%HB_DR%lib\wce\poccarm\*.*"                   >> _hbfiles
echo "%HB_DR%bin\*-%HB_VS%-x64.dll"                 >> _hbfiles
echo "%HB_DR%bin\harbour-%HB_VS%-wce-arm.dll"       >> _hbfiles
rem echo "%HB_DR%bin\harbour-%HB_VS%-os2.dll"           >> _hbfiles
echo "%HB_DR%tests\*.*"                             >> _hbfiles
echo "%HB_DR%doc\*.*"                               >> _hbfiles
echo "%HB_DR%comp\mingw\*"                          >> _hbfiles
echo "%HB_DR%extras\*.*"                            >> _hbfiles
echo "%HB_DR%contrib\*.*"                           >> _hbfiles

if exist %HB_RT%harbour-%HB_VF%-win.7z del %HB_RT%harbour-%HB_VF%-win.7z
"%HB_DIR_7Z%7za.exe" a -r %HB_RT%harbour-%HB_VF%-win.7z @_hbfiles >> %HB_RT%harbour-%HB_VF%-win-log.txt 2>&1

del _hbfiles

popd
