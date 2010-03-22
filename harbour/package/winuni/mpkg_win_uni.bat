@rem
@rem $Id$
@rem

@echo off

pushd

cd F:\hb\hb20\..

if exist %~dp0harbour-2.0.0-win-log.txt del %~dp0harbour-2.0.0-win-log.txt

set HB_ROOT=F:\hb\
"%HB_DIR_NSIS%makensis.exe" %~dp0mpkg_win_uni.nsi >> %~dp0harbour-2.0.0-win-log.txt

echo.> _hbfiles
echo "hb20\RELNOTES"                              >> _hbfiles
echo "hb20\INSTALL"                               >> _hbfiles
echo "hb20\COPYING"                               >> _hbfiles
echo "hb20\TODO"                                  >> _hbfiles
echo "hb20\ChangeLog*"                            >> _hbfiles
echo "hb20\bin\harbour-20.dll"                    >> _hbfiles
echo "hb20\bin\harbourmt-20.dll"                  >> _hbfiles
echo "hb20\bin\harbour.exe"                       >> _hbfiles
echo "hb20\bin\hbi18n.exe"                        >> _hbfiles
echo "hb20\bin\hbmk2.exe"                         >> _hbfiles
echo "hb20\bin\hbmk2.*.hbl"                       >> _hbfiles
echo "hb20\bin\hbpp.exe"                          >> _hbfiles
echo "hb20\bin\hbrun.exe"                         >> _hbfiles
echo "hb20\bin\hbtest.exe"                        >> _hbfiles
echo "hb20\bin\hbformat.exe"                      >> _hbfiles
echo "hb20\bin\hbmk.cfg"                          >> _hbfiles
echo "hb20\bin\upx*.*"                            >> _hbfiles
echo "hb20\include\*.*"                           >> _hbfiles
echo "hb20\bin\hbi18n-dll.exe"                    >> _hbfiles
echo "hb20\bin\hbmk2-dll.exe"                     >> _hbfiles
echo "hb20\bin\hbrun-dll.exe"                     >> _hbfiles
echo "hb20\bin\hbtest-dll.exe"                    >> _hbfiles
echo "hb20\bin\hbformat-dll.exe"                  >> _hbfiles
echo "hb20\bin\harbour-x64.exe"                   >> _hbfiles
echo "hb20\bin\hbi18n-x64.exe"                    >> _hbfiles
echo "hb20\bin\hbmk2-x64.exe"                     >> _hbfiles
echo "hb20\bin\hbpp-x64.exe"                      >> _hbfiles
echo "hb20\bin\hbrun-x64.exe"                     >> _hbfiles
echo "hb20\bin\hbtest-x64.exe"                    >> _hbfiles
echo "hb20\bin\hbformat-x64.exe"                  >> _hbfiles
echo "hb20\lib\win\mingw\*.*"                     >> _hbfiles
echo "hb20\lib\win\mingw64\*.*"                   >> _hbfiles
echo "hb20\lib\wce\mingwarm\*.*"                  >> _hbfiles
echo "hb20\addons\HARBOUR_README_ADDONS"          >> _hbfiles
rem echo "hb20\comp\djgpp\HARBOUR_README_DJGPP"       >> _hbfiles
echo "hb20\comp\watcom\HARBOUR_README_WATCOM"     >> _hbfiles
echo "hb20\comp\pocc\HARBOUR_README_POCC"         >> _hbfiles
echo "hb20\comp\mingw\HARBOUR_README_MINGW"       >> _hbfiles
echo "hb20\comp\mingw64\HARBOUR_README_MINGW64"   >> _hbfiles
echo "hb20\comp\mingwarm\HARBOUR_README_MINGWARM" >> _hbfiles
rem echo "hb20\lib\dos\djgpp\*.*"                     >> _hbfiles
echo "hb20\lib\dos\watcom\*.*"                    >> _hbfiles
echo "hb20\lib\linux\watcom\*.*"                  >> _hbfiles
echo "hb20\lib\os2\watcom\*.*"                    >> _hbfiles
echo "hb20\lib\win\msvc\*.*"                      >> _hbfiles
echo "hb20\lib\win\msvc64\*.*"                    >> _hbfiles
echo "hb20\bin\harbour-20-bcc.dll"                >> _hbfiles
echo "hb20\bin\harbourmt-20-bcc.dll"              >> _hbfiles
echo "hb20\lib\win\bcc\*.*"                       >> _hbfiles
echo "hb20\lib\win\watcom\*.*"                    >> _hbfiles
rem echo "hb20\lib\win\pocc\*.*"                      >> _hbfiles
rem echo "hb20\lib\win\pocc64\*.*"                    >> _hbfiles
rem echo "hb20\lib\wce\poccarm\*.*"                   >> _hbfiles
echo "hb20\bin\harbour-20-x64.dll"                >> _hbfiles
echo "hb20\bin\harbourmt-20-x64.dll"              >> _hbfiles
echo "hb20\bin\harbour-20-wce-arm.dll"            >> _hbfiles
echo "hb20\bin\harbourmt-20-wce-arm.dll"          >> _hbfiles
rem echo "hb20\bin\harbour-20-os2.dll"                >> _hbfiles
rem echo "hb20\bin\harbourmt-20-os2.dll"              >> _hbfiles
echo "hb20\tests\*.*"                             >> _hbfiles
echo "hb20\doc\*.*"                               >> _hbfiles
echo "hb20\comp\mingw\*"                          >> _hbfiles
echo "hb20\examples\*.*"                          >> _hbfiles
echo "hb20\contrib\*.*"                           >> _hbfiles

if exist %~dp0harbour-2.0.0-win.7z del %~dp0harbour-2.0.0-win.7z
7za a -r %~dp0harbour-2.0.0-win.7z @_hbfiles >> %~dp0harbour-2.0.0-win-log.txt

del _hbfiles

popd
