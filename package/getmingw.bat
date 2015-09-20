@echo off

:: Copyright 2015 Viktor Szakats (vszakats.net/harbour)

setlocal
pushd "%~dp0"

set "_MINGW_URL_32=https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win32/Personal Builds/mingw-builds/5.2.0/threads-posix/sjlj/i686-5.2.0-release-posix-sjlj-rt_v4-rev0.7z"
set  _MINGW_SUM_32=76faee6e1644e3ba49a38b803413c75e5c9e0bf1716dcf862824439ad1a32773
set "_MINGW_URL_64=https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win64/Personal Builds/mingw-builds/5.2.0/threads-posix/sjlj/x86_64-5.2.0-release-posix-sjlj-rt_v4-rev0.7z"
set  _MINGW_SUM_64=c0536c55a1d12882987afd0a9be377413eaf6cee105e921c949899fa9b308b35

if not exist "harbour.exe" (
   echo Error: This script has to be run from a Harbour binary installation.
   exit /b
)

harbour.exe -build 2>&1 | findstr "64-bit" > nul 2>&1
if %ERRORLEVEL% equ 0 (
   set _MINGW_URL=%_MINGW_URL_64%
   set _MINGW_SUM=%_MINGW_SUM_64%
) else (
   set _MINGW_URL=%_MINGW_URL_32%
   set _MINGW_SUM=%_MINGW_SUM_32%
)

if "%_MINGW_URL%" == "%_MINGW_URL_32%" echo Downloading 32-bit hosted dual-target MinGW...
if "%_MINGW_URL%" == "%_MINGW_URL_64%" echo Downloading 64-bit hosted dual-target MinGW...

set _DL_URL=%_MINGW_URL%
set _DL_DST=%TEMP%\mingw.7z
set _TMP=%TEMP%\_webdl.tmp
echo var http = new ActiveXObject(^"WinHttp.WinHttpRequest.5.1^");> "%_TMP%"
echo http.Open(^"GET^", ^"%_DL_URL%^", false);>> "%_TMP%"
echo http.Send();>> "%_TMP%"
echo if(http.Status() == 200) {>> "%_TMP%"
echo    var f = new ActiveXObject(^"ADODB.Stream^");>> "%_TMP%"
echo    f.type = 1; f.open(); f.write(http.responseBody);>> "%_TMP%"
echo    f.savetofile(^"%_DL_DST:\=\\%^", 2);>> "%_TMP%"
echo }>> "%_TMP%"
cscript //nologo /e:jscript "%_TMP%"
del "%_TMP%"

pushd ..
set _TRG=%CD%\comp\
popd

:: Requires Windows 7 or OpenSSL in PATH

:_WIN7
   certutil > nul 2>&1
   if %ERRORLEVEL% neq 0 goto _OPENSSL
   setlocal EnableDelayedExpansion
   for /f "skip=1 tokens=*" %%I in ('certutil -hashfile "%TEMP%\mingw.7z" SHA256') do if "!_HASH!" == "" set "_HASH=%%I"
   echo %_HASH: =% | findstr /i /c:"%_MINGW_SUM%"
   if %ERRORLEVEL% equ 1 goto _HASH_ERR
   goto _HASH_OK
:_OPENSSL
   openssl version > nul 2>&1
   if %ERRORLEVEL% neq 0 goto _MANUAL
   openssl dgst -sha256 "%TEMP%\mingw.7z" | findstr /i /c:"%_MINGW_SUM%"
   if %ERRORLEVEL% equ 1 goto _HASH_ERR
:_HASH_OK
   echo Checksum OK.
   goto _DONE
:_HASH_ERR
   echo Error: Checksum mismatch - corrupted download. Please retry.
   exit /b 1
:_MANUAL
   echo Expected SHA256 hash: %_MINGW_SUM%
:_DONE

echo Unpacking to '%_TRG%'...
if exist "%TEMP%\mingw.7z" (
   7za x -y -o..\comp "%TEMP%\mingw.7z" > nul
   del "%TEMP%\mingw.7z"
)

popd
endlocal
