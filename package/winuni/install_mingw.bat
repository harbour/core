@echo off

setlocal

cd /d "%~dp0"

harbour -build 2>&1 | find "32-bit" > nul 2>&1 && set "_URL_CLIENT=https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win32/Personal Builds/mingw-builds/4.9.2/threads-win32/sjlj/i686-4.9.2-release-win32-sjlj-rt_v4-rev2.7z"
harbour -build 2>&1 | find "64-bit" > nul 2>&1 && set "_URL_CLIENT=https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win64/Personal Builds/mingw-builds/4.9.2/threads-win32/sjlj/x86_64-4.9.2-release-win32-sjlj-rt_v4-rev2.7z"

set _DL_URL=%_URL_CLIENT%
set _DL_DST=mingw.7z

:: Download the client
set _TMP=%TEMP%\_webdl.js
echo var http = new ActiveXObject(^"WinHttp.WinHttpRequest.5.1^");> "%_TMP%"
echo http.Open(^"GET^", ^"%_DL_URL%^", false);>> "%_TMP%"
echo http.Send();>> "%_TMP%"
echo if(http.Status() == 200) {>> "%_TMP%"
echo    var f = new ActiveXObject(^"ADODB.Stream^");>> "%_TMP%"
echo    f.type = 1; f.open(); f.write(http.responseBody);>> "%_TMP%"
echo    f.savetofile(^"%_DL_DST:\=\\%^", 2);>> "%_TMP%"
echo }>> "%_TMP%"
cscript "%_TMP%" //Nologo
del "%_TMP%"

7za x -o..\comp mingw.7z > nul

endlocal
