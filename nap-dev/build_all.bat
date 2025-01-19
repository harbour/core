::
:: Build all script
:: Harbour/GTNAP/HBAWS/HBOFFICE
:: Using different compilers (VS, MinGW-GCC, MinGW-Clang)
:: Important! This script will remove previous Harbour compilations
:: It can take several minutes to finish all tasks
::
cd ..

:: Remove previous compilations
rmdir /s /q bin\win
rmdir /s /q lib\win
win-make clean

:: Compile Harbour using Visual Studio
:harbour_vs
call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
call win-make -j4 HB_CPU=x86_64 HB_BUILD_CONTRIBS=no HB_COMPILER=msvc64 || goto error_harbour_vs
echo ----------------------------------
echo Harbour msvc64 build successfully
echo ----------------------------------
goto harbour_mingw_gcc

:: Compile Harbour Mingw-64
:harbour_mingw_gcc
call mingw32-make.exe -j4 HB_CPU=x86_64 HB_BUILD_CONTRIBS=no HB_COMPILER=mingw64 || goto error_harbour_gcc
echo -----------------------------------
echo Harbour mingw64 build successfully
echo -----------------------------------
goto harbour_mingw_clang

:harbour_mingw_clang
call mingw32-make.exe -j4 HB_CPU=x86_64 HB_BUILD_CONTRIBS=no HB_COMPILER=clang || goto error_harbour_clang
echo ---------------------------------
echo Harbour clang build successfully
echo ---------------------------------
goto gtnap_build

:hboffice_build

:hbaws_build

:gtnap_build

echo ---------------------------------------
echo All build jobs generated successfully
echo ---------------------------------------

cd nap-dev
goto end

::
:: Errors
::
:error_harbour_vs
echo Error building Harbour using VisualStudio (msvc64)
goto end

:error_harbour_gcc
echo Error building Harbour using MinGW-GCC (mingw64)
goto end

:error_harbour_clang
echo Error building Harbour using Clang (clang)
goto end

:end
