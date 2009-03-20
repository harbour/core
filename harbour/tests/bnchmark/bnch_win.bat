@rem
@rem $Id$
@rem

@rem ---------------------------------------------------------------
@rem Benchmark for Harbour Project
@rem
@rem Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
@rem See doc/license.txt for licensing terms.
@rem
@rem This script requires:
@rem    - Windows XP or upper
@rem    - Harbour binary build
@rem    - Harbour binary build \bin dir as current dir
@rem    - This batch copied into current dir
@rem    - speedtst.prg copied into current dir
@rem    - envvars HB_CMP_FLAGS_[1-8] set to speedtst build flags
@rem    - envvars HB_RUN_FLAGS_[1-8] set to speedtst run flags (optional)
@rem    - envvar HB_RUN_FLAGS set to common speedtst run flags (optional)
@rem    - envvar HB_BENCH_RUNS set to number of times tests are run (default: 3)
@rem    - C compiler configured (just like you'd do for hbmk2)
@rem    - Running it using this command:
@rem      'hbbm_win.bat > results.txt 2>&1'
@rem ---------------------------------------------------------------

@if not "%OS%" == "Windows_NT" goto END

@echo off

echo.>_hbhwinfo.js
echo.var locator = WScript.CreateObject( "WbemScripting.SWbemLocator" );>>_hbhwinfo.js
echo.var services = locator.ConnectServer();>>_hbhwinfo.js
echo.var items = new Enumerator( services.ExecQuery( "SELECT * FROM Win32_Processor" ) );>>_hbhwinfo.js
echo.while( ! items.atEnd() )>>_hbhwinfo.js
echo.{>>_hbhwinfo.js
echo.   var item = items.item();>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "Name: " + item.Name );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "Description: " + item.Description );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "Manufacturer: " + item.Manufacturer );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "NumberOfCores: " + item.NumberOfCores );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "NumberOfLogicalProcessors: " + item.NumberOfLogicalProcessors );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "AddressWidth: " + item.AddressWidth + " bits" );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "DataWidth: " + item.DataWidth + " bits" );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "CurrentClockSpeed: " + item.CurrentClockSpeed + " MHz" );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "MaxClockSpeed: " + item.MaxClockSpeed + " MHz" );>>_hbhwinfo.js
echo.   WScript.StdOut.WriteLine( "ExtClock: " + item.ExtClock + " MHz" );>>_hbhwinfo.js
echo.   items.moveNext();>>_hbhwinfo.js
echo.}>>_hbhwinfo.js
echo.var items = new Enumerator( services.ExecQuery( "SELECT * FROM Win32_PhysicalMemory" ) );>>_hbhwinfo.js
echo.var totalMemory = 0;>>_hbhwinfo.js
echo.while( ! items.atEnd() )>>_hbhwinfo.js
echo.{>>_hbhwinfo.js
echo.   var item = items.item();>>_hbhwinfo.js
echo.   totalMemory += parseInt( item.Capacity ) / 1024 / 1024;>>_hbhwinfo.js
echo.   items.moveNext();>>_hbhwinfo.js
echo.}>>_hbhwinfo.js
echo.WScript.StdOut.WriteLine( "Physical memory: " + totalMemory + " MiB" );>>_hbhwinfo.js

@echo on

@rem ; Hardware information
cscript //nologo //E:javascript _hbhwinfo.js

@del _hbhwinfo.js

@rem ; Harbour information
harbour /build

@rem ; Pre-make cleanup
if exist speedtst_*.exe del speedtst_*.exe

@rem ; Default test flags
if "%HB_CMP_FLAGS_1%" == "" set HB_CMP_FLAGS_1=-st
if "%HB_CMP_FLAGS_2%" == "" set HB_CMP_FLAGS_2=-mt
if "%HB_BENCH_RUNS%" == "" set HB_BENCH_RUNS=3

@rem ; Creating speedtst executables
if not "%HB_CMP_FLAGS_1%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_1.exe %HB_CMP_FLAGS_1%
if not "%HB_CMP_FLAGS_2%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_2.exe %HB_CMP_FLAGS_2%
if not "%HB_CMP_FLAGS_3%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_3.exe %HB_CMP_FLAGS_3%
if not "%HB_CMP_FLAGS_4%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_4.exe %HB_CMP_FLAGS_4%
if not "%HB_CMP_FLAGS_5%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_5.exe %HB_CMP_FLAGS_5%
if not "%HB_CMP_FLAGS_6%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_6.exe %HB_CMP_FLAGS_6%
if not "%HB_CMP_FLAGS_7%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_7.exe %HB_CMP_FLAGS_7%
if not "%HB_CMP_FLAGS_8%" == "" hbmk2 speedtst.prg -q0 -trace -ospeedtst_8.exe %HB_CMP_FLAGS_8%

@rem ; speedtst executable information
dir speedtst_*.exe speedtst.prg

@rem ; pre-OS state information
tasklist

@rem ; Running speedtst executables in multiple runs
@for /l %%r in (1,1,%HB_BENCH_RUNS%) do @call :RUN_ALL %%r

@rem ; post-OS state information
tasklist

@goto END

:RUN_ALL

   @echo.
   @echo Run #%1
   if exist speedtst_1.exe speedtst_1.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_1%
   if exist speedtst_2.exe speedtst_2.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_2%
   if exist speedtst_3.exe speedtst_3.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_3%
   if exist speedtst_4.exe speedtst_4.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_4%
   if exist speedtst_5.exe speedtst_5.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_5%
   if exist speedtst_6.exe speedtst_6.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_6%
   if exist speedtst_7.exe speedtst_7.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_7%
   if exist speedtst_8.exe speedtst_8.exe %HB_RUN_FLAGS% %HB_RUN_FLAGS_8%
   @goto END

:END
