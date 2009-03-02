@rem
@rem $Id$
@rem

@echo off

rem Minimal initialization of environment variables for OS2 GCC build
rem for further information about see make_gnu.bat

if "%HB_ARCHITECTURE%" == "" set HB_ARCHITECTURE=os2
if "%HB_COMPILER%" == "" set HB_COMPILER=gcc

rem Set to constant value to be consistent with the non-GNU make files.

if "%HB_BIN_INSTALL%" == "" set HB_BIN_INSTALL=%HB_INSTALL_PREFIX%\bin
if "%HB_LIB_INSTALL%" == "" set HB_LIB_INSTALL=%HB_INSTALL_PREFIX%\lib
if "%HB_INC_INSTALL%" == "" set HB_INC_INSTALL=%HB_INSTALL_PREFIX%\include
if "%HB_DOC_INSTALL%" == "" set HB_DOC_INSTALL=%HB_INSTALL_PREFIX%\doc

rem Try to create install dirs.

if not exist %HB_BIN_INSTALL%\*.* md %HB_BIN_INSTALL%
if not exist %HB_LIB_INSTALL%\*.* md %HB_LIB_INSTALL%
if not exist %HB_INC_INSTALL%\*.* md %HB_INC_INSTALL%
if not exist %HB_DOC_INSTALL%\*.* md %HB_DOC_INSTALL%


rem In GCC3.2.2 the TCP/IP headers and libraries scheme have been changed.
rem The default is the current OS/2 tcpip toolkit (BSD 4.4 based).
rem To target the older OS/2 tcpip stack (BSD 4.3 based) and create
rem binaries which can be executed also on older OS2 versions you must
rem define TCPV40HDRS before including any TCP/IP headers and make
rem sure usr/lib/tcpipv4 is searched before usr/lib (this is to
rem get the right libsocket). It is recommended to use the -D
rem compiler option for the define and either the LIBRARY_PATH or
rem the -L compiler/linker option for the library.
rem For building Harbour you can also use HB_USER_LDFLAGS environment variable,
rem f.e.
rem         SET HB_USER_LDFLAGS=-Le:\usr\lib\tcpipv4
rem
rem If you are using newer OS2 version with tcp/ip stack >= 4.1
rem (eComStation, for example) and you do not need backward binary
rem compatibility then you can disable it by setting HB_OS2_TCP32
rem environment variable, f.e.
rem         SET HB_OS2_TCP32=yes

if "%HB_OS2_TCP32%" == "yes" goto tcp32
if "%HB_OS2_TCP32%" == "YES" goto tcp32
   set HB_USER_CFLAGS=-DTCPV40HDRS %HB_USER_CFLAGS%
:tcp32

make %HB_USER_MAKEFLAGS% %1 %2 %3 %4 %5 %6 %7 %8 %9
