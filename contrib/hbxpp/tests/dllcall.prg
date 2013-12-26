/*
 * Harbour Project source code:
 *    DLL call demonstration.
 *
 * Copyright 2008 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbxpp"

#include "simpleio.ch"

#include "dll.ch"

#define MAX_PATH                    260

#define MB_OK                       0x00000000
#define MB_OKCANCEL                 0x00000001
#define MB_ABORTRETRYIGNORE         0x00000002
#define MB_YESNOCANCEL              0x00000003
#define MB_YESNO                    0x00000004
#define MB_RETRYCANCEL              0x00000005
#define MB_CANCELTRYCONTINUE        0x00000006
#define MB_ICONHAND                 0x00000010
#define MB_ICONQUESTION             0x00000020
#define MB_ICONEXCLAMATION          0x00000030
#define MB_ICONASTERISK             0x00000040
#define MB_USERICON                 0x00000080
#define MB_DEFBUTTON2               0x00000100
#define MB_DEFBUTTON3               0x00000200
#define MB_DEFBUTTON4               0x00000300
#define MB_SYSTEMMODAL              0x00001000
#define MB_TASKMODAL                0x00002000
#define MB_HELP                     0x00004000
#define MB_NOFOCUS                  0x00008000
#define MB_SETFOREGROUND            0x00010000
#define MB_DEFAULT_DESKTOP_ONLY     0x00020000
#define MB_TOPMOST                  0x00040000
#define MB_RIGHT                    0x00080000
#define MB_RTLREADING               0x00100000

#define SPI_SETDRAGFULLWINDOWS      37

#define CSIDL_APPDATA               0x001a /* <username>\Application Data */
#define CSIDL_ADMINTOOLS            0x0030 /* <username>\Start Menu\Programs\Administrative Tools */

PROCEDURE Main()

   LOCAL hDLL
   LOCAL cData

#if defined( __PLATFORM__WINDOWS )

   IF hb_FileExists( "pscript.dll" )
      hDLL := DllLoad( "pscript.dll" )
      cData := Space( 24 )
      DllCall( hDll, DLL_OSAPI, "PSGetVersion", @cData )
      ? ">" + cData + "<"
      DllUnload( hDLL )

      // Testing failure 1
      hDLL := DllLoad( "pscript.dll" )
      cData := Space( 24 )
      DllCall( hDll, DLL_OSAPI, "PSGet__Version", @cData )
      ? ">" + cData + "<"
      DllUnload( hDLL )
   ENDIF

   // Testing failure 2
   hDLL := DllLoad( "nothere.dll" )
   cData := Space( 24 )
   DllCall( hDll, NIL, "PSGetVersion", @cData )
   ? cData
   DllUnload( hDLL )

   ? "MsgBox:", DllCall( "user32.dll", DLL_OSAPI, "MessageBoxA", 0, "Hello world!", "Harbour sez", hb_bitOr( MB_OKCANCEL, MB_ICONEXCLAMATION, MB_HELP ) )

   /* Force Windows not to show dragged windows contents */

   ? "Full content drag: OFF"
   ? DllCall( "user32.dll", DLL_OSAPI, "SystemParametersInfoA", SPI_SETDRAGFULLWINDOWS, 0, 0, 0 )
   Inkey( 0 )

   ? "Full content drag: ON"
   ? DllCall( "user32.dll", DLL_OSAPI, "SystemParametersInfoA", SPI_SETDRAGFULLWINDOWS, 1, 0, 0 )
   Inkey( 0 )

   ? "DLLCALL"
   cData := Space( MAX_PATH )
   ? DllCall( "shell32.dll", DLL_OSAPI, "SHGetFolderPathA", 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "REF:", cData

   ? "DLLCALL"
   cData := Space( MAX_PATH )
   ? DllCall( "shell32.dll", DLL_OSAPI, "SHGetFolderPathW", 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "REF:", cData

#endif

   RETURN
