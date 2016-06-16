/* Copyright 2008 Viktor Szakats (vszakats.net/harbour) */

/* DLL call demonstration. */

#require "hbxpp"

#include "simpleio.ch"

#include "dll.ch"

#define MAX_PATH                    260

#define SPI_SETDRAGFULLWINDOWS      37

#define CSIDL_APPDATA               0x001a  /* <username>\Application Data */
#define CSIDL_ADMINTOOLS            0x0030  /* <username>\Start Menu\Programs\Administrative Tools */

PROCEDURE Main()

   LOCAL hDLL
   LOCAL cData

#if defined( __PLATFORM__WINDOWS )

   IF hb_vfExists( "pscript.dll" )
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
   DllCall( hDll, , "PSGetVersion", @cData )
   ? cData
   DllUnload( hDLL )

   ? "MsgBox:", DllCall( "user32.dll", DLL_OSAPI, "MessageBoxA", 0, "Hello world!", "Harbour sez", 0 /* MB_OK */ )

   /* Force Windows not to show dragged windows' contents */

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
