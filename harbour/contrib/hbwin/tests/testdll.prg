/*
 * $Id$
 */

#include "simpleio.ch"

/*
 * Harbour Project source code:
 *    DLL call demonstration.
 *
 * Copyright 2008 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 */

#include "hbdyn.ch"

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

#define CSIDL_APPDATA               0x001a /* <username>\Application Data */
#define CSIDL_ADMINTOOLS            0x0030 /* <username>\Start Menu\Programs\Administrative Tools */

#define MAX_PATH 260

PROCEDURE Main()
   LOCAL hDLL
   LOCAL cData

   ? "MsgBox:", win_DllCall( { "MessageBoxA", "user32.dll" }, 0, "Hello world!", "Harbour sez", hb_bitOr( MB_OKCANCEL, MB_ICONEXCLAMATION, MB_HELP ) )

   IF hb_FileExists( "libcurl.dll" )
      hDLL := wapi_LoadLibrary( "libcurl.dll" )
      ? GetProcAddress( hDLL, "curl_version" )
      ? win_dllCall( { "curl_version", hDLL, HB_DYN_CTYPE_CHAR_PTR } )
      wapi_FreeLibrary( hDLL )
      ? win_dllCall( { "curl_version", "libcurl.dll", HB_DYN_CTYPE_CHAR_PTR } )
   ENDIF

   /* Force Windows not to show dragged windows contents */

   #define SPI_SETDRAGFULLWINDOWS 37

   ? "Full content drag: OFF"
   ? win_DllCall( { "SystemParametersInfo", "user32.dll" }, SPI_SETDRAGFULLWINDOWS, 0, 0, 0 )
   Inkey( 0 )

   ? "Full content drag: ON"
   ? win_DllCall( { "SystemParametersInfo", "user32.dll" }, SPI_SETDRAGFULLWINDOWS, 1, 0, 0 )
   Inkey( 0 )

   /* Get some standard Windows folders */

   hDLL := wapi_LoadLibrary( "shell32.dll" )
   ? "ValType( hDLL ): ", ValType( hDLL )
   ? "------"
   cData := Space( MAX_PATH )
   ? "WIN_DLLCALL (BOOL retval): ", win_dllCall( { "SHGetSpecialFolderPath", hDLL, HB_DYN_CTYPE_BOOL }, 0, @cData, CSIDL_APPDATA, 0 )
   ? "@cData: ", cData
   ? "------"
   cData := Space( MAX_PATH )
   ? "WIN_DLLCALL (BOOL retval): ", win_dllCall( { GetProcAddress( hDLL, "SHGetSpecialFolderPath" ), HB_DYN_CTYPE_BOOL }, 0, @cData, CSIDL_APPDATA, 0 )
   ? "@cData: ", cData
   ? "------"
   ? "WIN_DLLCALL: ", win_dllCall( { "SHGetFolderPath", hDLL }, 0, CSIDL_ADMINTOOLS, 0, 0, cData ) // WRONG
   ? "cData:", cData
   ? "------"
   cData := Space( MAX_PATH )
   ? "WIN_DLLCALL (PARAMS): ", win_dllCall( { "SHGetSpecialFolderPath", hDLL, NIL, NIL, NIL, HB_DYN_CTYPE_BOOL }, 0, @cData, CSIDL_APPDATA, 0 )
   ? "@cData: ", cData
   ? "------"
   cData := Space( MAX_PATH )
   ? "WIN_DLLCALL: ", win_dllCall( { "SHGetFolderPath", hDLL }, 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "@cData: ", cData
   ? "------"
   cData := Space( MAX_PATH )
   ? "cData BEFORE: ", cData
   ? "WIN_DLLCALL (MISSING @1): ", win_dllCall( { "SHGetFolderPath", hDLL }, 0, CSIDL_ADMINTOOLS, 0, 0, cData )
   ? "cData AFTER: ", cData
   ? "------"
   wapi_FreeLibrary( hDLL )

   RETURN
