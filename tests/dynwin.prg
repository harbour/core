/* Copyright 2008 Viktor Szakats (vszakats.net/harbour) */

/* Dynamic library call demonstration. (on Windows) */

#include "simpleio.ch"

#include "hbdyn.ch"

PROCEDURE Main()

   #if defined( __PLATFORM__WINDOWS )

   LOCAL hLib
   LOCAL cData

   #define MAX_PATH 260

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

   ? "MsgBox:", hb_DynCall( { "MessageBoxA", "user32.dll", HB_DYN_CALLCONV_STDCALL }, 0, "Hello world!", "Harbour sez", hb_bitOr( MB_OKCANCEL, MB_ICONEXCLAMATION, MB_HELP ) )

   hLib := hb_libLoad( "libcurl.dll" )
   IF ! Empty( hLib )
      ? hb_DynCall( { "curl_version", hLib, HB_DYN_CTYPE_CHAR_PTR } )
      hb_libFree( hLib )
      ? hb_DynCall( { "curl_version", "libcurl.dll", HB_DYN_CTYPE_CHAR_PTR } )
   ENDIF

   /* Force Windows not to show dragged windows contents */

   #define SPI_SETDRAGFULLWINDOWS 37

   ? "Full content drag: OFF"
   ? hb_DynCall( { "SystemParametersInfo", "user32.dll", HB_DYN_CALLCONV_STDCALL }, SPI_SETDRAGFULLWINDOWS, 0, 0, 0 )
   Inkey( 0 )

   ? "Full content drag: ON"
   ? hb_DynCall( { "SystemParametersInfo", "user32.dll", HB_DYN_CALLCONV_STDCALL }, SPI_SETDRAGFULLWINDOWS, 1, 0, 0 )
   Inkey( 0 )

   /* Get some standard Windows folders */

   #define CSIDL_APPDATA               0x001a /* <username>\Application Data */
   #define CSIDL_ADMINTOOLS            0x0030 /* <username>\Start Menu\Programs\Administrative Tools */

   hLib := hb_libLoad( "shell32.dll" )
   ? "ValType( hLib ):", ValType( hLib )
   ? "------"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL (BOOL retval):", hb_DynCall( { "SHGetSpecialFolderPathA", hLib, hb_bitOr( HB_DYN_CALLCONV_STDCALL, HB_DYN_CTYPE_BOOL ) }, 0, @cData, CSIDL_APPDATA, 0 )
   ? "@cData:", cData, "<"
   ? "------"
   ? "HB_DYNCALL:", hb_DynCall( { "SHGetFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL }, 0, CSIDL_ADMINTOOLS, 0, 0, cData ) // WRONG
   ? "cData:", cData, "<"
   ? "------"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL (PARAMS):", hb_DynCall( { "SHGetSpecialFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL, NIL, NIL, NIL, HB_DYN_CTYPE_BOOL }, 0, @cData, CSIDL_APPDATA, 0 )
   ? "@cData:", cData, "<"
   ? "------"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL @ASCII:", hb_DynCall( { "SHGetFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL }, 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "@cData:", cData, "<"
   ? "------"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL @UTF16:", hb_DynCall( { "SHGetFolderPathW", hLib, HB_DYN_CALLCONV_STDCALL + HB_DYN_ENC_UTF16 }, 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "@cData:", cData, "<"
   ? "------"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL @UTF16 + psz:", hb_DynCall( { "SHGetFolderPathW", hLib, HB_DYN_CALLCONV_STDCALL + HB_DYN_ENC_UTF16 + HB_DYC_OPT_NULLTERM }, 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "@cData:", cData, "<"
   ? "------"
   cData := Space( MAX_PATH )
   ? "cData BEFORE:", cData, "<"
   ? "HB_DYNCALL (MISSING @):", hb_DynCall( { "SHGetFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL }, 0, CSIDL_ADMINTOOLS, 0, 0, cData )
   ? "cData AFTER:", cData, "<"
   ? "------"
   hb_libFree( hLib )

   #endif

   RETURN
