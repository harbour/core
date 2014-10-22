/* Copyright 2008 Viktor Szakats (vszakats.net/harbour) */

/* Dynamic library call demonstration. (on Windows) */

#include "simpleio.ch"

#include "hbdyn.ch"

PROCEDURE Main()

   #if defined( __PLATFORM__WINDOWS )

   LOCAL hLib
   LOCAL cData

   #define MAX_PATH 260

   ? "MsgBox:", hb_DynCall( { "MessageBoxA", "user32.dll", HB_DYN_CALLCONV_STDCALL }, 0, "Hello world!", "Harbour sez", 0 /* MB_OK */ )

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
   ? "---"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL (BOOL retval):", hb_DynCall( { "SHGetSpecialFolderPathA", hLib, hb_bitOr( HB_DYN_CALLCONV_STDCALL, HB_DYN_CTYPE_BOOL ) }, 0, @cData, CSIDL_APPDATA, 0 )
   ? "@cData:", cData, "<"
   ? "---"
   ? "HB_DYNCALL:", hb_DynCall( { "SHGetFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL }, 0, CSIDL_ADMINTOOLS, 0, 0, cData ) // WRONG
   ? "cData:", cData, "<"
   ? "---"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL (PARAMS):", hb_DynCall( { "SHGetSpecialFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL, NIL, NIL, NIL, HB_DYN_CTYPE_BOOL }, 0, @cData, CSIDL_APPDATA, 0 )
   ? "@cData:", cData, "<"
   ? "---"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL @ASCII:", hb_DynCall( { "SHGetFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL }, 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "@cData:", cData, "<"
   ? "---"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL @UTF16:", hb_DynCall( { "SHGetFolderPathW", hLib, HB_DYN_CALLCONV_STDCALL + HB_DYN_ENC_UTF16 }, 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "@cData:", cData, "<"
   ? "---"
   cData := Space( MAX_PATH )
   ? "HB_DYNCALL @UTF16 + psz:", hb_DynCall( { "SHGetFolderPathW", hLib, HB_DYN_CALLCONV_STDCALL + HB_DYN_ENC_UTF16 + HB_DYC_OPT_NULLTERM }, 0, CSIDL_ADMINTOOLS, 0, 0, @cData )
   ? "@cData:", cData, "<"
   ? "---"
   cData := Space( MAX_PATH )
   ? "cData BEFORE:", cData, "<"
   ? "HB_DYNCALL (MISSING @):", hb_DynCall( { "SHGetFolderPathA", hLib, HB_DYN_CALLCONV_STDCALL }, 0, CSIDL_ADMINTOOLS, 0, 0, cData )
   ? "cData AFTER:", cData, "<"
   ? "---"
   hb_libFree( hLib )

   #endif

   RETURN
