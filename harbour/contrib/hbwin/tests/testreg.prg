/*
 * $Id$
 */

#require "hbwin"

#define KEY_WOW64_64KEY 0x0100
#define KEY_WOW64_32KEY 0x0200

PROCEDURE Main()

   LOCAL tmp

   ? win_regWrite( "HKCU\_TEST\mydword", 0x11223344, WIN_REG_DWORD )
   ? hb_NumToHex( win_regRead( "HKCU\_TEST\mydword" ) )

   ? win_regWrite( "HKCU\_TEST\myqword", 0x1122334455667788, WIN_REG_QWORD )
   ? hb_NumToHex( win_regRead( "HKCU\_TEST\myqword" ) )

   Inkey( 0 )

   ? win_regDelete( "HKCU\_TEST\teszt" )
   ? win_regDelete( "HKCU\_TEST\" )

   ? ">" + win_GetCommandLineParam() + "<"
   ? ">" + wapi_GetCommandLine() + "<"

   /* old API */
   ? ">" + hb_ValToStr( win_regGet( WIN_HKEY_CURRENT_USER, "Control Panel\Desktop", "Wallpaper" ) ) + "<"

   /* new API */
   ? ">" + hb_ValToStr( win_regRead( "HKCU\Environment\PATH" ) ) + "<"
   ? ">" + hb_ValToStr( tmp := win_regRead( "HKCU\Control Panel\Desktop\Wallpaper" ) ) + "<"
   ? ">" + hb_ValToStr( win_regRead( "" ) ) + "<"

   ? win_regWrite( "HKCU\Control Panel\Desktop\Wallpaper", "harbour.bmp" )
   ? win_regRead( "HKCU\Control Panel\Desktop\Wallpaper" )

   Inkey( 0 )

   ? win_regWrite( "HKCU\Control Panel\Desktop\Wallpaper", tmp )

   ? win_regRead( "HKLM\SOFTWARE\7-Zip\Path" )
   IF hb_osIs64bit()
      ? win_regRead( "HKLM\SOFTWARE\7-Zip\Path",, KEY_WOW64_32KEY )
      ? win_regRead( "HKLM\SOFTWARE\7-Zip\Path",, KEY_WOW64_64KEY )
   ENDIF

   RETURN
