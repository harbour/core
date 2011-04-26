/*
 * $Id$
 */

#include "hbwin.ch"

PROCEDURE Main()
   LOCAL tmp

   ? win_regWrite( "HKCU\_TEST\mydword", 0x11223344, WIN_REG_DWORD )
   ? hb_numtohex( win_regRead( "HKCU\_TEST\mydword" ) )

   ? win_regWrite( "HKCU\_TEST\myqword", 0x1122334455667788, WIN_REG_QWORD )
   ? hb_numtohex( win_regRead( "HKCU\_TEST\myqword" ) )

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

   RETURN
