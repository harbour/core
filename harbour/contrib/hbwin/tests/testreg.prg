/*
 * $Id$
 */

#include "hbwin.ch"

PROCEDURE Main()
   LOCAL tmp

   ? ">" + win_GetCommandLineParam() + "<"
   ? ">" + wapi_GetCommandLine() + "<"

   /* old API */
   ? ">" + hb_ValToStr( GetRegistry( HKEY_CURRENT_USER, "Control Panel\Desktop", "Wallpaper" ) ) + "<"

   /* new API */
   ? ">" + hb_ValToStr( tmp := win_regRead( "HKCU\Control Panel\Desktop\Wallpaper" ) ) + "<"
   ? ">" + hb_ValToStr( win_regRead( "" ) ) + "<"

   ? win_regWrite( "HKCU\Control Panel\Desktop\Wallpaper", "harbour.bmp" )

   Inkey( 0 )

   ? win_regWrite( "HKCU\Control Panel\Desktop\Wallpaper", tmp )

   RETURN
