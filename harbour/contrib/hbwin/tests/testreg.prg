/*
 * $Id$
 */

#include "hbwin.ch"

PROCEDURE Main()

   ? ">" + win_GetCommandLineParam() + "<"
   ? ">" + wapi_GetCommandLine() + "<"

   /* old API */
   ? ">" + hb_ValToStr( GetRegistry( HKEY_CURRENT_USER, "Control Panel\Desktop", "Wallpaper" ) ) + "<"

   /* new API */
   ? ">" + hb_ValToStr( win_regRead( "HKCU\Control Panel\Desktop\Wallpaper" ) ) + "<"
   ? ">" + hb_ValToStr( win_regRead( "" ) ) + "<"

// ? win_regWrite( "HKCU\Control Panel\Desktop\Wallpaper", "harbour.bmp" )

   RETURN
