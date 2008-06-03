/*
 * $Id$
 */

#include "hbw32.ch"

PROCEDURE Main()

   /* old API */
   ? ">" + hb_ValToStr( GetRegistry( HKEY_CURRENT_USER, "Control Panel\Desktop", "Wallpaper" ) ) + "<"

   /* new API */
   ? ">" + hb_ValToStr( w32_regRead( "HKCU\Control Panel\Desktop\Wallpaper" ) ) + "<"
   ? ">" + hb_ValToStr( w32_regRead( "" ) ) + "<"

// ? w32_regWrite( "HKCU\Control Panel\Desktop\Wallpaper", "harbour.bmp" )

   RETURN
