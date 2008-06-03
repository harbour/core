/*
 * $Id$
 */

#include "hbw32.ch"

PROCEDURE Main()

   ? GetRegistry( HKEY_CURRENT_USER, "Control Panel\Desktop", "Wallpaper" )
   ? w32_regRead( "HKCU\Control Panel\Desktop\Wallpaper" )
   ? w32_regRead( "" )

   // ? w32_regWrite( "HKCU\Control Panel\Desktop\Wallpaper", "harbour.bmp" )

   Inkey( 0 )

   RETURN
