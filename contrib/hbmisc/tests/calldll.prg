/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbmisc"

PROCEDURE Main()

#if defined( __PLATFORM__WINDOWS )

   LOCAL nFlags

   nFlags := -1
   ? CallDll( "InternetGetConnectedState", "wininet.dll", @nFlags, 0 )
   ? nFlags

#endif

   RETURN
