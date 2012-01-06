/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

PROCEDURE Main()
   LOCAL nFlags

   nFlags := -1
   ? CALLDLL32( "InternetGetConnectedState", "wininet.dll", @nFlags, 0 )
   ? nFlags

   RETURN
