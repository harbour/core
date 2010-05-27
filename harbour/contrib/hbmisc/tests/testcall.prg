/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 */

PROCEDURE Main()

   ? CALLDLL32( "curl_version", "libcurl.dll" )

   ? CALLDLL32( "SSLeay_version", "libeay32.dll", 0 )
   ? CALLDLL32( "SSLeay_version", "libeay32.dll", 1 )
   ? CALLDLL32( "SSLeay_version", "libeay32.dll", 2 )
   ? CALLDLL32( "SSLeay_version", "libeay32.dll", 3 )
   ? CALLDLL32( "SSLeay_version", "libeay32.dll", 4 )

   ? HB_DYNACALL1( "SSLeay_version", "libeay32.dll", 0, 4 )

   RETURN
