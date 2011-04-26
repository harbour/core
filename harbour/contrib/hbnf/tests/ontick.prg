/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 */

PROCEDURE Main()

   FT_ONTICK( {|| OutStd( hb_milliSeconds(), hb_eol() ) } )

   Inkey( 0 )

   FT_ONTICK( {|| OutStd( hb_milliSeconds(), hb_eol() ) }, 18 )

   Inkey( 0 )

   FT_ONTICK()

   Inkey( 0 )

   RETURN
