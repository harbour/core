/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for GT full screen color output
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

PROCEDURE Main( xBlink )

   LOCAL bg, fg, n

   CLS
   ?
   ? OS(), Version(), Date(), Time()
   ? "GT" + hb_gtVersion(), hb_gtVersion( 1 )
   ?

   SetBlink( Empty( xBlink ) )
   FOR bg := 0 TO 15
      FOR fg := 0 TO 15
         n := bg * 16 + fg
         @ 5 + bg, 5 + fg * 4 SAY "[" + hb_NumToHex( n, 2 ) + "]" COLOR hb_NToColor( n )
      NEXT
   NEXT
   ?
   ?
   Inkey( 0 )

   RETURN
