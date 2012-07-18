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
   ? hb_gtVersion(), hb_gtVersion( 1 )
   ?
   Inkey( 0 )
   SetBlink( Empty( xBlink ) )
   FOR bg := 0 TO 15
      FOR fg := 0 TO 15
         n := bg * 16 + fg
         @ 5 + bg, 5 + fg * 4 SAY "[" + NUM2HEX( n ) + "]" COLOR NTOCOLOR( n )
      NEXT
   NEXT
   ?
   ?
   WHILE Inkey( 0 ) != 13
   ENDDO

   RETURN

STATIC FUNCTION NTOCOLOR( nClr )

   RETURN LTrim( Str( Int( nClr % 16 ), 2 ) ) + "/" + ;
      LTrim( Str( Int( nClr / 16 ), 2 ) )

STATIC FUNCTION NUM2HEX( nVal )

   LOCAL cHex := "", i, n

   FOR i := 1 TO 2
      n := nVal % 16
      cHex := Chr( n + iif( n > 9, 55, 48 ) ) + cHex
      nVal := Int( nVal / 16 )
   NEXT

   RETURN cHex
