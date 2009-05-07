/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ALERT() function
 *
 * Released to Public Domain by Vladimir Kazimirchik <v_kazimirchik@yahoo.com>
 * www - http://www.harbour-project.org
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    Changes for higher Clipper compatibility, console mode, extensions
 *    __NONOALERT()
 *
 * See COPYING for licensing terms.
 *
 */

#include "box.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"

/* TOFIX: Clipper defines a clipped window for Alert() [vszakats] */

/* NOTE: Clipper will return NIL if the first parameter is not a string, but
         this is not documented. This implementation converts the first
         parameter to a string if another type was passed. You can switch back
         to Clipper compatible mode by undefining constant HB_EXTENSION. [vszakats] */

/* NOTE: Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
         This is fixed. [vszakats] */

/* NOTE: nDelay parameter is a Harbour extension. */

#ifdef HB_C52_UNDOC
STATIC s_lNoAlert
#endif

FUNCTION Alert( xMessage, aOptions, cColorNorm, nDelay )
   LOCAL cMessage
   LOCAL cColorHigh
   LOCAL aOptionsOK
   LOCAL nEval
#ifdef HB_EXTENSION
   LOCAL lFirst
   LOCAL cLine
#endif

#ifdef HB_C52_UNDOC

   DEFAULT s_lNoAlert TO hb_argCheck( "NOALERT" )

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

#ifdef HB_EXTENSION

   IF PCount() == 0
      RETURN NIL
   ENDIF

   IF ISARRAY( xMessage )

      cMessage := ""

      lFirst := .T.
      FOR nEval := 1 TO Len( xMessage )
         IF ISCHARACTER( cLine := xMessage[ nEval ] )
            cMessage += iif( lFirst, "", Chr( 10 ) ) + cLine
            lFirst := .F.
         ENDIF
      NEXT
   ELSEIF ISCHARACTER( xMessage )
      cMessage := StrTran( xMessage, ";", Chr( 10 ) )
   ELSE
      cMessage := hb_CStr( xMessage )
   ENDIF

#else

   IF !ISCHARACTER( xMessage )
      RETURN NIL
   ENDIF

   cMessage := StrTran( xMessage, ";", Chr( 10 ) )

#endif

   IF !ISARRAY( aOptions )
      aOptions := {}
   ENDIF

   IF !ISCHARACTER( cColorNorm ) .OR. EMPTY( cColorNorm )
      cColorNorm := "W+/R" // first pair color (Box line and Text)
      cColorHigh := "W+/B" // second pair color (Options buttons)
   ELSE
      cColorHigh := StrTran( StrTran( iif( At( "/", cColorNorm ) == 0, "N", SubStr( cColorNorm, At( "/", cColorNorm ) + 1 ) ) + "/" +;
                                      iif( At( "/", cColorNorm ) == 0, cColorNorm, Left( cColorNorm, At( "/", cColorNorm ) - 1 ) ), "+", "" ), "*", "" )
   ENDIF

   aOptionsOK := {}
   FOR nEval := 1 TO Len( aOptions )
      IF ISCHARACTER( aOptions[ nEval ] ) .AND. !Empty( aOptions[ nEval ] )
         AAdd( aOptionsOK, aOptions[ nEval ] )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { "Ok" }
#ifdef HB_C52_STRICT
   /* NOTE: Clipper allows only four options [vszakats] */
   ELSEIF Len( aOptionsOK ) > 4
      ASize( aOptionsOK, 4 )
#endif
   ENDIF

   RETURN hb_gtAlert( cMessage, aOptionsOK, cColorNorm, cColorHigh, nDelay )

#ifdef HB_C52_UNDOC

PROCEDURE __NONOALERT()

   s_lNoAlert := .F.

   RETURN

#endif
