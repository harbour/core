/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ALERT(), HB_ALERT() functions
 *
 * Released to Public Domain by Vladimir Kazimirchik <v_kazimirchik@yahoo.com>
 * www - http://harbour-project.org
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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
         this is not documented. [vszakats] */

/* NOTE: Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
         This is fixed. [vszakats] */

#ifdef HB_CLP_UNDOC
STATIC s_lNoAlert
#endif

FUNCTION Alert( cMessage, aOptions, cColorNorm )
   LOCAL cColorHigh
   LOCAL aOptionsOK
   LOCAL nEval

#ifdef HB_CLP_UNDOC

   IF s_lNoAlert == NIL
      s_lNoAlert := hb_argCheck( "NOALERT" )
   ENDIF

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   IF ! ISCHARACTER( cMessage )
      RETURN NIL
   ENDIF

   cMessage := StrTran( cMessage, ";", Chr( 10 ) )

   IF ! ISARRAY( aOptions )
      aOptions := {}
   ENDIF

   IF ! ISCHARACTER( cColorNorm ) .OR. Empty( cColorNorm )
      cColorNorm := "W+/R" // first pair color (Box line and Text)
      cColorHigh := "W+/B" // second pair color (Options buttons)
   ELSE
      cColorHigh := StrTran( StrTran( iif( At( "/", cColorNorm ) == 0, "N", SubStr( cColorNorm, At( "/", cColorNorm ) + 1 ) ) + "/" +;
                                      iif( At( "/", cColorNorm ) == 0, cColorNorm, Left( cColorNorm, At( "/", cColorNorm ) - 1 ) ), "+", "" ), "*", "" )
   ENDIF

   aOptionsOK := {}
   FOR nEval := 1 TO Len( aOptions )
      IF ISCHARACTER( aOptions[ nEval ] ) .AND. ! Empty( aOptions[ nEval ] )
         AAdd( aOptionsOK, aOptions[ nEval ] )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { "Ok" }
#ifdef HB_CLP_STRICT
   /* NOTE: Clipper allows only four options [vszakats] */
   ELSEIF Len( aOptionsOK ) > 4
      ASize( aOptionsOK, 4 )
#endif
   ENDIF

   RETURN hb_gtAlert( cMessage, aOptionsOK, cColorNorm, cColorHigh )

/* NOTE: xMessage can be of any type. This is a Harbour extension over Alert(). */
/* NOTE: nDelay parameter is a Harbour extension over Alert(). */

FUNCTION hb_Alert( xMessage, aOptions, cColorNorm, nDelay )
   LOCAL cMessage
   LOCAL cColorHigh
   LOCAL aOptionsOK
   LOCAL nEval

#ifdef HB_CLP_UNDOC

   IF s_lNoAlert == NIL
      s_lNoAlert := hb_argCheck( "NOALERT" )
   ENDIF

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   IF PCount() == 0
      RETURN NIL
   ENDIF

   IF ISARRAY( xMessage )
      cMessage := ""
      FOR nEval := 1 TO Len( xMessage )
         cMessage += iif( nEval == 1, "", Chr( 10 ) ) + hb_CStr( xMessage[ nEval ] )
      NEXT
   ELSEIF ISCHARACTER( xMessage )
      cMessage := StrTran( xMessage, ";", Chr( 10 ) )
   ELSE
      cMessage := hb_CStr( xMessage )
   ENDIF

   IF ! ISARRAY( aOptions )
      aOptions := {}
   ENDIF

   IF !ISCHARACTER( cColorNorm ) .OR. Empty( cColorNorm )
      cColorNorm := "W+/R" // first pair color (Box line and Text)
      cColorHigh := "W+/B" // second pair color (Options buttons)
   ELSE
      cColorHigh := StrTran( StrTran( iif( At( "/", cColorNorm ) == 0, "N", SubStr( cColorNorm, At( "/", cColorNorm ) + 1 ) ) + "/" +;
                                      iif( At( "/", cColorNorm ) == 0, cColorNorm, Left( cColorNorm, At( "/", cColorNorm ) - 1 ) ), "+", "" ), "*", "" )
   ENDIF

   aOptionsOK := {}
   FOR nEval := 1 TO Len( aOptions )
      IF ISCHARACTER( aOptions[ nEval ] ) .AND. ! Empty( aOptions[ nEval ] )
         AAdd( aOptionsOK, aOptions[ nEval ] )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { "Ok" }
#ifdef HB_CLP_STRICT
   /* NOTE: Clipper allows only four options [vszakats] */
   ELSEIF Len( aOptionsOK ) > 4
      ASize( aOptionsOK, 4 )
#endif
   ENDIF

   RETURN hb_gtAlert( cMessage, aOptionsOK, cColorNorm, cColorHigh, nDelay )

#ifdef HB_CLP_UNDOC

PROCEDURE __NONOALERT()

   s_lNoAlert := .F.

   RETURN

#endif
