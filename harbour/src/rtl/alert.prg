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
   LOCAL cOption

#ifdef HB_CLP_UNDOC

   IF s_lNoAlert == NIL
      s_lNoAlert := hb_argCheck( "NOALERT" )
   ENDIF

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   IF ! HB_ISSTRING( cMessage )
      RETURN NIL
   ENDIF

   cMessage := StrTran( cMessage, ";", Chr( 10 ) )

   hb_default( @aOptions, {} )

   IF ! HB_ISSTRING( cColorNorm ) .OR. Empty( cColorNorm )
      cColorNorm := "W+/R" // first pair color (Box line and Text)
      cColorHigh := "W+/B" // second pair color (Options buttons)
   ELSE
      cColorHigh := StrTran( StrTran( iif( At( "/", cColorNorm ) == 0, "N", SubStr( cColorNorm, At( "/", cColorNorm ) + 1 ) ) + "/" + ;
         iif( At( "/", cColorNorm ) == 0, cColorNorm, Left( cColorNorm, At( "/", cColorNorm ) - 1 ) ), "+", "" ), "*", "" )
   ENDIF

   aOptionsOK := {}
   FOR EACH cOption IN aOptions
      IF HB_ISSTRING( cOption ) .AND. ! Empty( cOption )
         AAdd( aOptionsOK, cOption )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { "Ok" }
#ifdef HB_CLP_STRICT
   ELSEIF Len( aOptionsOK ) > 4 /* NOTE: Clipper allows only four options [vszakats] */
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
   LOCAL cOption
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

   IF HB_ISARRAY( xMessage )
      cMessage := ""
      FOR nEval := 1 TO Len( xMessage )
         cMessage += iif( nEval == 1, "", Chr( 10 ) ) + hb_CStr( xMessage[ nEval ] )
      NEXT
   ELSEIF HB_ISSTRING( xMessage )
      cMessage := StrTran( xMessage, ";", Chr( 10 ) )
   ELSE
      cMessage := hb_CStr( xMessage )
   ENDIF

   hb_default( @aOptions, {} )

   IF ! HB_ISSTRING( cColorNorm ) .OR. Empty( cColorNorm )
      cColorNorm := "W+/R" // first pair color (Box line and Text)
      cColorHigh := "W+/B" // second pair color (Options buttons)
   ELSE
      cColorHigh := StrTran( StrTran( iif( At( "/", cColorNorm ) == 0, "N", SubStr( cColorNorm, At( "/", cColorNorm ) + 1 ) ) + "/" + ;
         iif( At( "/", cColorNorm ) == 0, cColorNorm, Left( cColorNorm, At( "/", cColorNorm ) - 1 ) ), "+", "" ), "*", "" )
   ENDIF

   aOptionsOK := {}
   FOR EACH cOption IN aOptions
      IF HB_ISSTRING( cOption ) .AND. ! Empty( cOption )
         AAdd( aOptionsOK, cOption )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { "Ok" }
#ifdef HB_CLP_STRICT
   ELSEIF Len( aOptionsOK ) > 4 /* NOTE: Clipper allows only four options [vszakats] */
      ASize( aOptionsOK, 4 )
#endif
   ENDIF

   RETURN hb_gtAlert( cMessage, aOptionsOK, cColorNorm, cColorHigh, nDelay )

#ifdef HB_CLP_UNDOC

PROCEDURE __NoNoAlert()

   s_lNoAlert := .F.

   RETURN

#endif
