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
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    Changes for higher Clipper compatibility, console mode, extensions
 *    __NONOALERT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbsetup.ch"

#include "box.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* TOFIX: Clipper defines a clipped window for Alert() [vszakats] */

/* NOTE: Clipper will return NIL if the first parameter is not a string, but
         this is not documented. This implementation converts the first 
         parameter to a string if another type was passed. You can switch back 
         to Clipper compatible mode by defining constant
         HB_C52_STRICT. [vszakats] */

/* NOTE: Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
         This is fixed. [vszakats] */

/* NOTE: nDelay parameter is a Harbour extension. */

#ifdef HB_C52_UNDOC
STATIC s_lNoAlert
#endif

FUNCTION Alert( xMessage, aOptions, cColorNorm, nDelay )
   LOCAL nChoice
   LOCAL aSay, nPos, nWidth, nOpWidth, nInitRow, nInitCol, nEval
   LOCAL nKey, aPos, nCurrent, aHotkey, aOptionsOK
   LOCAL cColorHigh

   LOCAL nOldRow
   LOCAL nOldCol
   LOCAL nOldCursor
   LOCAL cOldScreen

   LOCAL nOldDispCount
   LOCAL nCount

#ifdef HB_COMPAT_C53
   LOCAL nMRow, nMCol
#endif

   /* TOFIX: Clipper decides at runtime, whether the GT is linked in,
             if it is not, the console mode is choosen here. [vszakats] */
   LOCAL lConsole := .F.

#ifdef HB_C52_UNDOC

   DEFAULT s_lNoAlert TO hb_argCheck( "NOALERT" )

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   aSay := {}

#ifdef HB_C52_STRICT

   IF !ISCHARACTER( xMessage )
      RETURN NIL
   ENDIF

   DO WHILE ( nPos := At( ';', xMessage ) ) != 0
      AAdd( aSay, Left( xMessage, nPos - 1 ) )
      xMessage := SubStr( xMessage, nPos + 1 )
   ENDDO
   AAdd( aSay, xMessage )

#else

   IF PCount() == 0
      RETURN NIL
   ENDIF

   IF ISARRAY( xMessage )

      FOR nEval := 1 TO Len( xMessage )
         IF ISCHARACTER( xMessage[ nEval ] )
            AAdd( aSay, xMessage[ nEval ] )
         ENDIF
      NEXT

   ELSE

      DO CASE
      CASE ValType( xMessage ) $ "CM" /* Do nothing, just speed up things */
      CASE ValType( xMessage ) == "N" ; xMessage := LTrim( Str( xMessage ) )
      CASE ValType( xMessage ) == "D" ; xMessage := DToC( xMessage )
      CASE ValType( xMessage ) == "L" ; xMessage := iif( xMessage, ".T.", ".F." )
      CASE ValType( xMessage ) == "O" ; xMessage := xMessage:className + " Object"
      CASE ValType( xMessage ) == "B" ; xMessage := "{||...}"
      OTHERWISE                       ; xMessage := "NIL"
      ENDCASE

      DO WHILE ( nPos := At( ';', xMessage ) ) != 0
         AAdd( aSay, Left( xMessage, nPos - 1 ) )
         xMessage := SubStr( xMessage, nPos + 1 )
      ENDDO
      AAdd( aSay, xMessage )

   ENDIF

#endif

   IF !ISARRAY( aOptions )
      aOptions := {}
   ENDIF

   IF !ISCHARACTER( cColorNorm )
      cColorNorm := "W+/R"
      cColorHigh := "W+/B"
   ELSE
      cColorHigh := StrTran( StrTran( iif( At( "/", cColorNorm ) == 0, "N", SubStr( cColorNorm, At( "/", cColorNorm ) + 1 ) ) + "/" +;
                                      iif( At( "/", cColorNorm ) == 0, cColorNorm, Left( cColorNorm, At( "/", cColorNorm ) - 1 ) ), "+", "" ), "*", "" )
   ENDIF

   IF nDelay == NIL
      nDelay := 0
   ENDIF

   /* The longest line */
   nWidth := 0
   AEval( aSay, {| x | nWidth := Max( Len( x ), nWidth ) } )

   /* Cleanup the button array */
   aOptionsOK := {}
   FOR nEval := 1 TO Len( aOptions )
      IF ISCHARACTER( aOptions[ nEval ] ) .AND. !Empty( aOptions[ nEval ] )
         AAdd( aOptionsOK, aOptions[ nEval ] )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { 'Ok' }
#ifdef HB_C52_STRICT
   /* NOTE: Clipper allows only four options [vszakats] */
   ELSEIF Len( aOptionsOK ) > 4
      aSize( aOptionsOK, 4 )
#endif
   ENDIF

   /* Total width of the botton line (the one with choices) */
   nOpWidth := 0
   AEval( aOptionsOK, {| x | nOpWidth += Len( x ) + 4 } )

   /* what's wider ? */
   nWidth := Max( nWidth + 2 + iif( Len( aSay ) == 1, 4, 0 ), nOpWidth + 2 )

   /* box coordinates */
   nInitRow := Int( ( ( MaxRow() - ( Len( aSay ) + 4 ) ) / 2 ) + .5 )
   nInitCol := Int( ( ( MaxCol() - ( nWidth + 2 ) ) / 2 ) + .5 )

   /* detect prompts positions */
   aPos := {}
   aHotkey := {}
   nCurrent := nInitCol + Int( ( nWidth - nOpWidth ) / 2 ) + 2
   AEval( aOptionsOK, {| x | AAdd( aPos, nCurrent ), AAdd( aHotKey, Upper( Left( x, 1 ) ) ), nCurrent += Len( x ) + 4 } )

   nChoice := 1

   IF lConsole

      FOR nEval := 1 TO Len( aSay )
         OutStd( aSay[ nEval ] )
         IF nEval < Len( aSay )
            OutStd( hb_OSNewLine() )
         ENDIF
      NEXT

      OutStd( " (" )
      FOR nEval := 1 TO Len( aOptionsOK )
         OutStd( aOptionsOK[ nEval ] )
         IF nEval < Len( aOptionsOK )
            OutStd( ", " )
         ENDIF
      NEXT
      OutStd( ") " )

      /* choice loop */
      DO WHILE .T.

         nKey := Inkey( nDelay, INKEY_ALL )

         DO CASE

         CASE nKey == 0

            EXIT

         CASE nKey == K_ESC

            nChoice := 0
            EXIT

         CASE aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } ) > 0

            nChoice := aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } )
            EXIT

         ENDCASE

      ENDDO

      OutStd( Chr( nKey ) )

   ELSE

      /* PreExt */
      nCount := nOldDispCount := DispCount()

      DO WHILE nCount-- != 0
         DispEnd()
      ENDDO

      /* save status */
      nOldRow := Row()
      nOldCol := Col()
      nOldCursor := SetCursor( SC_NONE )
      cOldScreen := SaveScreen( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1 )

      /* draw box */
      DispBox( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1, B_SINGLE + ' ', cColorNorm )

      FOR nEval := 1 TO Len( aSay )
         DispOutAt( nInitRow + nEval, nInitCol + 1 + Int( ( ( nWidth - Len( aSay[ nEval ] ) ) / 2 ) + .5 ), aSay[ nEval ], cColorNorm )
      NEXT

      /* choice loop */
      DO WHILE .T.

         FOR nEval := 1 TO Len( aOptionsOK )
            DispOutAt( nInitRow + Len( aSay ) + 2, aPos[ nEval ], " " + aOptionsOK[ nEval ] + " ",;
               iif( nEval == nChoice, cColorHigh, cColorNorm ) )
         NEXT

         nKey := Inkey( nDelay, INKEY_ALL )

         DO CASE
         CASE nKey == K_ENTER .OR. ;
              nKey == K_SPACE .OR. ;
              nKey == 0

            EXIT

         CASE nKey == K_ESC

            nChoice := 0
            EXIT

#ifdef HB_COMPAT_C53

         CASE nKey == K_LBUTTONDOWN

            nMRow := MRow()
            nMCol := MCol()

            FOR nEval := 1 TO Len( aOptionsOK )
               IF nMRow == nInitRow + Len( aSay ) + 2 .AND. ;
                  nMCol >= aPos[ nEval ] .AND. nMCol <= aPos[ nEval ] + ;
                  Len( aOptionsOK[ nEval ] ) + 2 - 1
                  nChoice := nEval
                  EXIT
               ENDIF
            NEXT

            IF nChoice == nEval
               nChoice := 0
               EXIT
            ENDIF

#endif

         CASE ( nKey == K_LEFT .OR. nKey == K_SH_TAB ) .AND. Len( aOptionsOK ) > 1

            nChoice--
            IF nChoice == 0
               nChoice := Len( aOptionsOK )
            ENDIF

            nDelay := 0

         CASE ( nKey == K_RIGHT .OR. nKey == K_TAB ) .AND. Len( aOptionsOK ) > 1

            nChoice++
            IF nChoice > Len( aOptionsOK )
               nChoice := 1
            ENDIF

            nDelay := 0

         CASE aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } ) > 0

            nChoice := aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } )
            EXIT

         ENDCASE

      ENDDO

      /* Restore status */
      RestScreen( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1, cOldScreen )
      SetCursor( nOldCursor )
      SetPos( nOldRow, nOldCol )

      /* PostExt */
      DO WHILE nOldDispCount-- != 0
         DispBegin()
      ENDDO

   ENDIF

   RETURN nChoice

#ifdef HB_C52_UNDOC

PROCEDURE __NONOALERT()

   s_lNoAlert := .F.

   RETURN

#endif

