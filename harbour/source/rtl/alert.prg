/* 
 * $Id$
 */

/*
   Harbour Project source code
   www - http://www.Harbour-Project.org

   Written by Vladimir Kazimirchik <v_kazimirchik@yahoo.com>
   http://i.am/kzm
   Clipper compatibility additions by Victor Szel <info@szelvesz.hu>

   Released into public domain.
*/

#include "box.ch"
#include "inkey.ch"

// ; TOFIX: Clipper defines a clipped window for Alert()
// ; Clipper will return NIL if the first parameter is not a string, but
//   this is not documented. This implementation converts the first parameter
//   to a string if another type was passed. You can switch back to
//   Clipper compatible mode by defining constant
//   HARBOUR_STRICT_CLIPPER_COMPATIBILITY.
// ; Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
//   This is fixed.
// ; nDelay parameter is a Harbour addition.

FUNCTION Alert( xMessage, aOptions, cColorNorm, nDelay )
   LOCAL nChoice
   LOCAL aSay, nPos, nWidth, nOpWidth, nInitRow, nInitCol, iEval
   LOCAL nKey, aPos, nCurrent, aHotkey, aOptionsOK
   LOCAL cColorHigh

   LOCAL nOldRow
   LOCAL nOldCol
   LOCAL nOldCursor
   LOCAL cOldScreen

   LOCAL nOldDispCount
   LOCAL nCount

   /* TOFIX: Clipper decides at runtime, whether the GT is linked in, */
   /*        if it is not, the console mode is choosed here */
   LOCAL lConsole := .F.

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
// TODO: Enable this when we have a function for querying the command line
//       parameters.
// IF "//NOALERT" $ /* Upper( cCommandLine ) */
//    QUIT
// ENDIF
#endif

   aSay := {}

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY

   IF !( ValType( xMessage ) == "C" )
      RETURN NIL
   ENDIF

   DO WHILE ( nPos := At( ';', xMessage ) ) != 0
      AAdd( aSay, Left( xMessage, nPos - 1 ) )
      xMessage := SubStr( xMessage, nPos + 1 )
   ENDDO
   AAdd( aSay, xMessage )

#else

   IF ValType( xMessage ) == "A"

      FOR iEval := 1 TO Len( xMessage )
         IF ValType( xMessage[ iEval ] ) == "C"
            AAdd( aSay, xMessage[ iEval ] )
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

   IF !( ValType( aOptions ) == "A" )
      aOptions := {}
   ENDIF

   IF !( ValType( cColorNorm ) == "C" )
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
   FOR iEval := 1 TO Len( aOptions )
      IF ValType( aOptions[ iEval ] ) == "C" .AND. !Empty( aOptions[ iEval ] )
         AAdd( aOptionsOK, aOptions[ iEval ] )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { 'Ok' }
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
   // ; Clipper allows only four options
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

   IF lConsole

      FOR iEval := 1 TO Len( aSay )
         OutStd( aSay[ iEval ] )
         IF iEval < Len( aSay )
            OutStd( Chr( 13 ) + Chr( 10 ) )
         ENDIF
      NEXT

      OutStd( " (" )
      FOR iEval := 1 TO Len( aOptionsOK )
         OutStd( aOptionsOK[ iEval ] )
         IF iEval < Len( aOptionsOK )
            OutStd( ", " )
         ENDIF
      NEXT
      OutStd( ") " )

   ELSE

      /* PreExt */
      nCount := nOldDispCount := DispCount()

      DO WHILE nCount-- != 0
         DispEnd()
      ENDDO

      /* save status */
      nOldRow := Row()
      nOldCol := Col()
      nOldCursor := SetCursor( 0 )
      cOldScreen := SaveScreen( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1 )

      /* draw box */
      @ nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1  ;
            BOX B_SINGLE + ' ' COLOR cColorNorm

      FOR iEval := 1 TO Len( aSay )
         @ nInitRow + iEval, nInitCol + 1 + Int( ( ( nWidth - Len( aSay[ iEval ] ) ) / 2 ) + .5 ) SAY aSay[ iEval ] ;
               COLOR cColorNorm
      NEXT

   ENDIF

   nChoice := 1

   /* choice loop */
   DO WHILE .T.

      IF !lConsole
         FOR iEval := 1 TO Len( aOptionsOK )
            @ nInitRow + Len( aSay ) + 2, aPos[ iEval ] SAY " " + aOptionsOK[ iEval ] + " " ;
                  COLOR iif( iEval == nChoice, cColorHigh, cColorNorm )
         NEXT
      ENDIF

      nKey := Inkey( nDelay )

      DO CASE
      CASE nKey == K_ENTER .OR. nKey == 0

         EXIT

      CASE nKey == K_ESC

         nChoice := 0
         EXIT

      CASE ( nKey == K_LEFT .OR. nKey == K_SH_TAB ) .AND. Len( aOptionsOK ) > 1

         nChoice--
         IF nChoice == 0
            nChoice := Len( aOptionsOK )
         ENDIF

      CASE ( nKey == K_RIGHT .OR. nKey == K_TAB ) .AND. Len( aOptionsOK ) > 1

         nChoice++
         IF nChoice > Len( aOptionsOK )
            nChoice := 1
         ENDIF

      CASE aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } ) > 0

         nChoice := aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } )
         EXIT

      ENDCASE

   ENDDO

   IF lConsole

      OutStd( Chr( nKey ) )

   ELSE

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
