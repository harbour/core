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
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    Documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbsetup.ch"

#include "box.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"

// ; TOFIX: Clipper defines a clipped window for Alert() [vszakats]
// ; Clipper will return NIL if the first parameter is not a string, but
//   this is not documented. This implementation converts the first parameter
//   to a string if another type was passed. You can switch back to
//   Clipper compatible mode by defining constant
//   HARBOUR_STRICT_CLIPPER_COMPATIBILITY. [vszakats]
// ; Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
//   This is fixed. [vszakats]
// ; nDelay parameter is a Harbour extension.

STATIC s_lNoAlert := NIL

/*  $DOC$
 *  $FUNCNAME$
 *      ALERT()
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Display a dialog box with a message
 *  $SYNTAX$
 *      ALERT( <xMessage>, [<aOptions>], [<cColorNorm>],
 *             [<nDelay>] ) --> nChoice or NIL
 *  $ARGUMENTS$
 *      <xMessage> Message to display in the dialog box. <xMessage> can be
 *      of any Harbour type.
 *      If <xMessage> is an array of Character strings, each element would
 *      be displayed in a new line. If <xMessage> is a Character
 *      string, you could split the message to several lines by placing
 *      a semicolon (;) in the desired places.
 *
 *      <aOptions> Array with available response. Each element should be
 *      Character string. If omitted, default is { "Ok" }.
 *
 *      <cColorNorm> Color string to paint the dialog box with.
 *      If omitted, default color is "W+/R".
 *
 *      <nDelay> Number of seconds to wait to user response before abort.
 *      Default value is 0, that wait forever.
 *  $RETURNS$
 *      ALERT() return Numeric value representing option number chosen.
 *      If ESC was pressed, return value is zero. The return value is NIL
 *      if ALERT() is called with no parameters, or if <xMessage> type is
 *      not Character and HARBOUR_STRICT_CLIPPER_COMPATIBILITY option was
 *      used. If <nDelay> seconds had passed without user response, the
 *      return value is 1.
 *  $DESCRIPTION$
 *      ALERT() display simple dialog box on screen and let the user select
 *      one option. The user can move the highlight bar using arrow keys or
 *      TAB key. To select an option the user can press ENTER, SPACE or the
 *      first letter of the option.
 *
 *      If the program is executed with the //NOALERT command line switch,
 *      nothing is displayed and it simply returns NIL. This switch could
 *      be overridden with __NONOALERT().
 *
 *      If the GT system is linked in, ALERT() display the message using
 *      the full screen I/O system, if not, the information is printed to
 *      the standard output using OUTSTD().
 *  $EXAMPLES$
 *      LOCAL cMessage, aOptions, nChoice
 *
 *      // harmless message
 *      cMessage := "Major Database Corruption Detected!;" +  ;
 *                  "(deadline in few hours);;"             +  ;
 *                  "where DO you want to go today?"
 *
 *      // define response option
 *      aOptions := { "Ok", "www.jobs.com", "Oops" }
 *
 *      // show message and let end user select panic level
 *      nChoice := ALERT( cMessage, aOptions )
 *      DO CASE
 *         CASE nChoice == 0
 *              // do nothing, blame it on some one else
 *         CASE nChoice == 1
 *              ? "Please call home and tell them you're gonn'a be late"
 *         CASE nChoice == 2
 *              // make sure your resume is up to date
 *         CASE nChoice == 3
 *              ? "Oops mode is not working in this version"
 *      ENDCASE
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      This function is sensitive to HARBOUR_STRICT_CLIPPER_COMPATIBILITY
 *      settings.
 *
 *      ON:  <xMessage> accept Character values only and return NIL if other
 *           types are passed,
 *      OFF: <xMessage> could be any type, and internally converted to
 *           Character string. If type is Array, multi-line message is
 *           displayed.
 *
 *      ON:  Only the first four valid <aOptions> are taken.
 *      OFF: <aOptions> could contain as many as needed options.
 *
 *      <cColorNorm> is a Harbour extension, or at least un-documented
 *      in Clipper 5.2 NG.
 *
 *      <nDelay> is a Harbour extension.
 *  $SEEALSO$
 *       @...PROMPT,MENU TO, STDOUT(),__NONOALERT()
 *  $END$
 */

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

   LOCAL nMRow, nMCol

   /* TOFIX: Clipper decides at runtime, whether the GT is linked in, */
   /*        if it is not, the console mode is choosen here. [vszakats] */
   LOCAL lConsole := .F.

   DEFAULT s_lNoAlert TO __argCheck( "NOALERT" )

   IF s_lNoAlert
      RETURN NIL
   ENDIF

   aSay := {}

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY

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
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
   // ; Clipper allows only four options [vszakats]
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

/*  $DOC$
 *  $FUNCNAME$
 *      __NONOALERT()
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Override //NOALERT command line switch
 *  $SYNTAX$
 *      __NONOALERT() --> NIL
 *  $ARGUMENTS$
 *      This function takes no arguments.
 *  $RETURNS$
 *      __NONOALERT() always return NIL.
 *  $DESCRIPTION$
 *      The //NOALERT command line switch cause Clipper to ignore calls to
 *      the ALERT() function, this function override this behavior
 *      and always display ALERT() dialog box.
 *  $EXAMPLES$
 *      // make sure alert are been displayed
 *      __NONOALERT()
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __NONOALERT() is an undocumented CA-Clipper function
 *  $SEEALSO$
 *  $END$
 */

PROCEDURE __NONOALERT()

   s_lNoAlert := .F.

   RETURN
