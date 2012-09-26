/*
 * $Id$
 */

THREAD STATIC t_lCrsState := .F.
THREAD STATIC t_lMinit := .F.

#ifdef FT_TEST

PROCEDURE Main( nRow, nCol )

// Pass valid row and column values for different video modes to change modes

   LOCAL nX, nY, cSavClr
   LOCAL cSavScr := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   LOCAL nXm, nYm
   LOCAL nSaveRow := MaxRow() + 1, nSaveCol := MaxCol() + 1
   LOCAL nMinor, nType, nIRQ
   LOCAL aType := { "Bus", "Serial", "InPort", "PS/2", "HP" }
   LOCAL nHoriz, nVert, nDouble
   LOCAL nTime

   IF nRow == NIL
      nRow := MaxRow() + 1
   ELSE
      nRow := Val( nRow )
   ENDIF

   IF nCol == NIL
      nCol := MaxCol() + 1
   ELSE
      nCol := Val( nCol )
   ENDIF

   IF  !FT_MINIT()
      @ MaxRow(), 0 SAY "Mouse driver is not installed!"

      RETURN ""
   ENDIF

// ..... Set up the screen
   cSavClr := SetColor( "w/n" )
   @ 0, 0, MaxRow(), MaxCol() BOX hb_UTF8ToStr( "░░░░░░░░░" )

   SetColor( "GR+/RB" )
//     scroll( 7,2,19,63,0 )
   @ 7, 2 TO 20, 63

   @ 17, 10 TO 19, 40 double

   SetColor( "N/W" )
   @ 18, 11 SAY "  Double Click here to Quit  "

   SetColor( "GR+/RB" )

   // ..... Start the demo

   @MaxRow(), 0 SAY "Driver version: " + ;
      AllTrim( Str( FT_MVERSION( @nMinor, @nType, @nIRQ ), 2, 0 ) ) + "." + ;
      AllTrim( Str( nMinor, 2, 0 ) )
   @ Row(), Col() SAY " " + aType[ nType ] + " mouse using IRQ " + Str( nIRQ, 1, 0 )

   FT_MGETSENS( @nHoriz, @nVert, @nDouble )  // Get the current sensitivities
   FT_MSETSENS( 70, 70, 60 )    // Bump up the sensitivity of the mouse

   FT_MSHOWCRS()
   FT_MSETCOORD( 10, 20 )  // just an arbitrary place for demo

   // put the unchanging stuff

   DevPos( 9, 10 )
   DevOut( "FT_MMICKEYS :" )

   DevPos( 10, 10 )
   DevOut( "FT_MGETPOS  :" )

   DevPos( 11, 10 )
   DevOut( "FT_MGETX    :" )

   DevPos( 12, 10 )
   DevOut( "FT_MGETY    :" )

   DevPos( 13, 10 )
   DevOut( "FT_MGETCOORD:" )

   DevPos( 14, 10 )
   DevOut( "FT_MBUTPRS  :" )

   DevPos( 16, 10 )
   DevOut( "FT_MBUTREL  :" )

   nX := nY := 1
   DO WHILE .T.

      // If we are not moving then wait for movement.
      // This whole demo is a bit artificial in its requirements when compared
      // to a "normal" CLIPPER program so some of these examples are a bit out of
      // the ordinary.

      DO WHILE nX == 0 .AND. nY == 0
         FT_MMICKEYS( @nX, @nY )
      ENDDO
      // tell the mouse driver where updates will be taking place so it can hide
      // the cursor when necessary.

      FT_MCONOFF( 9, 23, 16, 53 )
      nTime := - 1

      DevPos( 9, 23 )
      DevOut( nX )
      DevOut( nY )

      DevPos( 10, 23 )
      DevOut( FT_MGETPOS( @nX, @nY ) )
      DevOut( nX )
      DevOut( nY )

      DevPos( 11, 23 )
      DevOut( FT_MGETX() )

      DevPos( 12, 23 )
      DevOut( FT_MGETY() )

      DevPos( 13, 23 )
      DevOut( FT_MGETCOORD( @nX, @nY ) )
      DevOut( nX )
      DevOut( nY )

      nX := nY := 0
      DevPos( 14, 23 )
      DevOut( FT_MBUTPRS( 1 ) )
      DevOut( FT_MBUTPRS( 0,, nX, nY ) )
      DevPos( 15, 23 )

      // show only the last Press since it flashes by so quickly

      IF nX != 0 .OR. nY != 0
         DevOut( nX )
         DevOut( nY )
      ENDIF

      nX := nY := 0
      DevPos( 16, 23 )
      DevOut( FT_MBUTREL( 0,, @nX, @nY ) )

      // show only the last release since it flashes by so quickly

      IF nX != 0 .OR. nY != 0
         DevOut( nX )
         DevOut( nY )
      ENDIF

      // Restore the cursor if it has been hidden

      FT_MSHOWCRS()

      IF FT_MINREGION( 18, 11, 18, 39 )

         // Change the type of cursor when in the box. Just slightly different than the
         // normal. The character is shown in high intensity.

         FT_MDEFCRS( 0, 32767, 32512 )
         IF FT_MDBLCLK( 2, 0, 0.8 )
            EXIT
         ENDIF
      ENDIF

      IF FT_MINREGION( 18, 11, 18, 39 )

         // Change the type of cursor when in the box. Just slightly different than the
         // normal. The character is shown in high intensity.

         FT_MDEFCRS( 0, 32767, 32512 )
      ELSE

         // Put the cursor back to normal mode

         FT_MDEFCRS( 0, 30719, 30464 )
      ENDIF

      FT_MMICKEYS( @nX, @nY )
   ENDDO

   FT_MHIDECRS()

   SetMode( nSaveRow, nSaveCol )
   SetColor( cSavClr )
   RestScreen( 0, 0, MaxRow(), MaxCol(), cSavScr )
   DevPos( MaxRow(), 0 )

// Reset sensitivity

   FT_MSETSENS( nHoriz, nVert, nDouble )

   RETURN

#endif

FUNCTION FT_MMICKEYS( nX, nY ) // read mouse motion counters
/*
   aReg[ AX ] := 11                // set mouse function call 11
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */

   LOCAL areturn

   areturn := _mget_mics()
   nX := areturn[1]               // store horizontal motion units
   nY := areturn[2]               // store vertical motion units

   RETURN NIL                     // no function output

FUNCTION FT_MDBLCLK( nClick, nButton, nInterval, nRow, nCol, nStart )

   LOCAL nVert, nHorz  // local row and col coordinates
   LOCAL lDouble       // double click actually occurred
   LOCAL lDone          // loop flag
   LOCAL nPrs           // number of presses which occurred

// Initialize any empty arguments

   IF nClick == NIL
      nClick := 1
   ENDIF

   IF nButton == NIL
      nButton := 0
   ENDIF

   IF nRow == NIL
      nRow := FT_MGETX()
   ENDIF

   IF nCol == NIL
      nCol := FT_MGETY()
   ENDIF

   IF nInterval == NIL
      nInterval := 0.5
   ENDIF

   IF nStart == NIL
      nStart := Seconds()
   ENDIF

   nVert := nRow
   nHorz := nCol
   lDouble := lDone := nClick == 0

// Wait for first press if requested

   DO WHILE !lDone

      FT_MBUTPRS( nButton, @nPrs, @nVert, @nHorz )
      nVert := Int( nVert/8 )
      nHorz := Int( nHorz/8 )

      lDouble := ( nPrs > 0 )
      ldone := Seconds() - nStart >= nInterval .OR. lDouble

   ENDDO

// if we have not moved then keep the preliminary double click setting

   lDouble := lDouble .AND. ( nVert == nRow .AND. nHorz == nCol )

// change start time if we waited for first click. nInterval is the
// maximum time between clicks not the total time for two clicks if
// requested.

   IF nClick > 0
      nStart := Seconds()
   ENDIF

// If we have fulfilled all of the requirements then wait for second click

   IF lDouble

      lDouble := lDone := .F.

      DO WHILE !lDone

         FT_MBUTPRS( nButton, @nPrs, @nVert, @nHorz )
         nVert := Int( nVert/8 )
         nHorz := Int( nHorz/8 )

         lDouble := ( nPrs > 0 )
         lDone := Seconds() - nStart >= nInterval .OR. lDouble

      ENDDO

      // make sure we haven't moved

      lDouble := lDouble .AND. ( nVert == nRow .AND. nHorz == nCol )

   ENDIF

   RETURN lDouble

FUNCTION FT_MCONOFF( nTop, nLeft, nBottom, nRight )

// Fill the registers

/*
   aReg[ AX ]:=16
   aReg[ DX ]:=nTop*8
   aReg[ CX ]:=nLeft*8
   aReg[DI]:=nBottom*8
   aReg[SI]:=nRight*8
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   _mse_conoff( nTop * 8, nLeft * 8, nBottom * 8, nRight * 8 )

   RETURN NIL

FUNCTION FT_MINREGION( nTR, nLC, nBR, nRC )

   RETURN FT_MGETX() >= nTR .AND. FT_MGETX() <= nBR .AND. ;
      FT_MGETY() >= nLC .AND. FT_MGETY() <= nRC

FUNCTION FT_MSETSENS( nHoriz, nVert, nDouble )

   LOCAL nCurHoriz, nCurVert, nCurDouble

// Get current values

   FT_MGETSENS( @nCurHoriz, @nCurVert, @nCurDouble )

// Set defaults if necessary

   IF ! HB_ISNUMERIC( nHoriz )
      nHoriz := nCurHoriz
   ENDIF

   IF ! HB_ISNUMERIC( nVert )
      nVert := nCurVert
   ENDIF

   IF ! HB_ISNUMERIC( nDouble )
      nDouble := nCurDouble
   ENDIF

// Fill the registers
   _mset_sensitive( nHoriz, nVert, nDouble )

   RETURN nil

FUNCTION FT_MGETSENS( nHoriz, nVert, nDouble )
/*
* Fill the register

aReg[ AX ] := 27

* Execute interupt

FT_INT86( 51, aReg )        // execute mouse interrupt

*/

// Set the return values

   nHoriz := _mget_horispeed()
   nVert  := _mget_verspeed()
   nDouble := _mget_doublespeed()

   RETURN NIL

FUNCTION FT_MVERSION( nMinor, nType, nIRQ )

   LOCAL aReturn

// Set up register
/*
aReg[ AX ] := 36

// Call interupt

FT_INT86( 51, aReg)
*/
// decode out of half registers
   areturn := _mget_mversion()

   nMinor := areturn[ 1 ]
   nType  := areturn[ 2 ]
   nIRQ   := areturn[ 3 ]

// Return

   RETURN areturn[ 4 ]

FUNCTION FT_MSETPAGE( nPage )

// Set up register
/*
aReg[ AX ] := 29
aReg[ BX ] := nPage

// Call interupt

FT_INT86( 51, aReg)
*/
   _mset_page( nPage )

   RETURN NIL

FUNCTION FT_MGETPAGE()

// Set up register
/*
aReg[ AX ] := 30

// Call interupt

FT_INT86( 51, aReg)
*/

   RETURN _mget_page()

FUNCTION FT_MINIT()

// If not previously initialized then try

   IF !t_lMinit
      t_lMinit := ( FT_MRESET() != 0 )
   ELSE
      // Reset maximum x and y limits

      FT_MYLIMIT( 0, 8 * 24 )
      FT_MXLIMIT( 0, 8 * 80 )
   ENDIF

   RETURN t_lMinit

FUNCTION FT_MRESET()

   LOCAL lStatus
/*
   aReg[ AX ] := 0          // set mouse function call 0
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */

   t_lCrsState := .F.         // Cursor is off after reset
   lStatus := _m_reset()
// Reset maximum x and y limits

   FT_MYLIMIT( 0, 8 * MaxRow() )
   FT_MXLIMIT( 0, 8 * MaxCol() )

   RETURN lStatus          // return status code

FUNCTION FT_MCURSOR( lState )

   LOCAL lSavState := t_lCrsState

   IF HB_ISLOGICAL( lState )
      IF ( t_lCrsState := lState )
         FT_MSHOWCRS()
      ELSE
         FT_MHIDECRS()
      ENDIF
   ENDIF

   RETURN lSavState

FUNCTION FT_MSHOWCRS()
   /*
   aReg[ AX ] := 1         // set mouse function call 1
   FT_INT86( 51, aReg ) // execute mouse interrupt
   */

   _mse_showcurs()
   t_lCrsState := .T.

   RETURN NIL              // no output from function

FUNCTION FT_MHIDECRS()   // decrement internal cursor flag and hide cursor
/*
   aReg[ AX ] := 2         // set mouse function call 2
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */

   _mse_mhidecrs()
   t_lCrsState := .F.

   RETURN NIL               // no output from function

FUNCTION FT_MGETPOS( nX, nY )

   LOCAL amse

   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )
/*
   aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   amse := _mse_getpos()

   nX := amse[1]               // store new x-coordinate
   nY := amse[2]               // store new y-coordinate

   RETURN amse[3]                 // return button status

FUNCTION FT_MGETX()

// Duplicated code from FT_MGETPOS() for speed reasons
/*
   aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
*/

   RETURN _m_getx() / 8        // return x-coordinate

FUNCTION FT_MGETY()

// Duplicated code from FT_MGETPOS() for speed reasons
   /*
   aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
 */

   RETURN _m_gety() / 8         // return y-coordinate

FUNCTION FT_MSETPOS( nX, nY )  // set mouse cursor location
/*
   aReg[ AX ] := 4                // set mouse function call 4
   aReg[ CX ] := nY               // assign new x-coordinate
   aReg[ DX ] := nX               // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt
  */

   _m_msetpos( nY, nX )

   RETURN NIL                     // no function output

FUNCTION FT_MSETCOORD( nX, nY )  // set mouse cursor location
/*
   aReg[ AX ] := 4                // set mouse function call 4
   aReg[ CX ] := nY*8             // assign new x-coordinate
   aReg[ DX ] := nX*8             // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */

   _m_MSETCOORD( nY * 8, nX * 8 )

   RETURN NIL                     // no function output

FUNCTION FT_MXLIMIT( nXMin, nXMax )   // set vertical minimum and maximum coordinates
/*
   aReg[ AX ] := 7                        // set mouse function call 7
   aReg[ CX ] := nXMin                    // load vertical minimum parameter
   aReg[ DX ] := nXMax                    // load vertical maximum parameter
   FT_INT86( 51, aReg )               // execute mouse interrupt
   */

   _m_mxlimit( nXMin, nXMAX )

   RETURN NIL

FUNCTION FT_MYLIMIT( nYMin, nYMax )  // set horizontal minimum and maximum coordinates
/*
   aReg[ AX ] := 8                       // set mouse function call 8
   aReg[ CX ] := nYMin                   // load horz minimum parameter
   aReg[ DX ] := nYMax                   // load horz maximum parameter
   FT_INT86( 51, aReg )              // execute mouse interrupt
   */

   _m_mYlimit( nYMin, nYMAX )

   RETURN NIL                           // no function output

FUNCTION FT_MBUTPRS( nButton, nButPrs, nX, nY ) // get button press information

   LOCAL aReg := {}
/*
   aReg[ AX ] := 5               // set mouse function call 5
   aReg[ BX ] := nButton         // pass parameter for left or right button
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */

   nButPrs := aReg[ 1 ] // store updated press count
   nX := aReg[ 2 ]      // x-coordinate at last press
   nY := aReg[ 3 ]      // y-coordinate at last press

   _m_MBUTPRS( nButton )

   RETURN aReg[4 ]                 // return button status

FUNCTION FT_MBUTREL( nButton, nButRel, nX, nY ) // get button release information

   LOCAL areg
   LOCAL iButton

   areg := _m_MBUTREL( nButton )
   nButRel := aReg[ 1 ]  // store updated release count
   nX := aReg[ 2 ]      // x-coordinate at last release
   nY := aReg[ 3 ]      // y-coordinate at last release
   iButton :=   aReg[ 4 ]                 // return button status

   RETURN iButton

FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks
/*
   aReg[ AX ] := 10         // set mouse function call 10
   aReg[ BX ] := nCurType   // load cursor type parameter
   aReg[ CX ] := nScrMask   // load screen mask value
   aReg[ DX ] := nCurMask   // load cursor mask value
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */

   _m_mdefcrs( nCurType, nScrMask, nCurMask )

   RETURN NIL              // no function output

FUNCTION FT_MGETCOORD( nX, nY )

// Duplicated code from FT_MGETPOS() for speed reasons
   LOCAL aReg
   LOCAL iButton
   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )
      /*
   aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )           // execute mouse interrupt
   */
   areg := _m_mgetcoord()
   nX := Int( aReg[ 1 ] / 8 )        // store new x-coordinate
   nY := Int( aReg[ 2 ] / 8 )        // store new y-coordinate
   iButton := aReg[ 3 ]              // return button status

   RETURN iButton
