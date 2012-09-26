/*
 * $Id$
 */

/*
 * File......: mouse1.prg
 * Author....: Leo Letendre
 * CIS ID....: 73607,233
 *
 * This is an original work by Robert DiFalco and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.9   17 Oct 1992 16:28:58   GLENN
 * Leo cleaned up documentation blocks.
 *
 *    Rev 1.8   28 Sep 1992 01:38:14   GLENN
 * Leo added FT_MGETSENS(), FT_MSETSENS(), FT_MSETPAGE(), FT_MGETPAGE(),
 * and FT_MVERSION().
 *
 *
 *    Rev 1.7   01 Jul 1992 01:45:18   GLENN
 * Leo added documentation to FT_MDEFCRS and others. Added FT_MCONOFF(),
 * FT_MINIT(), FT_MGETCOORD() and FT_MSETCOORD().  Restructured
 * FT_MGETX() and FT_MGETY() for possible speed improvement and MAIN to
 * better demonstrate some of the concerns when programming the mouse.
 * Added ability to change the number of rows and columns to demonstrate
 * use in 43, 50 row mode etc. which is now supported in FT_MRESET() and
 * FT_MINIT().
 *
 *    Rev 1.6   23 Sep 1991 01:14:38   GLENN
 * Corrected errors in syntax documention for FT_MBUTPRS() and FT_MDBLCLK(),
 * found by Nantucket's Steve Silverwood.
 *
 *    Rev 1.5   17 Aug 1991 15:34:52   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.4   15 Aug 1991 23:06:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.3   17 Jul 1991 22:28:40   GLENN
 * Leo fixed a potential bug in ft_mcursor().
 *
 *    Rev 1.2   27 May 1991 13:40:30   GLENN
 * Leo Letendre sent me a revision of mouse1.prg where he built in support
 * for a three-button mouse, and revised the "double click" detection
 * algorithm.
 *
 * Brought in compliance with new ft_int86().
 *
 *    Rev 1.1   11 May 1991 00:16:48   GLENN
 * ft_mgetpos() had a bug where the x and y coordinates were reversed.
 * Changed x coordinate to aRegs[3] and y coordinate to aRegs[4], just
 * like in ft_msetpos().
 *
 *    Rev 1.0   01 Apr 1991 01:01:48   GLENN
 * Nanforum Toolkit
 *
 */

// The original mouse routines were written by Robert diFalco but
// Leo Letendre has made such major additions and modifications
// and fixes that I've given him sole credit. -- G. Scott

#include "ftint86.ch"

THREAD STATIC t_aReg[ 10 ]
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

   IF ! SetMode( nRow, nCol )
      @MaxRow(), 0 SAY "Mode Change unsuccessful:" + Str( nRow, 2, 0 ) + " by";
         + Str( nCol, 3, 0 )
      RETURN NIL
   ENDIF

   IF Empty( FT_MINIT() )
      @ MaxRow(), 0 SAY "Mouse driver is not installed!"
      SetMode( nSaveRow, nSaveCol )
      RETURN ""
   ENDIF

// ..... Set up the screen
   cSavClr := SetColor( "w/n" )
   @ 0, 0, MaxRow(), MaxCol() BOX hb_UTF8ToStr( "░░░░░░░░░" )

   SetColor( "GR+/RB" )
   Scroll( 7, 2, 19, 63, 0 )
   @ 7, 2 TO 20, 63

   @ 17, 10 TO 19, 40 double

   SetColor( "N/W" )
   @ 18, 11 SAY "  Double Click here to Quit  "

   SetColor( "GR+/RB" )

// ..... Start the demo

   @MaxRow(), 0 SAY "Driver version: " + ;
      AllTrim( Str( FT_MVERSION(@nMinor,@nType,@nIRQ ),2,0 ) ) + "." + ;
      AllTrim( Str( nMinor,2,0 ) )
   @ Row(), Col() SAY " " + aType[nType] + " mouse using IRQ " + Str( nIRQ, 1, 0 )

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

FUNCTION FT_MINIT()

// If not previously initialized then try

   IF !t_lMinit
      t_lMinit := ( FT_MRESET() != 0 )
   ELSE
      // Reset maximum x and y limits

      FT_MYLIMIT( 0, 8 * MaxRow() )
      FT_MXLIMIT( 0, 8 * MaxCol() )
   ENDIF

   RETURN t_lMinit

FUNCTION FT_MRESET()

   t_aReg[ AX ] := 0        // set mouse function call 0
   FT_INT86( 51, t_aReg ) // execute mouse interrupt
   t_lCrsState := .F.     // Cursor is off after reset

// Reset maximum x and y limits

   FT_MYLIMIT( 0, 8 * MaxRow() )
   FT_MXLIMIT( 0, 8 * MaxCol() )

   RETURN t_aReg[ AX ]         // return status code

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

   t_aReg[ AX ] := 1         // set mouse function call 1
   FT_INT86( 51, t_aReg ) // execute mouse interrupt
   t_lCrsState := .T.

   RETURN NIL              // no output from function

FUNCTION FT_MHIDECRS()  // decrement internal cursor flag and hide cursor

   t_aReg[ AX ] := 2        // set mouse function call 2
   FT_INT86( 51, t_aReg ) // execute mouse interrupt
   t_lCrsState := .F.

   RETURN NIL              // no output from function

FUNCTION FT_MGETPOS( nX, nY )

   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )

   t_aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, t_aReg )        // execute mouse interrupt
   nX := t_aReg[ DX ]               // store new x-coordinate
   nY := t_aReg[ CX ]               // store new y-coordinate

   RETURN t_aReg[ BX ]                 // return button status

FUNCTION FT_MGETCOORD( nX, nY )

// Duplicated code from FT_MGETPOS() for speed reasons

   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )

   t_aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, t_aReg )         // execute mouse interrupt
   nX := Int( t_aReg[ DX ] / 8 )        // store new x-coordinate
   nY := Int( t_aReg[ CX ] / 8 )        // store new y-coordinate

   RETURN t_aReg[ BX ]                 // return button status

FUNCTION FT_MGETX()

// Duplicated code from FT_MGETPOS() for speed reasons

   t_aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, t_aReg )        // execute mouse interrupt

   RETURN Int( t_aReg[ DX ] / 8 )       // return x-coordinate

FUNCTION FT_MGETY()

// Duplicated code from FT_MGETPOS() for speed reasons

   t_aReg[ AX ] := 3                // set mouse function call 3
   FT_INT86( 51, t_aReg )        // execute mouse interrupt

   RETURN Int( t_aReg[ CX ] / 8 )       // return y-coordinate

FUNCTION FT_MSETPOS( nX, nY )  // set mouse cursor location

   t_aReg[ AX ] := 4                // set mouse function call 4
   t_aReg[ CX ] := nY               // assign new x-coordinate
   t_aReg[ DX ] := nX               // assign new y-coordinate
   FT_INT86( 51, t_aReg )        // execute mouse interrupt

   RETURN NIL                     // no function output

FUNCTION FT_MSETCOORD( nX, nY )  // set mouse cursor location

   t_aReg[ AX ] := 4                // set mouse function call 4
   t_aReg[ CX ] := nY * 8             // assign new x-coordinate
   t_aReg[ DX ] := nX * 8             // assign new y-coordinate
   FT_INT86( 51, t_aReg )        // execute mouse interrupt

   RETURN NIL                     // no function output

FUNCTION FT_MXLIMIT( nXMin, nXMax )   // set vertical minimum and maximum coordinates

   t_aReg[ AX ] := 7                        // set mouse function call 7
   t_aReg[ CX ] := nXMin                    // load vertical minimum parameter
   t_aReg[ DX ] := nXMax                    // load vertical maximum parameter
   FT_INT86( 51, t_aReg )               // execute mouse interrupt

   RETURN NIL

FUNCTION FT_MYLIMIT( nYMin, nYMax )  // set horizontal minimum and maximum coordinates

   t_aReg[ AX ] := 8                       // set mouse function call 8
   t_aReg[ CX ] := nYMin                   // load horz minimum parameter
   t_aReg[ DX ] := nYMax                   // load horz maximum parameter
   FT_INT86( 51, t_aReg )              // execute mouse interrupt

   RETURN NIL                           // no function output

FUNCTION FT_MBUTPRS( nButton, nButPrs, nX, nY ) // get button press information

   t_aReg[ AX ] := 5              // set mouse function call 5
   t_aReg[ BX ] := nButton        // pass parameter for left or right button
   FT_INT86( 51, t_aReg )       // execute mouse interrupt
   nButPrs := t_aReg[ BX ] // store updated press count
   nX := t_aReg[ DX ]      // x-coordinate at last press
   nY := t_aReg[ CX ]      // y-coordinate at last press

   RETURN t_aReg[ AX ]               // return button status

FUNCTION FT_MBUTREL( nButton, nButRel, nX, nY ) // get button release information

   t_aReg[ AX ] := 6                // set mouse function call 6
   t_aReg[ BX ] := nButton          // pass parameter for left or right button
   FT_INT86( 51, t_aReg )        // execute mouse interrupt
   nButRel := t_aReg[ BX ]  // store updated release count
   nX := t_aReg[ DX ]      // x-coordinate at last release
   nY := t_aReg[ CX ]      // y-coordinate at last release

   RETURN t_aReg[ AX ]                 // return button status

  /*
FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks

   t_aReg[ AX ] := 10        // set mouse function call 10
   t_aReg[ BX ] := nCurType  // load cursor type parameter
   t_aReg[ CX ] := nScrMask  // load screen mask value
   t_aReg[ DX ] := nCurMask  // load cursor mask value
   FT_INT86( 51, t_aReg ) // execute mouse interrupt

RETURN NIL              // no function output

*/
