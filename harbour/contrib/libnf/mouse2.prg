/*
 * File......: MOUSE1.PRG
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
 * Leo Letendre sent me a revision of MOUSE1.PRG where he built in support
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

static aReg[10]
static lCrsState:=.F.
static lMinit:=.F.

#ifdef FT_TEST

  FUNCTION MAIN(nRow,nCol)

* Pass valid row and column values for different video modes to change modes

     local nX, nY, cSavClr
     local cSavScr := savescreen( 0, 0, maxrow(), maxcol() )
     local nXm, nYm
     local nSaveRow:=MAXROW()+1, nSaveCol:=MAXCOL()+1
     local nMinor, nType, nIRQ
     local aType:={"Bus","Serial","InPort","PS/2","HP"}
     local nHoriz, nVert, nDouble
     local nTime

	IF nRow=NIL
		nRow=MAXROW()+1
	ELSE
		nRow=VAL(nRow)
	ENDIF

	IF nCol=NIL
		nCol=MAXCOL()+1
	ELSE
		nCol=VAL(nCol)
	ENDIF

	IF .NOT.SETMODE(nRow,nCol)
		@maxrow(),0 SAY "Mode Change unsuccessful:"+STR(nRow,2,0)+" by";
			+STR(nCol,3,0)
		RETURN NIL
	ENDIF

     if empty( FT_MINIT() )
        @ maxrow(), 0 say "Mouse driver is not installed!"
        SETMODE(nSaveRow,nSaveCol)
        return ""
     endif

     * ..... Set up the screen
     cSavClr := setcolor( "w/n" )
     @ 0,0,maxrow(),maxcol() box "°°°°°°°°°"

     setcolor( "GR+/RB" )
     scroll( 7,2,19,63,0 )
     @ 7,2 to 20,63

     @ 17, 10 to 19, 40 double

     setcolor( "N/W" )
     @ 18, 11 say "  Double Click here to Quit  "

     setcolor( "GR+/RB" )

     * ..... Start the demo

	@MAXROW(),0 SAY "Driver version: "+;
		ALLTRIM(STR(FT_MVERSION(@nMinor,@nType,@nIRQ),2,0))+"."+;
		ALLTRIM(STR(nMinor,2,0))
     @ ROW(),COL() SAY " "+aType[nType]+" mouse using IRQ "+STR(nIRQ,1,0)

     FT_MGETSENS(@nHoriz,@nVert,@nDouble)  // Get the current sensitivities
	FT_MSETSENS(70,70,60)    // Bump up the sensitivity of the mouse

     FT_MSHOWCRS()
     FT_MSETCOORD(10,20)  // just an arbitrary place for demo

* put the unchanging stuff

     devpos( 9, 10 )
     devout( "FT_MMICKEYS :" )

     devpos( 10, 10 )
     devout( "FT_MGETPOS  :" )

     devpos( 11, 10 )
     devout( "FT_MGETX    :" )

     devpos( 12, 10 )
     devout( "FT_MGETY    :")

     devpos( 13, 10 )
     devout( "FT_MGETCOORD:" )

     devpos( 14, 10 )
     devout( "FT_MBUTPRS  :" )

     devpos( 16, 10 )
     devout( "FT_MBUTREL  :" )

     nX := nY := 1
     do while .t.

* If we are not moving then wait for movement.
* This whole demo is a bit artificial in its requirements when compared
* to a "normal" CLIPPER program so some of these examples are a bit out of
* the ordinary.

        DO WHILE nX=0.AND.nY=0
             FT_MMICKEYS( @nX, @nY )
        ENDDO
* tell the mouse driver where updates will be taking place so it can hide
* the cursor when necessary.

        FT_MCONOFF( 9, 23, 16, 53 )
        nTime=-1

        devpos( 9, 23 )
        devout( nX )
        devout( nY )

        devpos( 10, 23 )
        DEVOUT( FT_MGETPOS( @nX, @nY ) )
        devout( nX )
        devout( nY )

        devpos( 11, 23 )
        DEVOUT(  FT_MGETX() )

        devpos( 12, 23 )
        DEVOUT( FT_MGETY() )

        devpos( 13, 23 )
        devout( FT_MGETCOORD( @nX, @nY ) )
        devout ( nX )
        devout ( nY )

        nX:=nY:=0
        devpos( 14, 23 )
        DEVOUT( FT_MBUTPRS(1) )
        DEVOUT( FT_MBUTPRS(0,, nX, nY) )
        devpos( 15, 23 )

* show only the last Press since it flashes by so quickly

        IF nX!=0.OR.nY!=0
             devout( nX )
             devout( nY )
        endif

        nX:=nY:=0
        devpos( 16, 23 )
        devout( FT_MBUTREL(0,, @nX, @nY) )

* show only the last release since it flashes by so quickly

        if nX!=0.OR.nY!=0
             devout( nX )
             devout( nY )
        endif

* Restore the cursor if it has been hidden

        FT_MSHOWCRS()

        if FT_MINREGION( 18, 11, 18, 39 )

* Change the type of cursor when in the box. Just slightly different than the
* normal. The character is shown in high intensity.

           FT_MDEFCRS(0,32767,32512)
           if FT_MDBLCLK(2,0,0.8)
              exit
           endif
        endif

        if FT_MINREGION( 18, 11, 18, 39 )

* Change the type of cursor when in the box. Just slightly different than the
* normal. The character is shown in high intensity.

           FT_MDEFCRS(0,32767,32512)
        else

* Put the cursor back to normal mode

           FT_MDEFCRS(0,30719,30464)
        endif

        FT_MMICKEYS( @nX, @nY )
     enddo

     FT_MHIDECRS()

     SETMODE(nSaveRow,nSaveCol)
     setcolor( cSavClr )
     restscreen( 0, 0, maxrow(), maxcol(), cSavScr )
     devpos( maxrow(), 0 )

// Reset sensitivity

     FT_MSETSENS(nHoriz, nVert, nDouble)

  RETURN nil


#endif



/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MINIT()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Initialize the mouse driver, vars and return status of mouse
 * $SYNTAX$
 *    FT_MINIT() -> lMouseStatus
 * $ARGUMENTS$
 *    NONE
 * $RETURNS$
 *    An logical representing the mouse status (.F. == mouse not installed)
 * $DESCRIPTION$
 *    Initializes the mouse drive, associated variables and returns mouse
 *    status. It checks to see if the mouse has been previously initialized
 *    and if so it does not reinitialize. The row and column limits of mouse
 *    movement is set to the maximum for the current video mode.
 *    Use FT_MSHOWCRS() to display the mouse cursor.
 * $EXAMPLES$
 *    IF .NOT. FT_MINIT()
 *       ? "No mouse driver is installed"
 *    ENDIF
 * $SEEALSO$
 *    FT_MRESET()
 * $END$
 */

FUNCTION FT_MINIT()

* If not previously initialized then try

   IF !lMinit
	lMinit=(FT_MRESET()!=0)
   ELSE
* Reset maximum x and y limits

     FT_MYLIMIT(0,8*MAXROW())
     FT_MXLIMIT(0,8*MAXCOL())
   ENDIF


RETURN lMinit

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MRESET()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Reset mouse driver and return status of mouse
 * $SYNTAX$
 *    FT_MRESET() -> nMouseStatus
 * $ARGUMENTS$
 *    NONE
 * $RETURNS$
 *    An integer representing the mouse status (0 == mouse not installed)
 * $DESCRIPTION$
 *    Resets the mouse driver and returns mouse status. Use FT_MSHOWCRS()
 *    to display the mouse cursor. The mouse is set to allow it to cover the
 *    complete screen (as defined by MAXCOL() and MAXROW()). This is necessary
 *    because at least some versions of the mouse drivers do not operate
 *    according to the documentation when confronted with a 43 or 50 line
 *    screen.
 *
 *    Normally, FT_MINIT() should be used to initialize the mouse since it
 *    will not reinitialize if already done.
 * $EXAMPLES$
 *    IF Empty( FT_MRESET() )
 *       ? "No mouse driver is installed"
 *    ENDIF
 *  $SEEALSO$
 *     FT_MINIT() FT_MSHOWCRS()
 * $END$
 */

FUNCTION FT_MRESET()

   aReg[AX] := 0          // set mouse function call 0
   FT_INT86( 51, aReg )  // execute mouse interrupt
   lCrsState=.F.         // Cursor is off after reset

* Reset maximum x and y limits

   FT_MYLIMIT(0,8*MAXROW())
   FT_MXLIMIT(0,8*MAXCOL())

RETURN aReg[AX] 	       // return status code


/* $DOC$
 * $FUNCNAME$
 *    FT_MCURSOR()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Set the mouse cursor
 * $SYNTAX$
 *    FT_MCURSOR( [ <lState> ] ) -> lCursorState
 * $ARGUMENTS$
 *    <lState> is a logical indicating whether to set the mouse cursor on.
 *             .T. - set mouse cursor on
 *             .F. - set mouse cursor off
 *             If omitted, no change is made to cursor state
 * $RETURNS$
 *    A logical indicating the previous mouse cursor state.
 * $DESCRIPTION$
 *    This function works like most Clipper state functions.  If no value
 *    is sent to FT_MCURSOR() it will return the state of the mouse cursor.
 * $EXAMPLES$
 *    IF !( FT_MCURSOR() )
 *       FT_MCURSOR( .T. )
 *    ENDIF
 * $END$
 */

FUNCTION FT_MCURSOR( lState )
   local lSavState := lCrsState

   if VALTYPE(lState)="L"
      if ( lCrsState := lState )
         FT_MSHOWCRS()
      else
         FT_MHIDECRS()
      endif
   ENDIF

RETURN lSavState


/* $DOC$
 * $FUNCNAME$
 *    FT_MSHOWCRS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Increment internal cursor flag and display mouse cursor
 * $SYNTAX$
 *    FT_MSHOWCRS() -> NIL
 * $ARGUMENTS$
 *    NONE
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    Displays the mouse cursor. Make sure to turn the mouse cursor off
 *    when redrawing screens.  The mouse cursor dutifully saves the screen
 *    under it, so if you draw over the mouse cursor it will create a "hole"
 *    in your screen when you move the mouse cursor.
 *
 *    Note: A call to FT_MHIDECRS() decrements a mouse driver variable
 *    which indicates whether the cursor is shown. The cursor is visible
 *    only when the variable = 0. Thus multiple calls to FT_MHIDECRS()
 *    require an equal number of calls to FT_MSHOWCRS() before the cursor
 *    will again be visible. Once the variable is 0 calls to FT_MSHOWCRS()
 *    does not increment the variable above 0.
 * $EXAMPLES$
 *    IF Empty( FT_MRESET() )
 *       FT_MSHOWCRS()
 *    ENDIF
 *  $SEEALSO$
 *     FT_MHIDECRS() FT_MCONOFF()
 * $END$
 */

FUNCTION FT_MSHOWCRS()

   aReg[AX] := 1         // set mouse function call 1
   FT_INT86( 51, aReg ) // execute mouse interrupt
   lCrsState := .t.

RETURN NIL              // no output from function



/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MHIDECRS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Decrement internal mouse cursor flag and hide mouse cursor
 * $SYNTAX$
 *    FT_MHIDECRS() -> NIL
 * $ARGUMENTS$
 *    NONE
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    Hides the mouse cursor. Make sure to turn the mouse cursor off when
 *    redrawing screens.  The mouse cursor dutifully saves the screen
 *    under it, so if you draw over the mouse cursor it will create a
 *    "hole" in your screen when you move the mouse cursor.
 *
 *    Note: A call to FT_MHIDECRS() decrements a mouse driver variable
 *    which indicates whether the cursor is shown. The cursor is visible
 *    only when the variable = 0. Thus multiple calls to FT_MHIDECRS()
 *    require an equal number of calls to FT_MSHOWCRS() before the cursor
 *    will again be visible. Once the variable is 0 calls to FT_MSHOWCRS()
 *    does not increment the varaible above 0.
 * $EXAMPLES$
 *    FT_MHIDECRS()
 *    @ 10, 10 to 20, 20
 *    FT_MSHOWCRS()
 *  $SEEALSO$
 *    FT_MSHOWCRS() FT_MCONOFF()
 * $END$
 */



FUNCTION FT_MHIDECRS()   // decrement internal cursor flag and hide cursor

   aReg[AX] := 2          // set mouse function call 2
   FT_INT86( 51, aReg )  // execute mouse interrupt
   lCrsState := .f.
RETURN NIL		          // no output from function


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MGETPOS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get mouse cursor position and button status
 * $SYNTAX$
 *    FT_MGETPOS( @<nX>, @<nY> ) -> nButtonStatus
 * $ARGUMENTS$
 *    <nX> is a variable that will receive the mouse X position in virtual
 *    screen coordinates.  It must be passed by reference.
 *
 *    <nY> is a variable that will receive the mouse Y position in virtual
 *    screen coordinates.  It must be passed by reference.
 * $RETURNS$
 *    an integer representing button status
 *
 *          - 0 for no button pressed
 *          - 1 for left pressed
 *          - 2 for right pressed
 *          - 3 for left and right pressed
 *          - 4 for middle pressed
 *          - 5 for left and middle pressed
 *          - 6 for right and middle pressed
 *          - 7 for all three buttons pressed
 * $DESCRIPTION$
 *    Loads cursor position into x and y coordinates passed by reference and
 *    returns the button status. The coordinate system in text mode has
 *    eight virtual coordinates per character cell. Thus x=16 means that you
 *    are in the Row 2. The values returned by this routine when in text mode
 *    and with mouse driver versions 6 and above are multiples of 8. We have
 *    experience with drivers prior to that version
 * $EXAMPLES$
 *    LOCAL nX, nY
 *    LOCAL nButton := FT_MGETPOS( @nX, @nY )
 *    ? "Mouse Row    :", nX
 *    ? "Mouse Column :", nY
 *    ? "Button Status:", nButton
 *  $SEEALSO$
 *    FT_MGETCOORD() FT_MSETPOS() FT_MDEFCRS() FT_MGETX() FT_MGETY()
 * $END$
 */




FUNCTION FT_MGETPOS( nX, nY )

   nX := if( nX == NIL, 0, nX )
   nY := if( nY == NIL, 0, nY )

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
   nX := aReg[DX]               // store new x-coordinate
   nY := aReg[CX]               // store new y-coordinate

RETURN aReg[BX]                 // return button status

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MGETCOORD()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get mouse cursor position (text coord.) and button status
 * $SYNTAX$
 *    FT_MGETPOS( @<nX>, @<nY> ) -> nButtonStatus
 * $ARGUMENTS$
 *    <nX> is a variable that will receive the mouse X position in text
 *    screen coordinates.  It must be passed by reference.
 *
 *    <nY> is a variable that will receive the mouse Y position in text
 *    screen coordinates.  It must be passed by reference.
 * $RETURNS$
 *    an integer representing button status
 *
 *          - 0 for no button pressed
 *          - 1 for left pressed
 *          - 2 for right pressed
 *          - 3 for left and right pressed
 *          - 4 for middle pressed
 *          - 5 for left and middle pressed
 *          - 6 for right and middle pressed
 *          - 7 for all three buttons pressed
 * $DESCRIPTION$
 *    Loads cursor position into x and y coordinates passed by reference and
 *    returns the button status.
 * $EXAMPLES$
 *    LOCAL nX, nY
 *    LOCAL nButton := FT_MGETCOORD( @nX, @nY )
 *    ? "Mouse Row    :", nX
 *    ? "Mouse Column :", nY
 *    ? "Button Status:", nButton
 *  $SEEALSO$
 *    FT_MGETPOS() FT_MSETPOS() FT_MDEFCRS() FT_MGETX() FT_MGETY()
 * $END$
 */

FUNCTION FT_MGETCOORD( nX, nY )

* Duplicated code from FT_MGETPOS() for speed reasons

   nX := if( nX == NIL, 0, nX )
   nY := if( nY == NIL, 0, nY )

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )         // execute mouse interrupt
   nX := INT(aReg[DX]/8)        // store new x-coordinate
   nY := INT(aReg[CX]/8)        // store new y-coordinate

RETURN aReg[BX]                 // return button status


/* $DOC$
 * $FUNCNAME$
 *    FT_MGETX()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get mouse cursor row position
 * $SYNTAX$
 *    FT_MGETX() -> nRowPos
 * $ARGUMENTS$
 *    NONE
 * $RETURNS$
 *    <nRowPos> which is the row position of mouse in virtual screen
 *              coordinates.
 * $DESCRIPTION$
 *    Retrieves mouse's row position in virtual screen coordinates. The
 *    values returned are multiples of 8 when in text mode and with at least
 *    Microsoft drivers 6 and above.
 * $EXAMPLES$
 *    ? FT_MGETX()
 *  $SEEALSO$
 *     FT_MGETCOORD() FT_MDEFCRS() FT_MGETPOS() FT_MGETY()
 * $END$
 */

FUNCTION FT_MGETX()

* Duplicated code from FT_MGETPOS() for speed reasons

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN( INT(aReg[DX]/8) )       // return x-coordinate

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MGETY()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get mouse cursor column position
 * $SYNTAX$
 *    FT_MGETY() -> nColPos
 * $ARGUMENTS$
 *    NONE
 * $RETURNS$
 *    <nColPos> Column position of mouse in virtual screen coordinates
 * $DESCRIPTION$
 *    Retrieves mouse's column position in virtual screen coordinates. The
 *    values returned are multiples of 8 when in text mode and with at least
 *    Microsoft drivers 6 and above.
 * $EXAMPLES$
 *    ? FT_MGETY()
 *  $SEEALSO$
 *    FT_MGETCOORD() FT_MDEFCRS() FT_MGETPOS() FT_MGETX()
 * $END$
 */

FUNCTION FT_MGETY()

* Duplicated code from FT_MGETPOS() for speed reasons

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN( INT(aReg[CX]/8))        // return y-coordinate

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MSETPOS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Position the mouse cursor using virtual screen coordinates
 * $SYNTAX$
 *    FT_MSETPOS( <nX>, <nY> ) -> NIL
 * $ARGUMENTS$
 *    <nX> is the desired mouse row.
 *
 *    <nY> is the desired mouse column.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    Positions mouse cursor on screen. The virtual coordinate system in text
 *    mode has eight virtual coordinates per character cell. Thus x=16 means
 *    that you are in the Row 2.
 * $EXAMPLES$
 *    FT_MSETPOS( 10, 20 )     // position mouse cursor at row 10, col 20
 *                             // in virtual screen coordinates
 *  $SEEALSO$
 *    FT_MGETPOS() FT_MGETCOORD() FT_MSETCOORD() FT_MGETX() FT_MGETY()
 * $END$
 */

FUNCTION FT_MSETPOS( nX, nY )  // set mouse cursor location

   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY               // assign new x-coordinate
   aReg[DX] := nX               // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN NIL                     // no function output


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MSETCOORD()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Position the mouse cursor using text screen coordinates
 * $SYNTAX$
 *    FT_MSETPOS( <nX>, <nY> ) -> NIL
 * $ARGUMENTS$
 *    <nX> is the desired mouse row.
 *
 *    <nY> is the desired mouse column.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    Positions mouse cursor on screen using text (normal row and column)
 *    coordinates.
 * $EXAMPLES$
 *    FT_MSETCOORD( 10, 20 )     // position mouse cursor at row 10, col 20
 *                             // in text screen coordinates
 *  $SEEALSO$
 *    FT_MGETPOS() FT_MGETCOORD() FT_MSETPOS() FT_MDEFCRS() FT_MGETX() FT_MGETY()
 * $END$
 */

FUNCTION FT_MSETCOORD( nX, nY )  // set mouse cursor location

   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY*8             // assign new x-coordinate
   aReg[DX] := nX*8             // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN NIL                     // no function output

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MXLIMIT()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Set vertical bounds of mouse using virtual screen coord.
 * $SYNTAX$
 *    FT_MXLIMIT( <nX1>, <nX2> ) -> NIL
 * $ARGUMENTS$
 *    <nX1> is the top row limit.
 *
 *    <nX2> is the bottom row limit.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    Set maximum vertical bounds of mouse using virtual screen coordinates.
 * $EXAMPLES$
 *    FT_MXLIMIT( 10, 20 )
 *  $SEEALSO$
 *     FT_MYLIMIT() FT_MINREGION()
 * $END$
 */

FUNCTION FT_MXLIMIT( nXMin, nXMax )   // set vertical minimum and maximum coordinates

   aReg[AX] = 7                        // set mouse function call 7
   aReg[CX] = nXMin                    // load vertical minimum parameter
   aReg[DX] = nXMax                    // load vertical maximum parameter
   FT_INT86( 51, aReg )               // execute mouse interrupt

RETURN NIL

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MYLIMIT()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Set horiz. bounds of mouse using virtual screen coordinates
 * $SYNTAX$
 *    FT_MYLIMIT( <nY1>, <nY2> ) -> NIL
 * $ARGUMENTS$
 *    <nY1> is the left column limit.
 *
 *    <nY2> is the right column limit.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    Set maximum horizontal bounds of mouse using virtual screen coordinates.
 * $EXAMPLES$
 *    FT_MYLIMIT( 10, 20 )
 *  $SEEALSO$
 *     FT_MXLIMIT() FT_MINREGION()
 * $END$
 */

FUNCTION FT_MYLIMIT( nYMin, nYMax )  // set horizontal minimum and maximum coordinates

   aReg[AX] = 8                       // set mouse function call 8
   aReg[CX] = nYMin                   // load horz minimum parameter
   aReg[DX] = nYMax                   // load horz maximum parameter
   FT_INT86( 51, aReg )              // execute mouse interrupt

RETURN NIL                           // no function output


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MBUTPRS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Retrieve button press status
 * $SYNTAX$
 *    FT_MBUTPRS( <nButton> [, @nButPrs [, @nX [, @nY] ] ] ) -> nButStatus
 * $ARGUMENTS$
 *    <nButton> is the mouse button number:
 *
 *               0 - Left   Button
 *               1 - Right  Button
 *               2 - Middle Button [if applicable]
 *
 *    <nButPrs> is the number of times the specified button was pressed
 *              since the last call to this routine. PASSED BY REFERENCE.
 *    <nX> is the X position of the cursor when the last press occurred.
 *              PASSED BY REFERENCE.
 *    <nY> is the Y position of the cursor when the last press occurred.
 *              PASSED BY REFERENCE.
 *
 * $RETURNS$
 *    An integer representing the button status:
 *
 *               0 - no buttons pressed
 *               1 - left button pressed
 *               2 - right button pressed
 *               3 - left and right pressed
 *               4 - middle pressed
 *               5 - left and middle pressed
 *               6 - middle and right buttons pressed
 *               7 - all 3 buttons pressed
 * $DESCRIPTION$
 *    Retrieves the mouse button status and the position of the cursor when
 *    a button was last pressed.
 * $EXAMPLES$
 *    IF Empty( FT_MBUTPRS(1) )
 *       ? "No Item selected"
 *    ENDIF
 *  $SEEALSO$
 *     FT_MBUTREL() FT_MDBLCLK()
 * $END$
 */

FUNCTION FT_MBUTPRS( nButton, nButPrs, nX, nY ) // get button press information

   aReg[AX] := 5	           // set mouse function call 5
   aReg[BX] := nButton          // pass parameter for left or right button
   FT_INT86( 51, aReg )        // execute mouse interrupt
   nButPrs := aReg[BX] // store updated press count
   nX := aReg[DX]      // x-coordinate at last press
   nY := aReg[CX]      // y-coordinate at last press

RETURN aReg[AX]                 // return button status



/*  $DOC$
 *  $FUNCNAME$
 *     FT_MBUTREL()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Get mouse button release information
 *  $SYNTAX$
 *     FT_MBUTREL( nButton [, @nButRel [, @nX [, @nY] ] ]) -> nBStat
 *  $ARGUMENTS$
 *     <nButton> is the mouse button number
 *               0 - Left   Button
 *               1 - Right  Button
 *               2 - Middle Button [if applicable]
 *
 *    <nButRel> is the number of times the specified button was released
 *              since the last call to this routine. PASSED BY REFERENCE.
 *
 *    <nX> is the X position of the cursor when the last release occurred.
 *              PASSED BY REFERENCE.
 *
 *    <nY> is the Y position of the cursor when the last release occurred.
 *              PASSED BY REFERENCE.
 *  $RETURNS$
 *     <nBStat>  - an integer representing button release status
 *                 0 - None
 *                 1 - Left
 *                 2 - Right
 *                 3 - Middle
 *  $DESCRIPTION$
 *     This function returns the release status of the mouse buttons and the
 *     coordinates of the last release.
 *  $EXAMPLES$
 *     IF( FT_MBUTREL( 0 ) == 1 )
 *        ? "Left button released"
 *     ENDIF
 *  $SEEALSO$
 *     FT_MBUTPRS() FT_MDBLCLK()
 *  $END$
 */



FUNCTION FT_MBUTREL( nButton, nButRel, nX, nY ) // get button release information

   aReg[AX] := 6                // set mouse function call 6
   aReg[BX] := nButton          // pass parameter for left or right button
   FT_INT86( 51, aReg )        // execute mouse interrupt
   nButRel := aReg[BX]  // store updated release count
   nX := aReg[DX]      // x-coordinate at last release
   nY := aReg[CX]      // y-coordinate at last release

RETURN aReg[AX]                 // return button status


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MDEFCRS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Define the mouse cursor
 * $SYNTAX$
 *    FT_MDEFCRS( <nCrsType>, <nScrMask>, <nCrsMask> ) -> NIL
 * $ARGUMENTS$
 *    <nCrsType> is the cursor type. A value of 0 indicates the software cursor
 *               (the default) and a value of 1 indicates the hardware cursor.
 *
 *    <nScrMask> is the screen mask for the software cursor or the first scan
 *               line of the hardware cursor. See the description for more
 *               information.
 *
 *    <nCrsMask> is the cursor mask for the software cursor of the last scan
 *               line of the hardware cursor. See the description for more
 *               information.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    In text mode the mouse cursor can either be a software generated or
 *    the actual hardware cursor. This routine allows one choose between them.
 *    The software cursor is the default and its effect on the character it
 *    covers is determined by the screen mask and the cursor mask. Both of
 *    these masks are 16 bit values (which in Clipper are passed as standard
 *    numerical values). The 16 bit masks are arranged in a manner identical
 *    to the way information is stored for each character cell on the screen.
 *    The low order 8 bits represent the actual character displayed while the
 *    high order bits represent the display atributes such as blinking,
 *    intensity and forground and background colors. The mask is represented in
 *    the diagram below:
 *
 *    Bit:     ³15    ³14      12³11       ³10       8³7       0³
 *    Function:³blink ³background³intensity³foreground³character³
 *
 *    Blinking and high intensity are on when the bit is 1. The background and
 *    foreground indicate which colors are used for each. The software mouse
 *    cursor uses these two values by taking the mask from the screen cell it
 *    is on and performing a logical AND on each bit with the screen mask
 *    value. The result is then logically XOR'ed with the cursor mask value.
 *    Thus to keep the character the same but invert the foreground and
 *    background colors the following values would be used:
 *
 *    Bit:     ³15    ³14      12³11       ³10       8³7       0³
 *    Function:³blink ³background³intensity³foreground³character³
 *    screen:  ³  0   ³   111    ³    0    ³   111    ³11111111 ³ =30719
 *    cursor:  ³  0   ³   111    ³    0    ³   111    ³00000000 ³ =30464
 *
 *    The hardware cursor is the text cursor provided by the video board. One
 *    specifies the range of scan lines which are on using <nScrMask> and
 *    <nCrsMask>. The range of values is dependant upon the type of monitor.
 *    The first scan line is 0.
 * $END$
 */
  /*
FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks

   aReg[AX] = 10   	   // set mouse function call 10
   aReg[BX] = nCurType   // load cursor type parameter
   aReg[CX] = nScrMask   // load screen mask value
   aReg[DX] = nCurMask   // load cursor mask value
   FT_INT86( 51, aReg ) // execute mouse interrupt

RETURN NIL              // no function output

*/

