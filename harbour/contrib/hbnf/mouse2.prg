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

THREAD static aReg[10]
THREAD static lCrsState:=.F.
THREAD static lMinit:=.F.

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

     IF nRow==NIL
         nRow:=MAXROW()+1
     ELSE
         nRow:=VAL(nRow)
     ENDIF

     IF nCol==NIL
         nCol:=MAXCOL()+1
     ELSE
         nCol:=VAL(nCol)
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
     @ 0,0,maxrow(),maxcol() box hb_UTF8ToStr( "░░░░░░░░░" )

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

        DO WHILE nX==0.AND.nY==0
             FT_MMICKEYS( @nX, @nY )
        ENDDO
* tell the mouse driver where updates will be taking place so it can hide
* the cursor when necessary.

        FT_MCONOFF( 9, 23, 16, 53 )
        nTime:=-1

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

FUNCTION FT_MINIT()

* If not previously initialized then try

   IF !lMinit
     lMinit := (FT_MRESET()!=0)
   ELSE
* Reset maximum x and y limits

     FT_MYLIMIT(0,8*MAXROW())
     FT_MXLIMIT(0,8*MAXCOL())
   ENDIF

RETURN lMinit

FUNCTION FT_MRESET()

   aReg[AX] := 0        // set mouse function call 0
   FT_INT86( 51, aReg ) // execute mouse interrupt
   lCrsState := .F.     // Cursor is off after reset

* Reset maximum x and y limits

   FT_MYLIMIT(0,8*MAXROW())
   FT_MXLIMIT(0,8*MAXCOL())

RETURN aReg[AX]         // return status code

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

FUNCTION FT_MSHOWCRS()

   aReg[AX] := 1         // set mouse function call 1
   FT_INT86( 51, aReg ) // execute mouse interrupt
   lCrsState := .t.

RETURN NIL              // no output from function

FUNCTION FT_MHIDECRS()  // decrement internal cursor flag and hide cursor

   aReg[AX] := 2        // set mouse function call 2
   FT_INT86( 51, aReg ) // execute mouse interrupt
   lCrsState := .f.
RETURN NIL              // no output from function

FUNCTION FT_MGETPOS( nX, nY )

   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
   nX := aReg[DX]               // store new x-coordinate
   nY := aReg[CX]               // store new y-coordinate

RETURN aReg[BX]                 // return button status

FUNCTION FT_MGETCOORD( nX, nY )

* Duplicated code from FT_MGETPOS() for speed reasons

   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )         // execute mouse interrupt
   nX := INT(aReg[DX]/8)        // store new x-coordinate
   nY := INT(aReg[CX]/8)        // store new y-coordinate

RETURN aReg[BX]                 // return button status

FUNCTION FT_MGETX()

* Duplicated code from FT_MGETPOS() for speed reasons

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN( INT(aReg[DX]/8) )       // return x-coordinate

FUNCTION FT_MGETY()

* Duplicated code from FT_MGETPOS() for speed reasons

   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN( INT(aReg[CX]/8))        // return y-coordinate

FUNCTION FT_MSETPOS( nX, nY )  // set mouse cursor location

   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY               // assign new x-coordinate
   aReg[DX] := nX               // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN NIL                     // no function output

FUNCTION FT_MSETCOORD( nX, nY )  // set mouse cursor location

   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY*8             // assign new x-coordinate
   aReg[DX] := nX*8             // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt

RETURN NIL                     // no function output

FUNCTION FT_MXLIMIT( nXMin, nXMax )   // set vertical minimum and maximum coordinates

   aReg[AX] := 7                        // set mouse function call 7
   aReg[CX] := nXMin                    // load vertical minimum parameter
   aReg[DX] := nXMax                    // load vertical maximum parameter
   FT_INT86( 51, aReg )               // execute mouse interrupt

RETURN NIL

FUNCTION FT_MYLIMIT( nYMin, nYMax )  // set horizontal minimum and maximum coordinates

   aReg[AX] := 8                       // set mouse function call 8
   aReg[CX] := nYMin                   // load horz minimum parameter
   aReg[DX] := nYMax                   // load horz maximum parameter
   FT_INT86( 51, aReg )              // execute mouse interrupt

RETURN NIL                           // no function output

FUNCTION FT_MBUTPRS( nButton, nButPrs, nX, nY ) // get button press information

   aReg[AX] := 5              // set mouse function call 5
   aReg[BX] := nButton        // pass parameter for left or right button
   FT_INT86( 51, aReg )       // execute mouse interrupt
   nButPrs := aReg[BX] // store updated press count
   nX := aReg[DX]      // x-coordinate at last press
   nY := aReg[CX]      // y-coordinate at last press

RETURN aReg[AX]               // return button status

FUNCTION FT_MBUTREL( nButton, nButRel, nX, nY ) // get button release information

   aReg[AX] := 6                // set mouse function call 6
   aReg[BX] := nButton          // pass parameter for left or right button
   FT_INT86( 51, aReg )        // execute mouse interrupt
   nButRel := aReg[BX]  // store updated release count
   nX := aReg[DX]      // x-coordinate at last release
   nY := aReg[CX]      // y-coordinate at last release

RETURN aReg[AX]                 // return button status

  /*
FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks

   aReg[AX] := 10        // set mouse function call 10
   aReg[BX] := nCurType  // load cursor type parameter
   aReg[CX] := nScrMask  // load screen mask value
   aReg[DX] := nCurMask  // load cursor mask value
   FT_INT86( 51, aReg ) // execute mouse interrupt

RETURN NIL              // no function output

*/
