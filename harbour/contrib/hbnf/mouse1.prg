/*
 * $Id$
 */

THREAD static s_lCrsState:=.F.
THREAD static s_lMinit:=.F.

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

     IF nRow == NIL
        nRow := MAXROW()+1
     ELSE
        nRow := VAL(nRow)
     ENDIF

     IF nCol == NIL
        nCol := MAXCOL()+1
     ELSE
        nCol := VAL(nCol)
     ENDIF

     if  !FT_MINIT()
        @ maxrow(), 0 say "Mouse driver is not installed!"

        return ""
     endif

     * ..... Set up the screen
     cSavClr := setcolor( "w/n" )
     @ 0,0,maxrow(),maxcol() box hb_UTF8ToStr( "░░░░░░░░░" )

     setcolor( "GR+/RB" )
//     scroll( 7,2,19,63,0 )
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

        DO WHILE nX == 0 .AND. nY == 0
             FT_MMICKEYS( @nX, @nY )
        ENDDO
* tell the mouse driver where updates will be taking place so it can hide
* the cursor when necessary.

        FT_MCONOFF( 9, 23, 16, 53 )
        nTime := -1

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

FUNCTION FT_MMICKEYS( nX, nY ) // read mouse motion counters
/*
   aReg[AX] := 11                // set mouse function call 11
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   Local areturn
   areturn:=_mget_mics()
   nX := areturn[1]               // store horizontal motion units
   nY := areturn[2]               // store vertical motion units

RETURN NIL                     // no function output

FUNCTION FT_MDBLCLK( nClick, nButton, nInterval, nRow, nCol, nStart )

LOCAL nVert, nHorz  // local row and col coordinates
LOCAL lDouble       // double click actually occurred
LOCAL lDone          // loop flag
LOCAL nPrs           // number of presses which occurred

* Initialize any empty arguments

   if nClick==NIL
      nClick:=1
   endif

   if nButton==NIL
        nButton:=0
   endif

   if nRow==NIL
       nRow:=FT_MGETX()
   endif

   if nCol==NIL
       nCol:=FT_MGETY()
   endif

   if nInterval==NIL
       nInterval:=0.5
   endif

   if nStart==NIL
       nStart:=seconds()
   endif

   nVert:=nRow
   nHorz:=nCol
   lDouble:=lDone:=nClick==0

   // Wait for first press if requested

   do while !lDone

           FT_MBUTPRS( nButton, @nPrs, @nVert, @nHorz )
           nVert:=INT(nVert/8)
           nHorz:=INT(nHorz/8)

           lDouble:=(nPrs>0)
           ldone:= seconds() - nStart >= nInterval .or. lDouble

   enddo

   // if we have not moved then keep the preliminary double click setting

   lDouble:=lDouble.and.(nVert==nRow.and.nHorz==nCol)

   // change start time if we waited for first click. nInterval is the
   // maximum time between clicks not the total time for two clicks if
   // requested.

   if nClick>0
      nStart:=seconds()
   endif

   // If we have fulfilled all of the requirements then wait for second click

   if lDouble

      lDouble:=lDone:=.F.

      do while !lDone

           FT_MBUTPRS( nButton, @nPrs, @nVert, @nHorz )
           nVert:=INT(nVert/8)
           nHorz:=INT(nHorz/8)

           lDouble:=(nPrs>0)
           lDone:= seconds() - nStart >= nInterval .or. lDouble

      enddo

  // make sure we haven't moved

      lDouble:=lDouble.and.(nVert==nRow.and.nHorz==nCol)

   endif

RETURN lDouble

FUNCTION FT_MCONOFF( nTop, nLeft, nBottom, nRight )

* Fill the registers

/*
   aReg[AX]:=16
   aReg[DX]:=nTop*8
   aReg[CX]:=nLeft*8
   aReg[DI]:=nBottom*8
   aReg[SI]:=nRight*8
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   _mse_conoff(nTop*8,nLeft*8,nBottom*8,nRight*8)
RETURN NIL

FUNCTION FT_MINREGION( nTR, nLC, nBR, nRC )
RETURN ( FT_MGETX() >= nTR .and. FT_MGETX() <= nBR .and. ;
         FT_MGETY() >= nLC .and. FT_MGETY() <= nRC )

FUNCTION FT_MSETSENS(nHoriz, nVert, nDouble)
LOCAL nCurHoriz, nCurVert, nCurDouble

// Get current values

FT_MGETSENS(@nCurHoriz, @nCurVert, @nCurDouble)

// Set defaults if necessary

IF !( VALTYPE( nHoriz ) == "N" )
    nHoriz := nCurHoriz
ENDIF

IF !( VALTYPE( nVert ) == "N" )
    nVert := nCurVert
ENDIF

IF !( VALTYPE( nDouble ) == "N" )
    nDouble := nCurDouble
ENDIF

* Fill the registers
_mset_sensitive(nHoriz,nVert,nDouble)

RETURN nil

FUNCTION FT_MGETSENS(nHoriz, nVert, nDouble)
/*
* Fill the register

aReg[AX]:=27

* Execute interupt

FT_INT86( 51, aReg )        // execute mouse interrupt

*/
// Set the return values

nHoriz := _mget_horispeed()
nVert  := _mget_verspeed()
nDouble:= _mget_doublespeed()

RETURN NIL

FUNCTION FT_MVERSION(nMinor, nType, nIRQ)
Local aReturn
// Set up register
/*
aReg[AX] := 36

// Call interupt

FT_INT86( 51, aReg)
*/
// decode out of half registers
areturn:=_mget_mversion()

nMinor := areturn[1]
nType  := areturn[2]
nIRQ   := areturn[3]

// Return

RETURN areturn[4]

FUNCTION FT_MSETPAGE(nPage)

// Set up register
/*
aReg[AX] := 29
aReg[BX] := nPage

// Call interupt

FT_INT86( 51, aReg)
*/
_mset_page(nPage)
RETURN NIL

FUNCTION FT_MGETPAGE()

// Set up register
/*
aReg[AX] := 30

// Call interupt

FT_INT86( 51, aReg)
*/
RETURN _mget_page()

FUNCTION FT_MINIT()

* If not previously initialized then try

   IF !s_lMinit
      s_lMinit := ( FT_MRESET() != 0 )
   ELSE
* Reset maximum x and y limits

      FT_MYLIMIT(0,8*24)
      FT_MXLIMIT(0,8*80)
   ENDIF

RETURN s_lMinit

FUNCTION FT_MRESET()
LOCAL lStatus
/*
   aReg[AX] := 0          // set mouse function call 0
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */
   s_lCrsState:=.F.         // Cursor is off after reset
lStatus:=_m_reset()
* Reset maximum x and y limits

   FT_MYLIMIT(0,8*MAXROW())
   FT_MXLIMIT(0,8*MAXCOL())

RETURN lStatus          // return status code

FUNCTION FT_MCURSOR( lState )
   local lSavState := s_lCrsState

   if VALTYPE(lState)=="L"
      if ( s_lCrsState := lState )
         FT_MSHOWCRS()
      else
         FT_MHIDECRS()
      endif
   ENDIF

RETURN lSavState

FUNCTION FT_MSHOWCRS()
   /*
   aReg[AX] := 1         // set mouse function call 1
   FT_INT86( 51, aReg ) // execute mouse interrupt
   */
      _mse_showcurs()
   s_lCrsState := .t.

RETURN NIL              // no output from function

FUNCTION FT_MHIDECRS()   // decrement internal cursor flag and hide cursor
/*
   aReg[AX] := 2         // set mouse function call 2
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */
   _mse_mhidecrs()
   s_lCrsState := .f.
RETURN NIL               // no output from function

FUNCTION FT_MGETPOS( nX, nY )
   Local amse
   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )
/*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   amse:=_mse_getpos()

   nX := amse[1]               // store new x-coordinate
   nY := amse[2]               // store new y-coordinate

RETURN amse[3]                 // return button status

FUNCTION FT_MGETX()

* Duplicated code from FT_MGETPOS() for speed reasons
/*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
*/
RETURN( _m_getx()/8 )       // return x-coordinate

FUNCTION FT_MGETY()

* Duplicated code from FT_MGETPOS() for speed reasons
   /*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
 */
RETURN( _m_gety()/8)        // return y-coordinate

FUNCTION FT_MSETPOS( nX, nY )  // set mouse cursor location
/*
   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY               // assign new x-coordinate
   aReg[DX] := nX               // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt
  */
  _m_msetpos(nY,nX)
RETURN NIL                     // no function output

FUNCTION FT_MSETCOORD( nX, nY )  // set mouse cursor location
/*
   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY*8             // assign new x-coordinate
   aReg[DX] := nX*8             // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   _m_MSETCOORD(nY*8,nX*8)
RETURN NIL                     // no function output

FUNCTION FT_MXLIMIT( nXMin, nXMax )   // set vertical minimum and maximum coordinates
/*
   aReg[AX] := 7                        // set mouse function call 7
   aReg[CX] := nXMin                    // load vertical minimum parameter
   aReg[DX] := nXMax                    // load vertical maximum parameter
   FT_INT86( 51, aReg )               // execute mouse interrupt
   */
    _m_mxlimit(nXMin,nXMAX)
RETURN NIL

FUNCTION FT_MYLIMIT( nYMin, nYMax )  // set horizontal minimum and maximum coordinates
/*
   aReg[AX] := 8                       // set mouse function call 8
   aReg[CX] := nYMin                   // load horz minimum parameter
   aReg[DX] := nYMax                   // load horz maximum parameter
   FT_INT86( 51, aReg )              // execute mouse interrupt
   */
  _m_mYlimit(nYMin,nYMAX)
RETURN NIL                           // no function output

FUNCTION FT_MBUTPRS( nButton, nButPrs, nX, nY ) // get button press information
local aReg:={}
/*
   aReg[AX] := 5               // set mouse function call 5
   aReg[BX] := nButton         // pass parameter for left or right button
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   nButPrs := aReg[1] // store updated press count
   nX := aReg[2]      // x-coordinate at last press
   nY := aReg[3]      // y-coordinate at last press

_m_MBUTPRS(nButton)
RETURN aReg[4]                 // return button status

FUNCTION FT_MBUTREL( nButton, nButRel, nX, nY ) // get button release information
local areg
Local iButton
   areg:=_m_MBUTREL(nButton)
   nButRel := aReg[1]  // store updated release count
   nX := aReg[2]      // x-coordinate at last release
   nY := aReg[3]      // y-coordinate at last release
   iButton:=   aReg[4]                 // return button status

RETURN iButton

FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks
/*
   aReg[AX] := 10         // set mouse function call 10
   aReg[BX] := nCurType   // load cursor type parameter
   aReg[CX] := nScrMask   // load screen mask value
   aReg[DX] := nCurMask   // load cursor mask value
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */
_m_mdefcrs(nCurType, nScrMask, nCurMask )
RETURN NIL              // no function output

FUNCTION FT_MGETCOORD( nX, nY )

* Duplicated code from FT_MGETPOS() for speed reasons
local aReg
local iButton
   nX := iif( nX == NIL, 0, nX )
   nY := iif( nY == NIL, 0, nY )
      /*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )         // execute mouse interrupt
   */
   areg:=_m_mgetcoord()
   nX := INT(aReg[1]/8)        // store new x-coordinate
   nY := INT(aReg[2]/8)        // store new y-coordinate
   iButton:= aReg[3]                 // return button status

RETURN iButton
