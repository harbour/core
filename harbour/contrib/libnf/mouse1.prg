
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

     if  !FT_MINIT() 
        @ maxrow(), 0 say "Mouse driver is not installed!"

        return ""
     endif

     * ..... Set up the screen
     cSavClr := setcolor( "w/n" )
     @ 0,0,maxrow(),maxcol() box "°°°°°°°°°"

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
 *    FT_MMICKEYS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get mickeys
 * $SYNTAX$
 *    FT_MMICKEYS( @<nX>, @<nY> ) -> NIL
 * $ARGUMENTS$
 *    <nX> is a variable that will receive the vertical mickey count.
 *
 *    <nY> is a variable that will receive the horizontal mickey count.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    <nX> and <nY> must be passed by reference to receive
 *    the mouse position in Mickeys.
 * $EXAMPLES$
 *    FT_MMICKEYS( @nX, @nY )
 *    ? nX
 *    ? nY
 * $END$
 */

FUNCTION FT_MMICKEYS( nX, nY ) // read mouse motion counters
/*
   aReg[AX] = 11                // set mouse function call 11
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   Local areturn:={}
   areturn:=_mget_mics()
   nX := areturn[1]               // store horizontal motion units
   nY := areturn[2]               // store vertical motion units

RETURN NIL                     // no function output


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MDBLCLK()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Return true if a double click was detected
 * $SYNTAX$
 *    FT_MDBLCLK( [ <nClick> [, <nButton> [, <nInterval> [, <nRow> [, <nCol>;
 *                [, <nTime> ] ] ] ] ] ] ) -> lIsDoubleClk
 * $ARGUMENTS$
 *    <nClick> is a numeric value.  If it is zero FT_MDBLCLK() will not
 *             check for the first press but rather will simply wait the
 *             specified period for a single press. This is useful if this
 *             routine is called from one which in turn responded to a button
 *             press. If it is not present or not equal to 0, then FT_MDBLCLK()
 *             will wait for two presses of the specified button.
 *
 *    <nButton> is the mouse button number
 *               0 - Left   Button
 *               1 - Right  Button
 *               2 - Middle Button [if applicable]
 *
 *    <nInterval> is the interval to wait for the first click if requested
 *           and the time to wait for the second. If not present then defaults
 *           to 0.5 second.
 *
 *    <nRow> is the row number for the mouse cursor location for a double click
 *           to be valid. If not present then the current position is taken as
 *           the valid location.
 *
 *    <nCol> is the column number for the mouse cursor location for a double
 *           click to be valid. If not present, then the current position is
 *           taken as the valid location.
 *
 *    <nTime> is an optional start time for the waiting period for the first
 *           click (of either one or two requested). If not given then the
 *           time is set at entry into this routine. This is useful when this
 *           routine is called from another routine which was called in
 *           response to a mouse click but needs to know if a double click
 *           has occurred
 * $RETURNS$
 *    .T. if a double click was detected.
 * $DESCRIPTION$
 *    This is a mouse meta function that checks for the presence
 *    of a double click.
 * $EXAMPLES$
 *   IF FT_MISREGION( 10, 10, 11, 20 ) .AND.;
 *      FT_MDBLCLK(0,1,,FT_MGETX(),FT_MGETY())  && double click, right button
 *                                              && at current location with
 *                                              && default interval
 *
 *         MnuItem1()
 *   ENDIF
 *  $SEEALSO$
 *     FT_MBUTPRS() FT_MBUTREL()
 * $END$
 */

FUNCTION FT_MDBLCLK( nClick, nButton, nInterval, nRow, nCol, nStart )

LOCAL nVert, nHorz  // local row and col coordinates
LOCAL lDouble:=.F.  // double click actually occurred
LOCAL lDone          // loop flag
LOCAL nPrs           // number of presses which occurred

* Initialize any empty arguments

   if nClick=NIL
      nClick=1
   endif

   if nButton=NIL
        nButton=0
   endif

   if nRow=NIL
       nRow=FT_MGETX()
   endif

   if nCol=NIL
       nCol=FT_MGETY()
   endif

   if nInterval=NIL
       nInterval=0.5
   endif

   if nStart=NIL
       nStart=seconds()
   endif

   nVert=nRow
   nHorz=nCol
   lDouble:=lDone:=nClick==0

   // Wait for first press if requested

   do while !lDone

           FT_MBUTPRS( nButton, @nPrs, @nVert, @nHorz )
           nVert=INT(nVert/8)
           nHorz=INT(nHorz/8)

           lDouble=(nPrs>0)
           ldone= seconds() - nStart >= nInterval .or. lDouble

   enddo

   // if we have not moved then keep the preliminary double click setting

   lDouble=lDouble.and.(nVert=nRow.and.nHorz=nCol)

   // change start time if we waited for first click. nInterval is the
   // maximum time between clicks not the total time for two clicks if
   // requested.

   if nClick>0
      nStart=seconds()
   endif

   // If we have fulfilled all of the requirements then wait for second click

   if lDouble

      lDouble:=lDone:=.F.

      do while !lDone

           FT_MBUTPRS( nButton, @nPrs, @nVert, @nHorz )
           nVert=INT(nVert/8)
           nHorz=INT(nHorz/8)

           lDouble=(nPrs>0)
           lDone= seconds() - nStart >= nInterval .or. lDouble

      enddo

  // make sure we haven't moved

      lDouble=lDouble.and.(nVert=nRow.and.nHorz=nCol)

   endif


RETURN lDouble

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MCONOFF()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Turn mouse cursur off if in specified region
 * $SYNTAX$
 *    FT_MCONOFF( <nTop>, <nLeft>, <nBottom>, <nRight> )
 * $ARGUMENTS$
 *    <nTop>, <nLeft> <nBottom> <nRight> are the four corners of the
 *          screen region in row and column coordinates.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    This function tells the mouse driver to hide the cursor if it is in
 *    the given region. The driver hides the cursor by decrementing the cursor
 *    flag. A call to FT_MSHOWCRS is required to turn the cursor back on.
 *    Calling FT_MSHOWCRS also disables this function.
 *
 *    See FT_MSHOWCRS for a discussion of the cursor display flag.
 * $EXAMPLES$
 *    FT_MCONOFF( 10, 10, 11, 20 )
 *  $SEEALSO$
 *     FT_MSHOWCRS() FT_MHIDECRS() FT_MXLIMIT() FT_MYLIMIT() FT_MINREGION()
 * $END$
 */

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

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MINREGION()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Test if the mouse cursor is in the passed region
 * $SYNTAX$
 *    FT_MINREGION( <nT>, <nL>, <nB>, <nR> ) -> lInRegion
 * $ARGUMENTS$
 *    <nT>, <nL> <nB> <nR> are the four corners of the screen region.
 * $RETURNS$
 *    .T. if mouse is in specified region.
 * $DESCRIPTION$
 *    This function will check to see if the mouse cursor is
 *    within the confines of the specified region.
 * $EXAMPLES$
 *    IF FT_MINREGION( 10, 10, 11, 20 )
 *      nChoice := 1
 *    ENDIF
 *  $SEEALSO$
 *     FT_MXLIMIT() FT_MYLIMIT() FT_MINREGION()
 * $END$
 */

FUNCTION FT_MINREGION( nTR, nLC, nBR, nRC )
RETURN ( FT_MGETX() >= nTR .and. FT_MGETX() <= nBR .and. ;
         FT_MGETY() >= nLC .and. FT_MGETY() <= nRC )


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MSETSENS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Set the mouse sensitivity parameters
 * $SYNTAX$
 *    FT_MSETSENS( <nHoriz>, <nVert>, <nDouble> ) -> NIL
 * $ARGUMENTS$
 *    <nHoriz>  is the sensitivity of the mouse on the horizontal axis. This
 *              value is the integer percentage of highest sensitivity and
 *              thus has a range of 1 to 100. The default value is 50 and at
 *              this setting about 3.2 inches of mouse movement will move
 *              the mouse cursor across the screen. If NIL, the current
 *              value is used.
 *    <nVert>   is the relative sensitivity of the mouse on the vertical axis.
 *              The value is an integer percentage of the highest sensitivity
 *              and thus has a range of 1 to 100. The default value is 50 and
 *              requires about 2 inches of mouse movement will move from top
 *              to bottom of the screen.If NIL, the current value is used.
 *    <nDouble> is the relative sensitivity of the mouse to doubling the ratio
 *              of cursor movement to mouse movement. The default
 *              value is 50. If NIL, the current value is used.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    This function allows one to control the mouse movement sensitivity. The
 *    first two arguments control the amount of movement necessary to move
 *    the cursor a given amount. The values are the percentage of full
 *    sensitivity and the default values after installing the mouse driver
 *    is 50 which represents approximately 3.2 inches of horizontal
 *    and 2 inches of vertical mouse movement to cover the entire screen.
 *    A value of 100 requires about 0.9 inches of horizontal mouse movement to
 *    cover the screen from one side to the other.
 *
 *    The third argument changes the threshold above which the mouse moves at
 *    twice the normal speed. The value is a percentage of full sensitivity
 *    with the default (50) providing doubling at 64 mickeys per second.
 *
 *    NOTE: These values are NOT restored after resetting the mouse driver/
 *    hardware. A well behaved application should reset them to the
 *    original value upon exiting.
 *
 *    NOTE: The above description is counter to all of the documentation
 *    I have available. However, it does not work the way it is documented
 *    with Microsoft drivers versions 6.16, 6.24, 7.04 and 8.20. The above
 *    movement values are documented to be the number of mickeys per 8
 *    pixels and the double speed value as the number mickeys per second
 *    required to double the speed. Each of these values should range from 1
 *    to 32K but the driver forces a maximum of 100. Also the documentation
 *    states that resetting the mouse will reset these values. This is not
 *    the case.
 *
 *
 * $EXAMPLES$
 *    FT_MSETSENS( 75,75,50 )     // a little less mouse movement necessary.
 *  $SEEALSO$
 *    FT_MGETSENS()
 * $END$
 */

FUNCTION FT_MSETSENS(nHoriz, nVert, nDouble)
LOCAL nCurHoriz, nCurVert, nCurDouble

// Get current values

FT_MGETSENS(@nCurHoriz, @nCurVert, @nCurDouble)

// Set defaults if necessary

IF VALTYPE(nHoriz)!="N"
	nHoriz=nCurHoriz
ENDIF

IF VALTYPE(nVert)!="N"
	nVert=nCurVert
ENDIF

IF VALTYPE(nDouble)!="N"
	nDouble=nCurDouble
ENDIF

* Fill the registers
_mset_sensitive(nHoriz,nVert,nDouble)

RETURN nil


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MGETSENS()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get the mouse sensitivity parameters
 * $SYNTAX$
 *    FT_MGETSENS( <@nHoriz>, <@nVert>, <@nDouble> ) -> NIL
 * $ARGUMENTS$
 *    <nHoriz>  is the percentage of maximum horizontal sensitivity. PASSED
 *              BY REFERENCE.
 *    <nVert>   is the percentage of maximum vertical sensitivity. PASSED BY
 *              REFERENCE.
 *    <nDouble> is the percentage of maximum sensitivity for doubling the
 *              mouse cursor's speed on the screen. PASSED BY REFERENCE.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    This function returns the current values of the mouse movement
 *    sensitivity parameters. The first two arguments control the amount of
 *    movement necessary to move the cursor a given amount. The third argument
 *    determines the threshold above which the mouse moves at twice the normal
 *    speed. For further discussion of these values see FT_MSETSENS()
 * $EXAMPLES$
 *    FT_MGETSENS( @nHoriz, @nVert, @nDouble )
 *  $SEEALSO$
 *    FT_MSETSENS()
 * $END$
 */

FUNCTION FT_MGETSENS(nHoriz, nVert, nDouble)
/*
* Fill the register

aReg[AX]=27

* Execute interupt

FT_INT86( 51, aReg )        // execute mouse interrupt

*/                           
// Set the return values

nHoriz = _mget_horispeed()
nVert  = _mget_verspeed()
nDouble= _mget_doublespeed()

RETURN NIL


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MVERSION()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get the mouse driver version
 * $SYNTAX$
 *    FT_MVERSION( <@nMinor>, <@nType>, <@nIRQ>  ) -> <nMajor>
 * $ARGUMENTS$
 *    <nMinor>  is the Minor version number. PASSED BY REFERENCE.
 *    <nType>   is the Mouse type. PASSED BY REFERENCE.
 *                  1 = Bus Mouse
 *                  2 = Serial Mouse
 *                  3 = InPort Mouse
 *                  4 = PS/2 Mouse
 *                  5 = HP Mouse
 *    <nIRQ>    is the IRQ number used for the mouse. PASSED BY REFERENCE.
 *                  0 = PS/2
 *                  2,3,4,5 or 7 = IRQ number
 * $RETURNS$
 *    <nMajor> which is the major version number of the mouse driver.
 * $DESCRIPTION$
 *    This function returns the current values of the mouse driver version
 *    number and type. The major version would be 6 and the minor version
 *    would be 10 if the driver were version 6.10. The mouse type and IRQ
 *    numbers are also returned.
 *
 *    NOTE: It appears that the values reported when one starts the mouse
 *    driver actually have the minor version in hexadecimal! Thus on bootup
 *    my screen showed 6.24 but this routine returned 30 for the minor version
 *    number!
 * $EXAMPLES$
 *    nMajor=FT_MVERSION( @nMinor )
 *    IF (nMajor+nMinor/100)<7.2
 *         ? "Sorry mouse driver version too old"
 *         RETURN
 *    ENDIF
 *  $SEEALSO$
 *    FT_MSETSENS()
 * $END$
 */

FUNCTION FT_MVERSION(nMinor, nType, nIRQ)
Local aReturn:={}
// Set up register
/*
aReg[AX] = 36

// Call interupt

FT_INT86( 51, aReg)
*/
// decode out of half registers
areturn:=_mget_mversion()

nMinor=areturn[1]
nType=areturn[2]
nIRQ=areturn[3]

// Return

RETURN areturn[4]


/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MSETPAGE()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Set the display page for the mouse pointer
 * $SYNTAX$
 *    FT_MSETPAGE( <@nPage> ) -> NIL
 * $ARGUMENTS$
 *    <nPage>  is the desired display page.
 * $RETURNS$
 *    NIL
 * $DESCRIPTION$
 *    This function sets the display page for the mouse cursor. The valid
 *    values of nPage is dependent upon the display mode. See FT_SETVPG()
 *    for changing the current video page
 * $EXAMPLES$
 *    FT_MSETPAGE( 1 )  // Sets the mouse cursor to page 1
 *  $SEEALSO$
 *    FT_MGETPAGE()
 * $END$
 */

FUNCTION FT_MSETPAGE(nPage)

// Set up register
/*
aReg[AX] = 29
aReg[BX]=nPage

// Call interupt

FT_INT86( 51, aReg)
*/
_mset_page(nPage)
RETURN NIL

/*
 * $DOC$
 * $FUNCNAME$
 *    FT_MGETPAGE()
 * $CATEGORY$
 *    Keyboard/Mouse
 * $ONELINER$
 *    Get the display page for the mouse pointer
 * $SYNTAX$
 *    FT_MGETPAGE() -> <nPage>
 * $ARGUMENTS$
 *    None
 * $RETURNS$
 *    <nPage>  is the display page on which the mouse is currently being
 *         displayed
 * $DESCRIPTION$
 *    This function gets the display page for the mouse cursor. The valid
 *    values of nPage is dependent upon the display mode. See FT_SETVPG()
 *    for changing the current video page
 * $EXAMPLES$
 *    nPage = FT_MGETPAGE( )  // Gets the mouse cursor display page
 *  $SEEALSO$
 *    FT_MSETPAGE()
 * $END$
 */

FUNCTION FT_MGETPAGE(nPage)


// Set up register
/*
aReg[AX] = 30

// Call interupt

FT_INT86( 51, aReg)
*/
RETURN _mget_page()




FUNCTION FT_MINIT()

* If not previously initialized then try

   IF !lMinit
	lMinit=(FT_MRESET()!=0)
   ELSE
* Reset maximum x and y limits

     FT_MYLIMIT(0,8*24)
     FT_MXLIMIT(0,8*80)
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
LOCAL lStatus
/*
   aReg[AX] := 0          // set mouse function call 0
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */
   lCrsState=.F.         // Cursor is off after reset
lStatus:=_m_reset()
* Reset maximum x and y limits

   FT_MYLIMIT(0,8*MAXROW())
   FT_MXLIMIT(0,8*MAXCOL())

RETURN lStatus          // return status code


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
   /*
   aReg[AX] := 1         // set mouse function call 1
   FT_INT86( 51, aReg ) // execute mouse interrupt
   */
      _mse_showcurs()
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
/*
   aReg[AX] := 2          // set mouse function call 2
   FT_INT86( 51, aReg )  // execute mouse interrupt
   */
   _mse_mhidecrs()
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
   Local amse:={}
   nX := if( nX == NIL, 0, nX )
   nY := if( nY == NIL, 0, nY )
/*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   amse:=_mse_getpos()

   nX := amse[1]               // store new x-coordinate
   nY := amse[2]               // store new y-coordinate

RETURN amse[3]                 // return button status



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
/*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
*/
RETURN( _m_getx()/8 )       // return x-coordinate

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
   /*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )        // execute mouse interrupt
 */
RETURN( _m_gety()/8)        // return y-coordinate

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
/*
   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY               // assign new x-coordinate
   aReg[DX] := nX               // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt
  */
  _m_msetpos(nY,nX)
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
/*
   aReg[AX] := 4                // set mouse function call 4
   aReg[CX] := nY*8             // assign new x-coordinate
   aReg[DX] := nX*8             // assign new y-coordinate
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   _m_MSETCOORD(nY*8,nX*8)
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
/*
   aReg[AX] = 7                        // set mouse function call 7
   aReg[CX] = nXMin                    // load vertical minimum parameter
   aReg[DX] = nXMax                    // load vertical maximum parameter
   FT_INT86( 51, aReg )               // execute mouse interrupt
   */
    _m_mxlimit(nXMin,nXMAX)
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
/*
   aReg[AX] = 8                       // set mouse function call 8
   aReg[CX] = nYMin                   // load horz minimum parameter
   aReg[DX] = nYMax                   // load horz maximum parameter
   FT_INT86( 51, aReg )              // execute mouse interrupt
   */
  _m_mYlimit(nYMin,nYMAX)
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
local aReg:={}
/*
   aReg[AX] := 5	           // set mouse function call 5
   aReg[BX] := nButton          // pass parameter for left or right button
   FT_INT86( 51, aReg )        // execute mouse interrupt
   */
   nButPrs := aReg[1] // store updated press count
   nX := aReg[2]      // x-coordinate at last press
   nY := aReg[3]      // y-coordinate at last press
   
_m_MBUTPRS(nButton)
RETURN aReg[4]                 // return button status

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
local areg:={}
Local iButton
   areg:=_m_MBUTREL(nButton)
   nButRel := aReg[1]  // store updated release count
   nX := aReg[2]      // x-coordinate at last release
   nY := aReg[3]      // y-coordinate at last release
   iButton:=   aReg[4]                 // return button status

RETURN iButton


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

FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks
/*
   aReg[AX] = 10   	   // set mouse function call 10
   aReg[BX] = nCurType   // load cursor type parameter
   aReg[CX] = nScrMask   // load screen mask value
   aReg[DX] = nCurMask   // load cursor mask value
   FT_INT86( 51, aReg ) // execute mouse interrupt
   */
_m_mdefcrs(nCurType, nScrMask, nCurMask ) 
RETURN NIL              // no function output


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
local aReg:={}
local iButton
   nX := if( nX == NIL, 0, nX )
   nY := if( nY == NIL, 0, nY )
      /*
   aReg[AX] := 3                // set mouse function call 3
   FT_INT86( 51, aReg )         // execute mouse interrupt
   */
   areg:=_m_mgetcoord()
   nX := INT(aReg[1]/8)        // store new x-coordinate
   nY := INT(aReg[2]/8)        // store new y-coordinate
   iButton:= aReg[3]                 // return button status
   
RETURN iButton

