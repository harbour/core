/*
 * $Id$
 */

THREAD STATIC t_lCrsState := .F.
THREAD STATIC t_lMinit := .F.

FUNCTION FT_MMICKEYS( /* @ */ nX, /* @ */ nY ) // read mouse motion counters

   LOCAL aReturn := _mget_mics()

   nX := aReturn[ 1 ]             // store horizontal motion units
   nY := aReturn[ 2 ]             // store vertical motion units

   RETURN NIL                     // no function output

FUNCTION FT_MDBLCLK( nClick, nButton, nInterval, nRow, nCol, nStart )

   LOCAL nVert, nHorz  // local row and col coordinates
   LOCAL lDouble       // double click actually occurred
   LOCAL lDone         // loop flag
   LOCAL nPrs          // number of presses which occurred

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
      nVert := Int( nVert / 8 )
      nHorz := Int( nHorz / 8 )

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
         nVert := Int( nVert / 8 )
         nHorz := Int( nHorz / 8 )

         lDouble := ( nPrs > 0 )
         lDone := Seconds() - nStart >= nInterval .OR. lDouble

      ENDDO

      // make sure we haven't moved

      lDouble := lDouble .AND. ( nVert == nRow .AND. nHorz == nCol )

   ENDIF

   RETURN lDouble

FUNCTION FT_MCONOFF( nTop, nLeft, nBottom, nRight )

   _mse_conoff( nTop * 8, nLeft * 8, nBottom * 8, nRight * 8 )

   RETURN NIL

FUNCTION FT_MINREGION( nTR, nLC, nBR, nRC )

   RETURN ;
      FT_MGETX() >= nTR .AND. FT_MGETX() <= nBR .AND. ;
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

   RETURN NIL

FUNCTION FT_MGETSENS( /* @ */ nHoriz, /* @ */ nVert, /* @ */ nDouble )

   nHoriz  := _mget_horispeed()
   nVert   := _mget_verspeed()
   nDouble := _mget_doublespeed()

   RETURN NIL

FUNCTION FT_MVERSION( /* @ */ nMinor, /* @ */ nType, /* @ */ nIRQ )

   LOCAL aReturn := _mget_mversion()

   nMinor := aReturn[ 1 ]
   nType  := aReturn[ 2 ]
   nIRQ   := aReturn[ 3 ]

   RETURN aReturn[ 4 ]

FUNCTION FT_MSETPAGE( nPage )

   RETURN _mset_page( nPage )

FUNCTION FT_MGETPAGE()

   RETURN _mget_page()

FUNCTION FT_MINIT()

   // If not previously initialized then try

   IF ! t_lMinit
      t_lMinit := ( FT_MRESET() != 0 )
   ELSE
      // Reset maximum x and y limits

      FT_MYLIMIT( 0, 8 * 24 )
      FT_MXLIMIT( 0, 8 * 80 )
   ENDIF

   RETURN t_lMinit

FUNCTION FT_MRESET()

   LOCAL lStatus

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

   _mse_showcurs()

   t_lCrsState := .T.

   RETURN NIL              // no output from function

FUNCTION FT_MHIDECRS()   // decrement internal cursor flag and hide cursor

   _mse_mhidecrs()

   t_lCrsState := .F.

   RETURN NIL               // no output from function

FUNCTION FT_MGETPOS( /* @ */ nX, /* @ */ nY )

   LOCAL aReturn := _mse_getpos()

   nX := aReturn[ 1 ]                  // store new x-coordinate
   nY := aReturn[ 2 ]                  // store new y-coordinate

   RETURN aReturn[ 3 ]                 // return button status

FUNCTION FT_MGETX()

   RETURN _m_getx() / 8        // return x-coordinate

FUNCTION FT_MGETY()

   RETURN _m_gety() / 8         // return y-coordinate

FUNCTION FT_MSETPOS( nX, nY )  // set mouse cursor location

   RETURN _m_msetpos( nY, nX )

FUNCTION FT_MSETCOORD( nX, nY )  // set mouse cursor location

   RETURN _m_MSETCOORD( nY * 8, nX * 8 )

FUNCTION FT_MXLIMIT( nXMin, nXMax )   // set vertical minimum and maximum coordinates

   RETURN _m_mxlimit( nXMin, nXMAX )

FUNCTION FT_MYLIMIT( nYMin, nYMax )  // set horizontal minimum and maximum coordinates

   RETURN _m_mYlimit( nYMin, nYMAX )

FUNCTION FT_MBUTPRS( nButton, /* @ */ nButPrs, /* @ */ nX, /* @ */ nY ) // get button press information

   LOCAL aReturn := _m_MBUTPRS( nButton )

   nButPrs := aReturn[ 1 ]      // store updated press count
   nX      := aReturn[ 2 ]      // x-coordinate at last press
   nY      := aReturn[ 3 ]      // y-coordinate at last press

   RETURN aReturn[ 4 ]          // return button status

FUNCTION FT_MBUTREL( nButton, /* @ */ nButRel, /* @ */ nX, /* @ */ nY ) // get button release information

   LOCAL aReturn := _m_MBUTREL( nButton )

   nButRel := aReturn[ 1 ]       // store updated release count
   nX      := aReturn[ 2 ]       // x-coordinate at last release
   nY      := aReturn[ 3 ]       // y-coordinate at last release

   RETURN aReturn[ 4 ]           // return button status

FUNCTION FT_MDEFCRS( nCurType, nScrMask, nCurMask )   // define text cursor type and masks

   RETURN _m_mdefcrs( nCurType, nScrMask, nCurMask )

// Duplicated code from FT_MGETPOS() for speed reasons
FUNCTION FT_MGETCOORD( /* @ */ nX, /* @ */ nY )

   LOCAL aReturn := _m_mgetcoord()

   nX := Int( aReturn[ 1 ] / 8 )   // store new x-coordinate
   nY := Int( aReturn[ 2 ] / 8 )   // store new y-coordinate

   RETURN aReturn[ 3 ]             // return button status
