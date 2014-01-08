/*
 * Author....: Jim Orlowski
 * CIS ID....: ?
 *
 * This is an original work by Jim Orlowski and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   28 Sep 1991 02:56:56   GLENN
 * Moved Jim's "Tricks used" comment out of the file header and
 * into the source code area.
 *
 *    Rev 1.3   28 Sep 1991 02:52:22   GLENN
 * Jim's modifications:
 *
 *  1.  Changed SaveScreen() and RestScreen() to use MaxRow(), MaxCol()
 *      instead of 24,79
 *
 *  2.  Added Nantucket's cleaner code for:
 *        - Cleaned up logic around line 334 while loop section
 *        - Added refreshCurrent and another stabilize around line 349
 *        - TbSkipWhil() was redone
 *             Note: Leo's line was changed to:
 *                 ELSEIF n > 0 .AND. RecNo() != LastRec() + 1
 *
 *  3.  Added DispBegin() and DispEnd() around both Stabilize sections
 *
 *
 *
 *
 *    Rev 1.2   15 Aug 1991 23:04:20   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:08   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:22   GLENN
 * Nanforum Toolkit
 *
 */

/* The tricks are:

   1. Setting up functions for goTop() and goBottom() so that you can
      quickly move to the right record when the user presses the
      Ctrl-PgUp ( goTop() ) and Ctrl-PgDn ( goBottom() ) keys.

   2. Passing and evaluating the block for the TbSkipWhil().
 */

#include "inkey.ch"
#include "setcurs.ch"

FUNCTION ft_BrwsWhl( aFields, bWhileCond, cKey, nFreeze, lSaveScrn, ;
      cColorList, cColorShad, nTop, nLeft, nBottom, nRight )

   LOCAL b, column, i
   LOCAL lKeepScrn, cScrnSave
   LOCAL cColorSave, cColorBack, nCursSave
   LOCAL lMore, nKey, nPassRec

   __defaultNIL( @nFreeze, 0 )
   __defaultNIL( @lSaveScrn, .T. )
   __defaultNIL( @cColorList, "N/W, N/BG, B/W, B/BG, B/W, B/BG, R/W, B/R" )
   __defaultNIL( @cColorShad, "N/N" )
   __defaultNIL( @nTop, 2 )
   __defaultNIL( @nLeft, 2 )
   __defaultNIL( @nBottom, MaxRow() - 2 )
   __defaultNIL( @nRight, MaxCol() - 2 )

   lKeepScrn := PCount() > 6

   dbSeek( cKey )
   IF ! Found() .OR. LastRec() == 0
      RETURN 0
   ENDIF

   /* make new browse object */
   b := TBrowseDB( nTop, nLeft, nBottom, nRight )

   /* default heading and column separators */
   b:headSep := hb_UTF8ToStrBox( "═╤═" )
   b:colSep  := hb_UTF8ToStrBox( " │ " )
   b:footSep := hb_UTF8ToStrBox( "═╧═" )

   /* add custom TbSkipWhil() (to handle passed condition) */
   b:skipBlock := {| x | TbSkipWhil( x, bWhileCond ) }

   /* Set up substitute goto top and goto bottom
      with While's top and bottom records */
   b:goTopBlock    := {|| TbWhileTop( cKey ) }
   b:goBottomBlock := {|| TbWhileBot( cKey ) }

   /* colors */
   b:colorSpec := cColorList

   /* add a column for each field in the current workarea */
   FOR EACH i IN aFields

      /* make the new column */
      column := TBColumnNew( i[ 1 ], i[ 2 ] )

#if 0
      /* these are color setups from tbdemo.prg from Nantucket */
      IF HB_ISNUMERIC( Eval( i[ 2 ] )
         column:defColor := { 5, 6 }
         column:colorBlock := {| x | iif( x < 0, { 7, 8 }, { 5, 6 } ) }
      ELSE
         column:defColor := { 3, 4 }
      ENDIF
#endif

      /* To simplify I just used 3rd and 4th colors from passed cColorList
         This way 1st is SAY, 2nd is GET, 3rd and 4th are used here,
         5th is Unselected Get, extras can be used as in tbdemo.prg */
      column:defColor := { 3, 4 }

      b:addColumn( column )
   NEXT

   /* freeze columns */
   IF nFreeze != 0
      b:freeze := nFreeze
   ENDIF

   /* save old screen and colors */
   IF lSaveScrn
      cScrnSave := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   ENDIF
   cColorSave := SetColor()

   /* Background Color Is Based On First Color In Passed cColorList */
   cColorBack := iif( "," $ cColorList, ;
      SubStr( cColorList, 1, At( ",", cColorList ) - 1 ), cColorList )

   IF ! lKeepScrn
      SetColor( cColorBack )
      hb_Scroll()
   ENDIF

   /* make a window shadow */
   hb_Scroll( nTop + 1, nLeft + 1, nBottom + 1, nRight + 1,,, cColorShad )
   hb_Scroll( nTop, nLeft, nBottom, nRight,,, cColorBack )
   SetColor( cColorSave )

   nCursSave := SetCursor( SC_NONE )

   lMore := .T.
   DO WHILE lMore

      /* stabilize the display */
      nKey := 0
      DispBegin()
      DO WHILE nKey == 0 .AND. ! b:stable
         b:stabilize()
         nKey := Inkey()
      ENDDO
      DispEnd()

      IF b:stable
         /* display is stable */
         IF b:hitTop .OR. b:hitBottom
            Tone( 125, 0 )
         ENDIF

         /* Make sure that the current record is showing
            up-to-date data in case we are on a network. */
         DispBegin()
         b:refreshCurrent()
         b:forceStable()
         DispEnd()

         /* everything's done. just wait for a key */
         nKey := Inkey( 0 )
      ENDIF

      /* process key */
      SWITCH nKey
      CASE K_DOWN
         b:down()
         EXIT

      CASE K_UP
         b:up()
         EXIT

      CASE K_PGDN
         b:pageDown()
         EXIT

      CASE K_PGUP
         b:pageUp()
         EXIT

      CASE K_CTRL_PGUP
         b:goTop()
         EXIT

      CASE K_CTRL_PGDN
         b:goBottom()
         EXIT

      CASE K_RIGHT
         b:Right()
         EXIT

      CASE K_LEFT
         b:Left()
         EXIT

      CASE K_HOME
         b:home()
         EXIT

      CASE K_END
         b:end()
         EXIT

      CASE K_CTRL_LEFT
         b:panLeft()
         EXIT

      CASE K_CTRL_RIGHT
         b:panRight()
         EXIT

      CASE K_CTRL_HOME
         b:panHome()
         EXIT

      CASE K_CTRL_END
         b:panEnd()
         EXIT

      CASE K_ESC
         nPassRec := 0
         lMore := .F.
         EXIT

      CASE K_ENTER
         nPassRec := RecNo()
         lMore := .F.
         EXIT

      ENDSWITCH
   ENDDO

   /* restore old screen */
   IF lSaveScrn
      RestScreen( 0, 0, MaxRow(), MaxCol(), cScrnSave )
   ENDIF
   SetCursor( nCursSave )
   SetColor( cColorSave )

   RETURN nPassRec

STATIC FUNCTION TbSkipWhil( n, bWhileCond )

   LOCAL i := 0

   IF n == 0 .OR. LastRec() == 0
      dbSkip( 0 )  /* significant on a network */

   ELSEIF n > 0 .AND. RecNo() != LastRec() + 1
      DO WHILE i < n
         dbSkip()
         IF Eof() .OR. ! Eval( bWhileCond )
            dbSkip( -1 )
            EXIT
         ENDIF
         i++
      ENDDO

   ELSEIF n < 0
      DO WHILE i > n
         dbSkip( -1 )
         IF Bof()
            EXIT
         ELSEIF ! Eval( bWhileCond )
            dbSkip()
            EXIT
         ENDIF
         i--
      ENDDO
   ENDIF

   RETURN i

STATIC PROCEDURE TbWhileTop( cKey )

   dbSeek( cKey )

   RETURN

/* SeekLast: Finds Last Record For Matching Key
   Developed By Jon Cole
   With softseek set on, seek the first record after condition.
   This is accomplished by incrementing the right most character of the
   string cKey by one ascii character.  After SEEKing the new string,
   back up one record to get to the last record which matches cKey.
 */
STATIC PROCEDURE TbWhileBot( cKey )

   dbSeek( Left( cKey, Len( cKey ) - 1 ) + Chr( Asc( Right( cKey, 1 ) ) + 1 ), .T. )
   dbSkip( -1 )

   RETURN
