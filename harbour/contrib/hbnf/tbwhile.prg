/*
 * $Id$
 */

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
 *  1.  Changed SAVESCREEN() and RESTSCREEN to use MaxRow(), MaxCol()
 *      instead of 24,79
 *
 *  2.  Added Nantucket's cleaner code for:
 *        - Cleaned up logic around line 334 while loop section
 *        - Added refreshCurrent and another stabilize around line 349
 *        - TbSkipWhile was redone
 *             Note: Leo's line was changed to:
 *                 ELSEIF n > 0 .AND. RECNO() != LASTREC() + 1
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
 *
 * 1. Setting up functions for goTop() and goBottom() so that you can
 *    quickly move to the right record when the user presses the
 *    Ctrl-PgUp ( goTop() ) and Ctrl-PgDn ( goBottom() ) keys.
 *
 * 2. Passing and evaluating the block for the TbSkipWhil().
 */

#include "inkey.ch"
#include "set.ch"
#include "setcurs.ch"

/* ------------------------------------------------------------------- */

FUNCTION FT_BRWSWHL( aFields, bWhileCond, cKey, nFreeze, lSaveScrn, ;
      cColorList, cColorShad, nTop, nLeft, nBottom, nRight )

   LOCAL b, column, i
   LOCAL cHead, bField, lKeepScrn, cScrnSave
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

   SEEK cKey
   IF ! Found() .OR. LastRec() == 0
      RETURN 0
   ENDIF

   /* make new browse object */
   b := TBRowseDb( nTop, nLeft, nBottom, nRight )

   /* default heading and column separators */
   b:headSep := hb_UTF8ToStrBox( "═╤═" )
   b:colSep  := hb_UTF8ToStrBox( " │ " )
   b:footSep := hb_UTF8ToStrBox( "═╧═" )

   /* add custom 'TbSkipWhil' (to handle passed condition) */
   b:skipBlock := {| x | TbSkipWhil( x, bWhileCond ) }

   /* Set up substitute goto top and goto bottom */
   /* with While's top and bottom records        */
   b:goTopBlock    := {|| TbWhileTop( cKey ) }
   b:goBottomBlock := {|| TbWhileBot( cKey ) }

   /* colors */
   b:colorSpec := cColorList

   /* add a column for each field in the current workarea */
   FOR i := 1 TO Len( aFields )
      cHead  := aFields[ i, 1 ]
      bField := aFields[ i, 2 ]

      /* make the new column */
      column := TBColumnNew( cHead, bField )

      /* these are color setups from tbdemo.prg from Nantucket */
      // IF cType == "N"
      //   column:defColor := { 5, 6 }
      //   column:colorBlock := {| x | iif( x < 0, { 7, 8 }, { 5, 6 } ) }
      // ELSE
      //   column:defColor := { 3, 4 }
      // ENDIF

      /* To simplify I just used 3rd and 4th colors from passed cColorList */
      /* This way 1st is SAY, 2nd is GET, 3rd and 4th are used here,
      /* 5th is Unselected Get, extras can be used as in tbdemo.prg */
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
      CLS
   ENDIF

   /* make a window shadow */
   SetColor( cColorShad )
   hb_Scroll( nTop + 1, nLeft + 1, nBottom + 1, nRight + 1 )
   SetColor( cColorBack )
   hb_Scroll( nTop, nLeft, nBottom, nRight )
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

         // Make sure that the current record is showing
         // up-to-date data in case we are on a network.
         DispBegin()
         b:refreshCurrent()
         DO WHILE ! b:stabilize()
         ENDDO
         DispEnd()

         /* everything's done. just wait for a key */
         nKey := Inkey( 0 )
      ENDIF

      /* process key */
      DO CASE
      CASE nKey == K_DOWN
         b:down()

      CASE nKey == K_UP
         b:up()

      CASE nKey == K_PGDN
         b:pageDown()

      CASE nKey == K_PGUP
         b:pageUp()

      CASE nKey == K_CTRL_PGUP
         b:goTop()

      CASE nKey == K_CTRL_PGDN
         b:goBottom()

      CASE nKey == K_RIGHT
         b:Right()

      CASE nKey == K_LEFT
         b:Left()

      CASE nKey == K_HOME
         b:home()

      CASE nKey == K_END
         b:end()

      CASE nKey == K_CTRL_LEFT
         b:panLeft()

      CASE nKey == K_CTRL_RIGHT
         b:panRight()

      CASE nKey == K_CTRL_HOME
         b:panHome()

      CASE nKey == K_CTRL_END
         b:panEnd()

      CASE nKey == K_ESC
         nPassRec := 0
         lMore := .F.

      CASE nKey == K_RETURN
         nPassRec := RecNo()
         lMore := .F.
      ENDCASE
   ENDDO

   /* restore old screen */
   IF lSaveScrn
      RestScreen( 0, 0, MaxRow(), MaxCol(), cScrnSave )
   ENDIF
   SetCursor( nCursSave )
   SetColor( cColorSave )

   RETURN nPassRec

/* -------------------------------------------------------------------- */

STATIC FUNCTION TbSkipWhil( n, bWhileCond )

   LOCAL i := 0

   IF n == 0 .OR. LastRec() == 0
      SKIP 0  // significant on a network

   ELSEIF n > 0 .AND. RecNo() != LastRec() + 1
      WHILE i < n
         SKIP 1
         IF EOF() .OR. ! Eval( bWhileCond )
            SKIP -1
            EXIT
         ENDIF
         i++
      ENDDO

   ELSEIF n < 0
      DO WHILE i > n
         SKIP -1
         IF BOF()
            EXIT
         ELSEIF ! Eval( bWhileCond )
            SKIP
            EXIT
         ENDIF
         i--
      ENDDO
   ENDIF

   RETURN i

/* -------------------------------------------------------------------- */

STATIC FUNCTION TbWhileTop( cKey )

   SEEK cKey

   RETURN NIL

/* -------------------------------------------------------------------- */

STATIC FUNCTION TbWhileBot( cKey )

   // SeekLast: Finds Last Record For Matching Key
   // Developed By Jon Cole
   // With softseek set on, seek the first record after condition.
   // This is accomplished by incrementing the right most character of the
   // string cKey by one ascii character.  After SEEKing the new string,
   // back up one record to get to the last record which matches cKey.

   LOCAL cSoftSave := Set( _SET_SOFTSEEK, .T. )
   SEEK Left( cKey, Len( cKey ) - 1 ) + Chr( Asc( Right( cKey, 1 ) ) + 1 )
   Set( _SET_SOFTSEEK, cSoftSave )
   SKIP -1

   RETURN NIL
