/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBEDIT() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    DBEDIT() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "dbedit.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define HB_DBEMPTY() ( LastRec() == 0 .OR. ( ( Eof() .OR. RecNo() == LastRec() + 1 ) .AND. Bof() ) )

/* TODO: put more comprehensive $EXAMPLES$.
         DBEDIT() is a complex function, the doc I had made cover all the
         parameters but probably not good enough for a new user that does
         not know what this function is all about and how to use it. I am
         not that good with the English language (and I did not want to
         COPY the NG text) I suggest later some one should add to this
         text. [chkedem] */

/* NOTE: Extension: Harbour supports codeblocks as the xUserFunc parameter */
/* NOTE: Clipper is buggy and will throw an error if the number of
         columns is zero. (Check: dbEdit(0,0,20,20,{})) */
/* NOTE: Clipper will throw an error if there's no database open */
/* NOTE: The NG says that the return value is NIL, but it's not. */
/* NOTE: There's an undocumented result code in Clipper (3), which is not
         supported in Harbour */
/* NOTE: Harbour is multithreading ready/reentrant, Clipper is not */

/*  $DOC$
 *  $FUNCNAME$
 *      DBEDIT()*
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Browse records in a table
 *  $SYNTAX$
 *      DBEDIT( [<nTop>], [<nLeft>], [<nBottom>], [<nRight>], [<acColumns>],
 *              [<xUserFunc>], [<xColumnSayPictures>], [<xColumnHeaders>],
 *              [<xHeadingSeparators>], [<xColumnSeparators>],
 *              [<xFootingSeparators>], [<xColumnFootings>] ) --> lOk
 *  $ARGUMENTS$
 *      <nTop> coordinate for top row display. <nTop> could range from 0 to
 *      MAXROW(), default is 0.
 *
 *      <nLeft> coordinate for left column display. <nLeft> could range from
 *      0 to MAXCOL(), default is 0.
 *
 *      <nBottom> coordinate for bottom row display. <nBottom> could range
 *      from 0 to MAXROW(), default is MAXROW().
 *
 *      <nRight> coordinate for right column display. <nRight> could range
 *      from 0 to MAXCOL(), default is MAXCOL().
 *
 *      <acColumns> is an array of character expressions that contain
 *      database fields names or expressions to display in each column.
 *      If not specified, the default is to display all fields from the
 *      database in the current work area.
 *
 *      <xUserFunc> is a name of a user defined function or a code block
 *      that would be called every time unrecognized key is been pressed or
 *      when there are no keys waiting to be processed and DBEDIT() goes
 *      into idle mode. If <xUserFunc> is a character string, it must
 *      contain root name of a valid user define function without
 *      parentheses. Both the user define function or the code block should
 *      accept two parameters: nMode, nCurrentColumn. Both should return
 *      a numeric value that correspond to one of the expected return codes
 *      (see table below for a list of nMode and return codes).
 *
 *      <xColumnSayPictures> is an optional picture. If <xColumnSayPictures>
 *      is a character string, all columns would used this value as a
 *      picture string. If <xColumnSayPictures> is an array, each element
 *      should be a character string that correspond to a picture string
 *      for the column with the same index. Look at the help for @...SAY
 *      to get more information about picture values.
 *
 *      <xColumnHeaders> contain the header titles for each column, if this
 *      is a character string, all columns would have that same header, if
 *      this is an array, each element is a character string that contain
 *      the header title for one column. Header may be split to more than
 *      one line by placing semicolon (;) in places where you want to break
 *      line. If omitted, the default value for each column header is taken
 *      from <acColumns> or field name if <acColumns> was not specified.
 *
 *      <xHeadingSeparators> is an array that contain characters that draw
 *      the lines separating the headers and the fields data. Instead of an
 *      array you can use a character string that would be used to display
 *      the same line for all fields. Default value is a double line.
 *
 *      <xColumnSeparators> is an array that contain characters that draw
 *      the lines separating displayed columns. Instead of an array you can
 *      use a character string that would be used to display the same line
 *      for all fields. Default value is a single line.
 *
 *      <xFootingSeparators> is an array that contain characters that draw
 *      the lines separating the fields data area and the footing area.
 *      Instead of an array you can use a character string that would be
 *      used to display the same line for all footers. Default is to have to
 *      no footing separators.
 *
 *      <xColumnFootings> contain the footing to be displayed at the bottom
 *      of each column, if this is a character string, all columns would
 *      have that same footer, if this is an array, each element is a
 *      character string that contain the footer for one column. Footer may
 *      be split to more than one line by placing semicolon (;) in places
 *      where you want to break line. If omitted, no footer are displayed.
 *  $RETURNS$
 *      DBEDIT() return .F. if there is no database in use or if the number
 *      of columns to display is zero, else DBEDIT() return .T.
 *  $DESCRIPTION$
 *      DBEDIT() display and edit records from one or more work areas in
 *      a grid on screen. Each column is defined by element from <acColumns>
 *      and is the equivalent of one field. Each row is equivalent of one
 *      database record.
 *
 *      Following are active keys that handled by DBEDIT():
 *      ---------------------------------------------------
 *
 *      Left           - Move one column to the left (previous field)
 *      Right          - Move one column to the right (next field)
 *      Up             - Move up one row (previous record)
 *      Down           - Move down one row (next record)
 *      Page-Up        - Move to the previous screen
 *      Page-Down      - Move to the next screen
 *      Ctrl Page-Up   - Move to the top of the file
 *      Ctrl Page-Down - Move to the end of the file
 *      Home           - Move to the leftmost visible column
 *      End            - Move to the rightmost visible column
 *      Ctrl Left      - Pan one column to the left
 *      Ctrl Right     - Pan one column to the right
 *      Ctrl Home      - Move to the leftmost column
 *      Ctrl End       - Move to the rightmost column
 *
 *      When <xUserFunc> is omitted, two more keys are active:
 *
 *      Esc            - Terminate BROWSE()
 *      Enter          - Terminate BROWSE()
 *
 *      When DBEDIT() execute <xUserFunc> it pass the following arguments:
 *      nMode and the index of current record in <acColumns>. If <acColumns>
 *      is omitted, the index number is the FIELD() number of the open
 *      database structure.
 *
 *      DBEDIT() nMode could be one of the following:
 *      ---------------------------------------------
 *
 *      DE_IDLE         0      DBEDIT() is idle, all movement keys have been
 *                             handled.
 *      DE_HITTOP       1      Attempt to cursor past top of file.
 *      DE_HITBOTTOM    2      Attempt to cursor past bottom of file.
 *      DE_EMPTY        3      No records in work area, database is empty.
 *      DE_EXCEPT       4      Key exception.
 *
 *      The user define function or code block must return a value that tell
 *      DBEDIT() what to do next.
 *
 *      User function return codes:
 *      ---------------------------
 *
 *      DE_ABORT        0      Abort DBEDIT().
 *      DE_CONT         1      Continue DBEDIT() as is.
 *      DE_REFRESH      2      Force reread/redisplay of all data rows.
 *
 *      The user function is called once in each of the following cases:
 *      - The database is empty.
 *      - The user try to move past top of file or past bottom file.
 *      - Key exception, the uses had pressed a key that is not handled by
 *        DBEDIT().
 *      - The keyboard buffer is empty or a screen refresh had just occurred
 *
 *      DBEDIT() is a compatibility function, it is superseded by the
 *      TBrowse class and there for not recommended for new applications.
 *  $EXAMPLES$
 *      // Browse a file using default values
 *      USE Test
 *      DBEDIT()
 *
 *      // TODO: put a longer example or point to a sample file
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      <xUserFunc> can take a code block value, this is an Harbour
 *      extension.
 *
 *      CA-Clipper will throw an error if there's no database open, Harbour
 *      would return .F.
 *
 *      CA-Clipper is buggy and will throw an error if the number of columns
 *      zero, Harbour would return .F.
 *
 *      The CA-Clipper 5.2 NG state that the return value is NIL, this is
 *      wrong and should be read logical.
 *
 *      There is an undocumented result code (3) from the user defined
 *      function in Clipper (both 87 and 5.x). This is an Append Mode which:
 *      "split the screen to allow data to be appended in windowed area".
 *      This mode is not supported by Harbour.
 *  $FILES$
 *      Header files are dbedit.ch, inkey.ch
 *  $SEEALSO$
 *     '@...SAY' BROWSE() tbrow.ngo:'TBrowse class' TRANSFORM()
 *  $END$
 */

FUNCTION dbEdit(;
      nTop,;
      nLeft,;
      nBottom,;
      nRight,;
      acColumns,;
      xUserFunc,;
      xColumnSayPictures,;
      xColumnHeaders,;
      xHeadingSeparators,;
      xColumnSeparators,;
      xFootingSeparators,;
      xColumnFootings )

   LOCAL oBrowse
   LOCAL nKey
   LOCAL bAction
   LOCAL lException

   LOCAL nOldCursor

   LOCAL nPos
   LOCAL nColCount
   LOCAL oColumn
   LOCAL nAliasPos
   LOCAL cAlias
   LOCAL cFieldName
   LOCAL cHeading
   LOCAL cBlock
   LOCAL bBlock

   IF !Used()
      RETURN .F.
   ENDIF

   /* ------------------------------------------------------ */
   /* Set up the environment, evaluate the passed parameters */
   /* ------------------------------------------------------ */

   IF !ISNUMBER( nTop ) .OR. nTop < 0
      nTop := 0
   ENDIF
   IF !ISNUMBER( nLeft ) .OR. nLeft < 0
      nLeft := 0
   ENDIF
   IF !ISNUMBER( nBottom ) .OR. nBottom > MaxRow() .OR. nBottom < nTop
      nBottom := MaxRow()
   ENDIF
   IF !ISNUMBER( nRight ) .OR. nRight > MaxCol() .OR. nRight < nLeft
      nRight := MaxCol()
   ENDIF

   oBrowse := TBrowseDB( nTop, nLeft, nBottom, nRight )

   oBrowse:SkipBlock := {| nRecs | dbEditSkipped( nRecs ) }
   oBrowse:HeadSep   := iif( ISCHARACTER( xHeadingSeparators ), xHeadingSeparators, Chr( 205 ) + Chr( 209 ) + Chr( 205 ) )
   oBrowse:ColSep    := iif( ISCHARACTER( xColumnSeparators ), xColumnSeparators, " " + Chr( 179 ) + " " )
   oBrowse:FootSep   := iif( ISCHARACTER( xFootingSeparators ), xFootingSeparators, "" )
   oBrowse:AutoLite  := .F.

   // Calculate the number of columns

   IF ISARRAY( acColumns )
      nColCount := Len( acColumns )
      nPos := 1
      DO WHILE nPos <= nColCount .AND. ISCHARACTER( acColumns[ nPos ] ) .AND. !Empty( acColumns[ nPos ] )
         nPos++
      ENDDO
      nColCount := nPos - 1

      IF nColCount == 0
         RETURN .F.
      ENDIF
   ELSE
      nColCount := FCount()
   ENDIF

   // Generate the TBrowse columns

   FOR nPos := 1 TO nColCount

      IF ISARRAY( acColumns )
         IF ( nAliasPos := At( "->", acColumns[ nPos ] ) ) > 0
            cAlias := SubStr( acColumns[ nPos ], 1, nAliasPos - 1 )
            cFieldName := SubStr( acColumns[ nPos ], nAliasPos + 2 )
            cHeading := cAlias + "->;" + cFieldName
         ELSE
            cHeading := acColumns[ nPos ]
         ENDIF
         cBlock := acColumns[ nPos ]
      ELSE
         cBlock := FieldName( nPos )
         cHeading := cBlock
      ENDIF

      IF Type( cBlock ) == "M"

         bBlock := {|| "  <Memo>  " }

      ELSEIF "->" $ cBlock

         IF Upper( cAlias ) == "M"
            bBlock := MemvarBlock( cBlock )
         ELSEIF Upper( cAlias ) == "FIELD"
            bBlock := FieldWBlock( cFieldName, Select() )
         ELSE
            bBlock := FieldWBlock( cFieldName, Select( cAlias ) )
         ENDIF

      ELSEIF !Empty( FieldPos( cBlock ) )
         bBlock := FieldWBlock( cBlock, Select() )
      ELSE
         bBlock := NIL
      ENDIF

      IF bBlock == NIL
         bBlock := &( "{||" + cBlock + "}" )
      ENDIF

      IF ISARRAY( xColumnHeaders ) .AND. Len( xColumnHeaders ) >= nPos .AND. ISCHARACTER( xColumnHeaders[ nPos ] )
         cHeading := xColumnHeaders[ nPos ]
      ELSEIF ISCHARACTER( xColumnHeaders )
         cHeading := xColumnHeaders
      ENDIF

      //

      oColumn := TBColumnNew( cHeading, bBlock )

      IF ISARRAY( xColumnSayPictures ) .AND. nPos <= Len( xColumnSayPictures ) .AND. ISCHARACTER( xColumnSayPictures[ nPos ] ) .AND. !Empty( xColumnSayPictures[ nPos ] )
         oColumn:Picture := xColumnSayPictures[ nPos ]
      ELSEIF ISCHARACTER( xColumnSayPictures ) .AND. !Empty( xColumnSayPictures )
         oColumn:Picture := xColumnSayPictures
      ENDIF

      IF ISARRAY( xColumnFootings ) .AND. nPos <= Len( xColumnFootings ) .AND. ISCHARACTER( xColumnFootings[ nPos ] )
         oColumn:Footing := xColumnFootings[ nPos ]
      ELSEIF ISCHARACTER( xColumnFootings )
         oColumn:Footing := xColumnFootings
      ENDIF

      IF ISARRAY( xHeadingSeparators ) .AND. nPos <= Len( xHeadingSeparators ) .AND. ISCHARACTER( xHeadingSeparators[ nPos ] )
         oColumn:HeadSep := xHeadingSeparators[ nPos ]
      ENDIF

      IF ISARRAY( xColumnSeparators ) .AND. nPos <= Len( xColumnSeparators ) .AND. ISCHARACTER( xColumnSeparators[ nPos ] )
         oColumn:ColSep := xColumnSeparators[ nPos ]
      ENDIF

      IF ISARRAY( xFootingSeparators ) .AND. nPos <= Len( xFootingSeparators ) .AND. ISCHARACTER( xFootingSeparators[ nPos ] )
         oColumn:FootSep := xFootingSeparators[ nPos ]
      ENDIF

      oBrowse:AddColumn( oColumn )

   NEXT

   /* --------------------------- */
   /* Go into the processing loop */
   /* --------------------------- */

   IF Eof()
      dbGoBottom()
   ENDIF

   nOldCursor := SetCursor( SC_NONE )
   lException := .F.

   DO WHILE .T.

      DO WHILE !oBrowse:Stabilize() .AND. NextKey() == 0
      ENDDO

      IF ( nKey := InKey() ) == 0

         IF !lException
            IF !dbEditCallUser( oBrowse, xUserFunc, 0 )
               oBrowse:forceStable()
               EXIT
            ENDIF
            oBrowse:forceStable()
         ENDIF

         oBrowse:Hilite()
         nKey := InKey( 0 )
         oBrowse:DeHilite()

         IF ( bAction := SetKey( nKey ) ) != NIL
            Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
            LOOP
         ENDIF

      ENDIF

      lException := .F.

      DO CASE
      CASE nKey == K_DOWN       ; oBrowse:Down()
      CASE nKey == K_UP         ; oBrowse:Up()
      CASE nKey == K_PGDN       ; oBrowse:PageDown()
      CASE nKey == K_PGUP       ; oBrowse:PageUp()
      CASE nKey == K_CTRL_PGUP  ; oBrowse:GoTop()
      CASE nKey == K_CTRL_PGDN  ; oBrowse:GoBottom()
      CASE nKey == K_RIGHT      ; oBrowse:Right()
      CASE nKey == K_LEFT       ; oBrowse:Left()
      CASE nKey == K_HOME       ; oBrowse:Home()
      CASE nKey == K_END        ; oBrowse:End()
      CASE nKey == K_CTRL_LEFT  ; oBrowse:PanLeft()
      CASE nKey == K_CTRL_RIGHT ; oBrowse:PanRight()
      CASE nKey == K_CTRL_HOME  ; oBrowse:PanHome()
      CASE nKey == K_CTRL_END   ; oBrowse:PanEnd()
      OTHERWISE
         IF !dbEditCallUser( oBrowse, xUserFunc, nKey )
            EXIT
         ENDIF
         lException := .T.
      ENDCASE
   ENDDO

   SetCursor( nOldCursor )

   RETURN .T.

STATIC FUNCTION dbEditCallUser( oBrowse, xUserFunc, nKey )
   LOCAL nMode
   LOCAL nResult
   LOCAL nPrevRecNo

   DO CASE
   CASE nKey != 0           ; nMode := DE_EXCEPT
   CASE HB_DBEMPTY()        ; nMode := DE_EMPTY
   CASE oBrowse:hitBottom() ; nMode := DE_HITBOTTOM
   CASE oBrowse:hitTop()    ; nMode := DE_HITTOP
   OTHERWISE                ; nMode := DE_IDLE
   ENDCASE

   oBrowse:forceStable()

   nPrevRecNo := RecNo()

   IF ( ISCHARACTER( xUserFunc ) .AND. !Empty( xUserFunc ) ) .OR. ISBLOCK( xUserFunc )
      nResult := Do( xUserFunc, nMode, oBrowse:ColPos() )
   ELSE
      nResult := iif( nKey == K_ENTER .OR. nKey == K_ESC, DE_ABORT, DE_CONT )
   ENDIF

   IF Eof() .AND. !HB_DBEMPTY()
      dbSkip( -1 )
   ENDIF

   IF nResult == DE_REFRESH .OR. nPrevRecNo != RecNo()

      IF nResult != DE_ABORT

         IF Set( _SET_DELETED ) .AND. Deleted() .OR. !Empty( dbFilter() ) .AND. ! &( dbFilter() )
            dbSkip( 1 )
         ENDIF

         IF Eof()
            dbGoBottom()
         ENDIF

         nPrevRecNo := RecNo()

         oBrowse:RefreshAll():forceStable()
         DO WHILE nPrevRecNo != RecNo()
            oBrowse:Up():forceStable()
         ENDDO

      ENDIF

   ELSE
      oBrowse:Refreshcurrent()
   ENDIF

   RETURN nResult != DE_ABORT

STATIC FUNCTION dbEditSkipped( nRecs )
   LOCAL nSkipped := 0

   IF LastRec() != 0
      IF nRecs == 0
         IF Eof()
            dbSkip( -1 )
            nSkipped := -1
         ELSE
            dbSkip( 0 )
         ENDIF
      ELSEIF nRecs > 0 .AND. RecNo() != LastRec() + 1
         DO WHILE nSkipped < nRecs
            dbSkip( 1 )
            IF Eof()
               dbSkip( -1 )
               EXIT
            ENDIF
            nSkipped++
         ENDDO
      ELSEIF nRecs < 0
         DO WHILE nSkipped > nRecs
            dbSkip( -1 )
            IF Bof()
               EXIT
            ENDIF
            nSkipped--
         ENDDO
      ENDIF
   ENDIF

   RETURN nSkipped

