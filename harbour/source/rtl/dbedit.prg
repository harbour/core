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

#include "common.ch"
#include "dbedit.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define HB_DBEMPTY() ( LastRec() == 0 .OR. ( ( Eof() .OR. RecNo() == LastRec() + 1 ) .AND. Bof() ) )

/* NOTE: Extension: Harbour supports codeblocks as the xUserFunc parameter 
         [vszakats] */
/* NOTE: Clipper is buggy and will throw an error if the number of
         columns is zero. (Check: dbEdit(0,0,20,20,{})) [vszakats] */
/* NOTE: Clipper will throw an error if there's no database open [vszakats] */
/* NOTE: The NG says that the return value is NIL, but it's not. [vszakats] */
/* NOTE: There's an undocumented result code in Clipper (3), which is not
         supported in Harbour. [vszakats] */
/* NOTE: Harbour is multithreading ready/reentrant, Clipper is not. 
         [vszakats] */

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

