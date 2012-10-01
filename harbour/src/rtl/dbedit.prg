/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBEDIT() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "dbedit.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* NOTE: Extension: Harbour supports codeblocks as the xUserFunc parameter
         [vszakats] */
/* NOTE: Clipper is buggy and will throw an error if the number of
         columns is zero. (Check: dbEdit(0,0,20,20,{})) [vszakats] */
/* NOTE: Clipper will throw an error if there's no database open [vszakats] */
/* NOTE: The NG says that the return value is NIL, but it's not. [vszakats] */
/* NOTE: Harbour is multithreading ready and Clipper only reentrant safe
         [vszakats] */

FUNCTION DBEDIT( nTop, nLeft, nBottom, nRight, ;
                 acColumns, xUserFunc, ;
                 xColumnSayPictures, xColumnHeaders, ;
                 xHeadingSeparators, xColumnSeparators, ;
                 xFootingSeparators, xColumnFootings )

   LOCAL nOldCUrsor, nKey, lContinue, nPos, nAliasPos, nColCount
   LOCAL lDoIdleCall, lAppend, lFlag
   LOCAL cHeading, cBlock
   LOCAL bBlock
   LOCAL oBrowse
   LOCAL oColumn
   LOCAL aCol

   IF !Used()
      RETURN .F.
   ELSEIF EOF()
      dbGoBottom()
   ENDIF

   IF ! HB_ISNUMERIC( nTop ) .OR. nTop < 0
      nTop := 0
   ENDIF
   IF ! HB_ISNUMERIC( nLeft ) .OR. nLeft < 0
      nLeft := 0
   ENDIF
   IF ! HB_ISNUMERIC( nBottom ) .OR. nBottom > MaxRow() .OR. nBottom < nTop
      nBottom := MaxRow()
   ENDIF
   IF ! HB_ISNUMERIC( nRight ) .OR. nRight > MaxCol() .OR. nRight < nLeft
      nRight := MaxCol()
   ENDIF

   oBrowse := TBrowseDb( nTop, nLeft, nBottom, nRight )
   oBrowse:headSep   := iif( HB_ISSTRING( xHeadingSeparators ), xHeadingSeparators, hb_UTF8ToStrBox( "═╤═" ) )
   oBrowse:colSep    := iif( HB_ISSTRING( xColumnSeparators ), xColumnSeparators, hb_UTF8ToStrBox( " │ " ) )
   oBrowse:footSep   := iif( HB_ISSTRING( xFootingSeparators ), xFootingSeparators, "" )
   oBrowse:skipBlock := {| nRecs | Skipped( nRecs, lAppend ) }
   oBrowse:autoLite  := .F. /* Set to .F. just like in CA-Cl*pper. [vszakats] */

   IF HB_ISARRAY( acColumns )
      nColCount := 0
      FOR EACH aCol IN acColumns
         IF HB_ISSTRING( aCol ) .AND. !Empty( aCol )
            nColCount++
         ELSE
            EXIT
         ENDIF
      NEXT
   ELSE
      nColCount := FCount()
   ENDIF

   IF nColCount == 0
      RETURN .F.
   ENDIF

   /* Generate the TBrowse columns */

   FOR nPos := 1 TO nColCount

      IF HB_ISARRAY( acColumns )
         cBlock := acColumns[ nPos ]
         IF ( nAliasPos := At( "->", cBlock ) ) > 0
            cHeading := SubStr( cBlock, 1, nAliasPos - 1 ) + "->;" + ;
                        SubStr( cBlock, nAliasPos + 2 )
         ELSE
            cHeading := cBlock
         ENDIF
      ELSE
         cBlock := FieldName( nPos )
         cHeading := cBlock
      ENDIF

      /* Simplified logic compared to CA-Cl*pper. In the latter there
         is logic to detect several typical cBlock types (memvar,
         aliased field, field) and using MemvarBlock()/FieldWBlock()/FieldBlock()
         calls to create codeblocks for them if possible. In Harbour,
         simple macro compilation will result in faster code for all
         situations. As Maurilio Longo has pointed, there is no point in
         creating codeblocks which are able to _assign_ values, as dbEdit()
         is a read-only function. [vszakats] */

      bBlock := iif( Type( cBlock ) == "M", {|| "  <Memo>  " }, hb_macroBlock( cBlock ) )

      /* ; */

      IF HB_ISARRAY( xColumnHeaders ) .AND. Len( xColumnHeaders ) >= nPos .AND. HB_ISSTRING( xColumnHeaders[ nPos ] )
         cHeading := xColumnHeaders[ nPos ]
      ELSEIF HB_ISSTRING( xColumnHeaders )
         cHeading := xColumnHeaders
      ENDIF

      oColumn := TBColumnNew( cHeading, bBlock )

      IF HB_ISARRAY( xColumnSayPictures ) .AND. nPos <= Len( xColumnSayPictures ) .AND. HB_ISSTRING( xColumnSayPictures[ nPos ] ) .AND. !Empty( xColumnSayPictures[ nPos ] )
         oColumn:picture := xColumnSayPictures[ nPos ]
      ELSEIF HB_ISSTRING( xColumnSayPictures ) .AND. !Empty( xColumnSayPictures )
         oColumn:picture := xColumnSayPictures
      ENDIF

      IF HB_ISARRAY( xColumnFootings ) .AND. nPos <= Len( xColumnFootings ) .AND. HB_ISSTRING( xColumnFootings[ nPos ] )
         oColumn:footing := xColumnFootings[ nPos ]
      ELSEIF HB_ISSTRING( xColumnFootings )
         oColumn:footing := xColumnFootings
      ENDIF

      IF HB_ISARRAY( xHeadingSeparators ) .AND. nPos <= Len( xHeadingSeparators ) .AND. HB_ISSTRING( xHeadingSeparators[ nPos ] )
         oColumn:headSep := xHeadingSeparators[ nPos ]
      ENDIF

      IF HB_ISARRAY( xColumnSeparators ) .AND. nPos <= Len( xColumnSeparators ) .AND. HB_ISSTRING( xColumnSeparators[ nPos ] )
         oColumn:colSep := xColumnSeparators[ nPos ]
      ENDIF

      IF HB_ISARRAY( xFootingSeparators ) .AND. nPos <= Len( xFootingSeparators ) .AND. HB_ISSTRING( xFootingSeparators[ nPos ] )
         oColumn:footSep := xFootingSeparators[ nPos ]
      ENDIF

      oBrowse:addColumn( oColumn )

   NEXT

   nOldCUrsor := SetCursor( SC_NONE )

   /* --------------------------- */
   /* Go into the processing loop */
   /* --------------------------- */

   lAppend := .F.
   lFlag := .T.
   lDoIdleCall := .T.
   lContinue := .T.

   DO WHILE lContinue

      DO WHILE ! oBrowse:stabilize()
         nKey := Nextkey()
#ifdef HB_COMPAT_C53
         IF nKey != 0 .AND. nKey != K_MOUSEMOVE
#else
         IF nKey != 0
#endif
            EXIT
         ENDIF
      ENDDO

      IF ( nKey := Inkey() ) == 0
         IF lDoIdleCall
            lContinue := CallUser( oBrowse, xUserFunc, 0, @lAppend, @lFlag )
            oBrowse:forceStable()
         ENDIF
         IF lContinue .AND. lFlag
            oBrowse:hiLite()
#ifdef HB_COMPAT_C53
            DO WHILE ( nKey := Inkey( 0 ) ) == K_MOUSEMOVE
            ENDDO
#else
            nKey := Inkey( 0 )
#endif
            oBrowse:deHilite()
            IF ( bBlock := SetKey( nKey ) ) != NIL
               Eval( bBlock, ProcName( 1 ), ProcLine( 1 ), "" )
               LOOP
            ENDIF
         ELSE
            lFlag := .T.
         ENDIF
      ENDIF

      lDoIdleCall := .T.

      IF nKey != 0
#ifdef HB_CLP_UNDOC
         IF lAppend
            SWITCH nKey
               CASE K_DOWN
               CASE K_PGDN
               CASE K_CTRL_PGDN
                  oBrowse:hitBottom := .T.
                  LOOP
               CASE K_UP
               CASE K_PGUP
               CASE K_CTRL_PGUP
                  oBrowse:hitTop := .T.
                  LOOP
            ENDSWITCH
         ENDIF
#endif
         SWITCH nKey
#ifdef HB_COMPAT_C53
            CASE K_LBUTTONDOWN
            CASE K_LDBLCLK
               TBMouse( oBrowse, MRow(), MCol() )
               EXIT
#endif
            CASE K_DOWN          ; oBrowse:down()     ; EXIT
            CASE K_UP            ; oBrowse:up()       ; EXIT
            CASE K_PGDN          ; oBrowse:pageDown() ; EXIT
            CASE K_PGUP          ; oBrowse:pageUp()   ; EXIT
            CASE K_CTRL_PGUP     ; oBrowse:goTop()    ; EXIT
            CASE K_CTRL_PGDN     ; oBrowse:goBottom() ; EXIT
            CASE K_RIGHT         ; oBrowse:right()    ; EXIT
            CASE K_LEFT          ; oBrowse:left()     ; EXIT
            CASE K_HOME          ; oBrowse:home()     ; EXIT
            CASE K_END           ; oBrowse:end()      ; EXIT
            CASE K_CTRL_LEFT     ; oBrowse:panLeft()  ; EXIT
            CASE K_CTRL_RIGHT    ; oBrowse:panRight() ; EXIT
            CASE K_CTRL_HOME     ; oBrowse:panHome()  ; EXIT
            CASE K_CTRL_END      ; oBrowse:panEnd()   ; EXIT
            OTHERWISE
               lContinue := CallUser( oBrowse, xUserFunc, nKey, @lAppend, @lFlag )
               lDoIdleCall := .F.
               EXIT
         ENDSWITCH
      ENDIF
   ENDDO

   SetCursor( nOldCUrsor )

   RETURN .T.


/* NOTE: CA-Cl*pper uses intermediate function CALLUSER()
 *       to execute user function. We're replicating this behavior
 *       for code which may check ProcName() results in user function
 */
STATIC FUNCTION CallUser( oBrowse, xUserFunc, nKey, lAppend, lFlag )

   LOCAL nPrevRecNo

   LOCAL nAction
   LOCAL nMode := iif( nKey != 0,                  DE_EXCEPT,    ;
                  iif( !lAppend .AND. IsDbEmpty(), DE_EMPTY,     ;
                  iif( oBrowse:hitBottom,          DE_HITBOTTOM, ;
                  iif( oBrowse:hitTop,             DE_HITTOP, DE_IDLE ) ) ) )

   oBrowse:forceStable()

   nPrevRecNo := RecNo()

   /* NOTE: CA-Cl*pper won't check the type of the return value here,
            and will crash if it's a non-NIL, non-numeric type. We're
            replicating this behavior. */
   nAction := iif( HB_ISBLOCK( xUserFunc ), ;
                                 Eval( xUserFunc, nMode, oBrowse:colPos ), ;
              iif( HB_ISSTRING( xUserFunc ) .AND. !Empty( xUserFunc ), ;
                                 &xUserFunc( nMode, oBrowse:colPos ), ;
              iif( nKey == K_ENTER .OR. nKey == K_ESC, DE_ABORT, DE_CONT ) ) )

   IF !lAppend .AND. EOF() .AND. !IsDbEmpty()
      dbSkip( -1 )
   ENDIF

#ifdef HB_CLP_UNDOC
   IF nAction == DE_APPEND

      IF ( lAppend := !( lAppend .AND. EOF() ) )
         dbGoBottom()
         oBrowse:down()
      ELSE
         oBrowse:refreshAll():forceStable()
      ENDIF
      lFlag := .F.
      RETURN .T.
   ENDIF
#endif

   IF nAction == DE_REFRESH .OR. nPrevRecNo != RecNo()

      IF nAction != DE_ABORT

         lAppend := .F.

         IF ( Set( _SET_DELETED ) .AND. Deleted() ) .OR. ;
            ( !Empty( dbfilter() ) .AND. !&( dbFilter() ) )
            dbSkip()
         ENDIF
         IF EOF()
            dbGoBottom()
         ENDIF

         nPrevRecNo := RecNo()
         oBrowse:refreshAll():forceStable()
         DO WHILE nPrevRecNo != RecNo()
            oBrowse:Up():forceStable()
         ENDDO

         lFlag := .F.

      ENDIF
   ELSE
      oBrowse:refreshCurrent()
   ENDIF

   RETURN nAction != DE_ABORT


/* helper function to detect empty tables. It's not perfect but
 * it functionally uses the same conditions as CA-Cl*pper
 */
STATIC FUNCTION IsDbEmpty()

   RETURN LastRec() == 0 .OR. ;
          ( BOF() .AND. ( EOF() .OR. RecNo() == LastRec() + 1 ) )

/* Helper function: TBrowse skipBlock */
STATIC FUNCTION Skipped( nRecs, lAppend )

   LOCAL nSkipped := 0

   IF LastRec() != 0
      IF nRecs == 0
         IF EOF() .AND. !lAppend
            dbSkip( -1 )
            nSkipped := -1
         ELSE
            dbSkip( 0 )
         ENDIF
      ELSEIF nRecs > 0 .AND. RecNo() != LastRec() + 1
         DO WHILE nSkipped < nRecs
            dbSkip()
            IF Eof()
               IF lAppend
                  nSkipped++
               ELSE
                  dbSkip( -1 )
               ENDIF
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
