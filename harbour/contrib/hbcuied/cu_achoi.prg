/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CUI Forms Editor 
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                       Harbour CUI Editor Source
 *
 *                             Pritpal Bedi
 *                               13Aug2011
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "inkey.ch"  
#include "setcurs.ch"
#include "set.ch"    
#include "achoice.ch"
#include "common.ch" 
#include "hbclass.ch"

//----------------------------------------------------------------------//

#define  K_MOVING          1001
#define  K_LEFT_DOWN       1002
#define  K_LEFT_DBLCLICK   1006
#define  K_LEFT_UP         1003
#define  K_RIGHT_DOWN      1004
#define  K_RIGHT_DBLCLICK  1007
#define  K_RIGHT_UP        1005

//----------------------------------------------------------------------//

#define INRANGE( xLo, xVal, xHi )  ( ( xVal >= xLo ) .AND. ( xVal <= xHi ) )
#define BETWEEN( xLo, xVal, xHi )  min( max( xLo, xVal ), xHi )

/*----------------------------------------------------------------------*/

FUNCTION VouchAChoice( nTop, nLft, nBtm, nRgt, acItems, xSelect, cUserFunc, nPos, nHiLytRow, oWin, nLastKey, cargo_ )
   LOCAL nChoice, oChoice
   LOCAL crs := SetCursor( 0 )
   
   oChoice := AChoiceNew():New( nTop, nLft, nBtm, nRgt, acItems, xSelect, ;
                                cUserFunc, ;
                                nPos, nHiLytRow, oWin, nLastKey, cargo_   )
   oChoice:Exe()
   nChoice  := oChoice:nPos
   nLastKey := oChoice:nKey
   oChoice:Destroy()
   
   SetCursor( crs )
   
   RETURN ( nChoice )

/*----------------------------------------------------------------------*/

CREATE CLASS AChoiceNew

   VAR nTop, nLeft, nBottom, nRight
   VAR acItems
   VAR xSelect
   VAR cUserFunc
   VAR nPos
   VAR nHiLiteRow
   VAR oWin
   VAR cargo_
   VAR nNumCols
   VAR nNumRows
   VAR acCopy
   VAR alSelect
   VAR nNewPos
   VAR lFinished
   VAR nKey
   VAR nMode
   VAR nAtTop
   VAR nAtBtm
   VAR nItems
   VAR bScan
   VAR lUserFunc
   VAR nUserFunc
   VAR bUserFunc
   VAR cLoClr
   VAR cHiClr
   VAR cUnClr
   VAR nFrstItem
   VAR nLastItem
   VAR bAction
   VAR mrc_

   METHOD init
   METHOD Destroy
   METHOD DispPageNew
   METHOD DispLineNew
   METHOD Up
   METHOD Down
   METHOD PageUp
   METHOD PageDown
   METHOD GoTop
   METHOD GoBottom
   METHOD Top
   METHOD Bottom
   METHOD GoTo
   METHOD Exe
   METHOD DeHilite
   METHOD HiLite
   METHOD DispAtNew

ENDCLASS

//----------------------------------------------------------------------//

METHOD AChoiceNew:Destroy()

   RETURN NIL

//----------------------------------------------------------------------//

METHOD AChoiceNew:init( nTop, nLft, nBtm, nRgt, acItems, xSelect, ;
                        cUserFunc, nPos, nHiLiteRow, oWin, nLastKey, cargo_ )
   LOCAL nCntr
   
   HB_SYMBOL_UNUSED( nLastKey )
   
   DEFAULT nTop       TO 0              // The topmost row of the window
   DEFAULT nLft       TO 0              // The leftmost column of the window
   DEFAULT nBtm       TO maxrow() + 1   // The bottommost row of the windows
   DEFAULT nRgt       TO maxcol() + 1   // The rightmost column of the window
   DEFAULT acItems    TO {}             // The items FROM which TO choose
   DEFAULT xSelect    TO .T.            // Array OR logical, what is selectable
   DEFAULT cUserFunc  TO NIL            // Optional FUNCTION FOR key exceptions
   DEFAULT nPos       TO 1              // The number of the selected item
   DEFAULT nHiLiteRow TO 0              // The row TO be highlighted
   
   ::nTop        := nTop
   ::nLeft       := nLft
   ::nBottom     := nBtm
   ::nRight      := nRgt
   ::acItems     := acItems
   ::xSelect     := xSelect
   ::cUserFunc   := cUserFunc
   ::nPos        := nPos
   ::nHiLiteRow  := nHiLiteRow
   ::oWin        := oWin
   ::cargo_      := cargo_
   
   ::nNumCols    := 0                    // Number of columns IN the window
   ::nNumRows    := 0                    // Number of rows IN the window
   ::acCopy      := {}                   // A padded copy of the items
   ::alSelect    := {}                   // Select permission
   ::nNewPos     := 0                    // The NEXT item TO be selected
   ::lFinished   := .F.                  // Is processing finished?
   ::nKey        := 0                    // The keystroke TO be processed
   ::nMode       := AC_IDLE              // The current operating mode
   ::nAtTop      := 1                    // The number of the item at the top
   ::nAtBtm      := 1                    // The number of the item at the bottom
   ::nItems      := 0                    // The number of items
   ::bScan       := { | cX | IF( left( cX, 1 ) == upper( chr( ::nKey ) ), .T., .F. ) }
   ::lUserFunc   := ( !empty( ::cUserFunc ) )
   ::nUserFunc   := 0                    // RETURN value FROM user FUNCTION
   ::bUserFunc   := { || AC_ABORT }      // Block form of user FUNCTION
   ::cLoClr      := Before( ",", setcolor() )
   ::cHiClr      := Before( ",", After( ",", setcolor() ) )
   ::cUnClr      := After( ",", After( ",", After( ",", After( ",", setcolor() ) ) ) )
   ::nFrstItem   := 0
   ::nLastItem   := 0
   ::bAction     := NIL
   ::mrc_        := {}
   
   IF ::lUserFunc
      ::bUserFunc := &( "{|nM,nP,nH,nK,aC|" + ::cUserFunc + "(nM,nP,nH,nK,aC)}" )
   ENDIF
   
   IF empty( ::cHiClr )
      ::cHiClr := After( "/", ::cLoClr ) + "/" + Before( "/", ::cLoClr )
   ENDIF
   
   IF empty( ::cUnClr )
      ::cUnClr := ::cLoClr
   ENDIF
   
   ::nNumCols := ::nRight - ::nLeft + 1
   ::nNumRows := ::nBottom - ::nTop + 1
   
   aeval( ::acItems, { | x | IF( valtype( x ) == "C", aadd( ::acCopy, padr( x, ::nNumCols ) ), .F. ) } )
   ::nItems := len( ::acCopy )
   
   ::alSelect := array( ::nItems )
   
   IF valtype( ::xSelect ) == "A"
      afill( ::alSelect, .T. )
      FOR nCntr := 1 TO len( ::xSelect )
         IF nCntr <= ::nItems
            IF valtype( ::xSelect[ nCntr ] ) == "C"
               IF empty( ::xSelect[ nCntr ] )
                  ::lFinished := .T.
                  ::nPos      := 0
               ELSE
                  ::alSelect[ nCntr ] := &( ::xSelect[ nCntr ] )
               ENDIF
            ELSE
               ::alSelect[ nCntr ] := ::xSelect[ nCntr ]
            ENDIF
         ELSE
            nCntr := len( ::xSelect ) + 1
         ENDIF
      NEXT
   ELSE
      afill( ::alSelect, ::xSelect )
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Exe()

   IF !( ::lFinished )
      ::nFrstItem := ascan( ::alSelect, .T. )  // First valid item
   
      IF ::nFrstItem == 0
         ::nLastItem := 0
         ::nPos      := 0
         ::nMode     := AC_NOITEM
      ELSE
         ::nMode     := AC_IDLE
         ::nLastItem := ::nItems
         DO WHILE ( !( ::alSelect[ ::nLastItem ] ) )
            ::nLastItem--
         ENDDO
      ENDIF
   
      // Ensure hilighted item can be selected
      ::nPos := BETWEEN( ::nFrstItem, ::nPos, ::nLastItem )
      ::nNewPos := ::nPos
      IF !( ::alSelect[ ::nNewPos ] )
         IF ::nNewPos == ::nLastItem
            ::nNewPos := ::nFrstItem
         ENDIF
         DO WHILE ( !( ::alSelect[ ::nNewPos ] ) )
            ::nNewPos++
         ENDDO
      ENDIF
      ::nPos := ::nNewPos
   
      // Force hilighted row TO be valid
      //
      ::nHiLiteRow := BETWEEN( 0, ::nHiLiteRow, ::nNumRows - 1 )
   
      // Force the topmost item TO be a valid index of the array
      //
      ::nAtTop := BETWEEN( 1, max( 1, ::nPos - ::nHiLiteRow ), ::nItems )
   
      // Ensure as much of the selection area as possible is covered
      //
      IF ( ::nAtTop + ::nNumRows - 1 ) > ::nItems
         ::nAtTop := max( 1, ::nItems - ::nNumrows + 1 )
      ENDIF
   
      ::DispPageNew()
   ENDIF
   
   DO WHILE ( !::lFinished )
   
      IF ::nMode != AC_GOTO .AND. ::nMode != AC_NOITEM
         ::nKey  := inkey( , INKEY_ALL + HB_INKEY_GTEVENT )
         ::nMode := AC_IDLE
         ::mrc_  := { 0, 0, mRow(), mCol(), 0, LastKey(), .f. }
      ENDIF
   
   #ifdef __WVT__
      IF nLastPos <> ::nPos
         Wvt_DrawFocusRect( ::nTop + ( ::nPos - ::nAtTop ), ::nLeft, ;
                            ::nTop + ( ::nPos - ::nAtTop ), ::nRight )
         nLastPos := ::nPos
      ENDIF
   #ENDIF
   
      DO CASE
      CASE ( ::bAction := SetKey( ::nKey ) ) != NIL
         eval( ::bAction, ProcName( 1 ), ProcLine( 1 ), '' )
   
      CASE ::nKey == K_MOVING
         ::nPos := ::DispAtNew()
   
      CASE ::nKey == K_MWFORWARD
         ::Up()
   
      CASE ::nKey == K_MWBACKWARD
         ::Down()
   
      CASE ::nKey == K_LDBLCLK
         ::nPos := ::DispAtNew()
         ::nMode  := AC_SELECT
   
      CASE ::nKey == K_LEFT_DOWN
         IF ::mrc_[ 3 ] >= ::nTop  .AND. ::mrc_[ 3 ] <= ::nBottom .AND. ;
            ::mrc_[ 4 ] >= ::nLeft .AND. ::mrc_[ 4 ] <= ::nRight
            keyboard( chr( K_ENTER ) )
         ENDIF
   
      CASE ( ( ::nKey == K_ESC ) .OR. ( ::nMode == AC_NOITEM ) ) .AND. ( !::lUserFunc )
         ::nMode     := AC_ABORT
         ::nPos      := 0
         ::lFinished := .T.
   
      CASE ::nKey == K_UP
         ::Up()
   
      CASE ::nKey == K_DOWN
         ::Down()
   
      CASE ::nKey == K_PGUP
         ::PageUp()
   
      CASE ::nKey == K_PGDN
         ::PageDown()
   
      CASE ::nKey == K_HOME
         ::Top()
   
      CASE ::nKey == K_END
         ::Bottom()
   
      CASE ( ::nKey == K_CTRL_HOME .OR. ::nKey == K_CTRL_PGUP )
         ::GoTop()
   
      CASE ( ::nKey == K_CTRL_END .OR. ::nKey == K_CTRL_PGDN )
         ::GoBottom()
   
      CASE ( ::nKey == K_ENTER ) .AND. ( !::lUserFunc )
         ::nMode     := AC_SELECT
         ::lFinished := .T.
   
      CASE ( ::nKey == K_RIGHT ) .AND. ( !::lUserFunc )
         ::nPos      := 0
         ::lFinished := .T.
   
      CASE ( ::nKey == K_LEFT ) .AND. ( !::lUserFunc )
         ::nPos      := 0
         ::lFinished := .T.
   
      CASE INRANGE( 32, ::nKey, 255 ) .AND. ( ( !::lUserFunc ) .OR. ( ::nMode == AC_GOTO ) )
         ::GoTo()
         ::nMode := AC_IDLE
   
      CASE ::nMode == AC_GOTO
         ::nMode := AC_IDLE
   
      OTHERWISE
         IF ::nKey == 0
            ::nMode := AC_IDLE
         ELSE
            ::nMode := AC_EXCEPT
         ENDIF
   
      ENDCASE
   
      IF ::lUserFunc
         ::nUserFunc := eval( ::bUserFunc, ::nMode, ::nPos, ;
                              ::nPos - ::nAtTop, ::nKey, ::cargo_ )
         DO CASE
         CASE ::nUserFunc == AC_ABORT
            ::lFinished := .T.
            ::nPos      := 0
   
         CASE ::nUserFunc == AC_SELECT
            ::lFinished := .T.
   
         CASE ::nUserFunc == AC_CONT
   
         CASE ::nUserFunc == AC_GOTO
            ::nMode := AC_GOTO
   
         ENDCASE
      ENDIF
   ENDDO
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DispPageNew()
   LOCAL nCntr
   LOCAL nRow := row()
   LOCAL nCol := col()
   LOCAL nRowPos, nPos
   
   DispBegin()
   
   FOR nCntr := 1 TO ::nNumRows
      nRowPos := ::nTop   + nCntr - 1
      nPos    := ::nAtTop + nCntr - 1
   
      IF INRANGE( 1, nPos, ::nItems )
         ::DispLineNew( nPos, nRowPos, nPos == ::nPos )
      ELSE
         DispOutAt( nRowPos, ::nLeft, space( len( ::acCopy[ 1 ] ) ), ::cLoClr, ::oWin )
      ENDIF
   NEXT
   
   DispEnd()
   
   SetPos( nRow,nCol )
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DispLineNew( nPos, nRow, lHiLite )

   DispOutAt( nRow, ::nLeft, ::acCopy[ nPos ],;
                IF( ::alSelect[ nPos ], ;
                  IF( lHiLite, ::cHiClr, ::cLoClr ), ::cUnClr ), ::oWin )
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DeHilite()

   ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .F. )
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:HiLite()

   ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .T. )
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Up()
   LOCAL nScroll
   
   IF ::nPos == ::nFrstItem
      ::nMode := AC_HITTOP
      IF ::nAtTop > max( 1, ::nPos - ::nNumRows + 1 )
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nPos - 1
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos--
      ENDDO
   
      IF INRANGE( ::nAtTop, ::nNewPos, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ELSE
         DispBegin()
   
         ::DeHilite()
   
         nScroll := max( -::nNumRows, ( ::nNewPos - ( ::nAtTop + ::nNumRows - 1 ) ) )
         Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, nScroll )
   
         ::nAtTop := ::nNewPos
         ::nPos   := max( ::nPos, ::nAtTop + ::nNumRows - 1 )
   
         DO WHILE ( ::nPos > ::nNewPos )
            ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .F. )
            ::nPos--
         ENDDO
   
         ::HiLite()
   
         Dispend()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Down()
   LOCAL nScroll
   
   IF ::nPos == ::nLastItem
      ::nMode := AC_HITBOTTOM
      IF ::nAtTop < min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::nAtTop := min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nPos + 1
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos++
      ENDDO
   
      IF INRANGE( ::nAtTop, ::nNewPos, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ELSE
         Dispbegin()
   
         ::DeHilite()
   
         nScroll := min( ::nNumRows, ( ::nNewPos - ( ::nAtTop + ::nNumRows - 1 ) ) )
         scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, nScroll )
   
         ::nAtTop := ::nNewPos - ::nNumRows + 1
         ::nPos   := max( ::nPos, ::nAtTop )
         DO WHILE ( ::nPos < ::nNewPos )
            ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .F. )
            ::nPos ++
         ENDDO
   
         ::Hilite()
   
         Dispend()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:PageUp()

   IF ::nPos == ::nFrstItem
      ::nMode := AC_HITTOP
      IF ::nAtTop > max( 1, ::nPos - ::nNumRows + 1 )
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      IF INRANGE( ::nAtTop, ::nFrstItem, ::nAtTop + ::nNumRows - 1 )
         ::nPos   := ::nFrstItem
         ::nAtTop := max( ::nPos - ::nNumRows + 1, 1 )
         ::DispPageNew()
      ELSE
         IF ( ::nPos - ::nNumRows + 1 ) < ::nFrstItem
            ::nPos   := ::nFrstItem
            ::nAtTop := ::nFrstItem
         ELSE
            ::nPos   := max( ::nFrstItem, ::nPos - ::nNumRows + 1 )
            ::nAtTop := max( 1, ::nAtTop - ::nNumRows + 1 )
            DO WHILE ( ::nPos > ::nFrstItem ) .AND. ( !( ::alSelect[ ::nPos ] ) )
               ::nPos--
               ::nAtTop--
            ENDDO
            ::nAtTop := max( 1, ::nAtTop )
         ENDIF
         ::DispPageNew()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:PageDown()
   LOCAL nGap
   
   IF ::nPos == ::nLastItem
      ::nMode := AC_HITBOTTOM
      IF ::nAtTop < min( ::nPos, max( 1, ::nItems - ::nNumRows + 1 ) )
         ::nAtTop := min( ::nPos, max( 1, ::nItems - ::nNumRows + 1 ) )
         ::DispPageNew()
      ENDIF
   ELSE
      IF INRANGE( ::nAtTop, ::nLastItem, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nLastItem
         ::Hilite()
      ELSE
         nGap := ::nPos - ::nAtTop
         ::nPos := min( ::nLastItem, ::nPos + ::nNumRows - 1 )
         IF ( ::nPos + ::nNumRows - 1 ) > ::nLastItem
            ::nAtTop := ::nLastItem - ::nNumRows + 1
            ::nPos   := min( ::nLastItem, ::nAtTop + nGap )
         ELSE
            ::nAtTop := ::nPos - nGap
         ENDIF
   
         DO WHILE ( ::nPos < ::nLastItem ) .AND. !( ::alSelect[ ::nPos ] )
            ::nPos++
            ::nAtTop++
         ENDDO
   
         DO WHILE ( ::nAtTop + ::nNumRows - 1 ) > ::nItems
            ::nAtTop--
         ENDDO
         ::DispPageNew()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Top()

   IF ::nPos == ::nFrstItem
      IF ::nAtTop == max( 1, ::nPos - ::nNumRows + 1 )
         ::nMode := AC_HITTOP
      ELSE
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nAtTop
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos++
      ENDDO
      IF ::nNewPos != ::nPos
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Bottom()

   IF ::nPos == ::nLastItem
      IF ::nAtTop == min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::nMode := AC_HITBOTTOM
      ELSE
         ::nAtTop := min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nAtTop + ::nNumRows - 1
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos--
      ENDDO
      IF ::nNewPos != ::nPos
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:GoTop()

   IF ::nPos == ::nFrstItem
      IF ::nAtTop == max( 1, ::nPos - ::nNumRows + 1 )
         ::nMode := AC_HITTOP
      ELSE
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nPos   := ::nFrstItem
      ::nAtTop := ::nPos
      ::DispPageNew()
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:GoBottom()

   IF ::nPos == ::nLastItem
      IF ::nAtTop == min( ::nLastItem, ::nItems - ::nNumRows + 1 )
         ::nMode := AC_HITBOTTOM
      ELSE
         ::nAtTop := min( ::nLastItem, ::nItems - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      IF INRANGE( ::nAtTop, ::nLastItem, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nLastItem
         ::HiLite()
      ELSE
         ::nPos   := ::nLastItem
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:GoTo()

   ::nNewPos := ascan( ::acCopy, ::bScan, ::nPos + 1 )
   DO WHILE INRANGE( ::nPos, ::nNewPos, ::nLastItem ) .AND. !( ::alSelect[ ::nNewPos ] )
      ::nNewPos := ascan( ::acCopy, ::bScan, ::nNewPos + 1 )
   ENDDO
   
   IF ::nNewPos == 0
      ::nNewPos := ascan( ::acCopy, ::bScan )
      DO WHILE INRANGE( 1, ::nNewPos, ::nLastItem ) .AND. !( ::alSelect[ ::nNewPos ] )
         ::nNewPos := ascan( ::acCopy, ::bScan, ::nNewPos + 1 )
      ENDDO
   ENDIF
   
   IF INRANGE( ::nFrstItem, ::nNewPos, ::nLastItem ) .AND. ::alSelect[ ::nNewPos ]
      IF INRANGE( ::nAtTop, ::nNewPos, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ELSE
         ::nPos   := ::nNewPos
         ::nAtTop := BETWEEN( 1, ::nPos - ::nNumRows + 1, ::nItems )
         ::DispPageNew()
      ENDIF
   ENDIF
   
   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DispAtNew()
   LOCAL nNewPos
   
   IF ::mrc_[ 3 ] >= ::nTop .AND. ::mrc_[ 3 ] <= ::nTop + ::nNumRows - 1 ;
                                 .AND. ;
            ::mrc_[ 4 ] >= ::nLeft .AND. ::mrc_[ 4 ] <= ::nRight
   
      IF ( nNewPos := ::nAtTop + ( ::mrc_[ 3 ] - ::nTop ) ) <> ::nPos
         IF ::alSelect[ nNewPos ]
            ::DeHilite()
            ::nPos    := nNewPos
            ::nNewPos := ::nPos
            ::HiLite()
         ENDIF
      ENDIF
   ENDIF
   
   RETURN ::nPos

//----------------------------------------------------------------------//

STATIC FUNCTION Before( cDelim, cValue )
   LOCAL cRetVal := cValue
   
   IF cDelim $ cValue
      cRetVal := left( cValue, at( cDelim, cValue ) - 1 )
   ENDIF
   
   RETURN ( cRetVal )

//----------------------------------------------------------------------//

STATIC FUNCTION After( cDelim, cValue )
   LOCAL cRetVal := ''
   
   IF cDelim $ cValue
      cRetVal := substr( cValue, at( cDelim, cValue ) + 1 )
   ENDIF
   
   RETURN ( cRetVal )

//----------------------------------------------------------------------//

