/*
 * Harbour Project source code:
 * MySQL TBrowse
 * A TBrowse on a MySQL Table / query
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbclass.ch"

#include "inkey.ch"
#include "dbstruct.ch"

#include "mysql.ch"

/* NOTE:

   In fact no, the 'regular syntax is the same as the VO one,

   ACCESS Block METHOD Block()
   or
   ACCESS Block INLINE ::MyVal

   and

   ASSIGN Block( x ) METHOD Block( x )
   or
   ASSIGN Block( x ) INLINE ::MyVal := x

*/


CREATE CLASS TBColumnSQL FROM TBColumn

   VAR   oBrw                 // pointer to Browser containing this column, needed to be able to
                              // retreive field values from Browse instance variable oCurRow
// VAR   Picture              // From clipper 5.3
   VAR   nFieldNum            // This column maps field num from query

   MESSAGE  Block METHOD Block()          // When evaluating code block to get data from source this method
                                          // gets called. I need this since inside TBColumn Block I cannot
                                          // reference Column or Browser instance variables

   METHOD   New( cHeading, bBlock, oBrw )   // Saves inside column a copy of container browser

ENDCLASS


METHOD New( cHeading, bBlock, oBrw ) CLASS TBColumnSQL

   ::super:New( cHeading, bBlock )
   ::oBrw := oBrw

   RETURN Self


METHOD Block() CLASS TBColumnSQL

   LOCAL xValue := ::oBrw:oCurRow:FieldGet( ::nFieldNum )
   LOCAL xType := ::oBrw:oCurRow:FieldType( ::nFieldNum )

   DO CASE
   CASE xType == "N"
      xValue := "'" + Str( xValue, ::oBrw:oCurRow:FieldLen( ::nFieldNum ), ::oBrw:oCurRow:FieldDec( ::nFieldNum ) ) + "'"

   CASE xType == "D"
      xValue :=  "'" + DToC( xValue ) + "'"

   CASE xType == "L"
      xValue := iif( xValue, ".T.", ".F." )

   CASE xType == "C"
      // That is: if there is a double quote inside text substitute it with a string
      // which gets converted back to a double quote by macro operator. If not it would
      // give an error because of unbalanced double quotes.
      xValue := '"' + StrTran( xValue, '"', e"\" + '\"' + \"" ) + '"'

   CASE xType == "M"
      xValue := "' <MEMO> '"

   OTHERWISE
      xValue := "'" + xValue + "'"
   ENDCASE

   RETURN hb_macroBlock( xValue )

/* -------------------------------------------------------- */

/*
   This class is more or less like a TBrowseDB() object in that it receives an oQuery/oTable
   object and gives back a browseable view of it
*/
CREATE CLASS TBrowseSQL FROM TBrowse

   VAR      oCurRow                       // Active row inside table / sql query
   VAR      oQuery                        // Query / table object which we are browsing

   METHOD   New( nTop, nLeft, nBottom, nRight, oServer, oQuery, cTable )

   METHOD   EditField()                   // Editing of hilighted field, after editing does an update of
                                          // corresponding row inside table

   METHOD   BrowseTable( lCanEdit, aExitKeys ) // Handles standard moving inside table and if lCanEdit == .T.
                                               // allows editing of field. It is the stock ApplyKey() moved inside a table
                                               // if lCanEdit K_DEL deletes current row
                                               // When a key is pressed which is present inside aExitKeys it leaves editing loop

   METHOD   KeyboardHook( nKey )               // Where do all unknown keys go?

ENDCLASS


METHOD New( nTop, nLeft, nBottom, nRight, oServer, oQuery, cTable ) CLASS TBrowseSQL

   LOCAL i, oCol

   HB_SYMBOL_UNUSED( oServer )
   HB_SYMBOL_UNUSED( cTable )

   ::super:New( nTop, nLeft, nBottom, nRight )

   ::oQuery := oQuery

   // Let's get a row to build needed columns
   ::oCurRow := ::oQuery:GetRow( 1 )

   // positioning blocks
   ::SkipBlock := {| n | ::oCurRow := Skipper( @n, ::oQuery ), n }
   ::GoBottomBlock := {|| ::oCurRow := ::oQuery:GetRow( ::oQuery:LastRec() ), 1 }
   ::GoTopBlock := {|| ::oCurRow := ::oQuery:GetRow( 1 ), 1 }

   // Add a column for each field
   FOR i := 1 TO ::oQuery:FCount()

      // No bBlock now since New() would use it to find column length, but column is not ready yet at this point
      oCol := TBColumnSQL():New( ::oCurRow:FieldName( i ),, Self )

      IF !( ::oCurRow:FieldType( i ) == "M" )
         oCol:Width := Max( ::oCurRow:FieldLen( i ), Len( oCol:Heading ) )
      ELSE
         oCol:Width := 10
      ENDIF

      // which field does this column display
      oCol:nFieldNum := i

      // Add a picture
      DO CASE
      CASE ::oCurRow:FieldType( i ) == "N"
         oCol:picture := Replicate( "9", oCol:Width )

      CASE ::oCurRow:FieldType( i ) $ "CM"
         oCol:picture := Replicate( "!", oCol:Width )
      ENDCASE

      ::AddColumn( oCol )
   NEXT

   RETURN Self

STATIC FUNCTION Skipper( nSkip, oQuery )

   LOCAL i := 0

   DO CASE
   CASE nSkip == 0 .OR. oQuery:LastRec() == 0
      oQuery:Skip( 0 )

   CASE nSkip > 0
      DO WHILE i < nSkip           // Skip Foward

         IF oQuery:recno() == oQuery:lastrec()
            EXIT
         ENDIF
         oQuery:Skip( 1 )
         i++

      ENDDO

   CASE nSkip < 0
      DO WHILE i > nSkip           // Skip backward

         IF oQuery:recno() == 1
            EXIT
         ENDIF

         oQuery:Skip( -1 )
         i--

      ENDDO
   ENDCASE

   nSkip := i

   RETURN oQuery:GetRow( oQuery:RecNo() )


METHOD EditField() CLASS TBrowseSQL

   LOCAL oCol
   LOCAL aGetList
   LOCAL nKey
   LOCAL cMemoBuff, cMemo

   // Get the current column object from the browse
   oCol := ::getColumn( ::colPos )

   // Editing of a memo field requires a MemoEdit() window
   IF ::oCurRow:FieldType( oCol:nFieldNum ) == "M"

      /* save, clear, and frame window for memoedit */
      cMemoBuff := SaveScreen( 10, 10, 22, 69 )

      hb_Scroll( 10, 10, 22, 69, 0 )
      hb_DispBox( 10, 10, 22, 69 )

      /* use fieldspec for title */
      // @ 10, ( ( 76 - Len( ::oCurRow:FieldName( oCol:nFieldNum ) ) / 2 ) SAY "  " + ( ::oCurRow:FieldName( oCol:nFieldNum ) ) + "  "

      /* edit the memo field */
      cMemo := MemoEdit( ::oCurRow:FieldGet( oCol:nFieldNum ), 11, 11, 21, 68, .T. )

      IF LastKey() == K_CTRL_END
         ::oCurRow:FieldPut( oCol:nFieldNum, cMemo )

         /* NOTE: To do in a better way */
         IF ! ::oQuery:Update( ::oCurRow )
            Alert( Left( ::oQuery:Error(), 60 ) )
         ENDIF
      ENDIF

      RestScreen( 10, 10, 22, 69, cMemoBuff )

   ELSE
      // Create a corresponding GET
      // NOTE: I need to use ::oCurRow:FieldPut(...) when changing values since message redirection doesn't work at present
      //       time for write access to instance variables but only for reading them
      aGetList := { GetNew( Row(), Col(), ;
                            {| xValue | iif( xValue == NIL, Eval( oCol:Block ), ::oCurRow:FieldPut( oCol:nFieldNum, xValue ) ) }, ;
                            oCol:heading, ;
                            oCol:picture, ;
                            ::colorSpec ) }

      // Set initial cursor shape
      // SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )
      ReadModal( aGetList )
      // SetCursor( SC_NONE )

      /* NOTE: To do in a better way */
      IF ! ::oQuery:Update( ::oCurRow )
         Alert( Left( ::oQuery:Error(), 60 ) )
      ENDIF

   endif

   IF ! ::oQuery:Refresh()
      Alert( ::oQuery:Error() )
   ENDIF

   ::RefreshAll()

   // Check exit key from get
   nKey := LastKey()
   IF nKey == K_UP   .OR. nKey == K_DOWN .OR. ;
      nKey == K_PGUP .OR. nKey == K_PGDN

      // Ugh
      hb_keyIns( nKey )

   ENDIF

   RETURN Self


METHOD BrowseTable( lCanEdit, aExitKeys ) CLASS TBrowseSQL

   LOCAL nKey
   LOCAL lKeepGoing := .T.

   IF ! HB_ISNUMERIC( nKey )
      nKey := NIL
   ENDIF
   hb_default( @lCanEdit, .F. )
   hb_default( @aExitKeys, { K_ESC } )

   DO WHILE lKeepGoing

      DO WHILE .T.
         nKey := Inkey()
         IF ::Stabilize() .AND. nKey != 0
            EXIT
         ENDIF
      ENDDO

      IF nKey == 0
         nKey := Inkey( 0 )
      ENDIF

      IF AScan( aExitKeys, nKey ) > 0
         lKeepGoing := .F.
         LOOP
      ENDIF

      DO CASE
      CASE nKey == K_DOWN
         ::down()

      CASE nKey == K_PGDN
         ::pageDown()

      CASE nKey == K_CTRL_PGDN
         ::goBottom()

      CASE nKey == K_UP
         ::up()

      CASE nKey == K_PGUP
         ::pageUp()

      CASE nKey == K_CTRL_PGUP
         ::goTop()

      CASE nKey == K_RIGHT
         ::right()

      CASE nKey == K_LEFT
         ::left()

      CASE nKey == K_HOME
         ::home()

      CASE nKey == K_END
         ::end()

      CASE nKey == K_CTRL_LEFT
         ::panLeft()

      CASE nKey == K_CTRL_RIGHT
         ::panRight()

      CASE nKey == K_CTRL_HOME
         ::panHome()

      CASE nKey == K_CTRL_END
         ::panEnd()

      CASE nKey == K_ENTER .AND. lCanEdit
         ::EditField()

#if 0
      CASE nKey == K_DEL
         IF lCanEdit
            IF ! ::oQuery:Delete( ::oCurRow )
               Alert( "not deleted " + ::oQuery:Error() )
            ENDIF
            IF ! ::oQuery:Refresh()
               Alert( ::oQuery:Error() )
            ENDIF

            ::inValidate()
            ::refreshAll():forceStable()
         ENDIF
#endif

      OTHERWISE
         ::KeyboardHook( nKey )
      ENDCASE
   ENDDO

   RETURN Self

// Empty method to be subclassed
METHOD KeyboardHook( nKey ) CLASS TBrowseSQL

   HB_SYMBOL_UNUSED( nKey )

   RETURN Self
