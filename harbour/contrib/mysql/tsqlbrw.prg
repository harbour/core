/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MySQL TBrowse
 * A TBrowse on a MySQL Table / query
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "dbstruct.ch"
#include "mysql.ch"

/* NOTE:

In fact no, the 'regular syntax is the same as the VO one,

ACCESS Block Method Block()
or
ACCESS Block Inline ::MyVal

and

ASSIGN Block(x) Method Block(x)
or
ASSIGN Block(x) INLINE  ::MyVal := x

*/


CLASS TBColumnSQL from TBColumn

   DATA  oBrw                 // pointer to Browser containing this column, needed to be able to
                              // retreive field values from Browse instance variable oCurRow
   DATA  Picture              // From clipper 5.3
   DATA  nFieldNum            // This column maps field num from query

   MESSAGE  Block METHOD Block()          // When evaluating code block to get data from source this method
                                          // gets called. I need this since inside TBColumn Block I cannot
                                          // reference Column or Browser instance variables

   METHOD   New(cHeading, bBlock, oBrw)   // Saves inside column a copy of container browser

ENDCLASS


METHOD New(cHeading, bBlock, oBrw) CLASS TBColumnSQL

   super:New(cHeading, bBlock)

   ::oBrw := oBrw

return Self


METHOD Block() CLASS TBColumnSQL

   local xValue := ::oBrw:oCurRow:FieldGet(::nFieldNum)

   do case
   case ValType(xValue) == "N"
      xValue := Str(xValue)

   case ValType(xValue) == "D"
      xValue := DToC(xValue)

   case ValType(xValue) == "L"
      xValue := iif(xValue, ".T.", ".F.")

   case ValType(xValue) $ "CM"
      xValue := "'" + xValue + "'"

   otherwise
   endcase

return &("{|| " + xValue + "}")


/*--------------------------------------------------------------------------------------------------*/


/*
   This class is more or less like a TBrowseDB() object in that it receives an oQuery/oTable
   object and gives back a browseable view of it
*/
CLASS TBrowseSQL from TBrowse

   DATA     oCurRow                       // Active row inside table / sql query
   DATA     oQuery                        // Query / table object which we are browsing

   METHOD   New(nTop, nLeft, nBottom, nRight, oServer, oQuery, cTable)
   METHOD   EditField()                   // Editing of hilighted field, after editing does an update of
                                          // corresponding row inside table
   METHOD   BrowseTable(nKey, lCanEdit)   // Handles standard moving inside table and if lCanEdit == .T.
                                          // allows editing of field. It is the stock ApplyKey() moved inside a table
ENDCLASS


METHOD New(nTop, nLeft, nBottom, nRight, oServer, oQuery, cTable) CLASS TBrowseSQL

   local i, oCol

   super:New()

   if nTop != NIL
      ::nTop := nTop
   endif

   if nLeft != NIL
      ::nLeft := nLeft
   endif

   if nBottom != NIL
      ::nBottom := nBottom
   endif

   if nRight != NIL
      ::nRight := nRight
   endif

   ::oQuery := oQuery

   // positioning blocks
   ::SkipBlock := {|n| ::oCurRow := Skipper(@n, ::oQuery), n }
   ::GoBottomBlock := {|| ::oCurRow := ::oQuery:GetRow(::oQuery:LastRec()), 1 }
   ::GoTopBlock := {|| ::oCurRow := ::oQuery:GetRow(1), 1 }

   // Let's get a row to build needed columns
   ::oCurRow := ::oQuery:GetRow()
   ::oQuery:Skip(-1)

   // Add a column for each field
   for i := 1 to ::oQuery:FCount()
      // No bBlock now since New() would use it to find column length, but column is not ready yet at this point
      oCol := TBColumnSQL():New(::oCurRow:FieldName(i),, Self)

      oCol:Width := Max(::oCurRow:aFieldStruct[i][MYSQL_FS_LENGTH], Len(oCol:Heading))

      // which field does this column display
      oCol:nFieldNum := i

      // Add a picture
      /*do case
      case ISNUMBER(::oCurRow:FieldGet(i))
          oCol:picture    := "@N999,999"

      case ISCHARACTER(::oCurRow:FieldGet(i))
         // For non-numeric, just use colors 3 and 4 ("B/W" and "B/BG")
         oCol:picture  := replicate("!", ::oCurRow:aFieldStruct[i][MYSQL_FS_LENGTH])

      endcase*/

      ::AddColumn(oCol)
   next

return Self


static function Skipper(nSkip, oQuery)

   local    i := 0

   do case
   case (nSkip == 0 .or. oQuery:LastRec() == 0)
      oQuery:Skip( 0 )

   case (nSkip > 0 .and. !oQuery:eof())
      while ( i < nSkip )           // Skip Foward

         if oQuery:eof()
            exit
         endif

         oQuery:Skip( 1 )
         i++

      enddo

   case ( nSkip < 0 )
      while ( i > nSkip )           // Skip backward

         if oQuery:bof()
            exit
         endif

         oQuery:Skip( -1 )
         i--

      enddo
   endcase

   nSkip := i

return oQuery:GetRow(oQuery:RecNo())


METHOD EditField() CLASS TBrowseSQL

   LOCAL lFlag := TRUE
   LOCAL oCol
   LOCAL aGetList
   LOCAL nKey
   LOCAL nLen
   LOCAL lAppend
   LOCAL bSavIns
   LOCAL nSavRecNo := recno()
   LOCAL xNewKey
   LOCAL xSavKey

   LOCAL xGetValue

   // If we're at EOF we're adding the first record, so turn on append mode
   //if EOF()
   //   lAppend := APP_MODE_ON( oBrowse )
   //else
   //   lAppend := APP_MODE_ACTIVE( oBrowse )
   //endif

   // Make sure screen is fully updated, dbf position is correct, etc.
   //oBrowse:forceStable()

   //if ( lAppend .and. ( recno() == lastrec() + 1 ) )
   //   dbAppend()

   //endif

   // Save the current record's key value (or NIL)
   //xSavKey := iif( empty( indexkey() ), NIL, &( indexkey() ) )

   // Get the current column object from the browse
   oCol := ::getColumn(::colPos)

   // Get picture len to force scrolling if var is larger than window
   //nLen := ::colWidth(::colPos)

   //Alert(Str(::colWidth(::colPos)))

   // Create a corresponding GET
   aGetList := { getnew( row(), col(),    ;
                        {|xValue| iif(xValue == nil, ::oCurRow:FieldGet(oCol:nFieldNum), ::oCurRow:FieldPut(oCol:nFieldNum, xValue))} ,;
                        oCol:heading,     ;
                        nil,     ;
                        ::colorSpec ) }

   // Set insert key to toggle insert mode and cursor shape
   //bSavIns := setkey( K_INS, { || InsToggle() } )

   // Set initial cursor shape
   //setcursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )
   ReadModal(aGetList)
   //setcursor( SC_NONE )
   //setkey( K_INS, bSavIns )

   // For this demo, we turn append mode off after each new record
   //APP_MODE_OFF( oBrowse )

   // Get the record's key value (or NIL) after the GET
   //xNewKey := if( empty( indexkey() ), NIL, &( indexkey() ) )

   if !::oQuery:Update(::oCurRow)
      Alert(::oQuery:Error())
   endif

   if !::oQuery:Refresh()
      Alert(::oQuery:Error())
   endif

   ::inValidate()
   ::refreshAll():forceStable()

   // if the key has changed (or if this is a new record)
   /*if !( xNewKey == xSavKey ) .or. ( lAppend .and. xNewKey != NIL )

      // do a complete refresh
      oBrowse:refreshAll():forceStable()

      // Make sure we're still on the right record after stabilizing
      while &( indexkey() ) > xNewKey .and. !oBrowse:hitTop()
         oBrowse:up():forceStable()

      enddo

   endif*/

   // Check exit key from get
   nKey := lastkey()
   if nKey == K_UP   .or. nKey == K_DOWN .or. ;
      nKey == K_PGUP .or. nKey == K_PGDN

      // Ugh
      keyboard( chr( nKey ) )

   endif

RETURN Self


METHOD BrowseTable(nKey, lCanEdit) CLASS TBrowseSQL

   default nKey      to nil
   default lCanEdit  to .F.


   do case
   case nKey == K_DOWN
      ::down()

   case nKey == K_PGDN
      ::pageDown()

   case nKey == K_CTRL_PGDN
      ::goBottom()

   case nKey == K_UP
      ::up()

   case nKey == K_PGUP
      ::pageUp()

   case nKey == K_CTRL_PGUP
      ::goTop()

   case nKey == K_RIGHT
      ::right()

   case nKey == K_LEFT
      ::left()

   case nKey == K_HOME
      ::home()

   case nKey == K_END
      ::end()

   case nKey == K_CTRL_LEFT
      ::panLeft()

   case nKey == K_CTRL_RIGHT
      ::panRight()

   case nKey == K_CTRL_HOME
      ::panHome()

   case nKey == K_CTRL_END
      ::panEnd()

   case nKey == K_RETURN
      if lCanEdit
         ::EditField()
      endif

   /*otherwise
      KEYBOARD chr( nKey )
      DoGet( oBrowse )*/
   otherwise

   endcase

return Self

