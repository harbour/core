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
   //DATA  Picture              // From clipper 5.3
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
   local xType := ::oBrw:oCurRow:FieldType(::nFieldNum)

   do case
      case xType == "N"
         xValue := Str(xValue, ::oBrw:oCurRow:FieldLen(::nFieldNum), ::oBrw:oCurRow:FieldDec(::nFieldNum))

      case xType == "D"
         xValue :=  "'" + DToC(xValue) + "'"

      case xType == "L"
         xValue := iif(xValue, ".T.", ".F.")

      case xType == "C"
         // Chr(34) is a double quote
         // That is: if there is a double quote inside text substitute it with a string
         // which gets converted back to a double quote by macro operator. If not it would
         // give an error because of unbalanced double quotes.
         xValue := Chr(34) + StrTran(xValue, Chr(34), Chr(34) + "+Chr(34)+" + Chr(34)) + Chr(34)

      case xType == "M"
         xValue := "' <MEMO> '"

      otherwise
   endcase

return &("{||" + xValue + "}")


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

   METHOD   BrowseTable(lCanEdit, aExitKeys) // Handles standard moving inside table and if lCanEdit == .T.
                                             // allows editing of field. It is the stock ApplyKey() moved inside a table
                                             // if lCanEdit K_DEL deletes current row
                                             // When a key is pressed which is present inside aExitKeys it leaves editing loop

   METHOD   KeyboardHook(nKey)            // Where do all unknown keys go?

ENDCLASS


METHOD New(nTop, nLeft, nBottom, nRight, oServer, oQuery, cTable) CLASS TBrowseSQL

   local i, oCol

   super:New(nTop, nLeft, nBottom, nRight)

   ::oQuery := oQuery

   // Let's get a row to build needed columns
   ::oCurRow := ::oQuery:GetRow(1)

   // positioning blocks
   ::SkipBlock := {|n| ::oCurRow := Skipper(@n, ::oQuery), n }
   ::GoBottomBlock := {|| ::oCurRow := ::oQuery:GetRow(::oQuery:LastRec()), 1 }
   ::GoTopBlock := {|| ::oCurRow := ::oQuery:GetRow(1), 1 }

   // Add a column for each field
   for i := 1 to ::oQuery:FCount()

      // No bBlock now since New() would use it to find column length, but column is not ready yet at this point
      oCol := TBColumnSQL():New(::oCurRow:FieldName(i),, Self)

      if ::oCurRow:FieldType(i) <> "M"
         oCol:Width := Max(::oCurRow:FieldLen(i), Len(oCol:Heading))
      else
         oCol:Width := 10
      endif

      // which field does this column display
      oCol:nFieldNum := i

      // Add a picture
      do case
         case ::oCurRow:FieldType(i) == "N"
            oCol:picture := replicate("9", oCol:Width)

         case ::oCurRow:FieldType(i) $ "CM"
            oCol:picture := replicate("!", oCol:Width)
      endcase

      ::AddColumn(oCol)
   next

return Self


static function Skipper(nSkip, oQuery)

   local    i := 0

   do case
   case (nSkip == 0 .OR. oQuery:LastRec() == 0)
      oQuery:Skip(0)

   case (nSkip > 0)
      while ( i < nSkip )           // Skip Foward

         if oQuery:eof()
            exit
         endif

         oQuery:Skip(1)
         i++

      enddo

   case ( nSkip < 0 )
      while ( i > nSkip )           // Skip backward

         if oQuery:bof()
            exit
         endif

         oQuery:Skip(-1)
         i--

      enddo
   endcase

   nSkip := i

return oQuery:GetRow(oQuery:RecNo())


METHOD EditField() CLASS TBrowseSQL

   local oCol
   local aGetList
   local nKey
   local xGetValue
   local cMemoBuff, cMemo

   // Get the current column object from the browse
   oCol := ::getColumn(::colPos)

   // Editing of a memo field requires a MemoEdit() window
   if ::oCurRow:FieldType(oCol:nFieldNum) == "M"

      /* save, clear, and frame window for memoedit */
      cMemoBuff := SaveScreen(10, 10, 22, 69)

      Scroll(10, 10, 22, 69, 0)
      DispBox(10, 10, 22, 69)

      /* use fieldspec for title */
      //@ 10,((76 - Len(::oCurRow:FieldName(oCol:nFieldNum)) / 2) SAY "  " + (::oCurRow:FieldName(oCol:nFieldNum)) + "  "

      /* edit the memo field */
      cMemo := MemoEdit(::oCurRow:FieldGet(oCol:nFieldNum), 11, 11, 21, 68, .T.)

      if Lastkey() == K_CTRL_END
         ::oCurRow:FieldPut(oCol:nFieldNum, cMemo)

         /* NOTE: To do in a better way */
         if !::oQuery:Update(::oCurRow)
            Alert(Left(::oQuery:Error(), 60))
         endif
      endif

      RestScreen(10, 10, 22, 69, cMemoBuff)

   else
      // Create a corresponding GET
      // NOTE: I need to use ::oCurRow:FieldPut(...) when changing values since message redirection doesn't work at present
      //       time for write access to instance variables but only for reading them
      aGetList := { getnew( row(), col(),    ;
                           {|xValue| iif(xValue == nil, Eval(oCol:Block), ::oCurRow:FieldPut(oCol:nFieldNum, xValue))} ,;
                           oCol:heading,     ;
                           oCol:picture,     ;
                           ::colorSpec ) }

      // Set initial cursor shape
      //setcursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )
      ReadModal(aGetList)
      //setcursor( SC_NONE )

      /* NOTE: To do in a better way */
      if !::oQuery:Update(::oCurRow)
         Alert(Left(::oQuery:Error(), 60))
      endif

   endif

   if !::oQuery:Refresh()
      Alert(::oQuery:Error())
   endif

   ::RefreshAll()

   // Check exit key from get
   nKey := LastKey()
   if nKey == K_UP   .or. nKey == K_DOWN .or. ;
      nKey == K_PGUP .or. nKey == K_PGDN

      // Ugh
      keyboard( chr( nKey ) )

   endif

RETURN Self


METHOD BrowseTable(lCanEdit, aExitKeys) CLASS TBrowseSQL

   local nKey
   local lKeepGoing := .T.

   default nKey      to nil
   default lCanEdit  to .F.
   default aExitKeys to {K_ESC}


   while lKeepGoing

      while !::Stabilize() .and. NextKey() == 0
      enddo

      nKey := Inkey(0)

      if AScan(aExitKeys, nKey) > 0
         lKeepGoing := .F.
         LOOP
      endif

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

         case nKey == K_RETURN .AND. lCanEdit
            ::EditField()

         /*case nKey == K_DEL
            if lCanEdit
               if ! ::oQuery:Delete(::oCurRow)
                  Alert("not deleted " + ::oQuery:Error())
               endif
               if !::oQuery:Refresh()
                  Alert(::oQuery:Error())
               endif

               ::inValidate()
               ::refreshAll():forceStable()
            endif*/

         otherwise
            ::KeyboardHook(nKey)

      endcase
   enddo

return Self


// Empty method to be subclassed
METHOD KeyboardHook(nKey) CLASS TBrowseSQL


return Self
