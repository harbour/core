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
#include "dbstruct.ch"
#include "mysql.ch"


CLASS TBColumnSQL from TBColumn

   DATA  oBrw                 // pointer to Browser containing this column, needed to be able to
                              // retreive field values from Browse instance variable oCurRow

   MESSAGE  Block METHOD Block()          // When evaluating code block to get data from source this method gets called
   METHOD   New(cHeading, bBlock, oBrw)   // Saves inside column a copy of container browser

ENDCLASS


METHOD New(cHeading, bBlock, oBrw) CLASS TBColumnSQL

   super:New(cHeading, bBlock)

   ::oBrw := oBrw

return Self


METHOD Block() CLASS TBColumnSQL

   local xValue := ::oBrw:oCurRow:FieldGet(::Cargo)
   local bBlock := "{|| '"

   if ISNUMBER(xValue)
      xValue := Str(xValue)
   elseif ISDATE(xValue)
      xValue := DToC(xValue)
   endif

   bBlock += xValue + " '}"

return &(bBlock)


/*--------------------------------------------------------------------------------------------------*/


/*
   This class is more or less like a TBrowseDB() object in that is receives an oQuery/oTable
   object and gives back a browseable view of it
*/
CLASS TBrowseSQL from TBrowse

   DATA     oCurRow                       // Active row inside table / sql query
   DATA     oQuery                        // Query / table object which we are browsing

   METHOD   New(nTop, nLeft, nBottom, nRight, oServer, oQuery, cTable)

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
      oCol := TBColumnSQL():New(::oCurRow:FieldName(i), nil, Self)
      oCol:Width := Max(::oCurRow:aFieldStruct[i][MYSQL_FS_LENGTH], Len(oCol:Heading))
      oCol:Cargo := i
      ::AddColumn(oCol)
   next

return Self


static function Skipper(nSkip, oQuery)

   //LOCAL lAppend := APP_MODE_ACTIVE( oBrowse )
   LOCAL i       := 0

   do case
   case ( nSkip == 0 .or. oQuery:lastrec() == 0 )
      // Skip 0 (significant on a network)
      oQuery:Skip( 0 )

   case ( nSkip > 0 .and. !oQuery:eof() )
      while ( i < nSkip )           // Skip Foward

         if oQuery:eof()
            //iif( lAppend, i++, dbskip( -1 ) )
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

   //Alert(Str(oQuery:RecNo()) +  " : " + str(i))

return oQuery:GetRow(oQuery:RecNo())


