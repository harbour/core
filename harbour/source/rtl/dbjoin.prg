/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DBJOIN() function
 *
 * Copyright 2005 Pavel Tsarenko <tpe2@mail.ru>
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


Function __dbJoin( cAlias, cFile, aFields, bFor, cRdd, nConnection, cdpId )
Local nMaster := Select()
Local nDetail := Select(cAlias)
Local nResult, aStruct, aJoinList, oError, lError := .F.

Select(nMaster)
if ( Empty(aStruct := FieldTwo(cAlias, aFields)) )
   return .F.
endif

begin sequence

   dbCreate(cFile, aStruct, cRdd, .T., "", , cdpId, nConnection)
   nResult := Select()
   aJoinList := __JoinList(nMaster, nDetail, nResult, aStruct)

   Select(nMaster)
   go top
   do while ! Eof()
      Select(nDetail)
      go top
      do while ! Eof()
         Select(nMaster)
         if Eval(bFor)
            __doJoinList(aJoinList)
         endif
         Select(nDetail)
         skip 
      enddo
      Select(nMaster)
      Skip 
   enddo
recover using oError
   lError := .T.
end sequence

if nResult != nil
   Select(nResult)
   close
endif
Select(nMaster)
if lError
   break( oError )
endif
return .T.


static function FieldTwo( cAlias, aFields )

Local aFldTemp, bFind, cField, aStruct

if Empty(aFields)
   return dbStruct()
endif
aFldTemp := {}
AEval(aFields, { |cFld| AAdd(aFldTemp, Trim(Upper(cFld))) })
aFields := aFldTemp
aStruct := {}
bFind := { |c| c == cField }
AEval(dbStruct(), { |a| ( cField := a[1], iif( ascan(aFields, bFind) == 0,;
   nil, AAdd(aStruct, a) ) ) })
select(cAlias)
bFind := { |cFld| "->" $ cFld .AND. SubStr(cFld, At("->", cFld) + 2) == ;
   cField }
AEval(dbStruct(), { |a| ( cField := a[ 1 ], iif( AScan(aFields, bFind) == 0,;
   nil, AAdd(aStruct, a) ) ) })
return aStruct


static function __JoinList(nMaster, nDetail, nResult, aStruct)
Local i, aList := {}, nPos
for i := 1 to len(aStruct)
   if ( nPos := (nMaster)->(FieldPos(aStruct[i][1])) ) # 0
      AAdd(aList, {nResult, nMaster, nPos, i})
   elseif ( nPos := (nDetail)->(FieldPos(aStruct[i][1])) ) # 0
      AAdd(aList, {nResult, nDetail, nPos, i})
   endif
next
Return aList


static function __doJoinList(aList)
local i, aJoin

if len(aList) > 0
   ( aList[1][1] )->( dbAppend() )
   for each aJoin in aList
      ( aJoin[1] )->(FieldPut( aJoin[4], ( aJoin[2] )->(FieldGet( aJoin[3] )) ))
   next
endif
return nil
