/*
 * $Id$
 */

/*
 * OBJFUNC
 *
 * Contains additional object oriented functions
 *
 * Copyright (C) 1999 Eddie Runia (eddie@runia.com)
 * Part of the Harbour Project www.harbour-project.org
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
 * Partial Copyright Antonio Linares (alinares@fivetech.com)
 *    partial copyright regarding function : __objGetMsgList
 */

#include "hboo.ch"

//
// <lRet> := __objHasData( <oObject>, <cSymbol> )
//
// Is the symbol present in the object as DATA ?
//
function __objHasData( oObject, cSymbol )

return __objHasMsg( oObject, cSymbol ) .and. __objHasMsg( oObject, "_" + cSymbol )


//
// <lRet> := __objHasMethod( <oObject>, <cSymbol> )
//
// Is the symbol present in the object as METHOD ?
//
function __objHasMethod( oObject, cSymbol )

return __objHasMsg( oObject, cSymbol ) .and. !__objHasMsg( oObject, "_" + cSymbol )

//
// <aData> __objGetMsgList( <oObject>, [lDataMethod] )
//
// Return an array containing the names of all the data items of oObject.
//
// lDataMethod = .T. (default) Return all DATAs
//               .F.           Return all METHODs
//
function __objGetMsgList( oObject, lDataMethod )

   local aInfo  := aSort( oObject:ClassSel() )
   local aData  := {}
   local n      := 1
   local nLen   := Len( aInfo )
   local lFoundDM                               // Found DATA ?

   IF !(ValType(lDataMethod) == "L")
        lDataMethod := .T.
   ENDIF

   do while n <= nLen .and. Substr( aInfo[ n ], 1, 1 ) != "_"

/* If in range and no set function found yet ( set functions begin with a   */
/* leading underscore ).                                                    */

      lFoundDM := !Empty( aScan( aInfo, "_" + aInfo[ n ], n + 1 ) )

/* Find position of matching set function in array with all symbols         */

      if lFoundDM == lDataMethod                // If found -> DATA
                                                //     else    METHOD
         aAdd( aData, aInfo[ n ] )
      endif
      n++
   enddo

return aData


//
// aData __objGetMethodList( oObject )
//
// Return an array containing the names of all the method of oObject.
//
function __objGetMethodList( oObject )

return __objGetMsgList( oObject, .F. )


//
// <aData> __objGetValueList( <oObject>, [<aExcept>] )
//
// Basically the same as __objGetMsgList except that it returns a 2D array
// containing :
//
// [x][1]  Symbol name
// [x][2]  Value of DATA
//
// aExcept is an optional list of DATA you do not want to collect
//
function __objGetValueList( oObject, aExcept )

   local aDataSymbol := __objGetMsgList( oObject )
   local nLen        := Len( aDataSymbol )
   local aData       := {}
   local cSymbol
   local n

   IF !(ValType(aExcept) == "A")
        aExcept := {}
   ENDIF

   for n := 1 to nLen
      cSymbol := aDataSymbol[ n ]
      if Empty( aScan( aExcept, cSymbol ) )
         aAdd( aData, { cSymbol, __objSendMsg( oObject, cSymbol ) } )
      endif
   next n
return aData


//
// __objSetValueList( <oObject>, <aData> )
//
// The reverse of __objGetValueList. 
// It puts an 2D array of DATA into an object.
//
function __objSetValueList( oObject, aData )

   aEval( aData, ;
        {|aItem| __objSendMsg( oObject, "_"+aItem[DATA_SYMBOL], aItem[DATA_VAL] ) } )

return oObject


//
// <oObj> := __objAddMethod( <oObj>, <cSymbol>, <nFuncPtr> )
//
// Add a method to an already existing class
//
function __objAddMethod( oObj, cSymbol, nFuncPtr )

   if __objHasMsg( oObj, cSymbol )
      QOut( "__objAddMethod: ", cSymbol, " already exists in class." )
   elseif ValType( nFuncPtr ) != "N"
      QOut( "__objAddMethod: Argument type error <nFuncPtr>" )
   elseif ValType( oObj ) != "O"
      QOut( "__objAddMethod: Argument type error <oObj>" )
   else
      __clsAddMsg( oObj:ClassH, cSymbol, nFuncPtr, MET_METHOD )
   endif
return oObj


//
// <oObj> := __objAddInline( <oObj>, <cSymbol>, <bInline> )
//
// Add an INLINE to an already existing class
//
function __objAddInline( oObj, cSymbol, bInline )

   if __objHasMsg( oObj, cSymbol )
      QOut( "__objAddInline: ", cSymbol, " already exists in class." )
   elseif ValType( bInline ) != "B"
      QOut( "__objAddInline: Argument type error <bInline>" )
   elseif ValType( oObj ) != "O"
      QOut( "__objAddInline: Argument type error <oObj>" )
   else
      __clsAddMsg( oObj:ClassH, cSymbol, bInline, MET_INLINE )
   endif
return oObj


//
// <oObj> := __objAddData( <oObj>, <cSymbol> )
//
// Add a DATA to an already existing class
//
function __objAddData( oObj, cSymbol )

   local nSeq

   if __objHasMsg( oObj, cSymbol ) .or. __objHasMsg( oObj, "_" + cSymbol )
      QOut( "__objAddData: ", cSymbol, " already exists in class." )
   elseif ValType( oObj ) != "O"
      QOut( "__objAddData: Argument type error <oObj>" )
   else
      nSeq := __cls_IncData( oObj:ClassH )         // Allocate new Seq#
      __clsAddMsg( oObj:ClassH, cSymbol,       nSeq, MET_DATA )
      __clsAddMsg( oObj:ClassH, "_" + cSymbol, nSeq, MET_DATA )
   endif
return oObj


//
// <oObj> := __objModMethod( <oObj>, <cSymbol>, <nFuncPtr> )
//
// Modify a method to an already existing class
//
function __objModMethod( oObj, cSymbol, nFuncPtr )

   if !__objHasMethod( oObj, cSymbol )
      QOut( "__objModMethod: ", cSymbol, " does not exist in class." )
   elseif ValType( nFuncPtr ) != "N"
      QOut( "__objModMethod: Argument type error <nFuncPtr>" )
   elseif ValType( oObj ) != "O"
      QOut( "__objModMethod: Argument type error <oObj>" )
   else
      __clsModMsg( oObj:ClassH, cSymbol, nFuncPtr )
   endif
return oObj


//
// <oObj> := __objModInline( <oObj>, <cSymbol>, <bInline> )
//
// Modify an INLINE to an already existing class
//
function __objModInline( oObj, cSymbol, bInline )

   if !__objHasMethod( oObj, cSymbol )
      QOut( "__objModInline: ", cSymbol, " does not exist in class." )
   elseif ValType( bInline ) != "B"
      QOut( "__objModInline: Argument type error <bInline>" )
   elseif ValType( oObj ) != "O"
      QOut( "__objModInline: Argument type error <oObj>" )
   else
      __clsModMsg( oObj:ClassH, cSymbol, bInline )
   endif
return oObj


//
// <oObj> := __objDelMethod( <oObj>, <cSymbol> )
//
// Delete a method from an already existing class
//
function __objDelMethod( oObj, cSymbol )

   if !__objHasMethod( oObj, cSymbol )
      QOut( "__objDelMethod: ", cSymbol, " does not exist in class." )
   elseif ValType( oObj ) != "O"
      QOut( "__objDelMethod: Argument type error <oObj>" )
   else
      __clsDelMsg( oObj:ClassH, cSymbol )
   endif
return oObj

function __objDelInline( oObj, cSymbol )
return __objDelMethod( oObj, cSymbol )              // Same story


//
// <oObj> := __objDelData( <oObj>, <cSymbol> )
//
// Delete a DATA from an already existing class
//
function __objDelData( oObj, cSymbol )

   local nSeq

   if !__objHasData( oObj, cSymbol )
      QOut( "__objDelData: ", cSymbol, " does not exist in class." )
   elseif ValType( oObj ) != "O"
      QOut( "__objDelData: Argument type error <oObj>" )
   else
      __clsDelMsg( oObj:ClassH, cSymbol,      )
      __clsDelMsg( oObj:ClassH, "_" + cSymbol )
      nSeq := __cls_DecData( oObj:ClassH )         // Decrease wData
   endif
return oObj

