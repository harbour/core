/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Dynamic Object management and misc. Object related functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 *    __objGetMsgList
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "error.ch"
#include "hboo.ch"

//
// <lRet> := __objHasData( <oObject>, <cSymbol> )
//
// Is the symbol present in the object as DATA ?
//
function __objHasData( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

return __objHasMsg( oObject, cSymbol ) .and. ;
       __objHasMsg( oObject, "_" + cSymbol )


//
// <lRet> := __objHasMethod( <oObject>, <cSymbol> )
//
// Is the symbol present in the object as METHOD ?
//
function __objHasMethod( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

return __objHasMsg( oObject, cSymbol ) .and. ;
       !__objHasMsg( oObject, "_" + cSymbol )

//
// <aData> __objGetMsgList( <oObject>, [lDataMethod] )
//
// Return an array containing the names of all the data items of oObject.
//
// lDataMethod = .T. (default) Return all DATAs
//               .F.           Return all METHODs
//
function __objGetMsgList( oObject, lDataMethod )

   local aInfo
   local aData
   local n
   local nLen
   local lFoundDM                               // Found DATA ?

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   IF !ISLOG( lDataMethod )
        lDataMethod := .T.
   ENDIF

   aInfo  := aSort( oObject:ClassSel() )
   aData  := {}
   n      := 1
   nLen   := Len( aInfo )

   do while n <= nLen .and. Substr( aInfo[ n ], 1, 1 ) != "_"

      /* If in range and no set function found yet ( set functions */
      /* begin with a leading underscore ).                        */

      lFoundDM := !Empty( aScan( aInfo, "_" + aInfo[ n ], n + 1 ) )

      /* Find position of matching set function in array with all symbols */

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

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

return __objGetMsgList( oObject, .F. )


//
// <aData> __objGetValueList( <oObject>, [<aExcept>] )
//
// Basically the same as __objGetMsgList except that it returns a 2D array
// containing :
//
// [x][DATA_SYMBOL]  Symbol name
// [x][DATA_VALUE]   Value of DATA
//
// aExcept is an optional list of DATA you do not want to collect
//
function __objGetValueList( oObject, aExcept )

   local aDataSymbol
   local nLen
   local aData
   local cSymbol
   local n

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   IF !ISARRAY( aExcept )
        aExcept := {}
   ENDIF

   aDataSymbol := __objGetMsgList( oObject )
   nLen        := Len( aDataSymbol )
   aData       := {}

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

   if !ISOBJECT( oObject )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   aEval( aData,;
        {|aItem| __objSendMsg( oObject, "_" + aItem[DATA_SYMBOL], aItem[DATA_VALUE] ) } )

return oObject


//
// <oObject> := __objAddMethod( <oObject>, <cSymbol>, <nFuncPtr> )
//
// Add a method to an already existing class
//
function __objAddMethod( oObject, cSymbol, nFuncPtr )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol ) .or. ;
      !ISNUM( nFuncPtr )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, nFuncPtr, MET_METHOD )
   else
      __errRT_BASE(EG_ARG, 3103, "Already existing symbol in class", ProcName( 0 ) )
   endif

return oObject


//
// <oObject> := __objAddInline( <oObject>, <cSymbol>, <bInline> )
//
// Add an INLINE to an already existing class
//
function __objAddInline( oObject, cSymbol, bInline )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, bInline, MET_INLINE )
   else
      __errRT_BASE(EG_ARG, 3103, "Already existing symbol in class", ProcName( 0 ) )
   endif

return oObject


//
// <oObject> := __objAddData( <oObject>, <cSymbol> )
//
// Add a DATA to an already existing class
//
function __objAddData( oObject, cSymbol )

   local nSeq

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if !__objHasMsg( oObject, cSymbol ) .and. ;
      !__objHasMsg( oObject, "_" + cSymbol )

      nSeq := __cls_IncData( oObject:ClassH )         // Allocate new Seq#
      __clsAddMsg( oObject:ClassH, cSymbol,       nSeq, MET_DATA )
      __clsAddMsg( oObject:ClassH, "_" + cSymbol, nSeq, MET_DATA )
   else
      __errRT_BASE(EG_ARG, 3103, "Already existing symbol in class", ProcName( 0 ) )
   endif

return oObject


//
// <oObject> := __objModMethod( <oObject>, <cSymbol>, <nFuncPtr> )
//
// Modify a method to an already existing class
//
function __objModMethod( oObject, cSymbol, nFuncPtr )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol ) .or. ;
      !ISNUM( nFuncPtr )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, nFuncPtr )
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject


//
// <oObject> := __objModInline( <oObject>, <cSymbol>, <bInline> )
//
// Modify an INLINE to an already existing class
//
function __objModInline( oObject, cSymbol, bInline )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol ) .or. ;
      !ISBLOCK( bInline )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, bInline )
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject


//
// <oObject> := __objDelMethod( <oObject>, <cSymbol> )
//
// Delete a method from an already existing class
//
function __objDelMethod( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasMethod( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject

function __objDelInline( oObject, cSymbol )
return __objDelMethod( oObject, cSymbol )              // Same story

//
// <oObject> := __objDelData( <oObject>, <cSymbol> )
//
// Delete a DATA from an already existing class
//
function __objDelData( oObject, cSymbol )

   if !ISOBJECT( oObject ) .or. ;
      !ISCHAR( cSymbol )
      __errRT_BASE(EG_ARG, 3101, NIL, ProcName( 0 ) )
   endif

   if __objHasData( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
      __clsDelMsg( oObject:ClassH, "_" + cSymbol )
      __cls_DecData( oObject:ClassH )         // Decrease wData
   else
      __errRT_BASE(EG_ARG, 3102, "Not existing symbol in class", ProcName( 0 ) )
   endif

return oObject

