/*
 * $Id$
 */

/*
   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com

 * Partial Copyright (C) 1999 Eddie Runia <eddie@runia.com>
 *    partial copyright regarding the following additions :
 *       Support for inheritance
 *       Support for default DATA values
 */

// Harbour Class TClass to build classes

#include "hboo.ch"

//----------------------------------------------------------------------------//

function TClass()

   static hClass := 0

   if hClass == 0
      hClass = __clsNew( "TCLASS", 8 )

      __clsAddMsg( hClass, "New",          @New(),          MET_METHOD )
      __clsAddMsg( hClass, "Create",       @Create(),       MET_METHOD )
      __clsAddMsg( hClass, "AddData",      @AddData(),      MET_METHOD )
      __clsAddMsg( hClass, "AddClassData", @AddClassData(), MET_METHOD )
      __clsAddMsg( hClass, "AddInline",    @AddInline(),    MET_METHOD )
      __clsAddMsg( hClass, "AddMethod",    @AddMethod(),    MET_METHOD )
      __clsAddMsg( hClass, "AddVirtual",   @AddVirtual(),   MET_METHOD )
      __clsAddMsg( hClass, "Instance",     @Instance(),     MET_METHOD )

      __clsAddMsg( hClass, "hClass",     1, MET_DATA )
      __clsAddMsg( hClass, "_hClass",    1, MET_DATA )
      __clsAddMsg( hClass, "cName",      2, MET_DATA )
      __clsAddMsg( hClass, "_cName",     2, MET_DATA )
      __clsAddMsg( hClass, "aDatas",     3, MET_DATA )
      __clsAddMsg( hClass, "_aDatas",    3, MET_DATA )
      __clsAddMsg( hClass, "aMethods",   4, MET_DATA )
      __clsAddMsg( hClass, "_aMethods",  4, MET_DATA )
      __clsAddMsg( hClass, "aClsDatas",  5, MET_DATA )
      __clsAddMsg( hClass, "_aClsDatas", 5, MET_DATA )
      __clsAddMsg( hClass, "aInlines",   6, MET_DATA )
      __clsAddMsg( hClass, "_aInlines",  6, MET_DATA )
      __clsAddMsg( hClass, "aVirtuals",  7, MET_DATA )
      __clsAddMsg( hClass, "_aVirtuals", 7, MET_DATA )
      __clsAddMsg( hClass, "cSuper",     8, MET_DATA )
      __clsAddMsg( hClass, "_cSuper",    8, MET_DATA )
   endif

return __clsInst( hClass )

//----------------------------------------------------------------------------//

static function New( cClassName, cSuper )

   local Self := QSelf()

   ::cName     = Upper( cClassName )
   ::aDatas    = {}
   ::aMethods  = {}
   ::aClsDatas = {}
   ::aInlines  = {}
   ::aVirtuals = {}
   if ValType( cSuper ) == "C"
      ::cSuper = cSuper
   endif

return Self

//----------------------------------------------------------------------------//

static function Create()

   local Self := QSelf()
   local n
   local nLen
   local nLenDatas   := Len( ::aDatas )
   local nDataBegin  := 0
   local nClassBegin := 0
   local hClass
   local hSuper
   local ahSuper := {}

   if ::cSuper == NIL
      hClass := __clsNew( ::cName, nLenDatas )

   else                                         // Single inheritance
      hSuper := __clsInstSuper( Upper( ::cSuper ) )
      hClass := __clsNew( ::cName, nLenDatas, hSuper )
                                                // Add class casts
      __clsAddMsg( hClass, Upper( ::cSuper ), hSuper, MET_SUPER )
      __clsAddMsg( hClass, "__SUPER", hSuper, MET_SUPER )

      nDataBegin := __cls_CntData( hSuper )          // Get offset for new DATAs
      nClassBegin := __cls_CntClsData( hSuper )      // Get offset for new ClassData
   endif

   ::hClass = hClass

   for n = 1 to nLenDatas
      __clsAddMsg( hClass, ::aDatas[ n ][ DATA_SYMBOL ], n + nDataBegin, MET_DATA, ;
                           ::aDatas[ n ][ DATA_VALUE ] )
      __clsAddMsg( hClass, "_" + ::aDatas[ n ][ DATA_SYMBOL ], n + nDataBegin, MET_DATA )
   next

   nLen = Len( ::aMethods )
   for n = 1 to nLen
      __clsAddMsg( hClass, ::aMethods[ n ][ 1 ], ::aMethods[ n ][ 2 ], MET_METHOD )
   next

   nLen = Len( ::aClsDatas )
   for n = 1 to nLen
      __clsAddMsg( hClass, ::aClsDatas[ n ], n + nClassBegin, MET_CLASSDATA )
      __clsAddMsg( hClass, "_" + ::aClsDatas[ n ], n + nClassBegin, MET_CLASSDATA )
   next

   nLen = Len( ::aInlines )
   for n = 1 to nLen
      __clsAddMsg( hClass, ::aInlines[ n ][ 1 ], ::aInlines[ n ][ 2 ],;
                MET_INLINE )
   next
//    __clsAddMsg( hClass, Upper( ::cName ), {|self|self}, MET_INLINE ) // Useful?

   nLen = Len( ::aVirtuals )
   for n = 1 to nLen
      __clsAddMsg( hClass, ::aVirtuals[ n ], n, MET_VIRTUAL )
   next

return nil

//----------------------------------------------------------------------------//

static function Instance()

   local Self := QSelf()

return __clsInst( ::hClass )

//----------------------------------------------------------------------------//

static function AddData( cData, xInit )         /* xInit is initializer     */

   local Self := QSelf()

   AAdd( ::aDatas, { cData, xInit } )

return nil

//----------------------------------------------------------------------------//

static function AddClassData( cData )

   local Self := QSelf()

   AAdd( ::aClsDatas, cData )

return nil

//----------------------------------------------------------------------------//

static function AddInline( cMethod, bCode )

   local Self := QSelf()

   AAdd( ::aInlines, { cMethod, bCode } )

return nil

//----------------------------------------------------------------------------//

static function AddMethod( cMethod, nFuncPtr )

   local Self := QSelf()

   AAdd( ::aMethods, { cMethod, nFuncPtr } )

return nil

//----------------------------------------------------------------------------//

static function AddVirtual( cMethod )

   local Self := QSelf()

   AAdd( ::aVirtuals, cMethod )

return nil

//----------------------------------------------------------------------------//
