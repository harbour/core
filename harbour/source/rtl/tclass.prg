/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
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
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    Support for inheritance
 *    Support for default DATA values
 *
 * See doc/license.txt for licensing terms.
 *
 */

// Harbour Class TClass to build classes

#include "common.ch"
#include "hboo.ch"

//----------------------------------------------------------------------------//

FUNCTION TClass()

   STATIC s_hClass := nil

   if s_hClass == nil
      s_hClass := __clsNew( "TCLASS", 10 )

      __clsAddMsg( s_hClass, "New",          @New(),          MET_METHOD )
      __clsAddMsg( s_hClass, "Create",       @Create(),       MET_METHOD )
      __clsAddMsg( s_hClass, "AddData",      @AddData(),      MET_METHOD )
      __clsAddMsg( s_hClass, "AddClassData", @AddClassData(), MET_METHOD )
      __clsAddMsg( s_hClass, "AddInline",    @AddInline(),    MET_METHOD )
      __clsAddMsg( s_hClass, "AddMethod",    @AddMethod(),    MET_METHOD )
      __clsAddMsg( s_hClass, "AddVirtual",   @AddVirtual(),   MET_METHOD )
      __clsAddMsg( s_hClass, "Instance",     @Instance(),     MET_METHOD )
      __clsAddMsg( s_hClass, "SetInit",      @SetInit(),      MET_METHOD )
      __clsAddMsg( s_hClass, "SetType",      @SetType(),      MET_METHOD )

      __clsAddMsg( s_hClass, "hClass",     1, MET_DATA )
      __clsAddMsg( s_hClass, "_hClass",    1, MET_DATA )
      __clsAddMsg( s_hClass, "cName",      2, MET_DATA )
      __clsAddMsg( s_hClass, "_cName",     2, MET_DATA )
      __clsAddMsg( s_hClass, "aDatas",     3, MET_DATA )
      __clsAddMsg( s_hClass, "_aDatas",    3, MET_DATA )
      __clsAddMsg( s_hClass, "aMethods",   4, MET_DATA )
      __clsAddMsg( s_hClass, "_aMethods",  4, MET_DATA )
      __clsAddMsg( s_hClass, "aClsDatas",  5, MET_DATA )
      __clsAddMsg( s_hClass, "_aClsDatas", 5, MET_DATA )
      __clsAddMsg( s_hClass, "aInlines",   6, MET_DATA )
      __clsAddMsg( s_hClass, "_aInlines",  6, MET_DATA )
      __clsAddMsg( s_hClass, "aVirtuals",  7, MET_DATA )
      __clsAddMsg( s_hClass, "_aVirtuals", 7, MET_DATA )
      __clsAddMsg( s_hClass, "cSuper",     8, MET_DATA )
      __clsAddMsg( s_hClass, "_cSuper",    8, MET_DATA )
      __clsAddMsg( s_hClass, "uInit",      9, MET_DATA )
      __clsAddMsg( s_hClass, "_uInit",     9, MET_DATA )
      __clsAddMsg( s_hClass, "cType",     10, MET_DATA )
      __clsAddMsg( s_hClass, "_cType",    10, MET_DATA )
   endif

   return __clsInst( s_hClass )

//----------------------------------------------------------------------------//

static function New( cClassName, cSuper )

   local Self := QSelf()

   ::cName     := Upper( cClassName )
   ::aDatas    := {}
   ::aMethods  := {}
   ::aClsDatas := {}
   ::aInlines  := {}
   ::aVirtuals := {}
   if ISCHARACTER( cSuper )
      ::cSuper := cSuper
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

   if ::cSuper == nil
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

   ::hClass := hClass

   for n := 1 to nLenDatas
      __clsAddMsg( hClass, ::aDatas[ n ][ DATA_SYMBOL ], n + nDataBegin, MET_DATA, ;
                           ::aDatas[ n ][ DATA_VALUE ] )
      __clsAddMsg( hClass, "_" + ::aDatas[ n ][ DATA_SYMBOL ], n + nDataBegin,;
                   MET_DATA )
   next

   nLen := Len( ::aMethods )
   for n := 1 to nLen
      __clsAddMsg( hClass, ::aMethods[ n ][ 1 ], ::aMethods[ n ][ 2 ], MET_METHOD )
   next

   nLen := Len( ::aClsDatas )
   for n := 1 to nLen
      __clsAddMsg( hClass, ::aClsDatas[ n ][ CLASSDATA_SYMBOL ], n + nClassBegin,;
                   MET_CLASSDATA, ::aClsDatas[ n ][ CLASSDATA_VALUE ] )
      __clsAddMsg( hClass, "_" + ::aClsDatas[ n ][ CLASSDATA_SYMBOL ],;
                   n + nClassBegin, MET_CLASSDATA )
   next

   nLen := Len( ::aInlines )
   for n := 1 to nLen
      __clsAddMsg( hClass, ::aInlines[ n ][ 1 ], ::aInlines[ n ][ 2 ],;
                MET_INLINE )
   next
//    __clsAddMsg( hClass, Upper( ::cName ), {|self|self}, MET_INLINE ) // Useful?

   nLen := Len( ::aVirtuals )
   for n := 1 to nLen
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

   if ::uInit != nil
      xInit := ::uInit
   endif

   AAdd( ::aDatas, { cData, xInit } )

return nil

//----------------------------------------------------------------------------//

static function AddClassData( cData, xInit )

   local Self := QSelf()

   if ::uInit != nil
      xInit := ::uInit
   endif

   AAdd( ::aClsDatas, { cData, xInit } )

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

static function SetInit( uValue )

   local Self := QSelf()

   ::uInit := uValue

return nil

//----------------------------------------------------------------------------//

static function SetType( cType )

   local Self := QSelf()

   ::cType := cType

return nil

//----------------------------------------------------------------------------//
