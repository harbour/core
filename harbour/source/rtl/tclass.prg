/*
 * $Id$
 */

// Harbour Class TClass to build classes

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

#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4
#define MET_SUPER     5

#define DAT_SYMBOL    1
#define DAT_INITVAL   2

//----------------------------------------------------------------------------//

function TClass()

   static hClass := 0

   if hClass == 0
      hClass = ClassCreate( "TCLASS", 8 )

      ClassAdd( hClass, "New",          @New(),          MET_METHOD )
      ClassAdd( hClass, "Create",       @Create(),       MET_METHOD )
      ClassAdd( hClass, "AddData",      @AddData(),      MET_METHOD )
      ClassAdd( hClass, "AddClassData", @AddClassData(), MET_METHOD )
      ClassAdd( hClass, "AddInline",    @AddInline(),    MET_METHOD )
      ClassAdd( hClass, "AddMethod",    @AddMethod(),    MET_METHOD )
      ClassAdd( hClass, "AddVirtual",   @AddVirtual(),   MET_METHOD )
      ClassAdd( hClass, "Instance",     @Instance(),     MET_METHOD )

      ClassAdd( hClass, "hClass",     1, MET_DATA )
      ClassAdd( hClass, "_hClass",    1, MET_DATA )
      ClassAdd( hClass, "cName",      2, MET_DATA )
      ClassAdd( hClass, "_cName",     2, MET_DATA )
      ClassAdd( hClass, "aDatas",     3, MET_DATA )
      ClassAdd( hClass, "_aDatas",    3, MET_DATA )
      ClassAdd( hClass, "aMethods",   4, MET_DATA )
      ClassAdd( hClass, "_aMethods",  4, MET_DATA )
      ClassAdd( hClass, "aClsDatas",  5, MET_DATA )
      ClassAdd( hClass, "_aClsDatas", 5, MET_DATA )
      ClassAdd( hClass, "aInlines",   6, MET_DATA )
      ClassAdd( hClass, "_aInlines",  6, MET_DATA )
      ClassAdd( hClass, "aVirtuals",  7, MET_DATA )
      ClassAdd( hClass, "_aVirtuals", 7, MET_DATA )
      ClassAdd( hClass, "cSuper",     8, MET_DATA )
      ClassAdd( hClass, "_cSuper",    8, MET_DATA )
   endif

return ClassInstance( hClass )

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
      hClass := ClassCreate( ::cName, nLenDatas )

   else                                         // Single inheritance
      hSuper := __InstSuper( Upper( ::cSuper ) )
      hClass := ClassCreate( ::cName, nLenDatas, hSuper )
                                                // Add class casts
      ClassAdd( hClass, Upper( ::cSuper ), hSuper, MET_SUPER )
      ClassAdd( hClass, "__SUPER", hSuper, MET_SUPER )

      nDataBegin := __WDatas( hSuper )          // Get offset for new DATAs
      nClassBegin := __WClsDatas( hSuper )      // Get offset for new ClassData
   endif

   ::hClass = hClass

   for n = 1 to nLenDatas
      ClassAdd( hClass, ::aDatas[ n ][ DAT_SYMBOL ], n + nDataBegin, MET_DATA, ;
                        ::aDatas[ n ][ DAT_INITVAL ] )
      ClassAdd( hClass, "_" + ::aDatas[ n ][ DAT_SYMBOL ], n + nDataBegin, MET_DATA )
   next

   nLen = Len( ::aMethods )
   for n = 1 to nLen
      ClassAdd( hClass, ::aMethods[ n ][ 1 ], ::aMethods[ n ][ 2 ], MET_METHOD )
   next

   nLen = Len( ::aClsDatas )
   for n = 1 to nLen
      ClassAdd( hClass, ::aClsDatas[ n ], n + nClassBegin, MET_CLASSDATA )
      ClassAdd( hClass, "_" + ::aClsDatas[ n ], n + nClassBegin, MET_CLASSDATA )
   next

   nLen = Len( ::aInlines )
   for n = 1 to nLen
      ClassAdd( hClass, ::aInlines[ n ][ 1 ], ::aInlines[ n ][ 2 ],;
                MET_INLINE )
   next
//   ClassAdd( hClass, Upper( ::cName ), {|self|self}, MET_INLINE ) // Useful?

   nLen = Len( ::aVirtuals )
   for n = 1 to nLen
      ClassAdd( hClass, ::aVirtuals[ n ], n, MET_VIRTUAL )
   next

return nil

//----------------------------------------------------------------------------//

static function Instance()

   local Self := QSelf()

return ClassInstance( ::hClass )

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
