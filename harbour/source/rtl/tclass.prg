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

   STATIC s_hClass := NIL

   IF s_hClass == NIL
      s_hClass := __clsNew( "TCLASS", 10 )

      __clsAddMsg( s_hClass, "New"         , @New()         , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Create"      , @Create()      , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddData"     , @AddData()     , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddClassData", @AddClassData(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddInline"   , @AddInline()   , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMethod"   , @AddMethod()   , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddVirtual"  , @AddVirtual()  , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Instance"    , @Instance()    , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "SetInit"     , @SetInit()     , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "SetType"     , @SetType()     , HB_OO_MSG_METHOD )

      __clsAddMsg( s_hClass, "hClass"      ,  1, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_hClass"     ,  1, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "cName"       ,  2, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_cName"      ,  2, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aDatas"      ,  3, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aDatas"     ,  3, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aMethods"    ,  4, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aMethods"   ,  4, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aClsDatas"   ,  5, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aClsDatas"  ,  5, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aInlines"    ,  6, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aInlines"   ,  6, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aVirtuals"   ,  7, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aVirtuals"  ,  7, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "cSuper"      ,  8, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_cSuper"     ,  8, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "uInit"       ,  9, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_uInit"      ,  9, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "cType"       , 10, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_cType"      , 10, HB_OO_MSG_DATA )
   ENDIF

   RETURN __clsInst( s_hClass )

//----------------------------------------------------------------------------//

STATIC FUNCTION New( cClassName, cSuper )

   LOCAL Self := QSelf()

   ::cName     := Upper( cClassName )
   ::aDatas    := {}
   ::aMethods  := {}
   ::aClsDatas := {}
   ::aInlines  := {}
   ::aVirtuals := {}
   IF ISCHARACTER( cSuper )
      ::cSuper    := cSuper
   ENDIF

   RETURN Self

//----------------------------------------------------------------------------//

STATIC FUNCTION Create()

   LOCAL Self := QSelf()
   LOCAL n
   LOCAL nLen
   LOCAL nLenDatas := Len( ::aDatas )
   LOCAL nDataBegin := 0
   LOCAL nClassBegin := 0
   LOCAL hClass
   LOCAL hSuper
   LOCAL ahSuper := {}

   IF ::cSuper == NIL
      hClass := __clsNew( ::cName, nLenDatas )

   else                                         // Single inheritance
      hSuper := __clsInstSuper( Upper( ::cSuper ) )
      hClass := __clsNew( ::cName, nLenDatas, hSuper )
                                                // Add class casts
      __clsAddMsg( hClass, Upper( ::cSuper ), hSuper, HB_OO_MSG_SUPER )
      __clsAddMsg( hClass, "__SUPER", hSuper, HB_OO_MSG_SUPER )

      nDataBegin := __cls_CntData( hSuper )          // Get offset for new DATAs
      nClassBegin := __cls_CntClsData( hSuper )      // Get offset for new ClassData
   ENDIF

   ::hClass := hClass

   FOR n := 1 TO nLenDatas
      __clsAddMsg( hClass, ::aDatas[ n ][ HB_OO_DATA_SYMBOL ], n + nDataBegin, HB_OO_MSG_DATA, ;
                           ::aDatas[ n ][ HB_OO_DATA_VALUE ] )
      __clsAddMsg( hClass, "_" + ::aDatas[ n ][ HB_OO_DATA_SYMBOL ], n + nDataBegin,;
                   HB_OO_MSG_DATA )
   NEXT

   nLen := Len( ::aMethods )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aMethods[ n ][ 1 ], ::aMethods[ n ][ 2 ], HB_OO_MSG_METHOD )
   NEXT

   nLen := Len( ::aClsDatas )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aClsDatas[ n ][ HB_OO_CLSD_SYMBOL ], n + nClassBegin,;
                   HB_OO_MSG_CLASSDATA, ::aClsDatas[ n ][ HB_OO_CLSD_VALUE ] )
      __clsAddMsg( hClass, "_" + ::aClsDatas[ n ][ HB_OO_CLSD_SYMBOL ],;
                   n + nClassBegin, HB_OO_MSG_CLASSDATA )
   NEXT

   nLen := Len( ::aInlines )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aInlines[ n ][ 1 ], ::aInlines[ n ][ 2 ],;
                HB_OO_MSG_INLINE )
   NEXT
//    __clsAddMsg( hClass, Upper( ::cName ), {| self | self }, HB_OO_MSG_INLINE ) // QUESTION: Useful ?

   nLen := Len( ::aVirtuals )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aVirtuals[ n ], n, HB_OO_MSG_VIRTUAL )
   NEXT

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION Instance()

   LOCAL Self := QSelf()

   RETURN __clsInst( ::hClass )

//----------------------------------------------------------------------------//

STATIC FUNCTION AddData( cData, xInit )  /* xInit is initializer */

   LOCAL Self := QSelf()

   IF ::uInit != NIL
      xInit := ::uInit
   ENDIF

   AAdd( ::aDatas, { cData, xInit } )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddClassData( cData, xInit )

   LOCAL Self := QSelf()

   IF ::uInit != NIL
      xInit := ::uInit
   ENDIF

   aAdd( ::aClsDatas, { cData, xInit } )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddInline( cMethod, bCode )

   LOCAL Self := QSelf()

   aAdd( ::aInlines, { cMethod, bCode } )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddMethod( cMethod, nFuncPtr )

   LOCAL Self := QSelf()

   aAdd( ::aMethods, { cMethod, nFuncPtr } )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddVirtual( cMethod )

   LOCAL Self := QSelf()

   aAdd( ::aVirtuals, cMethod )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION SetInit( uValue )

   LOCAL Self := QSelf()

   ::uInit := uValue

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION SetType( cType )

   LOCAL Self := QSelf()

   ::cType := cType

   if cType != nil .and. Upper( cType ) == "LOGICAL" .and. ::uInit == nil
      ::uInit = .f.
   else
      ::uInit = nil
   endif

   RETURN NIL

//----------------------------------------------------------------------------//
