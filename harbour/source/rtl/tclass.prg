/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base Class for internal handling of class creation
 *
 * Copyright 2000 J. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
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
 * Copyright 2000 J. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
 *    Multiple inheritance
 *    Support shared class DATA
 *    scoping (hidden, protected, readOnly)
 *    Use of __cls_param function to allow multiple superclass declaration
 *    Suppress of SetType and SetInit not more nedded
 *    Delegation and forwarding
 *    Reworking of hashing  as dicRealloc
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
      __clsAddMsg( s_hClass, "AddMultiData", @AddMultiData(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddClassData", @AddClassData(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMultiClsData", @AddMultiClsData(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddInline"   , @AddInline()   , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMethod"   , @AddMethod()   , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddVirtual"  , @AddVirtual()  , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Instance"    , @Instance()    , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "SetOnError"  , @SetOnError()  , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "cSuper"      , {|Self| iif(::acSuper == NIL .OR. len(::acSuper) == 0, NIL, ::acSuper[1] ) }, HB_OO_MSG_INLINE )
      __clsAddMsg( s_hClass, "_cSuper"     , {|Self, xVal| iif(::acSuper == NIL .OR. len(::acSuper) == 0, (::acSuper := { xVal} ), ::acSuper[1] := xVal), xVal }, HB_OO_MSG_INLINE )
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
      __clsAddMsg( s_hClass, "acSuper"     ,  8, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_acSuper"    ,  8, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "nOnError"    ,  9, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_nOnError"   ,  9, HB_OO_MSG_DATA )
   ENDIF

   RETURN __clsInst( s_hClass )

//----------------------------------------------------------------------------//

STATIC FUNCTION New( cClassName, xSuper )
// xSuper is used here as the new preprocessor file (HBCLASS.CH) send here
// always an array (if no superclass, this will be an empty one)
// In case of direct class creation (without the help of preprocessor) xSuper can be
// either NIL or contain the name of the superclass.

   LOCAL Self := QSelf()
   LOCAL nSuper, i

   ::acSuper := {}
   nSuper := 0

   If Valtype( xSuper ) == 'A'
      If Len( xSuper ) >= 1
         ::acSuper := xSuper
         nSuper := Len( xSuper )
      EndIf
   ElseIf Valtype( xSuper ) == 'C'
         ::acSuper := { xSuper }
         nSuper := 1
   EndIf

   ::cName     := Upper( cClassName )
   ::aDatas    := {}
   ::aMethods  := {}
   ::aClsDatas := {}
   ::aInlines  := {}
   ::aVirtuals := {}

   For i := 1 to nSuper
       If !ISCHARACTER( ::acSuper[i] )
          exit
       EndIf
   Next
   If i < nSuper
      nSuper := i - 1
      aSize(::acSuper, nSuper)
   EndIf

   RETURN Self

//----------------------------------------------------------------------------//

STATIC FUNCTION Create()

   LOCAL Self := QSelf()
   LOCAL n
   LOCAL nLen := Len( ::acSuper )
   LOCAL nLenDatas := Len( ::aDatas ) //Datas local to the class !!
   LOCAL nDataBegin := 0
   LOCAL nClassBegin := 0
   LOCAL hClass
   LOCAL ahSuper := array( nLen )
   Local aoSuper := array( nLen )

   If nLen == 0
      hClass := __clsNew( ::cName, nLenDatas )
   Else                                         // Multi inheritance
      For n := 1 to nLen
          ahSuper[n] := __clsInstSuper( Upper( ::acSuper[n] ) ) // Super handle available
          aoSuper[n] := __clsInst( ahSuper[n] )
      Next

      hClass := __clsNew( ::cName, nLenDatas, ahSuper )

      __clsAddMsg( hClass, Upper( ::acSuper[1] ), ahSuper[1], HB_OO_MSG_SUPER, aoSuper[1], HBCLSTP_CLASS + 1 )
      __clsAddMsg( hClass, "SUPER"              , ahSuper[1], HB_OO_MSG_SUPER, aoSuper[1], HBCLSTP_CLASS + 1 )
      __clsAddMsg( hClass, "__SUPER"            , ahSuper[1], HB_OO_MSG_SUPER, aoSuper[1], HBCLSTP_CLASS + 1 )

      nDataBegin   += __cls_CntData( ahSuper[1] )        // Get offset for new Datas
      nClassBegin  += __cls_CntClsData( ahSuper[1] )     // Get offset for new ClassData

      For n := 2 to nLen
          __clsAddMsg( hClass, Upper( ::acSuper[n] ), ahSuper[n], HB_OO_MSG_SUPER, aoSuper[n], HBCLSTP_CLASS + 1 )

          nDataBegin   += __cls_CntData( ahSuper[n] )        // Get offset for new DATAs
          nClassBegin  += __cls_CntClsData( ahSuper[n] )     // Get offset for new ClassData

      Next

   EndIf

   ::hClass := hClass

   FOR n := 1 TO nLenDatas
      __clsAddMsg( hClass, ::aDatas[ n ][ HB_OO_DATA_SYMBOL ]       , n + nDataBegin, ;
                   HB_OO_MSG_DATA, ::aDatas[ n ][ HB_OO_DATA_VALUE ], ::aDatas[ n ][ HB_OO_DATA_SCOPE ] )
      __clsAddMsg( hClass, "_" + ::aDatas[ n ][ HB_OO_DATA_SYMBOL ] , n + nDataBegin, ;
                   HB_OO_MSG_DATA,                                  , ::aDatas[ n ][ HB_OO_DATA_SCOPE ] )
   NEXT

   nLen := Len( ::aMethods )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aMethods[ n ][ HB_OO_MTHD_SYMBOL ], ::aMethods[ n ][ HB_OO_MTHD_PFUNCTION ], HB_OO_MSG_METHOD, , ::aMethods[ n ][ HB_OO_MTHD_SCOPE ] )
   NEXT

   nLen := Len( ::aClsDatas )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aClsDatas[ n ][ HB_OO_CLSD_SYMBOL ]      , n + nClassBegin,;
                   HB_OO_MSG_CLASSDATA, ::aClsDatas[ n ][ HB_OO_CLSD_VALUE ], ::aClsDatas[ n ][ HB_OO_CLSD_SCOPE ] )
      __clsAddMsg( hClass, "_" + ::aClsDatas[ n ][ HB_OO_CLSD_SYMBOL ], n + nClassBegin,;
                   HB_OO_MSG_CLASSDATA,                                     , ::aClsDatas[ n ][ HB_OO_CLSD_SCOPE ] )
   NEXT

   nLen := Len( ::aInlines )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aInlines[ n ][ HB_OO_MTHD_SYMBOL ], ::aInlines[ n ][ HB_OO_MTHD_PFUNCTION ],;
                   HB_OO_MSG_INLINE, , ::aInlines[ n ][ HB_OO_MTHD_SCOPE ] )
   NEXT

   nLen := Len( ::aVirtuals )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aVirtuals[ n ], n, HB_OO_MSG_VIRTUAL )
   NEXT

   if ::nOnError != nil
      __clsAddMsg( hClass, ::nOnError, , HB_OO_MSG_ONERROR )
   endif

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION Instance()

   LOCAL Self := QSelf()

   RETURN __clsInst( ::hClass )

//----------------------------------------------------------------------------//

STATIC FUNCTION AddData( cData, xInit, cType, nScope )  /* xInit is initializer */

   LOCAL Self := QSelf()

   AAdd( ::aDatas, { cData, xInit, cType, nScope } )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddMultiData( cType, xInit, nScope, aData )

   Local Self := QSelf()
   Local i
   Local nParam := Len(aData)

   For i := 1 to nParam
       If !ISCHARACTER( aData[i] )
          exit
       EndIf
   Next i
   If i < nParam
      nParam := i - 1
      aSize(aData, nParam)
   Endif

   For i := 1 to nParam

    ::AddData( aData[i], xInit, cType, nScope )

   next i

RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddClassData( cData, xInit, cType, nScope )

   LOCAL Self := QSelf()

   aAdd( ::aClsDatas, { cData, xInit, cType, nScope } )

RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddMultiClsData( cType, xInit, nScope, aData )

   Local Self := QSelf()
   Local i
   Local nParam := Len(aData)

   For i := 1 to nParam
       If !ISCHARACTER( aData[i] )
          exit
       EndIf
   Next i
   If i < nParam
      nParam := i - 1
      aSize(aData, nParam)
   Endif

   For i := 1 to nParam

   ::AddClassData( aData[i], xInit, cType, nScope )

   Next i

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddInline( cMethod, bCode, nScope )

   LOCAL Self := QSelf()

   aAdd( ::aInlines, { cMethod, bCode, nScope } )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddMethod( cMethod, nFuncPtr, nScope )

   LOCAL Self := QSelf()

   aAdd( ::aMethods, { cMethod, nFuncPtr, nScope } )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION AddVirtual( cMethod )

   LOCAL Self := QSelf()

   aAdd( ::aVirtuals, cMethod )

   RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION SetOnError( nFuncPtr )

   LOCAL Self := QSelf()

   ::nOnError := nFuncPtr

   RETURN NIL

//----------------------------------------------------------------------------//

