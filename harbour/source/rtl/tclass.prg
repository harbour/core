/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base Class for internal handling of class creation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 *    Preparing the InitClass class method (not working !! )
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    Support for inheritance
 *    Support for default DATA values
 *
 * See doc/license.txt for licensing terms.
 *
 */

// Harbour Class HBClass to build classes

#include "common.ch"
#include "hboo.ch"

REQUEST HBObject

FUNCTION HBClass()

   STATIC s_hClass /* NOTE: Automatically default to NIL */

   IF s_hClass == NIL
      s_hClass := __clsNew( "HBCLASS", 10)
/*    s_hClass := __clsNew( "HBCLASS", 11)  */

      __clsAddMsg( s_hClass, "New"            , @New()            , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Create"         , @Create()         , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddData"        , @AddData()        , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMultiData"   , @AddMultiData()   , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddClassData"   , @AddClassData()   , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMultiClsData", @AddMultiClsData(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddInline"      , @AddInline()      , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMethod"      , @AddMethod()      , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddClsMethod"   , @AddClsMethod()   , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddVirtual"     , @AddVirtual()     , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Instance"       , @Instance()       , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "SetOnError"     , @SetOnError()     , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "InitClass"      , @InitClass()      , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "cSuper"         , {| Self | iif( ::acSuper == NIL .OR. Len( ::acSuper ) == 0, NIL, ::acSuper[ 1 ] ) }, HB_OO_MSG_INLINE )
      __clsAddMsg( s_hClass, "_cSuper"        , {| Self, xVal | iif( ::acSuper == NIL .OR. Len( ::acSuper ) == 0, ( ::acSuper := { xVal } ), ::acSuper[ 1 ] := xVal ), xVal }, HB_OO_MSG_INLINE )
      __clsAddMsg( s_hClass, "hClass"         ,  1, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_hClass"        ,  1, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "cName"          ,  2, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_cName"         ,  2, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aDatas"         ,  3, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aDatas"        ,  3, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aMethods"       ,  4, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aMethods"      ,  4, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aClsDatas"      ,  5, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aClsDatas"     ,  5, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aClsMethods"    ,  6, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aClsMethods"   ,  6, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aInlines"       ,  7, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aInlines"      ,  7, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "aVirtuals"      ,  8, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_aVirtuals"     ,  8, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "acSuper"        ,  9, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_acSuper"       ,  9, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "nOnError"       , 10, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_nOnError"      , 10, HB_OO_MSG_DATA )
  /*  __clsAddMsg( s_hClass, "class"          , 11, HB_OO_MSG_DATA )
      __clsAddMsg( s_hClass, "_class"         , 11, HB_OO_MSG_DATA ) */

   ENDIF

   RETURN __clsInst( s_hClass )

//----------------------------------------------------------------------------//

// xSuper is used here as the new preprocessor file (HBCLASS.CH) send here
// always an array (if no superclass, this will be an empty one)
// In case of direct class creation (without the help of preprocessor) xSuper can be
// either NIL or contain the name of the superclass.

STATIC FUNCTION New( cClassName, xSuper )

   LOCAL Self := QSelf()
   LOCAL nSuper, i

   IF ISARRAY( xSuper ) .AND. Len( xSuper ) >= 1
      ::acSuper := xSuper
      nSuper := Len( xSuper )
   ELSEIF ISCHARACTER( xSuper ) .AND. ! empty( xSuper )
      ::acSuper := { xSuper }
      nSuper := 1
   ELSE
      ::acSuper := {}
      nSuper := 0
   ENDIF

   ::cName       := Upper( cClassName )

   ::aDatas      := {}
   ::aMethods    := {}
   ::aClsDatas   := {}
   ::aClsMethods := {}
   ::aInlines    := {}
   ::aVirtuals   := {}

   FOR i := 1 TO nSuper
      IF ! ISCHARACTER( ::acSuper[ i ] )
         EXIT
      ENDIF
   NEXT
   IF i < nSuper
      nSuper := i - 1
      ASize( ::acSuper, nSuper)
   ENDIF

   RETURN QSelf()

//----------------------------------------------------------------------------//

STATIC PROCEDURE Create(MetaClass)

   LOCAL Self := QSelf()
   LOCAL n
   LOCAL nLen := Len( ::acSuper )
   LOCAL nLenDatas := Len( ::aDatas ) //Datas local to the class !!
   LOCAL nDataBegin := 0
   LOCAL nClassBegin := 0
   LOCAL hClass
   LOCAL ahSuper := Array( nLen )

/* Self:Class := MetaClass */

   IF nLen == 0
      hClass := __clsNew( ::cName, nLenDatas )
   ELSE                                         // Multi inheritance
      FOR n := 1 TO nLen
         ahSuper[ n ] := __clsInstSuper( Upper( ::acSuper[ n ] ) ) // Super handle available
      NEXT

      hClass := __clsNew( ::cName, nLenDatas + nlen , ahSuper )

      nDataBegin   += __cls_CntData( ahSuper[ 1 ] )        // Get offset for new Datas
      nClassBegin  += __cls_CntClsData( ahSuper[ 1 ] )     // Get offset for new ClassData

      FOR n := 2 TO nLen
         nDataBegin   += __cls_CntData( ahSuper[ n ] )        // Get offset for new DATAs
         nClassBegin  += __cls_CntClsData( ahSuper[ n ] )     // Get offset for new ClassData
      NEXT

      __clsAddMsg( hClass, Upper( ::acSuper[ 1 ] ), ++nDataBegin, HB_OO_MSG_SUPER, ahSuper[ 1 ], HB_OO_CLSTP_CLASS + 1 )
      // nData begin stay here the same so as, SUPER and __SUPER will share the same pointer to super object with the first one.
      __clsAddMsg( hClass, "SUPER"                , nDataBegin, HB_OO_MSG_SUPER, ahSuper[ 1 ], 1 )
      __clsAddMsg( hClass, "__SUPER"              , nDataBegin, HB_OO_MSG_SUPER, ahSuper[ 1 ], 1 )

      FOR n := 2 TO nLen
         __clsAddMsg( hClass, Upper( ::acSuper[ n ] ), ++nDataBegin, HB_OO_MSG_SUPER, ahSuper[ n ], HB_OO_CLSTP_CLASS + 1 )
      NEXT

   ENDIF

   ::hClass := hClass

   // We will work here on the MetaClass object to add the Class Method
   // as needed
   //nLen := Len( ::aClsMethods )
   //FOR n := 1 TO nLen
   // // do it
   //NEXT
   ////

   //Local message...

   FOR n := 1 TO nLenDatas
      __clsAddMsg( hClass, ::aDatas[ n ][ HB_OO_DATA_SYMBOL ]       , n + nDataBegin, ;
                   HB_OO_MSG_DATA, ::aDatas[ n ][ HB_OO_DATA_VALUE ], ::aDatas[ n ][ HB_OO_DATA_SCOPE ],;
                   ::aDatas[ n ][ HB_OO_DATA_PERSISTENT ] )
      __clsAddMsg( hClass, "_" + ::aDatas[ n ][ HB_OO_DATA_SYMBOL ] , n + nDataBegin, ;
                   HB_OO_MSG_DATA,                                  , ::aDatas[ n ][ HB_OO_DATA_SCOPE ] )
   NEXT

   nLen := Len( ::aMethods )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aMethods[ n ][ HB_OO_MTHD_SYMBOL ], ::aMethods[ n ][ HB_OO_MTHD_PFUNCTION ], HB_OO_MSG_METHOD, NIL, ::aMethods[ n ][ HB_OO_MTHD_SCOPE ],;
                   ::aMethods[ n ][ HB_OO_MTHD_PERSISTENT ] )
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
                   HB_OO_MSG_INLINE, NIL, ::aInlines[ n ][ HB_OO_MTHD_SCOPE ],;
                   ::aInlines[ n ][ HB_OO_MTHD_PERSISTENT ] )
   NEXT

   nLen := Len( ::aVirtuals )
   FOR n := 1 TO nLen
      __clsAddMsg( hClass, ::aVirtuals[ n ], n, HB_OO_MSG_VIRTUAL )
   NEXT

   IF ::nOnError != NIL
      __clsAddMsg( hClass, "__OnError", ::nOnError, HB_OO_MSG_ONERROR )
   ENDIF

   RETURN

//----------------------------------------------------------------------------//

STATIC FUNCTION Instance()
 LOCAL Self := QSelf()
 Local oInstance := __clsInst( ::hClass )
 /*oInstance:Class := Self:Class*/
RETURN oInstance

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddData( cData, xInit, cType, nScope, lNoinit, lPersistent )

   LOCAL Self := QSelf()

   if lNoInit==NIL;lNoInit:=.F.;endif
   if lPersistent == nil; lpersistent := .f.; endif

   // Default Init for Logical and numeric
   IF ! lNoInit .AND. cType != NIL .AND. xInit == NIL
      IF Upper( Left( cType, 1 ) ) == "L"
         xInit := .F.
      ELSEIF Upper( Left( cType, 1 ) ) $ "NI"   /* Numeric Int */
         xInit := 0
      ENDIF
   ENDIF

   AAdd( ::aDatas, { cData, xInit, cType, nScope, lPersistent } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddMultiData( cType, xInit, nScope, aData, lNoInit, lPersistent )

   LOCAL Self := QSelf()
   LOCAL i
   LOCAL nParam := Len( aData )

   FOR i := 1 TO nParam
      IF ! ISCHARACTER( aData[ i ] )
         EXIT
      ENDIF
   NEXT
   IF i < nParam
      nParam := i - 1
      ASize( aData, nParam )
   ENDIF

   FOR i := 1 TO nParam
      ::AddData( aData[ i ], xInit, cType, nScope, lNoInit, lPersistent )
   NEXT

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddClassData( cData, xInit, cType, nScope, lNoInit )

   LOCAL Self := QSelf()

   if lNoInit==NIL;lNoInit:=.F.;endif

   // Default Init for Logical and numeric
   IF ! lNoInit .AND. cType != NIL .AND. xInit == NIL
      IF Upper( Left( cType, 1 ) ) == "L"
         xInit := .F.
      ELSEIF Upper( Left( cType, 1 ) ) $ "NI"  /* Numeric Int */
         xInit := 0
      ENDIF
   ENDIF

   AAdd( ::aClsDatas, { cData, xInit, cType, nScope } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddMultiClsData( cType, xInit, nScope, aData, lNoInit )

   LOCAL Self := QSelf()
   LOCAL i
   LOCAL nParam := Len( aData )

   FOR i := 1 TO nParam
      IF ! ISCHARACTER( aData[ i ] )
         EXIT
      ENDIF
   NEXT
   IF i < nParam
      nParam := i - 1
      ASize( aData, nParam )
   ENDIF

   FOR i := 1 TO nParam
      ::AddClassData( aData[ i ], xInit, cType, nScope, lNoInit )
   NEXT

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddInline( cMethod, bCode, nScope, lPersistent )

   LOCAL Self := QSelf(), nAt

   /* Remove possible ( <x,...> )*/
   IF ( nAt := At( "(", cMethod ) ) > 0
      cMethod := RTrim( Left( cMethod, nAt - 1 ) )
   ENDIF

   AAdd( ::aInlines, { cMethod, bCode, nScope, lPersistent } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddMethod( cMethod, nFuncPtr, nScope, lPersistent )

   LOCAL Self := QSelf(), nAt

   /* Remove possible ( <x,...> )*/
   IF ( nAt := At( "(", cMethod ) ) > 0
      cMethod := RTrim( Left( cMethod, nAt - 1 ) )
   ENDIF

   AAdd( ::aMethods, { cMethod, nFuncPtr, nScope, lPersistent } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddClsMethod( cMethod, nFuncPtr, nScope )

   LOCAL Self := QSelf(), nAt

   /* Remove possible ( <x,...> )*/
   IF ( nAt := At( "(", cMethod ) ) > 0
      cMethod := RTrim( Left( cMethod, nAt - 1 ) )
   ENDIF

   AAdd( ::aClsMethods, { cMethod, nFuncPtr, nScope } )

   RETURN

//----------------------------------------------------------------------------//
STATIC PROCEDURE AddVirtual( cMethod )

   LOCAL Self := QSelf(), nAt

   /* Remove possible ( <x,...> )*/
   IF ( nAt := At( "(", cMethod ) ) > 0
      cMethod := RTrim( Left( cMethod, nAt - 1 ) )
   ENDIF

   AAdd( ::aVirtuals, cMethod )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE SetOnError( nFuncPtr )

   LOCAL Self := QSelf()

   ::nOnError := nFuncPtr

   RETURN

//----------------------------------------------------------------------------//

STATIC FUNCTION InitClass()

   LOCAL Self := QSelf()

   RETURN Self

//----------------------------------------------------------------------------//