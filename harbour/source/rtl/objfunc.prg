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
 * Copyright 2000 Jf. Lefebvre <jfl@mafact.com> and Ra. Cuylen <rac@mafact.com>
 *    __objDerivedFrom
 *
 * New Param for Method :ClassSel() to allow it to return only ClassData array
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "error.ch"
#include "hboo.ch"

FUNCTION __objHasData( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN __objHasMsg( oObject, cSymbol ) .AND. ;
          __objHasMsg( oObject, "_" + cSymbol )

FUNCTION __objHasMethod( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN __objHasMsg( oObject, cSymbol ) .AND. ;
          !__objHasMsg( oObject, "_" + cSymbol )


// nCLassType can be 0, 1 or 2 see hbOO.ch
//#define HB_MSGLISTALL   0
//#define HB_MSGLISTCLASS 1
//#define HB_MSGLISTPURE  2

FUNCTION __objGetMsgList( oObject, lDataMethod, nClassType )
   LOCAL aInfo
   LOCAL aData
   LOCAL n
   LOCAL nLen

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   IF !ISLOGICAL( lDataMethod )
      lDataMethod := .T.
   ENDIF

   IF !ISNUMBER( nClassType  )
      nClasstype  := HB_MSGLISTALL
   ENDIF

   aInfo := ASort( oObject:ClassSel(nClassType) )
   aData := {}
   n     := 1
   nLen  := Len( aInfo )

   DO WHILE n <= nLen .AND. !( Substr( aInfo[ n ], 1, 1 ) == "_" )

      /* If in range and no set function found yet ( set functions */
      /* begin with a leading underscore ).                        */

      // If found -> DATA
      //     else    METHOD

      /* Find position of matching set function in array with all symbols */

      IF ( AScan( aInfo, {| tmp | tmp == ( "_" + aInfo[ n ] ) }, n + 1 ) != 0 ) == lDataMethod
         AAdd( aData, aInfo[ n ] )
      ENDIF

      n++

   ENDDO

   RETURN aData

FUNCTION __objGetMethodList( oObject )

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN __objGetMsgList( oObject, .F. )

FUNCTION __objGetValueList( oObject, aExcept )
   LOCAL aDataSymbol
   LOCAL nLen
   LOCAL aData
   LOCAL cSymbol
   LOCAL n

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   IF !ISARRAY( aExcept )
      aExcept := {}
   ENDIF

   aDataSymbol := __objGetMsgList( oObject )
   nLen        := Len( aDataSymbol )
   aData       := {}

   FOR n := 1 to nLen
      cSymbol := aDataSymbol[ n ]
      IF AScan( aExcept, {| tmp | tmp == cSymbol } ) == 0
         AAdd( aData, { cSymbol, __objSendMsg( oObject, cSymbol ) } )
      ENDIF
   NEXT

   RETURN aData

FUNCTION __ObjSetValueList( oObject, aData )

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSE
      AEval( aData, {| aItem | __objSendMsg( oObject, "_" + aItem[ HB_OO_DATA_SYMBOL ], aItem[ HB_OO_DATA_VALUE ] ) } )
   ENDIF

   RETURN oObject

FUNCTION __objAddMethod( oObject, cSymbol, nFuncPtr )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol ) .OR. !ISNUMBER( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, nFuncPtr, HB_OO_MSG_METHOD, NIL, 1 )
   ENDIF

   RETURN oObject

FUNCTION __objAddInline( oObject, cSymbol, bInline )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, bInline, HB_OO_MSG_INLINE, NIL, 1 )
   ENDIF

   RETURN oObject

FUNCTION __objAddData( oObject, cSymbol )
   LOCAL nSeq, hClass

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF !__objHasMsg( oObject, cSymbol ) .AND. !__objHasMsg( oObject, "_" + cSymbol )
      hClass := oObject:ClassH
      nSeq   := __cls_IncData( hClass )         // Allocate new Seq#
      __clsAddMsg( hClass,       cSymbol, nSeq, HB_OO_MSG_DATA, NIL, 1 )
      __clsAddMsg( hClass, "_" + cSymbol, nSeq, HB_OO_MSG_DATA, NIL, 1 )
   ENDIF

   RETURN oObject

FUNCTION __objModMethod( oObject, cSymbol, nFuncPtr )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol ) .OR. !ISNUMBER( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, nFuncPtr )
   ENDIF

   RETURN oObject

FUNCTION __objModInline( oObject, cSymbol, bInline )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol ) .OR. !ISBLOCK( bInline )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, bInline )
   ENDIF

   RETURN oObject

FUNCTION __objDelMethod( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
   ENDIF

   RETURN oObject

FUNCTION __objDelInline( oObject, cSymbol )
   RETURN __objDelMethod( oObject, cSymbol )              // Same story

FUNCTION __objDelData( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasData( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
      __clsDelMsg( oObject:ClassH, "_" + cSymbol )
      __cls_DecData( oObject:ClassH )         // Decrease wData
   ENDIF

   RETURN oObject

FUNCTION __objDerivedFrom( oObject, xSuper )
   LOCAL cClassName

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   IF ISOBJECT( xSuper )
      cClassName := xSuper:ClassName()
   ELSEIF ISCHARACTER( xSuper )
      cClassName := Upper( xSuper )
   ELSE
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN __clsParent( oObject:ClassH, cClassName )

