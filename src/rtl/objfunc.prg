/*
 * Harbour Project source code:
 * Dynamic Object management and misc. Object related functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
 * www - http://harbour-project.org
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 *    __objGetMsgList
 *
 * Copyright 2000 Jf. Lefebvre <jfl@mafact.com> and Ra. Cuylen <rac@mafact.com>
 *    __objDerivedFrom
 *
 * New Param for Method :ClassSel() to allow it to return only ClassData array
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "error.ch"
#include "hboo.ch"

FUNCTION __objHasData( oObject, cSymbol )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN ;
      __objHasMsg( oObject, cSymbol ) .AND. ;
      __objHasMsg( oObject, "_" + cSymbol )

FUNCTION __objHasMethod( oObject, cSymbol )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN ;
      __objHasMsg( oObject, cSymbol ) .AND. ;
      ! __objHasMsg( oObject, "_" + cSymbol )

FUNCTION __objGetMsgList( oObject, lDataMethod, nClassType )

   LOCAL aInfo
   LOCAL aData
   LOCAL cName
   LOCAL nFirst

   IF ! HB_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   hb_default( @lDataMethod, .T. )

   aInfo := oObject:ClassSel( hb_defaultValue( nClasstype, HB_MSGLISTALL ) )
   aData := {}
   nFirst := AScan( aInfo, {| n | hb_LeftEq( n, "_" ) } )

   FOR EACH cName IN aInfo

      /* Set functions begin with a leading underscore */
      IF ! hb_LeftEq( cName, "_" )

         /* Find position of matching set function in array with all symbols */
         /* If found: DATA, else: METHOD */
         IF ( AScan( aInfo, {| tmp | tmp == ( "_" + cName ) }, nFirst ) != 0 ) == lDataMethod
            AAdd( aData, cName )
         ENDIF
      ENDIF
   NEXT

   RETURN aData

FUNCTION __objGetMethodList( oObject )

   IF ! HB_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN __objGetMsgList( oObject, .F. )

FUNCTION __objGetValueList( oObject, aExcept )

   LOCAL aData
   LOCAL cSymbol

   IF ! HB_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   hb_default( @aExcept, {} )

   aData := {}
   FOR EACH cSymbol IN __objGetMsgList( oObject )
      IF hb_AScan( aExcept, cSymbol,,, .T. ) == 0
         AAdd( aData, { cSymbol, __objSendMsg( oObject, cSymbol ) } )
      ENDIF
   NEXT

   RETURN aData

FUNCTION __objSetValueList( oObject, aData )

   IF HB_ISOBJECT( oObject )
      AEval( aData, {| aItem | __objSendMsg( oObject, "_" + aItem[ HB_OO_DATA_SYMBOL ], aItem[ HB_OO_DATA_VALUE ] ) } )
   ELSE
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN oObject

FUNCTION __objAddMethod( oObject, cSymbol, nFuncPtr )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol ) .OR. ! HB_ISSYMBOL( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF ! __objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, nFuncPtr, HB_OO_MSG_METHOD, NIL, 1 )
   ENDIF

   RETURN oObject

FUNCTION __objAddInline( oObject, cSymbol, bInline )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF ! __objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, bInline, HB_OO_MSG_INLINE, NIL, 1 )
   ENDIF

   RETURN oObject

FUNCTION __objAddData( oObject, cSymbol )

   LOCAL nSeq, hClass

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF ! __objHasMsg( oObject, cSymbol ) .AND. ! __objHasMsg( oObject, "_" + cSymbol )
      hClass := oObject:ClassH
      nSeq   := __cls_IncData( hClass )         // Allocate new Seq#
      __clsAddMsg( hClass,       cSymbol, nSeq, HB_OO_MSG_ACCESS, NIL, 1 )
      __clsAddMsg( hClass, "_" + cSymbol, nSeq, HB_OO_MSG_ASSIGN, NIL, 1 )
   ENDIF

   RETURN oObject

FUNCTION __objModMethod( oObject, cSymbol, nFuncPtr )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol ) .OR. ! HB_ISSYMBOL( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, nFuncPtr )
   ENDIF

   RETURN oObject

FUNCTION __objModInline( oObject, cSymbol, bInline )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol ) .OR. ! HB_ISBLOCK( bInline )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, bInline )
   ENDIF

   RETURN oObject

FUNCTION __objDelMethod( oObject, cSymbol )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
   ENDIF

   RETURN oObject

FUNCTION __objDelInline( oObject, cSymbol )
   RETURN __objDelMethod( oObject, cSymbol )

FUNCTION __objDelData( oObject, cSymbol )

   IF ! HB_ISOBJECT( oObject ) .OR. ! HB_ISSTRING( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasData( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
      __clsDelMsg( oObject:ClassH, "_" + cSymbol )
      __cls_DecData( oObject:ClassH )         // Decrease wData
   ENDIF

   RETURN oObject

FUNCTION __objDerivedFrom( oObject, xSuper )

   LOCAL cClassName

   IF ! HB_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   IF HB_ISOBJECT( xSuper )
      cClassName := xSuper:ClassName()
   ELSEIF HB_ISSTRING( xSuper )
      cClassName := hb_asciiUpper( xSuper )
   ELSE
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   RETURN __clsParent( oObject:ClassH, cClassName )

FUNCTION __objGetProperties( oObject, lAllExported )

   LOCAL msg
   LOCAL aMsgList := __clsGetProperties( oObject:classH, lAllExported )

   FOR EACH msg IN aMsgList
      msg := { msg, __objSendMsg( oObject, msg ) }
   NEXT

   RETURN aMsgList
