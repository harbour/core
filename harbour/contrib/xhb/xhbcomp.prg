/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * xhb compatibility functions
 *
 * Copyright 2007 Viktor Szakats <harbour.01 syenar.hu>
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

#include "common.ch"
#include "hbclass.ch"
#include "xhb.ch"

ANNOUNCE XHB_LIB

INIT PROCEDURE xhb_Init()
   /* Add calls to do initial settings to Harbour to be more compatible with xhb. */
   ASSOCIATE CLASS xhb_Character WITH TYPE Character
   ASSOCIATE CLASS xhb_Numeric   WITH TYPE Numeric
   ASSOCIATE CLASS xhb_Array     WITH TYPE Array
   ASSOCIATE CLASS xhb_Hash      WITH TYPE Hash
RETURN

CREATE CLASS Character INHERIT HBScalar FUNCTION xhb_Character
   OPERATOR "[]" FUNCTION XHB_INDEX()
   OPERATOR "+"  FUNCTION XHB_PLUS()
   OPERATOR "-"  FUNCTION XHB_MINUS()
   OPERATOR "*"  FUNCTION XHB_MULT()
   OPERATOR "/"  FUNCTION XHB_DIV()
   OPERATOR "%"  FUNCTION XHB_MOD()
   OPERATOR "^"  FUNCTION XHB_POW()
   OPERATOR "++" FUNCTION XHB_INC()
   OPERATOR "--" FUNCTION XHB_DEC()

   METHOD AsString INLINE HB_QSelf()
ENDCLASS

CREATE CLASS Numeric INHERIT HBScalar FUNCTION xhb_Numeric
   OPERATOR "+"  FUNCTION XHB_PLUS()
   OPERATOR "-"  FUNCTION XHB_MINUS()
   OPERATOR "*"  FUNCTION XHB_MULT()
   OPERATOR "/"  FUNCTION XHB_DIV()
   OPERATOR "%"  FUNCTION XHB_MOD()
   OPERATOR "^"  FUNCTION XHB_POW()
   OPERATOR "++" FUNCTION XHB_INC()
   OPERATOR "--" FUNCTION XHB_DEC()

   METHOD AsString INLINE LTrim( Str( ( HB_QSelf() ) ) )
ENDCLASS

CREATE CLASS Array INHERIT HBScalar FUNCTION xhb_Array
   OPERATOR "[]" FUNCTION XHB_INDEX()
   OPERATOR "$$" FUNCTION XHB_INCLUDE()

   MESSAGE Add             METHOD Append
   METHOD  AddAll
   METHOD  Append
   METHOD  asString        INLINE '{...}' //ValtoPrg( HB_QSelf() )
   METHOD  At( n )         INLINE Self[ n ]
   METHOD  AtIndex( n )    INLINE Self[ n ]
   METHOD  AtPut( n, x )   INLINE Self[ n ] := x
   METHOD  Collect
   METHOD  Copy            INLINE aCopy( Self, Array( Len( Self ) ) )
   METHOD  DeleteAt
   METHOD  Do
   METHOD  IndexOf
   METHOD  Init( nLen )    INLINE ::Size := IIF( nLen == NIL, 0, nLen ), Self
   METHOD  InsertAt
   METHOD  Remove
   METHOD  Scan( bScan )   INLINE aScan( Self, bScan )
   METHOD  _Size( nLen )   INLINE aSize( Self, nLen ), nLen

ENDCLASS

CREATE CLASS Hash INHERIT HBScalar FUNCTION xhb_Hash
   ON ERROR FUNCTION XHB_HASHERROR()
   OPERATOR "+"  FUNCTION XHB_PLUS()
   OPERATOR "-"  FUNCTION XHB_MINUS()
   OPERATOR "$$" FUNCTION XHB_INCLUDE()

   METHOD Add( xKey, xValue )      INLINE Self[ xKey ] := xValue, Self
   METHOD AddAll( oCollection )
   METHOD AtIndex( nPos )          INLINE HGetValueAt( Self, nPos )
   METHOD AtPut( nPos, xValue )    INLINE HSetValueAt( Self, nPos, xValue )
   METHOD Append( xKey, xValue )   INLINE Self[ xKey ] := xValue, Self
   METHOD AsString()               INLINE '...HASH...' // ValToPrg( HB_QSelf() )
   METHOD Collect( bCollect )
   METHOD Copy()                   INLINE hCopy( Self, Hash() )
   METHOD DeleteAt( nPos )         INLINE hDelat( Self, nPos )
   METHOD Do( bBlock )
   METHOD IndexOf( xValue )        INLINE hScan( Self, xValue )
   METHOD Init( nLen )             INLINE ::Size := IIF( nLen == NIL, 0, nLen ), Self
   METHOD Remove( xValue )         INLINE hDel( Self, xValue )
   METHOD Scan( bScan )            INLINE hScan( Self, bScan )
   METHOD _Size( nLen )

ENDCLASS

//----------------------------------------------------------------------------//

METHOD AddAll( otherCollection ) CLASS Array
   otherCollection:Do( {|x| ::Add(x) } )
   RETURN Self

//----------------------------------------------------------------------------//

METHOD Append( x )   CLASS Array
   aAdd( Self, x )
   RETURN Self

//----------------------------------------------------------------------------//

METHOD Collect( bCollect ) CLASS Array
  LOCAL xElement, aResult[0]

  FOR EACH  xElement IN Self 
     IF Eval( bCollect, UnRef( xElement ) )
        aAdd( aResult, UnRef( xElement ) )
     END
  NEXT

RETURN aResult

//----------------------------------------------------------------------------//

METHOD deleteAt( nPos ) CLASS Array

  IF nPos > 0 .AND. nPos <= Len( Self )
     aDel( Self, nPos, .T. )
  ENDIF

RETURN Self

//----------------------------------------------------------------------------//

METHOD Do( bEval ) CLASS Array
  LOCAL xElement

  FOR EACH  xElement IN Self 
     bEval:Eval( UnRef( xElement ), HB_EnumIndex() )
  NEXT

RETURN Self

//----------------------------------------------------------------------------//

METHOD IndexOf( xValue ) CLASS Array
  LOCAL xElement, cType := ValType( xValue )

  FOR EACH xElement IN Self
     IF ValType( xElement ) == cType .AND. xElement == xValue
        RETURN HB_EnumIndex()
     END
  NEXT

RETURN 0

//----------------------------------------------------------------------------//

METHOD InsertAt( nPos, xValue ) CLASS Array

  IF nPos > Len( self )
    aSize( Self, nPos )
    Self[ nPos ] := xValue
  ELSEIF nPos > 0
    aIns( Self, nPos, xValue, .T. )
  ENDIF

RETURN Self

//----------------------------------------------------------------------------//

METHOD Remove( xValue ) CLASS Array
 
   ::DeleteAt( ::IndexOf( xValue ) )

RETURN Self

//----------------------------------------------------------------------------//

METHOD AddAll( oCollection ) CLASS HASH

   oCollection:Do( { |xKey, xValue| Self[ xKey ] := xValue } )

RETURN Self

//----------------------------------------------------------------------------//

METHOD Collect( bCollect ) CLASS HASH
   LOCAL xElement, aResult[0]

   FOR EACH xElement IN Self:Values
      IF Eval( bCollect, UnRef( xElement ) )
         aAdd( aResult, UnRef( xElement ) )
      END
   NEXT

RETURN aResult

//----------------------------------------------------------------------------//

METHOD Do( bDo ) CLASS HASH

   LOCAL xKey

   FOR EACH xKey IN Self:Keys
      Eval( bDo, xKey, Self[ xKey ] )
   NEXT

RETURN Self

//----------------------------------------------------------------------------//

METHOD _Size( nLen ) CLASS HASH

   LOCAL nOldLen := Len( Self ), Counter

   IF nLen == nOldLen
      RETURN nLen
   ELSEIF nLen > nOldLen
      hAllocate( Self, nLen )

      FOR Counter := nOldLen + 1 TO nLen
         Self[ "_SIZED_" + LTrim( Str( Counter ) ) ] := NIL
      NEXT
   ELSE
      FOR Counter := nOldLen TO nLen + 1
         hDelAt( Self, nLen + 1 )
      NEXT
   ENDIF

RETURN nLen

//----------------------------------------------------------------------------//

FUNCTION UnRef( xValue )

RETURN xValue

//----------------------------------------------------------------------------//
   
