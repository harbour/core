/*
 * CStr( xAnyType ) --> String
 *
 * Copyright 2001 Ron Pinkas <ron@@ronpinkas.com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "error.ch"

#include "hbclass.ch"

/*
   For performance NOT using OS indpendant R/T function,
   this define only used in ValTpPrg() which currently only used in win32.
 */

#define CRLF Chr( 13 ) + Chr( 10 )

#xtranslate Throw( <oErr> ) => ( Eval( ErrorBlock(), <oErr> ), Break( <oErr> ) )

//

FUNCTION CStrToVal( cExp, cType )

   IF ! HB_ISSTRING( cExp )
      Throw( xhb_ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
   ENDIF

   SWITCH cType
   CASE "C"
      RETURN cExp

   CASE "P"
      RETURN hb_HexToNum( cExp )

   CASE "D"
      IF IsDigit( SubStr( cExp, 3, 1 ) ) .AND. IsDigit( SubStr( cExp, 5, 1 ) )
         RETURN hb_SToD( cExp )
      ELSE
         RETURN CToD( cExp )
      ENDIF

   CASE "T"
      IF IsDigit( SubStr( cExp, 3, 1 ) ) .AND. ;
         IsDigit( SubStr( cExp, 5, 1 ) ) .AND. ;
         IsDigit( SubStr( cExp, 7, 1 ) )
         RETURN hb_SToT( cExp )
      ELSE
         RETURN hb_StrToTS( cExp )
      ENDIF

   CASE "L"
      RETURN SubStr( cExp, 1, 1 ) $ "TY" .OR. SubStr( cExp, 2, 1 ) $ "TY"

   CASE "N"
      RETURN Val( cExp )

   CASE "U"
      RETURN NIL

   /*
   CASE "A"
      Throw( xhb_ErrorNew( "CSTRTOVAL", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )

   CASE "B"
      Throw( xhb_ErrorNew( "CSTRTOVAL", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )

   CASE "O"
      Throw( xhb_ErrorNew( "CSTRTOVAL", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
   */

   OTHERWISE
      Throw( xhb_ErrorNew( "CSTRTOVAL", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
   ENDSWITCH

   RETURN NIL

//

FUNCTION StringToLiteral( cString )

   LOCAL lDouble := .F.
   LOCAL lSingle := .F.

   IF hb_regexHas( "\n|\r", cString ) .OR. ;
      ( ( lDouble := '"' $ cString ) .AND. ;
        ( lSingle := "'" $ cString ) .AND. ;
      hb_regexHas( "\[|\]", cString ) )

      cString := StrTran( cString, '"', '\"' )
      cString := StrTran( cString, Chr( 10 ), "\n" )
      cString := StrTran( cString, Chr( 13 ), "\r" )

      // TraceLog( cString )

      RETURN 'E"' + cString + '"'
   ELSEIF ! lDouble
      RETURN '"' + cString + '"'
   ELSEIF ! lSingle
      RETURN "'" + cString + "'"
   ENDIF

   RETURN "[" + cString + "]"

//

FUNCTION ValToPrg( xVal, cName, nPad, hRefs )

   LOCAL aVar, cRet, cPad, cRef, pRef

   // TraceLog( xVal, cName, nPad, hRefs )

   SWITCH ValType( xVal )
   CASE "C"
      RETURN StringToLiteral( xVal )

   CASE "D"
      RETURN "hb_SToD( '" + DToS( xVal ) + "' )"

   CASE "T"
      RETURN 't"' + hb_TSToStr( xVal, .T. ) + '"'

   CASE "L"
      RETURN iif( xVal, ".T.", ".F." )

   CASE "N"
      RETURN hb_ntos( xVal )

   CASE "A"
      pRef := __vmItemId( xVal )
      IF cName == NIL
         nPad := 0
         cName := "M->__ValToPrg_Array"
         hRefs := { => }
         cRet  := cName + " := "
      ELSEIF ! ( cRef := hb_HGetDef( hRefs, pRef ) ) == NIL
         RETURN cRef + " /* Cyclic */"
      ELSE
         cRet := ""
      ENDIF

      hRefs[ pRef ] := cName

      cRet  += "Array(" + hb_ntos( Len( xVal ) ) + ")" + CRLF

      nPad += 3
      cPad  := Space( nPad )

      FOR EACH aVar IN xVal
         cRef := cName + "[" + hb_ntos( aVar:__EnumIndex() ) + "]"
         cRet += cPad + cRef + " := " + ValToPrg( aVar, cRef, nPad, hRefs )
         IF ! Right( cRet, Len( CRLF ) ) == CRLF
            cRet += CRLF
         ENDIF
      NEXT

      nPad -= 3
      RETURN cRet

   CASE "H"
      pRef := __vmItemId( xVal )
      IF cName == NIL
         nPad := 0
         cName := "M->__ValToPrg_Hash"
         hRefs := { => }
         cRet  := cName + " := "
      ELSEIF ! ( cRef := hb_HGetDef( hRefs, pRef ) ) == NIL
         RETURN cRef + " /* Cyclic */"
      ELSE
         cRet := ""
      ENDIF

      hRefs[ pRef ] := cName

      cRet  += "{ => }" + CRLF

      nPad += 3
      cPad  := Space( nPad )

      FOR EACH aVar IN xVal
         cRef := cName + "[" + ValToPrg( aVar:__EnumKey() ) + "]"
         cRet += cPad + cRef + " := " + ValToPrg( aVar, cRef, nPad, hRefs )
         IF ! Right( cRet, Len( CRLF ) ) == CRLF
            cRet += CRLF
         ENDIF
      NEXT

      nPad -= 3
      RETURN cRet

   CASE "B"
      /* There is no support for codeblock serialization */
      RETURN "{|| /* block */ }"

   CASE "P"
      RETURN "0x" + hb_NumToHex( xVal )

   CASE "O"
      /* TODO: Use HBPersistent() when avialable! */
      pRef := __vmItemId( xVal )
      IF cName == NIL
         cName := "M->__ValToPrg_Object"
         nPad := 0
         hRefs := { => }
         cRet  := cName + " := "
      ELSEIF ! ( cRef := hb_HGetDef( hRefs, pRef ) ) == NIL
         RETURN cRef + " /* Cyclic */"
      ELSE
         cRet := ""
      ENDIF

      hRefs[ pRef ] := cName

      cRet += xVal:ClassName + "():New()" + CRLF

      nPad += 3
      cPad := Space( nPad )

      FOR EACH aVar IN __objGetValueList( xVal )
         cRef := cName + ":" + aVar[ 1 ]
         cRet += cPad + cRef + " := " + ValToPrg( aVar[ 2 ], cRef, nPad, hRefs )
         IF ! Right( cRet, Len( CRLF ) ) == CRLF
            cRet += CRLF
         ENDIF
      NEXT

      nPad -= 3
      RETURN cRet

   OTHERWISE
      // TraceLog( xVal, cName, nPad )
      IF xVal == NIL
         cRet := "NIL"
      ELSE
         Throw( xhb_ErrorNew( "VALTOPRG", 0, 3103, ProcName(), "Unsupported type: " + ValType( xVal ), { xVal } ) )
      ENDIF
   ENDSWITCH

   // TraceLog( cRet )

   RETURN cRet

//

FUNCTION PrgExpToVal( cExp )

   RETURN Eval( hb_macroBlock( cExp ) )

//

FUNCTION ValToArray( xVal )

   IF HB_ISARRAY( xVal )
      RETURN xVal
   ENDIF

   RETURN { xVal }

//

FUNCTION ValToBlock( xVal )

   IF HB_ISBLOCK( xVal )
      RETURN xVal
   ENDIF

   RETURN {|| xVal }

//

FUNCTION ValToCharacter( xVal )

   IF HB_ISSTRING( xVal )
      RETURN xVal
   ENDIF

   RETURN LTrim( CStr( xVal ) )

//

FUNCTION ValToDate( xVal )

   SWITCH ValType( xVal )
   CASE "A"
   CASE "H"
   CASE "L"
   CASE "O"
   CASE "U"
      EXIT

   CASE "B"
      RETURN ValToDate( Eval( xVal ) )

   CASE "C"
      RETURN iif( IsDigit( SubStr( xVal, 3, 1 ) ) .AND. ;
                  IsDigit( SubStr( xVal, 5, 1 ) ), hb_SToD( xVal ), ;
                                                   CToD( xVal ) )

   CASE "D"
      RETURN xVal

   CASE "T"
      RETURN hb_TToD( xVal )

   CASE "N"
      RETURN d"1900-01-01" + xVal

   CASE "P"
      RETURN d"1900-01-01" + hb_HexToNum( hb_NumToHex( xVal ) )

   OTHERWISE
      Throw( xhb_ErrorNew( "VALTODATE", 0, 3103, ProcName(), "Unsupported type", { xVal } ) )
   ENDSWITCH

   RETURN hb_SToD()

//

FUNCTION ValToTimeStamp( xVal )

   SWITCH ValType( xVal )
   CASE "A"
   CASE "H"
   CASE "L"
   CASE "O"
   CASE "U"
      EXIT

   CASE "B"
      RETURN ValToTimeStamp( Eval( xVal ) )

   CASE "C"
      RETURN iif( IsDigit( SubStr( xVal, 3, 1 ) ) .AND. ;
                  IsDigit( SubStr( xVal, 5, 1 ) ) .AND. ;
                  IsDigit( SubStr( xVal, 7, 1 ) ), hb_SToT( xVal ), ;
                                                   hb_StrToTS( xVal ) )

   CASE "D"
      RETURN hb_DToT( xVal )

   CASE "T"
      RETURN xVal

   CASE "N"
      RETURN t"1900-01-01" + xVal

   CASE "P"
      RETURN t"1900-01-01" + hb_HexToNum( hb_NumToHex( xVal ) )

   OTHERWISE
      Throw( xhb_ErrorNew( "VALTODATE", 0, 3103, ProcName(), "Unsupported type", { xVal } ) )
   ENDSWITCH

   RETURN hb_SToD()

//

FUNCTION ValToHash( xVal )

   IF HB_ISHASH( xVal )
      RETURN xVal
   ENDIF

   RETURN { ValToCharacter( xVal ) => xVal }

//

FUNCTION ValToLogical( xVal )

   SWITCH ValType( xVal )
   CASE "A"
   CASE "D"
   CASE "T"
   CASE "H"
   CASE "N"
   CASE "O"
   CASE "P"
      RETURN ! Empty( xVal )

   CASE "B"
      RETURN ValToLogical( Eval( xVal ) )

   CASE "C"
      IF Left( xVal, 1 ) == "." .AND. SubStr( xVal, 3, 1 ) == "." .AND. Upper( SubStr( xVal, 2, 1 ) ) $ "TFYN"
         RETURN Upper( SubStr( xVal, 2, 1 ) ) $ "TY"
      ELSEIF Len( xVal ) == 1 .AND. Upper( xVal ) $ "TFYN"
         RETURN Upper( xVal ) $ "TY"
      ELSE
         RETURN ! Empty( xVal )
      ENDIF
      EXIT

   CASE "L"
      RETURN xVal

   CASE "U"
      RETURN .F.

   OTHERWISE
      Throw( xhb_ErrorNew( "VALTOLOGICAL", 0, 3103, ProcName(), "Unsupported type", { xVal } ) )
   ENDSWITCH

   RETURN .F.

//

FUNCTION ValToNumber( xVal )

   SWITCH ValType( xVal )
   CASE "A"
   CASE "H"
      RETURN Len( xVal )

   CASE "B"
      RETURN ValToNumber( Eval( xVal ) )

   CASE "C"
      RETURN Val( xVal )

   CASE "D"
   CASE "T"
      RETURN xVal - 0d19000101

   CASE "L"
      RETURN iif( xVal, 1, 0 )

   CASE "O"
      RETURN xVal:hClass

   CASE "N"
      RETURN xVal

   CASE "P"
      RETURN xVal - 0

   CASE "U"
      RETURN 0

   OTHERWISE
      Throw( xhb_ErrorNew( "VALTONUMBER", 0, 3103, ProcName(), "Unsupported type", { xVal } ) )
   ENDSWITCH

   RETURN 0

//

FUNCTION ValToObject( xVal )

   SWITCH ValType( xVal )
   CASE "A"
      ENABLE TYPE CLASS ARRAY
      EXIT

   CASE "B"
      ENABLE TYPE CLASS BLOCK
      EXIT

   CASE "C"
      ENABLE TYPE CLASS CHARACTER
      EXIT

   CASE "D"
      ENABLE TYPE CLASS DATE
      EXIT

   CASE "T"
      ENABLE TYPE CLASS TIMESTAMP
      EXIT

   CASE "H"
      ENABLE TYPE CLASS HASH
      EXIT

   CASE "L"
      ENABLE TYPE CLASS LOGICAL
      EXIT

   CASE "N"
      ENABLE TYPE CLASS NUMERIC
      EXIT

   CASE "O"
      RETURN xVal

   CASE "P"
      ENABLE TYPE CLASS POINTER
      EXIT

   CASE "U"
      ENABLE TYPE CLASS NIL
      EXIT

   OTHERWISE
      Throw( xhb_ErrorNew( "VALTOOBJECT", 0, 3103, ProcName(), "Unsupported type", { xVal } ) )
   ENDSWITCH

   RETURN 0

//

FUNCTION ValToType( xVal, cType )

   SWITCH cType
   CASE "A"
      RETURN ValToArray( xVal )

   CASE "B"
      RETURN ValToBlock( xVal )

   CASE "C"
      RETURN ValToCharacter( xVal )

   CASE "D"
      RETURN ValToDate( xVal )

   CASE "T"
      RETURN ValToTimeStamp( xVal )

   CASE "H"
      RETURN ValToHash( xVal )

   CASE "L"
      RETURN ValToLogical( xVal )

   CASE "N"
      RETURN ValToNumber( xVal )

   CASE "O"
      RETURN ValToObject( xVal )

   CASE "P"
      RETURN ValToNumber( xVal )

   CASE "U"
      RETURN NIL

   OTHERWISE
      Throw( xhb_ErrorNew( "VALTOTYPE", 0, 3103, ProcName(), "Unsupported type", { xVal } ) )
   ENDSWITCH

   RETURN NIL
