/*
 * $Id$
 */

//
// Sample class Money with overloading of arythmetical operations
//
// Written by Pavel Tsarenko <tpe2 at mail.ru>
// www - http://harbour-project.org
//

#include "error.ch"
#include "hbclass.ch"

PROCEDURE Main()

   LOCAL m1 := Money():new( 12.2 )
   LOCAL m2 := Money():new( 7.8 )
   LOCAL m3 := m1 - m2

   ? "(12.2 - 7.8) == 4.4", ( 12.2 - 7.8 ) == 4.4
   ? m1:value
   ? m2:value
   ? ( m1 - m2 ) == 4.4
   ? m3 == 4.4
   ? m1:Str()
   ? m3:value
   ? m3 == 4.4
   m3 := 5.5
   ? ( m3 + m1 ):value
   ? ( m3 + 12.2 ):value
   ? ( m3 * 2 ):value
   ? ( m3 / 2 ):value
   ? ( m3 * m1 ):value

   RETURN

CREATE CLASS Money

   VAR nValue AS INTEGER INIT 0
   VAR nDec AS INTEGER INIT 2
   VAR nMul AS INTEGER INIT 100

   PROTECTED:

   METHOD normalize( xArg )
   METHOD Set( nValue ) INLINE ::nValue := Int( nValue * ::nMul )

   EXPORTED:

   METHOD new( nValue, nDec ) CONSTRUCTOR
   METHOD value()
   METHOD Str( nLen, nDec )
   METHOD getMoney( oMoney )

   OPERATOR ":=" ARG xArg INLINE ( ::nValue := ::normalize( xArg ), Self )
   OPERATOR "="  ARG xArg INLINE ( ::nValue =  ::normalize( xArg ) )
   OPERATOR "==" ARG xArg INLINE ( ::nValue == ::normalize( xArg ) )
   OPERATOR "!=" ARG xArg INLINE ( ::nValue != ::normalize( xArg ) )
   OPERATOR "<"  ARG xArg INLINE ( ::nValue <  ::normalize( xArg ) )
   OPERATOR "<=" ARG xArg INLINE ( ::nValue <= ::normalize( xArg ) )
   OPERATOR ">"  ARG xArg INLINE ( ::nValue >  ::normalize( xArg ) )
   OPERATOR ">=" ARG xArg INLINE ( ::nValue >= ::normalize( xArg ) )
   METHOD Equal( xArg )    OPERATOR "=="
   METHOD Plus( xArg )     OPERATOR "+"
   METHOD Minus( xArg )    OPERATOR "-"
   METHOD Multiple( xArg ) OPERATOR "*"
   METHOD Divide( xArg )   OPERATOR "/"

ENDCLASS

METHOD new( nValue, nDec ) CLASS Money

   hb_default( @nDec, 2 )
   hb_default( @nValue, 0 )

   ::nDec := nDec
   ::nMul := Int( 10 ^ nDec )
   ::Set( nValue )

   RETURN Self

METHOD value( ) CLASS Money

   RETURN ::nValue / ::nMul

METHOD getMoney( oMoney ) CLASS Money

   LOCAL nValue

   IF ::nDec == oMoney:nDec
      nValue := oMoney:nValue
   ELSE
      nValue := Int( oMoney:nValue * ( ::nMul / oMoney:nMul ) )
   ENDIF

   RETURN nValue

METHOD normalize( xArg ) CLASS Money

   LOCAL nValue

   IF IsMoney( xArg )
      nValue := ::getMoney( xArg )
   ELSEIF HB_ISNUMERIC( xArg )
      nValue := Int( xArg * ::nMul )
   ELSE
      nValue := Eval( ErrorBlock(), GenError( xArg ) )
   ENDIF

   RETURN nValue

METHOD Equal( xArg ) CLASS Money

   RETURN ::nValue == ::normalize( xArg )

METHOD Plus( xArg ) CLASS Money

   LOCAL oResult := Money():new( ::nDec )

   oResult:nValue := ::nValue + ::normalize( xArg )

   RETURN oResult

METHOD Minus( xArg ) CLASS Money

   LOCAL oResult := Money():new( ::nDec )

   oResult:nValue := ::nValue - ::normalize( xArg )

   RETURN oResult

METHOD Multiple( xArg ) CLASS Money

   LOCAL oResult := Money():new( ::nDec )

   IF IsMoney( xArg )
      oResult:nValue := Int( ::nValue * xArg:nValue / xArg:nMul )
   ELSEIF HB_ISNUMERIC( xArg )
      oResult:nValue := Int( ::nValue * xArg )
   ELSE
      Eval( ErrorBlock(), GenError( xArg ) )
   ENDIF

   RETURN oResult

METHOD Divide( xArg ) CLASS Money

   LOCAL oResult := Money():new( ::nDec )

   IF IsMoney( xArg )
      oResult:nValue := Int( ::nValue / xArg:nValue * xArg:nMul )
   ELSEIF HB_ISNUMERIC( xArg )
      oResult:nValue := Int( ::nValue / xArg )
   ELSE
      Eval( ErrorBlock(), GenError( xArg ) )
   ENDIF

   RETURN oResult

METHOD Str( nLen, nDec ) CLASS Money

   LOCAL cStr
   LOCAL nValue := ::value()

   IF nLen == NIL
      cStr := Str( nValue )
   ELSEIF nDec == NIL
      cStr := Str( nValue, nLen )
   ELSE
      cStr := Str( nValue, nLen, nDec )
   ENDIF

   RETURN cStr

STATIC FUNCTION IsMoney( xArg )

   RETURN HB_ISOBJECT( xArg ) .AND. xArg:className() = "MONEY"

STATIC FUNCTION GenError( xArg )

   LOCAL oError := ErrorNew()

   oError:description := hb_langErrMsg( EG_DATATYPE )
   oError:gencode := EG_DATATYPE
   oError:severity := ES_ERROR
   oError:cansubstitute := .T.
   oError:subsystem := "MONEY"
   oError:subcode := 0
   oError:args := { xArg }

   RETURN oError
