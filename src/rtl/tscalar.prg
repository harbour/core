/*
 * Harbour implementation of Class(y) Scalar classes
 *
 * Copyright 2004 Antonio Linares <alinares@fivetechsoft.com>
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

/* Class(y) documentation is located at:
   https://harbour.github.io/ng/classy/menu.html */

#include "hbclass.ch"

/* --- */

CREATE CLASS ScalarObject FUNCTION HBScalar

   METHOD Copy()
   METHOD IsScalar()
   METHOD AsString()
   METHOD AsExpStr()

   MESSAGE Become    METHOD BecomeErr()  /* a scalar cannot "become" another object */
   MESSAGE DeepCopy  METHOD Copy()

ENDCLASS

METHOD Copy() CLASS ScalarObject
   RETURN Self

METHOD IsScalar() CLASS ScalarObject
   RETURN .T.

METHOD AsString() CLASS ScalarObject

   SWITCH ValType( Self )
   CASE "B" ; RETURN "{ || ... }"
   CASE "M"
   CASE "C" ; RETURN Self
   CASE "D" ; RETURN DToC( Self )
   CASE "T" ; RETURN hb_TToC( Self )
   CASE "H" ; RETURN "{ ... => ... }"
   CASE "L" ; RETURN iif( Self, ".T.", ".F." )
   CASE "N" ; RETURN hb_ntos( Self )
   CASE "S" ; RETURN "@" + ::name + "()"
   CASE "P" ; RETURN "<0x...>"
   CASE "U" ; RETURN "NIL"
   ENDSWITCH

   RETURN "Error!"

METHOD AsExpStr() CLASS ScalarObject

   SWITCH ValType( Self )
   CASE "M"
   CASE "C" ; RETURN '"' + Self + '"'
   CASE "D" ; RETURN 'CToD("' + DToC( Self ) + '")'
   CASE "T" ; RETURN 'hb_CToT("' + hb_TToC( Self ) + '")'
   ENDSWITCH

   RETURN ::AsString()

METHOD PROCEDURE BecomeErr() CLASS ScalarObject

#if 0
   // Not implemented yet
   ::error( CSYERR_BECOME, "Message 'become' illegally sent to scalar", ::ClassName() )
#endif

   RETURN

/* --- */

CREATE CLASS Array INHERIT HBScalar FUNCTION __HBArray

   METHOD Init( nElements )

   METHOD AsString()
   METHOD At( n )
   METHOD AtPut( n, x )
   METHOD Add( x )
   METHOD AddAll( aOtherCollection )
   METHOD Collect( b )
   METHOD Copy()
   METHOD Do( b )
   METHOD DeleteAt( n )
   METHOD InsertAt( n, x )
   METHOD IndexOf( x )
   METHOD IsScalar()
   METHOD Remove( e )
   METHOD Scan( b )
   METHOD _Size( newSize )  // assignment method

   MESSAGE Append  METHOD Add

ENDCLASS

METHOD Init( nElements ) CLASS Array

   ::size := iif( nElements == NIL, 0, nElements )

   RETURN Self

METHOD AddAll( aOtherCollection ) CLASS Array

   aOtherCollection:Do( {| e | ::Add( e ) } )

   RETURN Self

METHOD AsString() CLASS Array
   RETURN "{ ... }"

METHOD At( n ) CLASS Array
   RETURN Self[ n ]

METHOD AtPut( n, x ) CLASS Array
   RETURN Self[ n ] := x

METHOD Add( x ) CLASS Array

   AAdd( Self, x )

   RETURN .T.

METHOD Collect( b ) CLASS Array

   LOCAL elem
   LOCAL result := {}

   FOR EACH elem IN Self
      IF Eval( b, elem )
         AAdd( result, elem )
      ENDIF
   NEXT

   RETURN result

METHOD Copy() CLASS Array
   RETURN ACopy( Self, Array( Len( Self ) ) )

METHOD DeleteAt( n ) CLASS Array

   IF n >= 1 .AND. n <= Len( Self )
      hb_ADel( Self, n, .T. )
   ENDIF

   RETURN Self

METHOD InsertAt( n, x ) CLASS Array

   DO CASE
   CASE n > Len( Self )
      ASize( Self, n )
      Self[ n ] := x
   CASE n >= 1
      hb_AIns( Self, n, x, .T. )
   ENDCASE

   RETURN Self

METHOD IsScalar() CLASS Array
   RETURN .T.

METHOD Do( b ) CLASS Array

   LOCAL i

   FOR i := 1 TO Len( Self )
      b:Eval( Self[ i ], i )
   NEXT

   RETURN Self

METHOD IndexOf( x ) CLASS Array

   LOCAL elem

   FOR EACH elem IN Self
      IF elem == x
         RETURN elem:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD PROCEDURE Remove( e ) CLASS Array

   ::DeleteAt( ::IndexOf( e ) )

   RETURN

METHOD Scan( b ) CLASS Array
   RETURN AScan( Self, b )

METHOD _Size( newSize ) CLASS Array

   ASize( Self, newSize )

   RETURN newSize  // so that assignment works according to standard rules

/* --- */

CREATE CLASS Block INHERIT HBScalar FUNCTION __HBBlock

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Block
   RETURN "{ || ... }"

/* --- */

CREATE CLASS Character INHERIT HBScalar FUNCTION __HBCharacter

   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD AsString() CLASS Character
   RETURN Self

METHOD AsExpStr() CLASS Character
   RETURN '"' + Self + '"'

/* --- */

CREATE CLASS Date INHERIT HBScalar FUNCTION __HBDate

   METHOD Year()
   METHOD Month()
   METHOD Day()
   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD AsString() CLASS Date
   RETURN DToC( Self )

METHOD AsExpStr() CLASS Date
   RETURN 'CToD("' + ::AsString() + '")'

METHOD Year() CLASS Date
   RETURN Year( Self )

METHOD Month() CLASS Date
   RETURN Month( Self )

METHOD Day() CLASS Date
   RETURN Day( Self )

/* --- */

CREATE CLASS TimeStamp INHERIT HBScalar FUNCTION __HBTimeStamp

   METHOD Date()
   METHOD Time()
   METHOD Year()
   METHOD Month()
   METHOD Day()
   METHOD Hour()
   METHOD Minute()
   METHOD Sec()

   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD AsString() CLASS TimeStamp
   RETURN hb_TToS( Self )

METHOD AsExpStr() CLASS TimeStamp
   RETURN 'hb_SToT("' + ::AsString() + '")'

METHOD Date() CLASS TimeStamp
   RETURN hb_TToC( Self,, "" )

METHOD Time() CLASS TimeStamp
   RETURN hb_TToC( Self, "", "hh:mm:ss" )

METHOD Year() CLASS TimeStamp
   RETURN Year( Self )

METHOD Month() CLASS TimeStamp
   RETURN Month( Self )

METHOD Day() CLASS TimeStamp
   RETURN Day( Self )

METHOD Hour() CLASS TimeStamp
   RETURN hb_Hour( Self )

METHOD Minute() CLASS TimeStamp
   RETURN hb_Minute( Self )

METHOD Sec() CLASS TimeStamp
   RETURN hb_Sec( Self )

/* --- */

CREATE CLASS Hash INHERIT HBScalar FUNCTION __HBHash

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Hash
   RETURN "{ ... => ... }"

/* --- */

CREATE CLASS Logical INHERIT HBScalar FUNCTION __HBLogical

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Logical
   RETURN iif( Self, ".T.", ".F." )

/* --- */

CREATE CLASS NIL INHERIT HBScalar FUNCTION __HBNil

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS NIL
   RETURN "NIL"

/* --- */

CREATE CLASS Numeric INHERIT HBScalar FUNCTION __HBNumeric

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Numeric
   RETURN hb_ntos( Self )

/* --- */

CREATE CLASS Symbol INHERIT HBScalar FUNCTION __HBSymbol

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Symbol
   RETURN "@" + ::name + "()"

/* --- */

CREATE CLASS Pointer INHERIT HBScalar FUNCTION __HBPointer

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Pointer
   RETURN "<0x...>"
