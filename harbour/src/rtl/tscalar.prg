/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour implementation of Class(y) Scalar classes
 *
 * Copyright 2004 Antonio Linares <alinares@fivetechsoft.com>
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

/* Class(y) documentation is located at:
   http://www.clipx.net/ng/classy/ngdebc.php */

#include "hbclass.ch"

/* -------------------------------------------- */

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
   CASE "T" ; RETURN HB_TToC( Self )
   CASE "H" ; RETURN "{ ... => ... }"
   CASE "L" ; RETURN iif( Self, ".T.", ".F." )
   CASE "N" ; RETURN hb_ntos( Self )
   CASE "S" ; RETURN "@" + Self:name + "()"
   CASE "P" ; RETURN "<0x...>"
   CASE "U" ; RETURN "NIL"
   ENDSWITCH

   RETURN "Error!"

METHOD AsExpStr() CLASS ScalarObject

   SWITCH ValType( Self )
   CASE "M"
   CASE "C" ; RETURN '"' + Self + '"'
   CASE "D" ; RETURN 'CToD("' + DToC( Self ) + '")'
   CASE "T" ; RETURN 'HB_CToT("' + HB_TToC( Self ) + '")'
   ENDSWITCH

   RETURN ::AsString()

METHOD BecomeErr() CLASS ScalarObject
   // Not implemented yet
   // ::error( CSYERR_BECOME, "Message 'become' illegally sent to scalar", ::ClassName() )
   RETURN NIL

/* -------------------------------------------- */

CREATE CLASS Array INHERIT HBScalar FUNCTION __HBArray

   METHOD Init

   METHOD AsString
   METHOD At
   METHOD AtPut
   METHOD Add
   METHOD AddAll
   METHOD Collect
   METHOD Copy
   METHOD Do
   METHOD DeleteAt
   METHOD InsertAt
   METHOD IndexOf
   METHOD IsScalar
   METHOD Remove
   METHOD Scan
   METHOD _Size                          // assignment method

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

   LOCAL i
   LOCAL currElem
   LOCAL result := {}
   LOCAL nElems := Len( Self )

   FOR i := 1 to nElems
      currElem := Self[ i ]
      IF Eval( b, currElem )
         AAdd( result, currElem )
      ENDIF
   NEXT

   RETURN result

METHOD Copy() CLASS Array
   RETURN ACopy( Self, Array( Len( Self ) ) )

METHOD DeleteAt( n ) CLASS Array

   IF n > 0 .AND. n <= Len( Self )
      hb_ADel( Self, n, .T. )
   ENDIF

   RETURN Self

METHOD InsertAt( n, x ) CLASS Array

   IF n > Len( Self )
      ASize( Self, n )
      Self[ n ] := x
   ELSEIF n > 0
      hb_AIns( Self, n, x, .T. )
   ENDIF

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

METHOD Remove( e ) CLASS Array

   ::DeleteAt( ::IndexOf( e ) )

   RETURN NIL

METHOD Scan( b ) CLASS Array
   RETURN AScan( Self, b )

METHOD _Size( newSize ) CLASS Array

   ASize( Self, newSize )

   RETURN newSize  // so that assignment works according to standard rules

/* -------------------------------------------- */

CREATE CLASS Block INHERIT HBScalar FUNCTION __HBBlock

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Block
   RETURN "{ || ... }"

/* -------------------------------------------- */

CREATE CLASS Character INHERIT HBScalar FUNCTION __HBCharacter

   METHOD AsString()
   METHOD AsExpStr()

ENDCLASS

METHOD AsString() CLASS Character
   RETURN Self

METHOD AsExpStr() CLASS Character
   RETURN '"' + Self + '"'

/* -------------------------------------------- */

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

/* -------------------------------------------- */

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
   RETURN HB_TTOS( Self )

METHOD AsExpStr() CLASS TimeStamp
   RETURN 'HB_STOT("' + ::AsString() + '")'

METHOD Date() CLASS TimeStamp
   RETURN HB_TTOC( Self, NIL, "" )

METHOD Time() CLASS TimeStamp
   RETURN HB_TTOC( Self, "", "HH:MM:SS" )

METHOD Year() CLASS TimeStamp
   RETURN Year( Self )

METHOD Month() CLASS TimeStamp
   RETURN Month( Self )

METHOD Day() CLASS TimeStamp
   RETURN Day( Self )

METHOD Hour() CLASS TimeStamp
   RETURN HB_HOUR( Self )

METHOD Minute() CLASS TimeStamp
   RETURN HB_MINUTE( Self )

METHOD Sec() CLASS TimeStamp
   RETURN HB_SEC( Self )

/* -------------------------------------------- */

CREATE CLASS Hash INHERIT HBScalar FUNCTION __HBHash

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Hash
   RETURN "{ ... => ... }"

/* -------------------------------------------- */

CREATE CLASS Logical INHERIT HBScalar FUNCTION __HBLogical

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Logical
   RETURN iif( Self, ".T.", ".F." )

/* -------------------------------------------- */

CREATE CLASS Nil INHERIT HBScalar FUNCTION __HBNil

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Nil
   RETURN "NIL"

/* -------------------------------------------- */

CREATE CLASS Numeric INHERIT HBScalar FUNCTION __HBNumeric

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Numeric
   RETURN hb_ntos( Self )

/* -------------------------------------------- */

CREATE CLASS Symbol INHERIT HBScalar FUNCTION __HBSymbol

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Symbol
   RETURN "@" + Self:name + "()"

/* -------------------------------------------- */

CREATE CLASS Pointer INHERIT HBScalar FUNCTION __HBPointer

   METHOD AsString()

ENDCLASS

METHOD AsString() CLASS Pointer
   RETURN "<0x...>"

/* -------------------------------------------- */
