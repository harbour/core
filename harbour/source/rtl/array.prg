/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour implementation of Class(y) Class Array
 *
 * Copyright 2004 Antonio Linares <alinares@fivetechsoft.com>
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

#include "hbclass.ch"

CREATE CLASS Array INHERIT ScalarObject FUNCTION HBArray

   METHOD  Init()

   METHOD  AsString()
   MESSAGE At         METHOD AtIndex      // 'at' is a reserved word
   METHOD  AtPut()
   MESSAGE Add        METHOD Append
   METHOD  AddAll()
   METHOD  Append()
   METHOD  Collect()
   METHOD  Copy()
   METHOD  Do()
   METHOD  DeleteAt()
   METHOD  InsertAt()
   METHOD  IndexOf()
   METHOD  IsScalar()
   METHOD  Remove()
   METHOD  Scan()
   METHOD  _Size                          // assignment method

ENDCLASS

METHOD Init( nElements ) CLASS Array

   ::size := iif( nElements == NIL, 0, nElements )

return Self

METHOD AddAll( aOtherCollection ) CLASS Array

    aOtherCollection:Do( {| e | ::Add( e ) } )

return Self

METHOD AsString() CLASS Array

return "{ ... }"

METHOD AtIndex( n ) CLASS Array

return Self[ n ]

METHOD AtPut( n, x ) CLASS Array

return Self[ n ] := x

METHOD Append( x ) CLASS Array

   AAdd( Self, x )

return .t.

METHOD Collect( b ) CLASS Array

   local i, currElem
   local result[ 0 ]
   local nElems := Len( Self )

   for i := 1 to nElems
      currElem := Self[ i ]
      if Eval( b, currElem )
         AAdd( result, currElem )
      endif
   next

return result

METHOD Copy() CLASS Array

return ACopy( Self, Array( Len( Self ) ) )

METHOD DeleteAt( n ) CLASS Array

   if n > 0 .and. n <= Len( Self )
      ADel( Self, n )
      ASize( Self, Len( Self ) - 1 )
   endif

return Self

METHOD InsertAt( n, x ) CLASS Array

   if n > Len( Self )
      ASize( Self, n )
      Self[ n ] := x
   elseif n > 0
      ASize( Self, Len( Self ) + 1 )
      AIns( Self, n )
      Self[ n ] := x
   endif

return Self

METHOD IsScalar() CLASS Array

return .t.

METHOD Do( b ) CLASS Array

   local i

   for i := 1 to Len( Self )
      b:Eval( Self[ i ], i )
   next

return Self

METHOD IndexOf( x ) CLASS Array

   local i
   local nElems := Len( Self )

   for i := 1 to nElems
      if Self[ i ] == x
         return i
      endif
   next

return 0

METHOD Remove( e ) CLASS Array

   ::DeleteAt( ::IndexOf( e ) )

return NIL

METHOD Scan( b ) CLASS Array

return AScan( Self, b )

METHOD _Size( newSize ) CLASS Array

   ASize( Self, newSize )

return newSize  // so that assignment works according to standard rules
