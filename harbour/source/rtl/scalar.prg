/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour implementation of Class(y) Class ScalarObject
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

// Class(y) documentation is located at:
// http://www.clipx.net/ng/classy/ngdebc.php

#include "hbclass.ch"

CREATE CLASS ScalarObject

   MESSAGE Become    METHOD BecomeErr()  // a scalar cannot "become" another object
   METHOD  Copy()
   MESSAGE DeepCopy  METHOD Copy()
   METHOD  IsScalar()
   METHOD  AsString()
   METHOD  AsExpStr()

ENDCLASS

METHOD Copy() CLASS ScalarObject

return Self

METHOD IsScalar() CLASS ScalarObject

return .t.

METHOD AsString() CLASS ScalarObject

   local cType := ValType( Self )

   do case
      case cType == "B"
           return "{ || ... }"

      case cType == "C"
           return Self

      case cType == "D"
           return DToC( Self )

      case cType == "L"
           return iif( Self, ".T.", ".F." )

      case cType == "N"
           return LTrim( Str( Self ) )

      case cType == "U"
           return "NIL"
   endcase

return "Error!"

METHOD AsExpStr() CLASS ScalarObject

   local cType := ValType( Self )

   if cType == "C"
      return ["] + Self + ["]
   elseif cType == "D"
      return [CToD("] + DToC( Self ) + [")]
   endif

return ::AsString()

METHOD BecomeErr() CLASS ScalarObject
    // Not implemented yet
    // ::error( CSYERR_BECOME, "Message 'become' illegally sent to scalar", ::ClassName() )
return NIL
