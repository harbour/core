/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Class TPersistent
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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
#include "common.ch"

CLASS TPersistent

   DATA   cName

   METHOD New()

   METHOD SaveToText( cObjectName )

   METHOD SaveToFile( cFileName ) INLINE MemoWrit( cFileName, ::SaveToText() )

ENDCLASS

METHOD New() CLASS TPersistent

   ::cName = "o" + ::ClassName()

return Self

METHOD SaveToText( cObjectName ) CLASS TPersistent

   local oNew := &( ::ClassName() + "()" ):New()
   local aProperties, n, uValue, cObject

   static nIndent := 0

   DEFAULT cObjectName TO ::cName

   cObject := Space( nIndent ) + "OBJECT " + If( nIndent != 0, "::", "" ) + ;
              cObjectName + " AS " + ::ClassName() + HB_OsNewLine() + HB_OsNewLine()

   aProperties = __ClsGetProperties( ::ClassH )

   for n = 1 to Len( aProperties )
      uValue = __objSendMsg( Self, aProperties[ n ] )
      if ValType( uValue ) == "O"
         if __objDerivedFrom( uValue, "TPERSISTENT" )
            nIndent += 3
            cObject += HB_OsNewLine() + uValue:SaveToText( aProperties[ n ] )
            nIndent -= 3
         endif
      else
         cObject += Space( nIndent ) + "   ::" + aProperties[ n ] + " = " + ;
                    If( ValType( uValue ) == "C", '"', "" ) + ;
                    HB_ValToStr( uValue ) + ;
                    If( ValType( uValue ) == "C", '"', "" )
      endif
      cObject += HB_OsNewLine()
   next

   cObject += HB_OsNewLine() + Space( nIndent ) + "ENDOBJECT" + HB_OsNewLine()

return cObject