/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Class HBPersistent
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

extern HB_STOD

CLASS HBPersistent

   METHOD CreateNew() INLINE Self

   METHOD LoadFromFile( cFileName ) INLINE ::LoadFromText( MemoRead( cFileName ) )

   METHOD LoadFromText( cObjectText )

   METHOD SaveToText( cObjectName )

   METHOD SaveToFile( cFileName ) INLINE MemoWrit( cFileName, ::SaveToText() )

ENDCLASS

METHOD LoadFromText( cObjectText ) CLASS HBPersistent

   local nLines := MLCount( cObjectText )
   local nLine  := 1, cLine, cToken
   local lStart := .t., aArray
   private oSelf

   if empty( cObjectText )
      return .F.
   endif

   while Empty( MemoLine( cObjectText,, nLine ) ) // We skip the first empty lines
      nLine++
   end

   while nLine <= nLines
      cLine  = MemoLine( cObjectText,, nLine )

      do case
         case Upper( LTrim( __StrToken( cLine, 1 ) ) ) == "OBJECT"
              if lStart
                 lStart = .f.
              else
              endif

         case Upper( LTrim( __StrToken( cLine, 1 ) ) ) == "ARRAY"
              if Left( cToken := LTrim( __StrToken( cLine, 2 ) ), 2 ) == "::"
                 __ObjSendMsg( Self, "_" + SubStr( cToken, 3 ), Array( Val( __StrToken( cLine, 4 ) ) ) )
              endif

         case Left( cToken := LTrim( __StrToken( cLine, 1, "=" ) ), 2 ) == "::"
              M->oSelf = Self
              cLine = StrTran( cLine, "::", "oSelf:" )
              cLine = StrTran( cLine, "=", ":=" )
              &( cLine )

      endcase

      nLine++
   end

return .T.

METHOD SaveToText( cObjectName ) CLASS HBPersistent

   local oNew := &( ::ClassName() + "()" ):CreateNew()
   local aProperties, n, uValue, cObject, cType

   static nIndent := -3

   DEFAULT cObjectName TO "o" + ::ClassName()

   nIndent += 3
   cObject := If( nIndent > 0, HB_OsNewLine(), "" ) + Space( nIndent ) + ;
              "OBJECT " + If( nIndent != 0, "::", "" ) + cObjectName + " AS " + ;
              ::ClassName() + HB_OsNewLine()

   aProperties = __ClsGetProperties( ::ClassH )

   for n = 1 to Len( aProperties )
      uValue = __objSendMsg( Self, aProperties[ n ] )

      if ! uValue == __objSendMsg( oNew, aProperties[ n ] )

         cType  = ValType( uValue )

         do case
            case cType == "A"
                 nIndent += 3
                 cObject += ArrayToText( uValue, aProperties[ n ], nIndent )
                 nIndent -= 3
                 if n < Len( aProperties )
                    cObject += HB_OsNewLine()
                 endif

            case cType == "O"
                 if __objDerivedFrom( uValue, "HBPERSISTENT" )
                    cObject += uValue:SaveToText( aProperties[ n ] )
                 endif
                 if n < Len( aProperties )
                    cObject += HB_OsNewLine()
                 endif

            otherwise
                 if n == 1
                    cObject += HB_OsNewLine()
                 endif
                 cObject += Space( nIndent ) + "   ::" + ;
                            aProperties[ n ] + " = " + ValToText( uValue ) + ;
                            HB_OsNewLine()
         endcase

      endif

   next

   cObject += HB_OsNewLine() + Space( nIndent ) + "ENDOBJECT" + HB_OsNewLine()
   nIndent -= 3

return cObject

static function ArrayToText( aArray, cName, nIndent )

   local cArray := HB_OsNewLine() + Space( nIndent ) + "ARRAY ::" + cName + ;
                   " LEN " + AllTrim( Str( Len( aArray ) ) ) + HB_OsNewLine()
   local n, uValue, cType

   for n = 1 to Len( aArray )
      uValue = aArray[ n ]
      cType  = ValType( uValue )

      do case
         case cType == "A"
              nIndent += 3
              cArray += ArrayToText( uValue, cName + "[ " + ;
                        AllTrim( Str( n ) ) + " ]", nIndent ) + HB_OsNewLine()
              nIndent -= 3

         case cType == "O"
              if __objDerivedFrom( uValue, "HBPERSISTENT" )
                 cArray += uValue:SaveToText( cName + "[ " + AllTrim( Str( n ) ) + ;
                           " ]" )
              endif

         otherwise
              if n == 1
                 cArray += HB_OsNewLine()
              endif
              cArray += Space( nIndent ) + "   ::" + cName + ;
                        + "[ " + AllTrim( Str( n ) ) + " ]" + " = " + ;
                        ValToText( uValue ) + HB_OsNewLine()
      endcase
   next

   cArray += HB_OsNewLine() + Space( nIndent ) + "ENDARRAY" + HB_OsNewLine()

return cArray

static function ValToText( uValue )

   local cType := ValType( uValue )
   local cText

   do case
      case cType == "C"
           cText = '"' + uValue + '"'

      case cType == "N"
           cText = AllTrim( Str( uValue ) )

      case cType == "D"
           cText = 'HB_STOD( "' + DToS( uValue ) + '" )'

      otherwise
           cText = HB_ValToStr( uValue )
   endcase

return cText
