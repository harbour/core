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

CREATE CLASS HBPersistent

   METHOD CreateNew() INLINE Self
   METHOD LoadFromFile( cFileName ) INLINE ::LoadFromText( MemoRead( cFileName ) )
   METHOD LoadFromText( cObjectText )
   METHOD SaveToText( cObjectName, nIndent )
   METHOD SaveToFile( cFileName ) INLINE MemoWrit( cFileName, ::SaveToText() )

ENDCLASS

METHOD LoadFromText( cObjectText ) CLASS HBPersistent

   local nFrom  := 1, cLine, cToken
   local lStart := .t.
   private oSelf

   if empty( cObjectText )
      return .F.
   endif

   do while Empty( ExtractLine( cObjectText, @nFrom ) ) // We skip the first empty lines
   enddo

   do while nFrom <= Len( cObjectText )
      cLine  := ExtractLine( cObjectText, @nFrom )

      do case
         case Upper( LTrim( hb_TokenGet( cLine, 1 ) ) ) == "OBJECT"
              if lStart
                 lStart := .f.
              else
              endif

         case Upper( LTrim( hb_TokenGet( cLine, 1 ) ) ) == "ARRAY"
              cLine := SubStr( cLine, At( "::", cLine ) )
              M->oSelf := Self
              cLine := StrTran( cLine, "::", "oSelf:" )
              cLine := StrTran( cLine, " LEN ", " = Array( " )
              cLine := RTrim( StrTran( cLine, "=", ":=", , 1 ) ) + " )"
              cLine := &( cLine )

         case Left( cToken := LTrim( hb_TokenGet( cLine, 1, "=" ) ), 2 ) == "::"
              M->oSelf := Self
              cLine := StrTran( cLine, "::", "oSelf:" )
              cLine := StrTran( cLine, "=", ":=", , 1 )
              cLine := &( cLine )

      endcase

   enddo

return .T.

METHOD SaveToText( cObjectName, nIndent ) CLASS HBPersistent

   local oNew := &( ::ClassName() + "()" ):CreateNew()
   local aProperties, n, uValue, uNewValue, cObject, cType

   DEFAULT cObjectName TO "o" + ::ClassName()

   if nIndent == NIL
      nIndent := 0
   else
      nIndent += 3
   endif

   cObject := iif( nIndent > 0, hb_OSNewLine(), "" ) + Space( nIndent ) + ;
              "OBJECT " + iif( nIndent != 0, "::", "" ) + cObjectName + " AS " + ;
              ::ClassName() + hb_OSNewLine()

   aProperties := __ClsGetProperties( ::ClassH )

   for n := 1 to Len( aProperties )
      uValue := __objSendMsg( Self, aProperties[ n ] )
      uNewValue := __objSendMsg( oNew, aProperties[ n ] )
      cType  := ValType( uValue )

      if cType != ValType( uNewValue ) .OR. ! uValue == uNewValue

         do case
            case cType == "A"
                 nIndent += 3
                 cObject += ArrayToText( uValue, aProperties[ n ], nIndent )
                 nIndent -= 3
                 if n < Len( aProperties )
                    cObject += hb_OSNewLine()
                 endif

            case cType == "O"
                 if __objDerivedFrom( uValue, "HBPERSISTENT" )
                    cObject += uValue:SaveToText( aProperties[ n ], nIndent )
                 endif
                 if n < Len( aProperties )
                    cObject += hb_OSNewLine()
                 endif

            otherwise
                 if n == 1
                    cObject += hb_OSNewLine()
                 endif
                 cObject += Space( nIndent ) + "   ::" + ;
                            aProperties[ n ] + " = " + ValToText( uValue ) + ;
                            hb_OSNewLine()
         endcase

      endif

   next

   cObject += hb_OSNewLine() + Space( nIndent ) + "ENDOBJECT" + hb_OSNewLine()

return cObject

static function ArrayToText( aArray, cName, nIndent )

   local cArray := hb_OSNewLine() + Space( nIndent ) + "ARRAY ::" + cName + ;
                   " LEN " + AllTrim( Str( Len( aArray ) ) ) + hb_OSNewLine()
   local n, uValue, cType

   for n := 1 to Len( aArray )
      uValue := aArray[ n ]
      cType  := ValType( uValue )

      do case
         case cType == "A"
              nIndent += 3
              cArray += ArrayToText( uValue, cName + "[ " + ;
                        AllTrim( Str( n ) ) + " ]", nIndent ) + hb_OSNewLine()
              nIndent -= 3

         case cType == "O"
              if __objDerivedFrom( uValue, "HBPERSISTENT" )
                 cArray += uValue:SaveToText( cName + "[ " + AllTrim( Str( n ) ) + ;
                                              " ]", nIndent )
              endif

         otherwise
              if n == 1
                 cArray += hb_OSNewLine()
              endif
              cArray += Space( nIndent ) + "   ::" + cName + ;
                        + "[ " + AllTrim( Str( n ) ) + " ]" + " = " + ;
                        ValToText( uValue ) + hb_OSNewLine()
      endcase
   next

   cArray += hb_OSNewLine() + Space( nIndent ) + "ENDARRAY" + hb_OSNewLine()

return cArray

static function ValToText( uValue )

   local cType := ValType( uValue )
   local cText

   do case
      case cType == "C"
           cText := hb_StrToExp( uValue )

      case cType == "N"
           cText := AllTrim( Str( uValue ) )

      case cType == "D"
           cText := DToS( uValue )
           cText := "0d" + iif( Empty( cText ), "00000000", cText )

      otherwise
           cText := hb_ValToStr( uValue )
   endcase

return cText

// Notice: nFrom must be supplied by reference

static function ExtractLine( cText, nFrom )

   local nAt := hb_At( Chr( 10 ), cText, nFrom )
  
   if nAt > 0
      cText := SubStr( cText, nFrom, nAt - nFrom )
      if Right( cText, 1 ) == Chr( 13 )
         cText := hb_StrShrink( cText, 1 )
      endif
      nFrom := nAt + 1
   else
      cText := SubStr( cText, nFrom )
      if Right( cText, 1 ) == Chr( 13 )
         cText := hb_StrShrink( cText, 1 )
      endif
      nFrom += Len( cText ) + 1
   endif

return cText
