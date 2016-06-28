/*
 * hb_sprintf() function
 *
 * Copyright 2003 Mauricio Abre <maurifull@datafull.com>
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

FUNCTION sprintf( ... )

   LOCAL nLen := 0, lUnsigned, l0 := .F., lSign := .F., nDec, xVal, nPos, cTok

   LOCAL aPar    := hb_AParams()
   LOCAL cReturn := ""
   LOCAL cString := iif( Len( aPar ) >= 1, aPar[ 1 ], "" )
   LOCAL nPar    := 2

   DO WHILE ! Empty( cString )

      nPos := Len( cString ) + 1
      cTok := NIL

      IF "%" $ cString
         nPos := At( "%", cString )
         cTok := "%"
      ENDIF
      IF "\" $ cString .AND. At( "\", cString ) < nPos
         nPos := At( "\", cString )
         cTok := "\"
      ENDIF

      cReturn += Left( cString, nPos - 1 )

      DO CASE
      CASE cTok == NIL
         EXIT

      CASE cTok == "\"

         SWITCH SubStr( cString, ++nPos, 1 )
         CASE "t"
            cReturn += "    "
            EXIT

         CASE "n"
            cReturn += Chr( 13 )
            EXIT

         CASE "r"
            cReturn += Chr( 10 )
            EXIT

         OTHERWISE
            cReturn += SubStr( cString, nPos, 1 )
            EXIT
         ENDSWITCH

      CASE cTok == "%"
         lUnsigned := .F.

         SWITCH SubStr( cString, ++nPos, 1 )
         CASE "%"
            cReturn += "%"
            EXIT

         CASE "+"
            cString := Stuff( cString, nPos, 1, "" )
            nPos    := At( "%", cString ) - 1
            lSign   := .T.
            EXIT

         CASE "0"
            cString := Stuff( cString, nPos, 1, "" )
            nPos    := At( "%", cString ) - 1
            l0      := .T.
            EXIT

         CASE "1"
         CASE "2"
         CASE "3"
         CASE "4"
         CASE "5"
         CASE "6"
         CASE "7"
         CASE "8"
         CASE "9"
            nLen    := Val( SubStr( cString, nPos ) )
            cTok    := Left( cString, nPos - 1 )
            DO WHILE SubStr( cString, nPos, 1 ) $ "1234567890."
               nPos++
            ENDDO
            cString := cTok + SubStr( cString, nPos )
            nPos    := At( "%", cString ) - 1
            EXIT

         CASE "u"
            lUnsigned := .T.
            nPos++

         CASE "d"
         CASE "l"
         CASE "f"
         CASE "i"
            xVal := aPar[ nPar++ ]
            hb_default( @xVal, 0 )
            IF nLen != 0
               IF nLen - Int( nLen ) > 0.0
                  nDec := hb_ntos( nLen )
                  DO WHILE Right( nDec, 1 ) == "0"
                     nDec := hb_StrShrink( nDec )
                  ENDDO
                  nDec := Val( SubStr( nDec, At( ".", nDec ) + 1 ) )
               ELSE
                  nDec := 0
               ENDIF
               cTok := Str( iif( lUnsigned, Abs( xVal ), xVal ), nLen, nDec )
            ELSE
               cTok := hb_ntos( iif( lUnsigned, Abs( xVal ), xVal ) )
            ENDIF
            IF l0
               IF "-" $ cTok .AND. ! hb_LeftEq( cTok, "-" )
                  cTok := StrTran( cTok, "-", " " )
                  cTok := "-" + SubStr( cTok, 2 )
               ENDIF
               cTok := StrTran( cTok, " ", "0" )
               l0   := .F.
            ENDIF
            IF lSign .AND. ! hb_LeftEq( cTok, "-" )
               IF nLen == 0
                  cTok := "+" + cTok
               ELSE
                  cTok := "+" + SubStr( cTok, 2 )
               ENDIF
               lSign := .F.
            ENDIF
            nLen := 0
            cReturn += cTok
            EXIT

         CASE "c"
         CASE "s"
            IF nLen == 0
               nLen := Len( hb_CStr( aPar[ nPar ] ) )
            ENDIF
            cReturn += PadL( hb_CStr( aPar[ nPar++ ] ), nLen )
            nLen  := 0
            l0    := .F.
            lSign := .F.
            EXIT

         ENDSWITCH
      ENDCASE

      cString := SubStr( cString, nPos + 1 )
   ENDDO

   RETURN cReturn
