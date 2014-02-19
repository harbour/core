/*
 * Harbour Project source code:
 * hb_ValToExp(), hb_CStr()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

FUNCTION hb_CStr( xVal )

   LOCAL v := ValType( xVal )

   SWITCH v
   CASE "C"
   CASE "M" ; RETURN xVal
   CASE "N" ; RETURN Str( xVal )
   CASE "D" ; RETURN iif( Empty( xVal ), "0d00000000", "0d" + DToS( xVal ) )
   CASE "T" ; RETURN 't"' + hb_TSToStr( xVal, .T. ) + '"'
   CASE "L" ; RETURN iif( xVal, ".T.", ".F." )
   CASE "S" ; RETURN "@" + xVal:name + "()"
   CASE "B" ; RETURN "{|| ... }"
   CASE "O" ; RETURN "{ " + xVal:className() + " Object }"
   CASE "A" ; RETURN "{ Array of " + hb_ntos( Len( xVal ) ) + " Items }"
   CASE "H" ; RETURN "{ Hash of " + hb_ntos( Len( xVal ) ) + " Items }"
   CASE "P" ; RETURN "<pointer>"
   OTHERWISE
      IF xVal == NIL
         RETURN "NIL"
      ENDIF
   ENDSWITCH

   RETURN "???:" + v

FUNCTION hb_ValToExp( xVal, lRaw )

   hb_default( @lRaw, .F. )

   RETURN s_valToExp( xVal, lRaw )

STATIC FUNCTION s_valToExp( xVal, lRaw, cInd, hRefs, cRefs, cObjs )

   LOCAL cVal, cKey, cClass
   LOCAL tmp
   LOCAL v := ValType( xVal )

   SWITCH v
   CASE "C"
   CASE "M" ; RETURN hb_StrToExp( xVal )
   CASE "N" ; RETURN hb_ntos( xVal )
   CASE "D" ; RETURN iif( Empty( xVal ), "0d00000000", "0d" + DToS( xVal ) )
   CASE "T" ; RETURN 't"' + hb_TSToStr( xVal, .T. ) + '"'
   CASE "L" ; RETURN iif( xVal, ".T.", ".F." )
   CASE "S" ; RETURN "@" + xVal:name + "()"
   CASE "O"
      cClass := xVal:className()
   CASE "A"
   CASE "H"
      tmp := __vmItemID( xVal )
      IF cInd == NIL
         cInd := cRefs := ""
         hRefs := { tmp => cInd }
      ELSEIF tmp $ hRefs
         IF !( cRefs == "" )
            cRefs += ","
         ENDIF
         cRefs += "{{" + cInd + "}," + hRefs[ tmp ] + "}"
         RETURN "NIL"
      ELSE
         hRefs[ tmp ] := "{" + cInd + "}"
         cInd += ","
      ENDIF

      IF v == "H"
         IF Empty( xVal )
            cVal := "{=>}"
         ELSE
            cVal := "{"
            FOR EACH tmp IN xVal
               cKey := s_valToExp( tmp:__enumKey(), lRaw )
               cVal += iif( tmp:__enumIsFirst(), "", ", " ) + ;
                  cKey + "=>" + ;
                  s_valToExp( tmp, lRaw, cInd + cKey, hRefs, @cRefs, @cObjs )
            NEXT
            cVal += "}"
         ENDIF
      ELSE
         cVal := "{"
         IF ! lRaw .AND. v == "O"
            FOR EACH tmp IN __objGetIVars( xVal )
               cVal += iif( tmp:__enumIsFirst(), '{"', ', {"' ) + ;
                       tmp[ 1 ] + '", ' + ;
                       s_valToExp( tmp[ 2 ], lRaw, ;
                                   cInd + hb_ntos( tmp:__enumIndex() ) + ",2", ;
                                   hRefs, @cRefs, @cObjs ) + "}"
            NEXT
         ELSE
            FOR EACH tmp IN xVal
               cVal += iif( tmp:__enumIsFirst(), "", ", " ) + ;
                  s_valToExp( tmp, lRaw, cInd + hb_ntos( tmp:__enumIndex() ), hRefs, @cRefs, @cObjs )
            NEXT
         ENDIF
         cVal += "}"
      ENDIF

      IF v == "O"
         IF cObjs == NIL
            cObjs := ""
         ELSE
            cObjs += ","
         ENDIF
         cObjs += '{"' + cClass + '",'
         IF ! cInd == ""
            cObjs += "{" + hb_StrShrink( cInd ) + "}"
         ENDIF
         cObjs += "}"
      ENDIF
      IF cInd == ""
         IF ! Empty( cRefs )
            cVal := "__itemSetRef( " + cVal + ", {" + cRefs + "} )"
         ENDIF
         IF ! Empty( cObjs )
            cVal := iif( lRaw, "__itemSetObjRaw( ", ;
                               "__itemSetObj( " ) + cVal + ", {" + cObjs + "} )"
         ENDIF
      ENDIF
      EXIT
   CASE "P" ; RETURN "<pointer>"
   CASE "B" ; RETURN "{|| ... }"
   OTHERWISE
      IF xVal == NIL
         cVal := "NIL"
      ELSE
         cVal := "???:" + v
      ENDIF
   ENDSWITCH

   RETURN cVal

FUNCTION __itemSetRef( xVal, aRefs )

   LOCAL aRef

   FOR EACH aRef in aRefs
      xVal[ hb_ArrayToParams( aRef[ 1 ] ) ] := ;
         iif( aRef[ 2 ] == NIL, xVal, xVal[ hb_ArrayToParams( aRef[ 2 ] ) ] )
   NEXT

   RETURN xVal

FUNCTION __itemSetObj( xVal, aObjs )

   LOCAL aRef

   FOR EACH aRef in aObjs
      __objRestoreIVars( iif( aRef[ 2 ] == NIL, xVal, ;
                              xVal[ hb_ArrayToParams( aRef[ 2 ] ) ] ), aRef[ 1 ] )
   NEXT

   RETURN xVal

FUNCTION __itemSetObjRaw( xVal, aObjs )

   LOCAL aRef

   FOR EACH aRef in aObjs
      __objSetClass( iif( aRef[ 2 ] == NIL, xVal, ;
                          xVal[ hb_ArrayToParams( aRef[ 2 ] ) ] ), aRef[ 1 ] )
   NEXT

   RETURN xVal
