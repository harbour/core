/*
 * Dumpvar function to display var contents
 *
 * Copyright 2003 Francesco Saverio Giudice <info@fsgiudice.com>
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

#include "hbclass.ch"

/* Send to hb_OutDebug() more parameters */
PROCEDURE __OutDebug( ... )

   LOCAL xVal

   FOR EACH xVal IN hb_AParams()
      hb_OutDebug( hb_DumpVar( xVal ) )
   NEXT

   RETURN

/* Return a string containing a dump of a variable
 *
 * 2006-09-24
 * - Added recursion limit
 * - Added front function with limited parameters and removed support for TAssociative Array
 */
FUNCTION hb_DumpVar( xVar, lRecursive, nMaxRecursionLevel )

   // TraceLog( "HB_DumpVariable: xVar, lAssocAsObj, lRecursive", xVar, lAssocAsObj, lRecursive )

   RETURN __HB_DumpVar( xVar,, lRecursive, 0, 1, hb_defaultValue( nMaxRecursionLevel, 0 ) )

STATIC FUNCTION __HB_DumpVar( xVar, lAssocAsObj, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )

   LOCAL cString := "", cKey
   LOCAL nEolLen

   hb_default( @lAssocAsObj, .F. )
   hb_default( @lRecursive, .F. )

   // TraceLog( "Recursion: xVar, nRecursionLevel, nMaxRecursionLevel", xVar, nRecursionLevel, nMaxRecursionLevel )

   // return if there is limit in recursion
   IF nMaxRecursionLevel > 0 .AND. nRecursionLevel > nMaxRecursionLevel
      RETURN AsString( xVar )
   ENDIF

   SWITCH ValType( xVar )
   CASE "O"

      IF ! lAssocAsObj .AND. xVar:className() == "TASSOCIATIVEARRAY"
         cString += Space( nIndent ) + "Type='Associative' -> " + hb_eol()
         // Keys extraction.
         IF Len( xVar:Keys ) > 0
            nEolLen := Len( hb_eol() )
            cString += Space( nIndent ) + "{" + hb_eol()
            FOR EACH cKey IN xVar:Keys
               cString += Space( nIndent ) + " '" + cKey + "' => " + asString( xVar:SendKey( cKey ) ) + ", " + hb_eol()
               IF lRecursive .AND. ValType( xVar:SendKey( cKey ) ) $ "AOH"
                  cString := hb_StrShrink( cString, nEolLen + 2 ) + hb_eol()
                  cString += __HB_DumpVar( xVar:SendKey( cKey ), , lRecursive, nIndent + 3, nRecursionLevel + 1, nMaxRecursionLevel )
                  cString := hb_StrShrink( cString, nEolLen ) + ", " + hb_eol()
               ENDIF
            NEXT
            cString := hb_StrShrink( cString, nEolLen + 2 ) + hb_eol()
            cString += Space( nIndent ) + "}" + hb_eol()
         ENDIF
      ELSE
         cString += Space( nIndent ) + "<" + xVar:className() + " Object>" + hb_eol()
         cString += Space( nIndent ) + " | " + hb_eol()
         cString += Space( nIndent ) + " +- PRIVATE/HIDDEN:" + hb_eol()
         cString += DShowProperties( xVar, HB_OO_CLSTP_HIDDEN, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )
         cString += Space( nIndent ) + " +- PROTECTED:" + hb_eol()
         cString += DShowProperties( xVar, HB_OO_CLSTP_PROTECTED, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )
         cString += Space( nIndent ) + " +- EXPORTED/VISIBLE/PUBLIC:" + hb_eol()
         cString += DShowProperties( xVar, HB_OO_CLSTP_EXPORTED, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )
#ifdef __XHARBOUR__
         cString += Space( nIndent ) + " +- PUBLISHED:" + hb_eol()
         cString += DShowProperties( xVar, HB_OO_CLSTP_PUBLISHED, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )
#endif
         cString += Space( nIndent ) + " +----------->" + hb_eol()
      ENDIF
      EXIT

   CASE "A"
      IF nRecursionLevel == 1
         cString += Space( nIndent ) + "Type='A' -> { Array of " + hb_ntos( Len( xVar ) ) + " Items }" + hb_eol()
      ENDIF
      IF nMaxRecursionLevel > 0 .AND. nRecursionLevel > nMaxRecursionLevel
         cString += AsString( xVar )
      ELSE
         cString += DShowArray( xVar, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )
      ENDIF
      EXIT

   CASE "H"
      IF nRecursionLevel == 1
         cString += Space( nIndent ) + "Type='H' -> { Hash of " + hb_ntos( Len( xVar ) ) + " Items }" + hb_eol()
      ENDIF
      IF nMaxRecursionLevel > 0 .AND. nRecursionLevel > nMaxRecursionLevel
         cString += AsString( xVar )
      ELSE
         cString += DShowHash( xVar, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )
      ENDIF
      EXIT

   OTHERWISE
      cString += Space( nIndent ) + AsString( xVar ) + hb_eol()
   ENDSWITCH

   RETURN cString

STATIC FUNCTION DShowProperties( oVar, nScope, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )

   LOCAL xProp, aProps
   LOCAL aMethods, aMth
   LOCAL cString := ""

   hb_default( @nIndent, 0 )

   IF HB_ISOBJECT( oVar )
#if 0
      lOldScope := __SetClassScope( .F. )
#endif
      aMethods  := __objGetMsgFullList( oVar, .F., HB_MSGLISTALL, nScope )
      aProps    := __objGetValueFullList( oVar, , nScope )
#if 0
      __SetClassScope( lOldScope )
#endif

      IF Len( aProps ) > 0
         cString += Space( nIndent ) + " |  +- >> Begin Data    ------" + hb_eol()
         FOR EACH xProp IN aProps
            cString += Space( nIndent ) + " |  +- " + PadR( xProp[ HB_OO_DATA_SYMBOL ], 20 ) + " [" + DecodeType( xProp[ HB_OO_DATA_TYPE ] ) +  "] [" + DecodeScope( xProp[ HB_OO_DATA_SCOPE ] ) + "] " + ValType( xProp[ HB_OO_DATA_VALUE ] ) + " => " + AsString( xProp[ HB_OO_DATA_VALUE ] ) + hb_eol()
            IF lRecursive .AND. ValType( xProp[ HB_OO_DATA_VALUE ] ) $ "AO"
               cString += __HB_DumpVar( xProp[ HB_OO_DATA_VALUE ], , lRecursive, nIndent + 3, nRecursionLevel + 1, nMaxRecursionLevel ) + hb_eol()
            ENDIF
         NEXT
         cString += Space( nIndent ) + " |  +- >> End   Data    ------" + hb_eol()
         cString += Space( nIndent ) + " |   " + hb_eol()
      ENDIF
      IF Len( aMethods ) > 0
         cString += Space( nIndent ) + " |  +- >> Begin Methods ------" + hb_eol()
         FOR EACH aMth IN aMethods
            cString += Space( nIndent ) + " |  +- " + PadR( aMth[ HB_OO_DATA_SYMBOL ], 20 ) + " [" + DecodeType( aMth[ HB_OO_DATA_TYPE ] ) + "]" + " [" + DecodeScope( aMth[ HB_OO_DATA_SCOPE ] ) +  "] " + hb_eol()
         NEXT
         cString += Space( nIndent ) + " |  +- >> End   Methods ------" + hb_eol()
         cString += Space( nIndent ) + " |     " + hb_eol()
      ENDIF
   ENDIF
   IF HB_ISNULL( cString )
      cString := Space( nIndent ) + " | " + hb_eol()
   ENDIF

   RETURN cString

STATIC FUNCTION DShowArray( aVar, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )

   LOCAL xVal, nChar, nEolLen
   LOCAL cString := ""

   hb_default( @nIndent, 0 )

   // TraceLog( "DShowArray: aVar, lRecursive", aVar, lRecursive )

   IF HB_ISARRAY( aVar )
      nEolLen := Len( hb_eol() )
      nChar := Len( hb_ntos( Len( aVar ) ) )  // return number of chars to display that value
      // i.e. if Len( aVar ) == 99, then nChar := 2
      cString += Space( nIndent ) + "{" + hb_eol()
      FOR EACH xVal IN aVar
         cString += Space( nIndent ) + " [" + LTrim( StrZero( xVal:__enumIndex(), nChar ) ) + "] => " + AsString( xVal ) + ", " + hb_eol()
         IF lRecursive .AND. ValType( xVal ) $ "AOH"
            cString := hb_StrShrink( cString, nEolLen + 2 ) + hb_eol()
            cString += __HB_DumpVar( xVal, , lRecursive, nIndent + 3, nRecursionLevel + 1, nMaxRecursionLevel )
            cString := hb_StrShrink( cString, nEolLen ) + ", " + hb_eol()
         ENDIF
      NEXT
      IF Len( aVar ) > 0
         cString := hb_StrShrink( cString, nEolLen + 2 ) + hb_eol()
      ENDIF
      cString += Space( nIndent ) + "}" + hb_eol()
   ENDIF

   RETURN cString

STATIC FUNCTION DShowHash( hVar, lRecursive, nIndent, nRecursionLevel, nMaxRecursionLevel )

   LOCAL xVal
   LOCAL nEolLen
   LOCAL cString := ""

   hb_default( @nIndent, 0 )

   // TraceLog( "DShowHash: hVar, ValType( hVar ), lRecursive", hVar, ValType( hVar ), ValToPrg( hVar ), lRecursive )

   IF HB_ISHASH( hVar )
      nEolLen := Len( hb_eol() )
      cString += Space( nIndent ) + "{" + hb_eol()
      FOR EACH xVal IN hVar
         cString += Space( nIndent ) + " [" + LTrim( AsString( xVal:__enumKey() ) ) + "] => " + AsString( xVal ) + ", " + hb_eol()
         IF lRecursive .AND. ValType( xVal ) $ "AOH"
            cString := hb_StrShrink( cString, nEolLen + 2 ) + hb_eol()
            cString += __HB_DumpVar( xVal, , lRecursive, nIndent + 3, nRecursionLevel + 1, nMaxRecursionLevel )
            cString := hb_StrShrink( cString, nEolLen ) + ", " + hb_eol()
         ENDIF
      NEXT
      IF Len( hVar ) > 0
         cString := hb_StrShrink( cString, nEolLen + 2 ) + hb_eol()
      ENDIF
      cString += Space( nIndent ) + "}" + hb_eol()
   ENDIF

   RETURN cString

STATIC FUNCTION DecodeScope( nScope AS NUMERIC )

   LOCAL cString := ""

   IF hb_bitAnd( nScope, HB_OO_CLSTP_EXPORTED  ) != 0
      cString += "Ex,"
   ENDIF
#ifdef __XHARBOUR__
   IF hb_bitAnd( nScope, HB_OO_CLSTP_PUBLISHED ) != 0
      cString += "Pu,"
   ENDIF
#endif
   IF hb_bitAnd( nScope, HB_OO_CLSTP_PROTECTED ) != 0
      cString += "Pr,"
   ENDIF
   IF hb_bitAnd( nScope, HB_OO_CLSTP_HIDDEN    ) != 0
      cString += "Hi,"
   ENDIF
   IF hb_bitAnd( nScope, HB_OO_CLSTP_CTOR      ) != 0
      cString += "Ct,"
   ENDIF
   IF hb_bitAnd( nScope, HB_OO_CLSTP_READONLY  ) != 0
      cString += "Ro,"
   ENDIF
   IF hb_bitAnd( nScope, HB_OO_CLSTP_SHARED    ) != 0
      cString += "Sh,"
   ENDIF
   IF hb_bitAnd( nScope, HB_OO_CLSTP_CLASS     ) != 0
      cString += "Cl,"
   ENDIF
   IF hb_bitAnd( nScope, HB_OO_CLSTP_SUPER     ) != 0
      cString += "Su,"
   ENDIF

   IF Right( cString, 1 ) == ","
      cString := hb_StrShrink( cString )
   ENDIF

   RETURN PadR( cString, 18 )

STATIC FUNCTION DecodeType( nType AS NUMERIC )

   SWITCH nType
   CASE HB_OO_MSG_METHOD        ; RETURN "Method"
   CASE HB_OO_MSG_DATA          ; RETURN "Data"
   CASE HB_OO_MSG_CLASSDATA     ; RETURN "Clsdata"
   CASE HB_OO_MSG_INLINE        ; RETURN "Inline"
   CASE HB_OO_MSG_VIRTUAL       ; RETURN "Virtual"
   CASE HB_OO_MSG_SUPER         ; RETURN "Super"
   CASE HB_OO_MSG_ONERROR       ; RETURN "OnError"
   CASE HB_OO_MSG_DESTRUCTOR    ; RETURN "Destructor"
   CASE HB_OO_PROPERTY          ; RETURN "Property"
   CASE HB_OO_MSG_PROPERTY      ; RETURN "MsgPrp"
   CASE HB_OO_MSG_CLASSPROPERTY ; RETURN "ClsPrp"
   CASE HB_OO_MSG_REALCLASS     ; RETURN "RealCls"
   CASE HB_OO_MSG_DELEGATE      ; RETURN "Delegate"
   CASE HB_OO_MSG_PERFORM       ; RETURN "Perform"
   ENDSWITCH

   RETURN Space( 7 )

STATIC FUNCTION asString( x )
   RETURN iif( HB_ISSTRING( x ), '"' + x + '"', hb_CStr( x ) )

#include "error.ch"

/* return all informations about classes, included type and scope */
STATIC FUNCTION __objGetMsgFullList( oObject, lData, nRange, nScope, nNoScope )

   LOCAL aMessages
   LOCAL aReturn
   LOCAL nFirstProperty, aMsg

   IF ! HB_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, , ProcName() )
   ENDIF

   hb_default( @lData, .T. )
   hb_default( @nNoScope, 0 )

   // nRange is already defaulted in ClassFullSel in classes.c

   aMessages := ASort( oObject:ClassSel( nRange, nScope, .T. ), , , {| x, y | x[ HB_OO_DATA_SYMBOL ] < y[ HB_OO_DATA_SYMBOL ] } )
   aReturn   := {}

   nFirstProperty := AScan( aMessages, {| aElement | hb_LeftEq( aElement[ HB_OO_DATA_SYMBOL ], "_" ) } )

   FOR EACH aMsg IN aMessages

      IF ! hb_LeftEq( aMsg[ HB_OO_DATA_SYMBOL ], "_" )
         IF ( AScan( aMessages, {| aElement | aElement[ HB_OO_DATA_SYMBOL ] == "_" + aMsg[ HB_OO_DATA_SYMBOL ] }, nFirstProperty ) > 0 ) == lData
            IF nNoScope == 0 .OR. hb_bitAnd( aMsg[ HB_OO_DATA_SCOPE ], nNoScope ) == 0
               AAdd( aReturn, aMsg )
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN aReturn

/* return all values from classes, included type and scope */
STATIC FUNCTION __objGetValueFullList( oObject, aExcept, nScope, nNoScope )

   LOCAL aReturn
   LOCAL aVar

   IF ! HB_ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, , ProcName( 0 ) )
   ENDIF

   hb_default( @aExcept, {} )

   aReturn := {}
   FOR EACH aVar IN __objGetMsgFullList( oObject, .T., HB_MSGLISTALL, nScope, nNoScope )
      IF hb_AScan( aExcept, aVar[ HB_OO_DATA_SYMBOL ], , , .T. ) == 0
         // TraceLog( "__objGetValueFullList():  aVar[HB_OO_DATA_SYMBOL]",  aVar[ HB_OO_DATA_SYMBOL ] )
         AAdd( aReturn, { aVar[ HB_OO_DATA_SYMBOL ], __SendRawMsg( oObject, aVar[ HB_OO_DATA_SYMBOL ] ), aVar[ HB_OO_DATA_TYPE ], aVar[ HB_OO_DATA_SCOPE ] } )
      ENDIF
   NEXT

   RETURN aReturn
