/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * C Structure Support.
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xharbour.org
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

#include "cstruct.ch"
#include "hboo.ch"
#include "error.ch"

#define CLASS_PROPERTIES 6

THREAD STATIC t_aActiveStructure

/* TOFIX: Add mutex protection for variables below. */
STATIC s_aClasses := {}
STATIC s_aArrayClasses := {}
STATIC s_aSynonyms := {}
STATIC s_lInitLongs := .T.

PROCEDURE __init_LONGLONGs()

   hb_CStructureCSyntax( "ULONGLONG", { "-4", "ulong[2]", }, , , 8 )
   __ClsSetModule( __ActiveStructure() )

   hb_CStructureCSyntax( "LONGLONG", { "4", "long[2]", }, , , 8 )
   __ClsSetModule( __ActiveStructure() )

   RETURN

//

FUNCTION __ActiveStructure( cStructure, nAlign )

#if 0
   LOCAL oErr
#endif
   LOCAL acMembers, aCTypes, hClass, Counter, cMember

   IF s_lInitLongs
      s_lInitLongs := .F.
      __init_LONGLONGs()
   ENDIF

   IF PCount() == 2
      cStructure := Upper( cStructure )

      IF AScan( s_aClasses, {| aClassInfo | aClassInfo[ 1 ] == cStructure } ) > 0
         /* In most cases we can simply ignore the redefinition, by returning a FAKED Structure Array! */
#if 0
         oErr := ErrorNew()
         oErr:Args := { cStructure, nAlign }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := "Structure already defined."
         oErr:Operation     := "__ActiveStructure()"
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := 1
         oErr:SubSystem     := "C Structure"

         RETURN Eval( ErrorBlock(), oErr )
#endif

         // In most cases we can simply ignore the redefinition, by returning a FAKED Structure Array!
         // TraceLog( "Redefinition of C Structure: " + cStructure )
         RETURN t_aActiveStructure := { cStructure, NIL, {}, {}, iif( HB_ISNUMERIC( nAlign ), nAlign, 8 ) }
      ENDIF

      AAdd( s_aClasses, { cStructure, NIL, {}, {}, iif( HB_ISNUMERIC( nAlign ), nAlign, 8 ) } )
      // TraceLog( "Registered: " + cStructure, ATail( s_aClasses )[ 5 ] )

      t_aActiveStructure := ATail( s_aClasses )
   ELSE
      // TraceLog( "Created: " + Str( nId ) )

      acMembers := t_aActiveStructure[ 3 ]
      aCTypes   := t_aActiveStructure[ 4 ]
      nAlign    := t_aActiveStructure[ 5 ]

      hClass := __clsNew( "C Structure " + t_aActiveStructure[ 1 ], Len( acMembers ) + CLASS_PROPERTIES )
      // __clsDelMsg( hClass, "C" )

      t_aActiveStructure[ 2 ] := hClass

      __clsAddMsg( hClass,  "Reset"     , @Reset()         , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Buffer"    , @Buffer()        , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Value"     , @Value()         , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "DeValue"   , @DeValue()       , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Array"     , @ArrayMethod()   , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "SayMembers", @SayMembers()    , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Init"      , @Init()          , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Pointer"   , @Pointer()       , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "GetPointer", @GetPointer()    , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "CopyTo"    , @__CStr_CopyTo() , HB_OO_MSG_METHOD )

      FOR EACH cMember IN acMembers
         __clsAddMsg( hClass,       cMember, cMember:__enumIndex(), HB_OO_MSG_PROPERTY )
      NEXT

      Counter := Len( acMembers )
      __clsAddMsg( hClass,  "aCTypes"       , ++Counter, HB_OO_MSG_PROPERTY, acTypes )
      __clsAddMsg( hClass,  "aCMembers"     , ++Counter, HB_OO_MSG_PROPERTY, acMembers, HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass,  "nAlign"        , ++Counter, HB_OO_MSG_PROPERTY, nAlign, HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass,  "SizeOf"        , ++Counter, HB_OO_MSG_PROPERTY, hb_SizeOfCstructure( aCTypes, nAlign ), HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass,  "_nID"          , ++Counter, HB_OO_MSG_PROPERTY, Len( s_aClasses ) )
      // WARNING InternalBuffer *MUST* remain the *LAST* Property!!!
      __clsAddMsg( hClass,  "InternalBuffer", ++Counter, HB_OO_MSG_PROPERTY, , HB_OO_CLSTP_READONLY )

      // TraceLog( Len( aCTypes ), aCTypes[ 1 ], aCTypes )
      RETURN hClass
   ENDIF

   RETURN t_aActiveStructure

//

PROCEDURE hb_Member( cMember, CType )

   LOCAL nLen, nAt

   IF Right( cMember, 1 ) == "]"
      nAt := At( "[", cMember )
      // nLen := Val( SubStr( cMember, nAt + 1, Len( cMember ) ) )
      // Support expressions like x + y, x - y, x * y
      nLen := &( SubStr( cMember, nAt + 1, Len( cMember ) - nAt - 1 ) )

      AAdd( t_aActiveStructure[ 3 ], Left( cMember, nAt - 1 ) )
      AAdd( t_aActiveStructure[ 4 ], hb_CTypeArrayId( CType, nLen ) )
   ELSE
      AAdd( t_aActiveStructure[ 3 ], cMember )
      AAdd( t_aActiveStructure[ 4 ], CType )
   ENDIF

   RETURN

//

FUNCTION hb_CStructureId( cStructure, lInplace )

   LOCAL nID
   LOCAL oErr

   cStructure := Upper( cStructure )

   nID := AScan( s_aClasses, {| aClassInfo | aClassInfo[ 1 ] == cStructure } )

   IF nID == 0
      nID := AScan( s_aSynonyms, {| aSynonym | aSynonym[ 1 ] == cStructure } )
      IF nID == 0
         oErr := ErrorNew()
         oErr:Args          := { cStructure, lInplace }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := "Structure not found: '" + cStructure + "'"
         oErr:Operation     := "hb_CStructureId()"
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := 3
         oErr:SubSystem     := "C Structure"

         RETURN Eval( ErrorBlock(), oErr )
      ELSE
         nID := s_aSynonyms[ nID ][ 2 ]
      ENDIF

      nID += iif( lInplace, 0, CTYPE_STRUCTURE_PTR - CTYPE_STRUCTURE )
   ELSE
      nID += iif( lInplace, CTYPE_STRUCTURE, CTYPE_STRUCTURE_PTR )
   ENDIF

   // TraceLog( cStructure, nID )

   RETURN nID

//

PROCEDURE hb_CStructureCSyntax( cStructure, aDefinitions, cTag, cSynonList, nAlign )

   LOCAL cElem, nAt, nIndex := 1
   LOCAL nLen, Counter, CType
   LOCAL oErr
   LOCAL nID, cSynon

   FOR EACH cElem IN aDefinitions
      // *** PP bug - remove when possible! ***
      IF cElem == NIL
         ASize( aDefinitions, nIndex - 1 )
         EXIT
      ENDIF

      IF ( nAt := At( "*", cElem ) ) > 1
         IF nIndex < Len( aDefinitions )
            hb_AIns( aDefinitions, nIndex + 1, SubStr( cElem, nAt + 1 ), .T. )
         ELSE
            AAdd( aDefinitions, SubStr( cElem, nAt + 1 ) )
         ENDIF

         aDefinitions[ nIndex ] := StrTran( Left( cElem, nAt ), " " )
      ELSEIF ( nAt := At( "-", cElem ) ) > 1
         IF nIndex < Len( aDefinitions )
            hb_AIns( aDefinitions, nIndex + 1, SubStr( cElem, nAt ), .T. )
         ELSE
            AAdd( aDefinitions, SubStr( cElem, nAt ) )
         ENDIF

         aDefinitions[ nIndex ] := RTrim( Left( cElem, nAt - 1 ) )
      ENDIF

      nIndex++
   NEXT

   __ActiveStructure( cStructure, nAlign )
   nID := Len( s_aClasses )

   IF ! Empty( cTag )
      AAdd( s_aSynonyms, { Upper( cTag ), nID + CTYPE_STRUCTURE } )
      // TraceLog( ATail( s_aSynonyms )[ 1 ], ATail( s_aSynonyms )[ 2 ] )
   ENDIF

   IF ! Empty( cSynonList )
      FOR EACH cSynon IN hb_ATokens( cSynonList )
         IF Left( cSynon, 1 ) == "*"
            AAdd( s_aSynonyms, { Upper( SubStr( cSynon, 2 ) ), nID + CTYPE_STRUCTURE_PTR } )
         ELSE
            AAdd( s_aSynonyms, { Upper( cSynon ), nID + CTYPE_STRUCTURE } )
         ENDIF

         // TraceLog( ATail( s_aSynonyms )[ 1 ], ATail( s_aSynonyms )[ 2 ] )
      NEXT
   ENDIF

   nLen := Len( aDefinitions )

   FOR Counter := 1 TO nLen STEP 2
      // TraceLog( "Member: " + aDefinitions[ Counter + 1 ], "Type: " + aDefinitions[ Counter ] )

      CType := aDefinitions[ Counter ]
      IF Val( CType ) != 0
         hb_Member( aDefinitions[ Counter + 1 ], Val( aDefinitions[ Counter ] ) )
      ELSE
         IF Right( CType, 1 ) == "*"
            CType := hb_CStructureId( Left( CType, Len( CType ) - 1 ), .F. )
         ELSE
            CType := hb_CStructureId( CType, .T. )

            IF CType == CTYPE_STRUCTURE .OR. CType == CTYPE_STRUCTURE_PTR
               oErr := ErrorNew()
               oErr:Args          := { cStructure, aDefinitions, cTag, cSynonList, nAlign }
               oErr:CanDefault    := .F.
               oErr:CanRetry      := .F.
               oErr:CanSubstitute := .T.
               oErr:Description   := "Undefined CType: '" + aDefinitions[ Counter ] + "'"
               oErr:Operation     := "hb_CStructureCSyntax()"
               oErr:Severity      := ES_ERROR
               oErr:SubCode       := 2
               oErr:SubSystem     := "C Structure"

               CType := Eval( ErrorBlock(), oErr )
            ENDIF
         ENDIF

         hb_Member( aDefinitions[ Counter + 1 ], CType )
      ENDIF
   NEXT

   RETURN

//

FUNCTION hb_CStructure( cStructure, nAlign )

   LOCAL hClass
   LOCAL oStructure
   LOCAL nID
   LOCAL oErr

   cStructure := Upper( cStructure )
   nID        := AScan( s_aClasses, {| aClassInfo | aClassInfo[ 1 ] == cStructure } )

   IF nID == 0
      oErr := ErrorNew()
      oErr:Args          := { cStructure, nAlign }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "Structure not initialized with __ActiveStructure()"
      oErr:Operation     := "hb_CStructure()"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 3
      oErr:SubSystem     := "C Structure"

      RETURN Eval( ErrorBlock(), oErr )
   ENDIF

   hClass := s_aClasses[ nId ][ 2 ]
   oStructure := __clsInst( hClass )

   AllocateMembers( oStructure )

   RETURN oStructure

//

STATIC PROCEDURE AllocateMembers( oStructure )

   LOCAL aCTypes, CType

   aCTypes := oStructure:aCTypes

   // TraceLog( "Scaning: " + oStructure:className() )

   FOR EACH CType IN aCTypes
      IF CType > CTYPE_STRUCTURE .AND. CType < CTYPE_STRUCTURE_PTR
         oStructure[ CType:__enumIndex() ] := hb_CStructureFromId( CType, , .F. )
         AllocateMembers( oStructure[ CType:__enumIndex() ] )
      ENDIF
   NEXT

   // TraceLog( "Finished: " + oStructure:className() )

   RETURN

//

FUNCTION hb_CStructureFromId( nID, nAlign )

   LOCAL hClass, oStructure
   LOCAL oErr

   // TraceLog( nId, s_aClasses )

   IF nID > CTYPE_STRUCTURE_PTR
      nID -= CTYPE_STRUCTURE_PTR
   ELSEIF nID > CTYPE_STRUCTURE
      nID -= CTYPE_STRUCTURE
   ELSE
      oErr := ErrorNew()
      oErr:Args          := { nID, nAlign }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "ID out of range."
      oErr:Operation     := "hb_CStructureFromId()"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 4
      oErr:SubSystem     := "C Structure"

      RETURN Eval( ErrorBlock(), oErr )
   ENDIF

   IF s_aClasses[ nID ][ 2 ] == NIL
      // Meta class was not created yet.
      RETURN hb_CStructure( s_aClasses[ nId ][ 1 ] )
   ELSE
      hClass := s_aClasses[ nId ][ 2 ]

      oStructure := __clsInst( hClass )
   ENDIF

   RETURN oStructure

//

FUNCTION hb_CTypeArrayId( CType, nLen )

   LOCAL hClass
   LOCAL Counter
   LOCAL nID
   LOCAL aCTypes, acMembers, cMember
   LOCAL cArrayClassName := "C Array of [" + hb_ntos( nLen ) + "] CType: " + Str( CType )

   nID := AScan( s_aArrayClasses, {| aArrayDefinitions | aArrayDefinitions[ 1 ] == CType .AND. aArrayDefinitions[ 2 ] == nLen } )

   IF nID == 0
      hClass := __clsNew( "C Structure " + cArrayClassName, nLen + CLASS_PROPERTIES )
      // __clsDelMsg( hClass, "C" )

      __ClsSetModule( hClass )
      AAdd( s_aClasses, { cArrayClassName, hClass, Array( nLen ), Array( nLen ), 1 } )
      nID := Len( s_aClasses )

      acMembers := s_aClasses[ nID ][ 3 ]
      aCTypes   := s_aClasses[ nID ][ 4 ]

      // Sames as s_aClasses[ nID ][ 4 ]
      AFill( aCTypes, CType )

      AAdd( s_aArrayClasses, { CType, nLen, nID } )

      __clsAddMsg( hClass,  "Reset"     , @Reset()         , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Buffer"    , @Buffer()        , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Value"     , @Value()         , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "DeValue"   , @DeValue()       , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Array"     , @ArrayMethod()   , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "SayMembers", @SayMembers()    , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Init"      , @Init()          , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "Pointer"   , @Pointer()       , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "GetPointer", @GetPointer()    , HB_OO_MSG_METHOD )
      __clsAddMsg( hClass,  "CopyTo"    , @__CStr_CopyTo() , HB_OO_MSG_METHOD )

      // IF Abs( CType ) == 1
      __clsAddMsg( hClass, "AsString", @AsString()   , HB_OO_MSG_METHOD )
      // ENDIF

      FOR Counter := 1 TO nLen
         cMember := hb_ntos( Counter )
         acMembers[ Counter ] := cMember
         __clsAddMsg( hClass,       cMember, Counter, HB_OO_MSG_PROPERTY )
      NEXT

      __clsAddMsg( hClass,  "aCTypes"       , Counter++, HB_OO_MSG_PROPERTY, aCTypes )
      __clsAddMsg( hClass,  "aCMembers"     , Counter++, HB_OO_MSG_PROPERTY, acMembers, HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass,  "nAlign"        , Counter++, HB_OO_MSG_PROPERTY, 1, HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass,  "SizeOf"        , Counter++, HB_OO_MSG_PROPERTY, hb_SizeOfCstructure( aCTypes, 1 ), HB_OO_CLSTP_READONLY )
      __clsAddMsg( hClass,  "_nID", Counter++, HB_OO_MSG_PROPERTY, nID )
      // WARNING InternalBuffer *MUST* remain the *LAST* Property!!!
      __clsAddMsg( hClass,  "InternalBuffer", Counter, HB_OO_MSG_PROPERTY, , HB_OO_CLSTP_READONLY )
      // TraceLog( "Registered: " + cArrayClassName, nID, Len( s_aArrayClasses ), hb_SizeOfCstructure( aCTypes, 1 ), nLen )
   ELSE
      nID := s_aArrayClasses[ nID ][ 3 ]
      // TraceLog( "Reused: " + s_aClasses[ nID ][ 1 ], nID )
   ENDIF

   RETURN nID + CTYPE_STRUCTURE

//

FUNCTION hb_Is_CStructure( x )

   RETURN Left( x:ClassName(), 11 ) == "C Structure"

//

STATIC FUNCTION SayMembers( cPad, lShowMembers, lReturnString )

   LOCAL xProperty, cOut := ""

   IF cPad == NIL
      cPad := ""
   ENDIF
   IF lShowMembers == NIL
      lShowMembers := .F.
   ENDIF
   IF lReturnString == NIL
      lReturnString := .F.
   ENDIF

#if 0
   QOut( cPad + SubStr( QSelf():className(), 13 ) )
   QOut( cPad + Replicate( "-", Len( SubStr( QSelf():className(), 13 ) ) ) )
#endif

   cOut += cPad + SubStr( QSelf():className(), 13 )
   cOut += hb_eol() + cPad + Replicate( "-", Len( SubStr( QSelf():className(), 13 ) ) )

   FOR EACH xProperty IN QSelf():Array
      IF hb_Is_CStructure( xProperty )
         IF lReturnString
            cOut += hb_eol() + hb_eol() + xProperty:SayMembers( cPad + cPad, lShowMembers, lReturnString )
         ELSE
            xProperty:SayMembers( cPad + cPad, lShowMembers )
         ENDIF
      ELSE
         // QOut( cPad + iif( lShowMembers, acMembers[ xProperty:__enumIndex() ], "" ) + ":", xProperty )
         cOut += hb_eol() + cPad + iif( lShowMembers, QSelf():acMembers[ xProperty:__enumIndex() ], "" ) + ":" + hb_CStr( xProperty )
      ENDIF
   NEXT

   IF ! lReturnString
      QOut( cOut )
   ENDIF

   RETURN iif( lReturnString, cOut, QSelf() )

//

STATIC FUNCTION Reset()

   LOCAL xProperty, nProperties := Len( QSelf() ) - CLASS_PROPERTIES

   FOR EACH xProperty IN QSelf()
      IF xProperty:__enumIndex() > nProperties
         EXIT
      ENDIF

      IF hb_Is_CStructure( xProperty )
         xProperty:Reset()
      ELSE
         xProperty := NIL
      ENDIF
   NEXT

   RETURN QSelf()

//

STATIC FUNCTION Buffer( Buffer, lAdopt )

   IF HB_ISSTRING( Buffer )
      IF Len( Buffer ) < QSelf():SizeOf
         // TraceLog( Buffer )
         Buffer := PadR( Buffer, QSelf():SizeOf, Chr( 0 ) )
      ENDIF

      QSelf():InternalBuffer := Buffer
      QSelf():DeValue( lAdopt )
   ENDIF

   IF ! HB_ISSTRING( QSelf():InternalBuffer )
      QSelf():InternalBuffer := QSelf():Value()
   ENDIF

   RETURN QSelf()

//

STATIC FUNCTION GetPointer()

   QSelf():InternalBuffer := hb_ArrayToStructure( QSelf(), QSelf():aCTypes, QSelf():nAlign )

   RETURN hb_String2Pointer( QSelf():InternalBuffer )

//

STATIC FUNCTION Value()

#if 0
   LOCAL aValues := {}

   AEval( QSelf(), {| xVal | AAdd( aValues, xVal ) }, 1, Len( QSelf() ) - CLASS_PROPERTIES )
#endif

   QSelf():InternalBuffer := hb_ArrayToStructure( QSelf(), QSelf():aCTypes, QSelf():nAlign )

   RETURN QSelf():InternalBuffer

//

STATIC FUNCTION DeValue( lAdopt )

#if 0
   LOCAL aValues := {}
#endif
   LOCAL Buffer := QSelf():InternalBuffer

#if 0
   TraceLog( QSelf():ClassName(), QSelf():nAlign, Buffer, Len( Buffer ), lAdopt )

   AEval( QSelf(), {| xVal | AAdd( aValues, xVal ) }, 1, Len( QSelf() ) - CLASS_PROPERTIES )
#endif

   IF ! HB_ISSTRING( Buffer ) .OR. Len( Buffer ) == 0
      // TraceLog( "EMPTY Buffer passed to " + ProcName() )
   ELSEIF Len( Buffer ) < QSelf():SizeOf
      // TraceLog( "Should have been caught at ::Buffer()!!!", Buffer )
      // Buffer := PadR( Buffer, QSelf():SizeOf, Chr( 0 ) )
   ELSE
      hb_StructureToArray( Buffer, QSelf():aCTypes, QSelf():nAlign, lAdopt, QSelf()  )
   ENDIF

   RETURN QSelf()

//

STATIC FUNCTION ArrayMethod()

   LOCAL aValues := {}

   AEval( QSelf(), {| xVal | AAdd( aValues, xVal ) }, 1, Len( QSelf() ) - CLASS_PROPERTIES )

   RETURN aValues

//

STATIC FUNCTION Init( aValues )

   LOCAL xProperty, nLen := Len( aValues )

   FOR EACH xProperty IN QSelf()
      IF xProperty:__enumIndex() > nLen
         EXIT
      ENDIF

      IF Left( xProperty:className(), 11 ) == "C Structure"
         xProperty:Init( aValues[ xProperty:__enumIndex() ] )
      ELSE
         xProperty := aValues[ xProperty:__enumIndex() ]
      ENDIF
   NEXT

   RETURN QSelf()

//

STATIC FUNCTION Pointer( nNewPointer, lAdopt )

   IF nNewPointer != NIL
      QSelf():Buffer( hb_Pointer2String( nNewPointer, QSelf():SizeOf ), lAdopt )
   ENDIF

   RETURN QSelf()

//

STATIC FUNCTION AsString()

   LOCAL cChar, nLen := Len( QSelf() ) - CLASS_PROPERTIES, cString := Space( nLen )

   FOR EACH cChar IN QSelf()
      IF cChar:__enumIndex() > nLen
         EXIT
      ENDIF

      IF cChar == 0
         cString := Left( cString, cChar:__enumIndex() - 1 )
         EXIT
      ENDIF

      cString[ cChar:__enumIndex() ] := cChar
   NEXT

   RETURN cString
