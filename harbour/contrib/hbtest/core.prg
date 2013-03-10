/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library
 *
 * Copyright 1999-2013 Viktor Szakats (harbour syenar.net)
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

#include "error.ch"

#define TEST_RESULT_COL1_WIDTH  1
#define TEST_RESULT_COL2_WIDTH  11
#define TEST_RESULT_COL3_WIDTH  44
#define TEST_RESULT_COL4_WIDTH  85

THREAD STATIC t_hParams := { => }

PROCEDURE hbtest_Setup( cName, xValue )

   IF HB_ISSTRING( cName ) .AND. ! Empty( cName )
      IF PCount() > 1
         t_hParams[ cName ] := xValue
      ELSEIF cName $ t_hParams
         hb_HDel( t_hParams, cName )
      ENDIF
   ENDIF

   RETURN

PROCEDURE hbtest_Call( cBlock, bBlock, xResultExpected )

   LOCAL xResult
   LOCAL oError
   LOCAL lPPError
   LOCAL lRTE
   LOCAL lFailed

   LOCAL bOut

   LOCAL cLangOld

   IF HB_ISSTRING( cBlock )
      lPPError := .F.
   ELSE
      cBlock := "[Preprocessor error]"
      lPPError := .T.
   ENDIF

   cLangOld := hb_langSelect( "en" ) /* to always have RTEs in one language */

   BEGIN SEQUENCE WITH ErrorBlock( {| oError | Break( oError ) } )
      xResult := Eval( bBlock )
      lRTE := .F.
   RECOVER USING oError
      xResult := ErrorMessage( oError )
      lRTE := .T.
   END SEQUENCE

   hb_langSelect( cLangOld )

   IF lRTE
      lFailed := !( XToStr( xResult, .F. ) == XToStr( xResultExpected, .F. ) )
   ELSE
      IF !( ValType( xResult ) == ValType( xResultExpected ) )
         IF HB_ISSTRING( xResultExpected ) .AND. ValType( xResult ) $ "ABOHPS"
            lFailed := !( XToStr( xResult, .F. ) == xResultExpected )
         ELSE
            lFailed := .T.
         ENDIF
      ELSE
         lFailed := !( xResult == xResultExpected )
      ENDIF
   ENDIF

   IF lFailed .OR. lPPError .OR. hb_HGetDef( t_hParams, "showall", .T. )
      bOut := hb_HGetDef( t_hParams, "output", {| cMsg | OutStd( cMsg ) } )
      IF lFailed
         Eval( bOut, ;
            PadR( iif( lFailed, "!", " " ), TEST_RESULT_COL1_WIDTH ) + " " + ;
            PadR( ProcName( 1 ) + "(" + hb_ntos( ProcLine( 1 ) ) + ")", TEST_RESULT_COL2_WIDTH ) + " " + ;
            RTrim( cBlock ) + ;
            hb_eol() + ;
            Space( 5 ) + "  Result: " + XToStr( xResult, .F. ) + ;
            hb_eol() + ;
            Space( 5 ) + "Expected: " + XToStr( xResultExpected, .F. ) + ;
            hb_eol() )
      ELSE
         Eval( bOut, ;
            PadR( iif( lFailed, "!", " " ), TEST_RESULT_COL1_WIDTH ) + " " + ;
            PadR( ProcName( 1 ) + "(" + hb_ntos( ProcLine( 1 ) ) + ")", TEST_RESULT_COL2_WIDTH ) + " " + ;
            PadR( cBlock, TEST_RESULT_COL3_WIDTH ) + " -> " + ;
            PadR( XToStr( xResult, .F. ), TEST_RESULT_COL4_WIDTH ) + " | " + ;
            XToStr( xResultExpected, .F. ) + ;
            hb_eol() )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION ErrorMessage( oError )

   LOCAL cMessage := ""
   LOCAL tmp

   IF HB_ISNUMERIC( oError:severity )
      SWITCH oError:severity
      CASE ES_WHOCARES     ; cMessage += "M " ; EXIT
      CASE ES_WARNING      ; cMessage += "W " ; EXIT
      CASE ES_ERROR        ; cMessage += "E " ; EXIT
      CASE ES_CATASTROPHIC ; cMessage += "C " ; EXIT
      ENDSWITCH
   ENDIF
   IF HB_ISNUMERIC( oError:genCode )
      cMessage += hb_ntos( oError:genCode ) + " "
   ENDIF
   IF HB_ISSTRING( oError:subsystem )
      cMessage += oError:subsystem + " "
   ENDIF
   IF HB_ISNUMERIC( oError:subCode )
      cMessage += hb_ntos( oError:subCode ) + " "
   ENDIF
   IF HB_ISSTRING( oError:description )
      cMessage += oError:description + " "
   ENDIF
   IF ! Empty( oError:operation )
      cMessage += "(" + oError:operation + ") "
   ENDIF
   IF ! Empty( oError:filename )
      cMessage += "<" + oError:filename + "> "
   ENDIF
   IF HB_ISNUMERIC( oError:osCode )
      cMessage += "OS:" + hb_ntos( oError:osCode ) + " "
   ENDIF
   IF HB_ISNUMERIC( oError:tries )
      cMessage += "#:" + hb_ntos( oError:tries ) + " "
   ENDIF

   IF HB_ISARRAY( oError:Args )
      cMessage += "A:" + hb_ntos( Len( oError:Args ) ) + ":"
      FOR tmp := 1 TO Len( oError:Args )
         cMessage += ValType( oError:Args[ tmp ] ) + ":" + XToStr( oError:Args[ tmp ], .T. )
         IF tmp < Len( oError:Args )
            cMessage += ";"
         ENDIF
      NEXT
      cMessage += " "
   ENDIF

   IF oError:canDefault .OR. ;
      oError:canRetry .OR. ;
      oError:canSubstitute

      cMessage += "F:"
      IF oError:canDefault
         cMessage += "D"
      ENDIF
      IF oError:canRetry
         cMessage += "R"
      ENDIF
      IF oError:canSubstitute
         cMessage += "S"
      ENDIF
   ENDIF

   RETURN cMessage

STATIC FUNCTION XToStr( xValue, lInString )

   SWITCH ValType( xValue )
   CASE "N" ; RETURN hb_ntos( xValue )
   CASE "D" ; RETURN iif( lInString, "0d" + iif( Empty( xValue ), "00000000", DToS( xValue ) ), 'hb_SToD( "' + DToS( xValue ) + '" )' )
   CASE "U" ; RETURN "NIL"
   CASE "C"
      xValue := __StrToExp( xValue )
      RETURN iif( lInString, xValue, '"' + xValue + '"' )
   CASE "M"
      xValue := __StrToExp( xValue )
      RETURN "M:" + iif( lInString, xValue, '"' + xValue + '"' )
   ENDSWITCH

   RETURN hb_CStr( xValue )

STATIC FUNCTION __StrToExp( cStr )

   LOCAL cResult := ""

   LOCAL nLen, nPos
   LOCAL nByte, cByte

   nLen := hb_BLen( cStr )
   FOR nPos := 1 TO nLen
      cByte := hb_BSubStr( cStr, nPos, 1 )
      nByte := hb_BCode( cByte )
      IF ! __ByteIsDisplayable( nByte ) .OR. cByte == '"'
         cResult += "\" + __ByteToOctal( nByte )
      ELSE
         cResult += cByte
      ENDIF
   NEXT

   RETURN cResult

STATIC FUNCTION __ByteIsDisplayable( nByte )
   RETURN nByte >= 32 .AND. nByte < 128

STATIC FUNCTION __ByteToOctal( nValue )

   LOCAL cResult := ""
   LOCAL nExp

   FOR nExp := 2 TO 0 STEP -1
      cResult += SubStr( "01234567", Int( nValue / ( 8 ^ nExp ) ) + 1, 1 )
      nValue %= 8 ^ nExp
   NEXT

   RETURN cResult
