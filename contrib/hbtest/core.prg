/*
 * Harbour Project source code:
 * Regression tests for the runtime library
 *
 * Copyright 1999-2013 Viktor Szakats (vszakats.net/harbour)
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

#pragma -gc3

#include "error.ch"

#define TEST_RESULT_COL1_WIDTH  1
#define TEST_RESULT_COL2_WIDTH  11
#define TEST_RESULT_COL3_WIDTH  40

THREAD STATIC t_hParams := { => }

STATIC s_lBanner
STATIC s_nStartTime
STATIC s_nCount
STATIC s_nPass
STATIC s_nFail

INIT PROCEDURE __hbtest_Init()

   s_lBanner := .F.
   s_nStartTime := hb_milliSeconds()
   s_nCount := 0
   s_nPass := 0
   s_nFail := 0

   RETURN

STATIC PROCEDURE hbtest_Banner()

   Eval( hb_HGetDef( t_hParams, "output", {| ... | OutStd( ... ) } ), ;
      Replicate( "-", 75 ) + hb_eol() + ;
      "    Version:", Version() + hb_eol() + ;
      "   Compiler:", hb_Compiler() + hb_eol() + ;
      "         OS:", OS() + hb_eol() + ;
      " Date, Time:", hb_TToC( hb_DateTime() ) + hb_eol() + ;
      Replicate( "=", 75 ) + hb_eol() + ;
      Space( TEST_RESULT_COL1_WIDTH ), ;
      PadR( "Location", TEST_RESULT_COL2_WIDTH ), ;
      PadR( "Test", TEST_RESULT_COL3_WIDTH ), "->", ;
      "Result" + hb_eol() + ;
      Replicate( "-", 75 ) + hb_eol() )

   RETURN

EXIT PROCEDURE __hbtest_Exit()

   IF s_lBanner

      Eval( hb_HGetDef( t_hParams, "output", {| ... | OutStd( ... ) } ), ;
         Replicate( "=", 75 ) + hb_eol() + ;
         "Test calls passed:", Str( s_nPass ), "(", hb_ntos( Round( ( 1 - ( s_nFail / s_nPass ) ) * 100, 2 ) ), "% )" + hb_eol() + ;
         "Test calls failed:", Str( s_nFail ), "(", hb_ntos( Round( ( s_nFail / s_nPass ) * 100, 2 ) ), "% )" + hb_eol() + ;
         "                   ----------" + hb_eol() + ;
         "            Total:", Str( s_nPass + s_nFail ), ;
         "( Time elapsed:", hb_ntos( hb_milliSeconds() - s_nStartTime ), "ms )" + hb_eol() )

      ErrorLevel( iif( s_nFail == 0, 0, 1 ) )
   ENDIF

   RETURN

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

   s_nCount++

   IF HB_ISSTRING( cBlock )
      lPPError := .F.
   ELSE
      cBlock := "[Preprocessor error]"
      lPPError := .T.
   ENDIF

   cLangOld := hb_langSelect( "en" ) /* to always have RTEs in one language */

   IF ! s_lBanner
      s_lBanner := .T.
      hbtest_Banner()
   ENDIF

   BEGIN SEQUENCE WITH ErrorBlock( {| oError | Break( oError ) } )
      xResult := Eval( bBlock )
      lRTE := .F.
   RECOVER USING oError
      xResult := ErrorMessage( oError )
      lRTE := .T.
   END SEQUENCE

   hb_langSelect( cLangOld )

   IF lRTE
      lFailed := !( XToStr( xResult ) == XToStr( xResultExpected ) )
   ELSE
      IF !( ValType( xResult ) == ValType( xResultExpected ) )
         IF HB_ISSTRING( xResultExpected ) .AND. ValType( xResult ) $ "ABOHPS"
            lFailed := !( XToStr( xResult ) == xResultExpected )
         ELSE
            lFailed := .T.
         ENDIF
      ELSE
         lFailed := !( xResult == xResultExpected )
      ENDIF
   ENDIF

   IF lFailed .OR. lPPError .OR. hb_HGetDef( t_hParams, "showall", .T. )
      bOut := hb_HGetDef( t_hParams, "output", {| ... | OutStd( ... ) } )
      IF lFailed
         Eval( bOut, ;
            PadR( iif( lFailed, "!", " " ), TEST_RESULT_COL1_WIDTH ), ;
            PadR( ProcName( 1 ) + "(" + hb_ntos( ProcLine( 1 ) ) + ")", TEST_RESULT_COL2_WIDTH ), ;
            RTrim( cBlock ) + hb_eol() + ;
            Space( 5 ) + "  Result:", XToStr( xResult ) + hb_eol() + ;
            Space( 5 ) + "Expected:", XToStr( xResultExpected ) + hb_eol() )
      ELSE
         Eval( bOut, ;
            PadR( iif( lFailed, "!", " " ), TEST_RESULT_COL1_WIDTH ), ;
            PadR( ProcName( 1 ) + "(" + hb_ntos( ProcLine( 1 ) ) + ")", TEST_RESULT_COL2_WIDTH ), ;
            PadR( cBlock, TEST_RESULT_COL3_WIDTH ), "->", ;
            XToStr( xResult ) + hb_eol() )
      ENDIF
   ENDIF

   IF lFailed
      s_nFail++
   ELSE
      s_nPass++
   ENDIF

   RETURN

STATIC FUNCTION ErrorMessage( oError )

   LOCAL cMessage := ""

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

   RETURN RTrim( cMessage )

STATIC FUNCTION XToStr( xValue )

   SWITCH ValType( xValue )
   CASE "N" ; RETURN hb_ntos( xValue )
   CASE "C"
   CASE "M" ; RETURN '"' + __StrToExp( xValue ) + '"'
   CASE "A"
   CASE "H"
   CASE "O" ; RETURN hb_ValToExp( xValue, .T. )
   ENDSWITCH

   RETURN hb_CStr( xValue )

STATIC FUNCTION __StrToExp( cStr )

   LOCAL cResult := ""
   LOCAL cByte

   FOR EACH cByte IN cStr  /* FOR EACH on byte stream */
      cResult += iif( hb_BCode( cByte ) < 32 .OR. hb_BCode( cByte ) >= 128 .OR. cByte == '"', ;
         "\" + __ByteEscape( hb_BCode( cByte ) ), cByte )
   NEXT

   RETURN cResult

STATIC FUNCTION __ByteEscape( nByte )

   LOCAL cResult
   LOCAL nExp

   IF nByte == 0
      RETURN "0"
   ELSE
      cResult := ""
      FOR nExp := 2 TO 0 STEP -1
         cResult += SubStr( "01234567", Int( nByte / ( 8 ^ nExp ) ) + 1, 1 )
         nByte %= 8 ^ nExp
      NEXT
   ENDIF

   RETURN cResult

FUNCTION hbtest_Object()

   LOCAL o := ErrorNew()

   o:description := "Harbour"

   RETURN o

/* TODO: add P, M */

FUNCTION hbtest_AllValues()
   RETURN { ;
      NIL, ;
      "HELLO", ;
      "Hello", ;
      "", ;
      "A" + Chr( 0 ) + "B", ;
      Chr( 13 ) + Chr( 10 ) + Chr( 141 ) + Chr( 10 ) + Chr( 9 ), ;
      "utf8-űŰőŐ©", ;
      0, ;
      0.0, ;
      10, ;
      65, ;
      100000, ;
      10.567, ; /* Use different number of decimals than the default */
      -10, ;
      -100000, ;
      -10.567, ; /* Use different number of decimals than the default */
      1234567890123, ;
      hb_SToD( "19840325" ), ;
      hb_SToD( "" ), ;
      hb_SToT( "19850325123456789" ), ;
      hb_SToT( "" ), ;
      .F., ;
      .T., ;
      @hbtest_AllValues(), ;
      {|| NIL }, ;
      {|| "(string)" }, ;
      hbtest_Object(), ;
      { => }, ;
      { "a" => "b" }, ;
      {}, ;
      { 9898 } }

FUNCTION hbtest_AllTypes()
   RETURN { ;
      NIL, ;
      "a", ;
      10, ;
      hb_SToD( "19840325" ), ;
      hb_SToT( "19850325123456789" ), ;
      .T., ;
      @hbtest_AllTypes(), ;
      {|| NIL }, ;
      hbtest_Object(), ;
      { "a" => "b" }, ;
      { 100 } }
