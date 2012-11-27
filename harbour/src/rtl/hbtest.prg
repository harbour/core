/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (main)
 *
 * Copyright 1999-2012 Viktor Szakats (harbour syenar.net)
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

#include "error.ch"

#define TEST_RESULT_COL1_WIDTH  1
#define TEST_RESULT_COL2_WIDTH  15
#define TEST_RESULT_COL3_WIDTH  40
#define TEST_RESULT_COL4_WIDTH  85
#define TEST_RESULT_COL5_WIDTH  85

THREAD STATIC t_hParams := { => }

PROCEDURE __hbtest_Setup( cName, xValue )

   IF HB_ISSTRING( cName ) .AND. ! Empty( cName )
      IF PCount() > 1
         t_hParams[ cName ] := xValue
      ELSEIF cName $ t_hParams
         hb_HDel( t_hParams, cName )
      ENDIF
   ENDIF

   RETURN

PROCEDURE __hbtest_Call( cBlock, bBlock, xResultExpected )

   LOCAL xResult
   LOCAL oError
   LOCAL lPPError
   LOCAL lFailed

   LOCAL bOut

   IF HB_ISSTRING( cBlock )
      lPPError := .F.
   ELSE
      cBlock := "[Preprocessor error]"
      lPPError := .T.
   ENDIF

   BEGIN SEQUENCE WITH ErrorBlock( {| oError | Break( oError ) } )
      xResult := Eval( bBlock )
   RECOVER USING oError
      xResult := ErrorMessage( oError )
   END SEQUENCE

   IF !( ValType( xResult ) == ValType( xResultExpected ) )
      IF HB_ISSTRING( xResultExpected ) .AND. ValType( xResult ) $ "ABOHPS"
         lFailed := !( XToStr( xResult ) == xResultExpected )
      ELSE
         lFailed := .T.
      ENDIF
   ELSE
      lFailed := !( xResult == xResultExpected )
   ENDIF

   IF lFailed .OR. lPPError .OR. hb_HGetDef( t_hParams, "showall", .T. )
      bOut := hb_HGetDef( t_hParams, "output", {| cMsg | OutStd( cMsg ) } )
      IF lFailed
         Eval( bOut, PadR( iif( lFailed, "!", " " ), TEST_RESULT_COL1_WIDTH ) + " " +;
                     PadR( ProcName( 1 ) + "(" + hb_ntos( ProcLine( 1 ) ) + ")", TEST_RESULT_COL2_WIDTH ) + " " +;
                     PadR( cBlock, TEST_RESULT_COL3_WIDTH ) +;
                     hb_eol() +;
                     Space( 5 ) + "  Result: " + XToStr( xResult ) +;
                     hb_eol() +;
                     Space( 5 ) + "Expected: " + XToStr( xResultExpected ) +;
                     hb_eol() )
      ELSE
         Eval( bOut, PadR( iif( lFailed, "!", " " ), TEST_RESULT_COL1_WIDTH ) + " " +;
                     PadR( ProcName( 1 ) + "(" + hb_ntos( ProcLine( 1 ) ) + ")", TEST_RESULT_COL2_WIDTH ) + " " +;
                     PadR( cBlock, TEST_RESULT_COL3_WIDTH ) + " -> " +;
                     PadR( XToStr( xResult ), TEST_RESULT_COL4_WIDTH ) + " | " +;
                     PadR( XToStr( xResultExpected ), TEST_RESULT_COL5_WIDTH ) +;
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
         cMessage += ValType( oError:Args[ tmp ] ) + ":" + XToStrE( oError:Args[ tmp ] )
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

FUNCTION XToStr( xValue )

   SWITCH ValType( xValue )
   CASE "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN '"' + xValue + '"'

   CASE "N" ; RETURN hb_ntos( xValue )
   CASE "D" ; RETURN "0d" + iif( Empty( xValue ), "0", DToS( xValue ) )
   CASE "T" ; RETURN "t" + '"' + hb_TSToStr( xValue, .T. ) + '"'
   CASE "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE "O" ; RETURN "{ " + xValue:className() + " Object }"
   CASE "U" ; RETURN "NIL"
   CASE "B" ; RETURN "{|| ... }"
   CASE "A" ; RETURN "{ Array of " + hb_ntos( Len( xValue ) ) + " Items }"
   CASE "H" ; RETURN "{ Hash of " + hb_ntos( Len( xValue ) ) + " Items }"
   CASE "M" ; RETURN "M:" + '"' + xValue + '"'
   CASE "S" ; RETURN "@" + xValue:name + "()"
   CASE "P" ; RETURN "<pointer>"
   ENDSWITCH

   RETURN iif( xValue == NIL, "NIL", "" )

FUNCTION XToStrE( xValue )

   SWITCH ValType( xValue )
   CASE "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN xValue

   CASE "N" ; RETURN hb_ntos( xValue )
   CASE "D" ; RETURN DToS( xValue )
   CASE "T" ; RETURN "t" + '"' + hb_TSToStr( xValue, .T. ) + '"'
   CASE "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE "O" ; RETURN "{ " + xValue:className() + " Object }"
   CASE "U" ; RETURN "NIL"
   CASE "B" ; RETURN "{|| ... }"
   CASE "A" ; RETURN "{ Array of " + hb_ntos( Len( xValue ) ) + " Items }"
   CASE "H" ; RETURN "{ Hash of " + hb_ntos( Len( xValue ) ) + " Items }"
   CASE "M" ; RETURN "M:" + xValue
   CASE "S" ; RETURN "@" + xValue:name + "()"
   CASE "P" ; RETURN "<pointer>"
   ENDSWITCH

   RETURN iif( xValue == NIL, "NIL", "" )
