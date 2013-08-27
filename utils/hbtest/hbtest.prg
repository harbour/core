/*
 * Harbour Project source code:
 * Regression tests for the runtime library (main)
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

/* Transform() tests mostly written by Eddie Runia <eddie@runia.com> */
/* Empty() tests written by Eddie Runia <eddie@runia.com> */
/* :class* tests written by Dave Pearson <davep@hagbard.demon.co.uk> */

/* NOTE: Always compile with /n switch */
/* NOTE: It's worth to make tests with and without the /z switch */
/* NOTE: Guard all Harbour extensions with __HARBOUR__ #ifdefs */
/* NOTE: Use ":className()" instead of ":className" to make your code work
         with Xbase++. Xbase++ seem to take differenciate between the
         object method and object variable form. In CA-Cl*pper and Harbour
         both syntax is accepted. Same goes for ":Eval()" */

/* TODO: Add checks for string parameters with embedded NUL character */
/* TODO: Add test cases for other string functions */
/* TODO: Incorporate tests from test/working/string*.prg */
/* TODO: String overflow on + and - tests */
/* TODO: Tests with MEMO type ? */
/* TODO: Tests with Log(0) type of invalid values */

#include "rt_main.ch"

#include "error.ch"
#include "fileio.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

#ifndef __HARBOUR__
   #xtranslate hb_eol() => ( Chr( 13 ) + Chr( 10 ) )
#endif

#define TEST_RESULT_COL1_WIDTH  1
#define TEST_RESULT_COL2_WIDTH  20
#define TEST_RESULT_COL3_WIDTH  40
#define TEST_RESULT_COL4_WIDTH  85
#define TEST_RESULT_COL5_WIDTH  85

STATIC s_nPass
STATIC s_nFail
STATIC s_nFhnd
STATIC s_nCount
STATIC s_lShowAll
STATIC s_lShortcut
STATIC s_aSkipList
STATIC s_nStartTime
STATIC s_nEndTime
STATIC s_lDBFAvail := .F.

#ifdef __HARBOUR__
   REQUEST HB_LANG_EN

   ANNOUNCE HB_GTSYS
   REQUEST HB_GT_CGI_DEFAULT
#endif

PROCEDURE Main( cPar1, cPar2 )

   OutStd( "Harbour Regression Test Suite" + hb_eol() +;
           "Copyright (c) 1999-2013, Viktor Szakats" + hb_eol() +;
           "http://harbour-project.org/" + hb_eol() )

   IF cPar1 == NIL
      cPar1 := ""
   ENDIF
   IF cPar2 == NIL
      cPar2 := ""
   ENDIF

   IF "/?" $ Lower( cPar1 ) .OR. ;
      "/h" $ Lower( cPar1 ) .OR. ;
      "-?" $ Lower( cPar1 ) .OR. ;
      "-h" $ Lower( cPar1 ) .OR. ;
      "-help" $ Lower( cPar1 ) .OR. ;
      "--help" $ Lower( cPar1 )

      OutStd( hb_eol() +;
              "Syntax:  hbtest [options]" + hb_eol() +;
              hb_eol() +;
              "Options:  -h, -?        Display this help." + hb_eol() +;
              "          -all          Display all tests, not only the failures." + hb_eol() +;
              "          -skip:<list>  Skip the listed test numbers." + hb_eol() )

      RETURN
   ENDIF

   /* Initialize test */

   TEST_BEGIN( cPar1 + " " + cPar2 )

   Main_HVM()
   Main_HVMA()
   Main_MATH()
   Main_DATE()
   Main_STR()
   Main_STRA()
   Main_TRANS()
   Comp_Str()
   Exact_Str()
   New_STRINGS()
#ifdef __HARBOUR__
#ifndef __PLATFORM__VXWORKS
   Long_STRINGS()
#endif
#endif
#ifdef __XPP__
   Long_STRINGS()
#endif
   Main_ARRAY()
   Main_FILE()
   Main_MISC()
#ifdef __HARBOUR__
   Main_OPOVERL()
   Main_CLASS()
   Main_MT()
#endif
   Main_LAST()

   /* Show results, return ERRORLEVEL and exit */

   TEST_END()

   RETURN

/* NOTE: These should always be called last, since they can mess up the test
         environment.

         Right now the failing __MRestore() will clear all memory variables,
         which is absolutely normal otherwise. */

STATIC PROCEDURE Main_LAST()

   HBTEST MemVarBlock( "mcString" )           IS "{||...}"
#ifndef __XPP__
   HBTEST __MRestore()                        IS "E 1 BASE 2007 Argument error (__MRESTORE) OS:0 #:0 "
#endif
   HBTEST MemVarBlock( "mcString" )           IS "{||...}"
#ifndef __XPP__
   HBTEST __MSave()                           IS "E 1 BASE 2008 Argument error (__MSAVE) OS:0 #:0 "
   HBTEST __MRestore( "$NOTHERE.MEM", .F. )   IS "E 21 BASE 2005 Open error <$NOTHERE.MEM> OS:2 #:1 F:DR"
#endif
   HBTEST MemVarBlock( "mcString" )           IS NIL
#ifndef __XPP__
   HBTEST __MSave( BADFNAME(), "*", .T. )     IS "E 20 BASE 2006 Create error <" + BADFNAME() + "> OS:2 #:1 F:DR"
#endif

   RETURN

STATIC PROCEDURE TEST_BEGIN( cParam )

   LOCAL bErrorOld

   s_nStartTime := Seconds()

   /* Set up the initial state */

#ifdef __HARBOUR__
   hb_cdpSelect( "EN" )
   hb_langSelect( "en" )
#endif
   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   SET EXACT OFF

   FErase( "NOT_HERE.$$$" )

   /* Options */

   s_lShowAll := ;
      "/all" $ Lower( cParam ) .OR. ;
      "-all" $ Lower( cParam )

   s_aSkipList := ListToNArray( CMDLGetValue( Lower( cParam ), "/skip:", "" ) )
   IF Empty( s_aSkipList )
      s_aSkipList := ListToNArray( CMDLGetValue( Lower( cParam ), "-skip:", "" ) )
   ENDIF

   /* Detect presence of shortcutting optimization */

   s_lShortcut := .T.
   IF .T. .OR. Eval( {|| s_lShortcut := .F. } )
      /* Do nothing */
   ENDIF

   s_nFhnd := 1 /* FHND_STDOUT */

   s_nCount := 0
   s_nPass := 0
   s_nFail := 0

   /* Feedback */

   OutMsg( s_nFhnd, ;
      "---------------------------------------------------------------------------" + hb_eol() +;
      "      Version: " + Version() + hb_eol() )
#ifdef __HARBOUR__
   OutMsg( s_nFhnd, ;
      "     Compiler: " + hb_Compiler() + hb_eol() )
#endif
   OutMsg( s_nFhnd, ;
      "           OS: " + OS() + hb_eol() +;
      "   Date, Time: " + DToC( Date() ) + " " + Time() + hb_eol() +;
      "Shortcut opt.: " + iif( s_lShortcut, "On", "Off" ) + hb_eol() +;
      "     Switches: " + cParam + hb_eol() +;
      "===========================================================================" + hb_eol() )

   OutMsg( s_nFhnd, ;
      PadR( "R", TEST_RESULT_COL1_WIDTH ) + " " +;
      PadR( "No.  Line", TEST_RESULT_COL2_WIDTH ) + " " +;
      PadR( "TestCall()", TEST_RESULT_COL3_WIDTH ) + " -> " +;
      PadR( "Result", TEST_RESULT_COL4_WIDTH ) + " | " +;
            "Expected" + hb_eol() +;
      "---------------------------------------------------------------------------" + hb_eol() )

   /* NOTE: mxNotHere intentionally not declared */
   PUBLIC mcLongerNameThen10Chars := "Long String Name!"
   PUBLIC mcString  := scString
   PUBLIC mcStringE := scStringE
   PUBLIC mcStringZ := scStringZ
   PUBLIC mcStringW := scStringW
   PUBLIC mnIntZ    := snIntZ
   PUBLIC mnDoubleZ := snDoubleZ
   PUBLIC mnIntP    := snIntP
   PUBLIC mnLongP   := snLongP
   PUBLIC mnDoubleP := snDoubleP
   PUBLIC mnIntN    := snIntN
   PUBLIC mnLongN   := snLongN
   PUBLIC mnDoubleN := snDoubleN
   PUBLIC mnDoubleI := snDoubleI
   PUBLIC mdDate    := sdDate
   PUBLIC mdDateE   := sdDateE
   PUBLIC mlFalse   := slFalse
   PUBLIC mlTrue    := slTrue
   PUBLIC moObject  := ErrorNew()
   PUBLIC muNIL     := suNIL
   PUBLIC mbBlock   := sbBlock
   PUBLIC mbBlockC  := sbBlockC
   PUBLIC maArray   := { 9898 }

#ifndef __XPP__
   // rddSetDefault( "DBFCDX" )
#endif

   bErrorOld := ErrorBlock( {| oError | Break( oError ) } )
   BEGIN SEQUENCE
      dbCreate( "_hbtmp_.dbf", { ;
         { "TYPE_C"   , "C", 15, 0 } ,;
         { "TYPE_C_E" , "C", 15, 0 } ,;
         { "TYPE_D"   , "D",  8, 0 } ,;
         { "TYPE_D_E" , "D",  8, 0 } ,;
         { "TYPE_M"   , "M", 10, 0 } ,;
         { "TYPE_M_E" , "M", 10, 0 } ,;
         { "TYPE_N_I" , "N", 11, 0 } ,;
         { "TYPE_N_IE", "N", 11, 0 } ,;
         { "TYPE_N_D" , "N", 11, 3 } ,;
         { "TYPE_N_DE", "N", 11, 3 } ,;
         { "TYPE_L"   , "L",  1, 0 } ,;
         { "TYPE_L_E" , "L",  1, 0 } } )

      USE ( "_hbtmp_.dbf" ) NEW ALIAS w_TEST EXCLUSIVE

      dbAppend()

      w_TEST->TYPE_C    := "<FieldValue>"
      w_TEST->TYPE_C_E  := ""
      w_TEST->TYPE_D    := sdDate
      w_TEST->TYPE_D_E  := sdDateE
      w_TEST->TYPE_M    := "<MemoValue>"
      w_TEST->TYPE_M_E  := ""
      w_TEST->TYPE_N_I  := 100
      w_TEST->TYPE_N_IE := 0
      w_TEST->TYPE_N_D  := 101.127
      w_TEST->TYPE_N_DE := 0
      w_TEST->TYPE_L    := .T.
      w_TEST->TYPE_L_E  := .F.

      s_lDBFAvail := .T.
   END SEQUENCE
   ErrorBlock( bErrorOld )

   IF ! s_lDBFAvail
      OutMsg( s_nFhnd, "WARNING ! Test .dbf could not be created. Related tests will be skipped." + hb_eol() )
   ENDIF

   RETURN

FUNCTION TEST_DBFAvail()
   RETURN s_lDBFAvail

PROCEDURE TEST_CALL( cBlock, bBlock, xResultExpected )

   LOCAL xResult
   LOCAL oError
   LOCAL bOldError
   LOCAL lPPError
   LOCAL lRTE
   LOCAL lFailed
   LOCAL lSkipped

   s_nCount++

   IF ValType( cBlock ) == "C"
      lPPError := .F.
   ELSE
      cBlock := "!! Preprocessor error !!"
      lPPError := .T.
   ENDIF

   lSkipped := AScan( s_aSkipList, s_nCount ) > 0

   IF lSkipped

      lFailed := .F.
      xResult := "!! Skipped !!"

   ELSE

      bOldError := ErrorBlock( {| oError | Break( oError ) } )

      BEGIN SEQUENCE
         xResult := Eval( bBlock )
         lRTE := .F.
      RECOVER USING oError
         xResult := ErrorMessage( oError )
         lRTE := .T.
      END SEQUENCE

      ErrorBlock( bOldError )

      IF lRTE
         lFailed := !( XToStr( xResult ) == XToStr( xResultExpected ) )
      ELSE
         IF !( ValType( xResult ) == ValType( xResultExpected ) )
            IF ValType( xResultExpected ) == "C" .AND. ValType( xResult ) $ "ABMO"
               lFailed := !( XToStr( xResult ) == xResultExpected )
            ELSE
               lFailed := .T.
            ENDIF
         ELSE
            lFailed := !( xResult == xResultExpected )
         ENDIF
      ENDIF

   ENDIF

   IF s_lShowAll .OR. lFailed .OR. lSkipped .OR. lPPError

      IF lFailed
         OutMsg( s_nFhnd, ;
            PadR( iif( lFailed, "!", iif( lSkipped, "S", " " ) ), TEST_RESULT_COL1_WIDTH ) + " " +;
            PadR( Str( s_nCount, 4 ) + " " + ProcName( 1 ) + "(" + LTrim( Str( ProcLine( 1 ), 5 ) ) + ")", TEST_RESULT_COL2_WIDTH ) + " " +;
            RTrim( cBlock ) +;
            hb_eol() +;
            Space( 5 ) + "  Result: " + XToStr( xResult ) +;
            hb_eol() +;
            Space( 5 ) + "Expected: " + XToStr( xResultExpected ) +;
            hb_eol() )
      ELSE
         OutMsg( s_nFhnd, ;
            PadR( iif( lFailed, "!", iif( lSkipped, "S", " " ) ), TEST_RESULT_COL1_WIDTH ) + " " +;
            PadR( Str( s_nCount, 4 ) + " " + ProcName( 1 ) + "(" + LTrim( Str( ProcLine( 1 ), 5 ) ) + ")", TEST_RESULT_COL2_WIDTH ) + " " +;
            PadR( cBlock, TEST_RESULT_COL3_WIDTH ) + " -> " +;
            PadR( XToStr( xResult ), TEST_RESULT_COL4_WIDTH ) + " | " +;
            RTrim( XToStr( xResultExpected ) ) +;
            hb_eol() )
      ENDIF
   ENDIF

   IF lFailed
      s_nFail++
   ELSE
      s_nPass++
   ENDIF

   RETURN

FUNCTION TEST_OPT_Z()
   RETURN s_lShortCut

STATIC PROCEDURE TEST_END()

   IF s_lDBFAvail
      dbSelectArea( "w_TEST" )
      dbCloseArea()
      FErase( "_hbtmp_.dbf" )
      FErase( "_hbtmp_.dbt" )
   ENDIF

   s_nEndTime := Seconds()

   OutMsg( s_nFhnd, ;
      "===========================================================================" + hb_eol() +;
      "Test calls passed: " + Str( s_nPass ) + " ( " + LTrim( Str( ( 1 - ( s_nFail / s_nPass ) ) * 100, 6, 2 ) ) + " % )" + hb_eol() +;
      "Test calls failed: " + Str( s_nFail ) + " ( " + LTrim( Str( ( s_nFail / s_nPass ) * 100, 6, 2 ) ) + " % )" + hb_eol() +;
      "                   ----------" + hb_eol() +;
      "            Total: " + Str( s_nPass + s_nFail ) +;
      " ( Time elapsed: " + LTrim( Str( s_nEndTime - s_nStartTime ) ) + " seconds )" + hb_eol() +;
      hb_eol() )

   IF s_nFail != 0
      IF "CLIPPER (R)" $ Upper( Version() )
         OutMsg( s_nFhnd, ;
            "WARNING ! Failures detected using CA-Cl*pper." + hb_eol() +;
            "Please fix those expected results which are not bugs in CA-Cl*pper itself." + hb_eol() )
      ELSE
         OutMsg( s_nFhnd, ;
            "WARNING ! Failures detected" + hb_eol() )
      ENDIF
   ENDIF

#ifdef __XPP__
   FClose( s_nFhnd )
#endif

   ErrorLevel( iif( s_nFail != 0, 1, 0 ) )

   RETURN

FUNCTION XToStr( xValue )

   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN '"' + xValue + '"'

   CASE cType == "N" ; RETURN LTrim( Str( xValue ) )
   CASE cType == "D" ; RETURN 'hb_SToD("' + DToS( xValue ) + '")'
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...}"
   CASE cType == "A" ; RETURN "{.[" + LTrim( Str( Len( xValue ) ) ) + "].}"
   CASE cType == "M" ; RETURN "M:" + '"' + xValue + '"'
   ENDCASE

   RETURN ""

FUNCTION XToStrE( xValue )

   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN xValue

   CASE cType == "N" ; RETURN LTrim( Str( xValue ) )
   CASE cType == "D" ; RETURN "0d" + iif( Empty( xValue ), "00000000", DToS( xValue ) )
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...}"
   CASE cType == "A" ; RETURN "{.[" + LTrim( Str( Len( xValue ) ) ) + "].}"
   CASE cType == "M" ; RETURN "M:" + xValue
   ENDCASE

   RETURN ""

FUNCTION XToStrX( xValue )

   LOCAL cType := ValType( xValue )

   LOCAL tmp
   LOCAL cRetVal

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 1 ), '" + Chr( 1 ) + "' )
      xValue := StrTran( xValue, Chr( 2 ), '" + Chr( 2 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN xValue

   CASE cType == "N" ; RETURN LTrim( Str( xValue ) )
   CASE cType == "D" ; RETURN DToS( xValue )
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...} -> " + XToStrX( Eval( xValue ) )
   CASE cType == "A"

      cRetVal := "{ "

      FOR tmp := 1 TO Len( xValue )
         cRetVal += XToStrX( xValue[ tmp ] )
         IF tmp < Len( xValue )
            cRetVal += ", "
         ENDIF
      NEXT

      RETURN cRetVal + " }"

   CASE cType == "M" ; RETURN "M:" + xValue
   ENDCASE

   RETURN ""

STATIC FUNCTION ErrorMessage( oError )

   LOCAL cMessage := ""
   LOCAL tmp

   IF ValType( oError:severity ) == "N"
      DO CASE
      CASE oError:severity == ES_WHOCARES     ; cMessage += "M "
      CASE oError:severity == ES_WARNING      ; cMessage += "W "
      CASE oError:severity == ES_ERROR        ; cMessage += "E "
      CASE oError:severity == ES_CATASTROPHIC ; cMessage += "C "
      ENDCASE
   ENDIF
   IF ValType( oError:genCode ) == "N"
      cMessage += LTrim( Str( oError:genCode ) ) + " "
   ENDIF
   IF ValType( oError:subsystem ) == "C"
      cMessage += oError:subsystem + " "
   ENDIF
   IF ValType( oError:subCode ) == "N"
      cMessage += LTrim( Str( oError:subCode ) ) + " "
   ENDIF
   IF ValType( oError:description ) == "C"
      cMessage += oError:description + " "
   ENDIF
   IF ! Empty( oError:operation )
      cMessage += "(" + oError:operation + ") "
   ENDIF
   IF ! Empty( oError:filename )
      cMessage += "<" + oError:filename + "> "
   ENDIF
   IF ValType( oError:osCode ) == "N"
      cMessage += "OS:" + LTrim( Str( oError:osCode ) ) + " "
   ENDIF
   IF ValType( oError:tries ) == "N"
      cMessage += "#:" + LTrim( Str( oError:tries ) ) + " "
   ENDIF

   IF ValType( oError:Args ) == "A"
      cMessage += "A:" + LTrim( Str( Len( oError:Args ) ) ) + ":"
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

STATIC FUNCTION ListToNArray( cString )

   LOCAL aArray := {}
   LOCAL nPos

   IF ! Empty( cString )
      DO WHILE ( nPos := At( ",", cString ) ) > 0
         AAdd( aArray, Val( AllTrim( Left( cString, nPos - 1 ) ) ) )
         cString := SubStr( cString, nPos + 1 )
      ENDDO

      AAdd( aArray, Val( AllTrim( cString ) ) )
   ENDIF

   RETURN aArray

STATIC FUNCTION CMDLGetValue( cCommandLine, cName, cRetVal )

   LOCAL tmp, tmp1

   IF ( tmp := At( cName, cCommandLine ) ) > 0
      IF ( tmp1 := At( " ", tmp := SubStr( cCommandLine, tmp + Len( cName ) ) ) ) > 0
           tmp := Left( tmp, tmp1 - 1 )
      ENDIF
      cRetVal := tmp
   ENDIF

   RETURN cRetVal

#ifdef __XPP__
FUNCTION hb_SToD( cDate )
   RETURN SToD( cDate )
#endif

#ifdef RT_NO_C
#ifndef __HARBOUR__
#ifndef __XPP__

FUNCTION hb_SToD( s )

   LOCAL cDf := Set( _SET_DATEFORMAT, "YYYY/MM/DD" ), dt

   dt := CToD( Stuff( Stuff( s, 7, 0, "/" ), 5, 0, "/" ) )
   Set( _SET_DATEFORMAT, cDf )

   RETURN dt

#endif
#endif
#endif

STATIC FUNCTION BADFNAME()
#ifdef __PLATFORM__UNIX
   RETURN "*BADNAM/*.MEM"
#else
   RETURN "*BADNAM*.MEM"
#endif

STATIC PROCEDURE OutMsg( hFile, cMsg )

   IF hFile == 1
      OutStd( cMsg )
   ELSEIF hFile == 2
      OutErr( cMsg )
   ELSE
      FWrite( hFile, cMsg )
   ENDIF

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
