/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (main)
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* TRANSFORM() tests mostly written by Eddie Runia <eddie@runia.com> */
/* EMPTY() tests written by Eddie Runia <eddie@runia.com> */
/* :class* tests written by Dave Pearson <davep@hagbard.demon.co.uk> */

/* NOTE: The test suite will not work if the DTOS() function is not included
         in Harbour (-DHB_COMPAT_XPP). */
/* NOTE: Always compile with /n switch */
/* NOTE: It's worth to make tests with and without the /z switch */
/* NOTE: Guard all Harbour extensions with __HARBOUR__ #ifdefs */

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
   #xtranslate HB_OSNewLine() => ( Chr( 13 ) + Chr( 10 ) )
#endif

STATIC s_nPass
STATIC s_nFail
STATIC s_cFileName
STATIC s_nFhnd
STATIC s_nCount
STATIC s_lShowAll
STATIC s_lShortcut
STATIC s_aSkipList
STATIC s_nStartTime
STATIC s_nEndTime

FUNCTION Main( cPar1, cPar2 )

   OutStd( "Harbour Regression Test Suite" + HB_OSNewLine() +;
           "Copyright 1999-2000, http://www.harbour-project.org" + HB_OSNewLine() )

   IF cPar1 == NIL
      cPar1 := ""
   ENDIF
   IF cPar2 == NIL
      cPar2 := ""
   ENDIF

   IF "/?" $ Upper( cPar1 ) .OR. ;
      "/H" $ Upper( cPar1 ) .OR. ;
      "-?" $ Upper( cPar1 ) .OR. ;
      "-H" $ Upper( cPar1 )

      OutStd( HB_OSNewLine() +;
              "Syntax:  hbtest [options]" + HB_OSNewLine() +;
              HB_OSNewLine() +;
              "Options:  /h, /?        Display this help." + HB_OSNewLine() +;
              "          /all          Display all tests, not only the failures." + HB_OSNewLine() +;
              "          /skip:<list>  Skip the listed test numbers." + HB_OSNewLine() )

      RETURN NIL
   ENDIF

   /* Initialize test */

   TEST_BEGIN( cPar1 + " " + cPar2 )

   Main_HVM()
   Main_MATH()
   Main_DATE()
   Main_STR()
   Main_TRANS()
   Comp_Str()
   Exact_Str()
#ifdef __HARBOUR__
   New_STRINGS()
   Long_STRINGS()
#endif
   Main_ARRAY()
   Main_FILE()
   Main_MISC()
#ifdef __HARBOUR__
   Main_OPOVERL()
#endif
   Main_LAST()

   /* Show results, return ERRORLEVEL and exit */

   TEST_END()

   RETURN NIL

/* NOTE: These should always be called last, since they can mess up the test
         environment.

         Right now the failing __MRestore() will clear all memory variables,
         which is absolutely normal otherwise. */

STATIC FUNCTION Main_LAST()

   TEST_LINE( MEMVARBLOCK( "mcString" )           , "{||...}"                                         )
   TEST_LINE( __MRestore()                        , "E BASE 2007 Argument error __MRESTORE "          )
   TEST_LINE( MEMVARBLOCK( "mcString" )           , "{||...}"                                         )
   TEST_LINE( __MSave()                           , "E BASE 2008 Argument error __MSAVE "             )
   TEST_LINE( __MRestore( "$NOTHERE.MEM", .F. )   , "E BASE 2005 Open error $NOTHERE.MEM F:DR"        )
   TEST_LINE( MEMVARBLOCK( "mcString" )           , NIL                                               )
   TEST_LINE( __MSave( "*BADNAM*.MEM", "*", .T. ) , "E BASE 2006 Create error *BADNAM*.MEM F:DR"      )

   RETURN NIL

STATIC FUNCTION TEST_BEGIN( cParam )

   s_nStartTime := Seconds()

   s_lShowAll := "/ALL" $ Upper( cParam ) .OR. ;
                 "-ALL" $ Upper( cParam )

   s_aSkipList := ListToNArray( CMDLGetValue( Upper( cParam ), "/SKIP:", "" ) )
   IF Empty( s_aSkipList )
      s_aSkipList := ListToNArray( CMDLGetValue( Upper( cParam ), "-SKIP:", "" ) )
   ENDIF

   /* Detect presence of shortcutting optimization */

   s_lShortcut := .T.
   IF .T. .OR. Eval( {|| s_lShortcut := .F. } )
      /* Do nothing */
   ENDIF

   /* Decide about output filename */

   DO CASE
   CASE "HARBOUR" $ Upper( Version() )     ; s_cFileName := "rtl_test.hb"
   CASE "CLIPPER (R)" $ Upper( Version() ) .AND. ;
        "5.3" $ Version()                  ; s_cFileName := "rtl_test.c53"
   CASE "CLIPPER (R)" $ Upper( Version() ) ; s_cFileName := "rtl_test.c5x"
   ENDCASE

   s_nFhnd := 1 /* FHND_STDOUT */
   s_cFileName := "(stdout)"

   s_nCount := 0
   s_nPass := 0
   s_nFail := 0

   /* Set up the initial state */

/* TODO: Need to add this, when multi language support will be available
         to make sure all error messages comes in the original English
         language. */
/* SET LANGID TO EN */
   SET DATE ANSI
   SET CENTURY ON
   SET EXACT OFF

   FErase( "NOT_HERE.$$$" )

   /* Feedback */

   /* NOTE: The 0 parameter of Version() will force Harbour to include the
            compiler version in the version string. */

   FWrite( s_nFhnd, "---------------------------------------------------------------------------" + HB_OSNewLine() +;
                    "      Version: " + Version( 0 ) + HB_OSNewLine() +;
                    "           OS: " + OS() + HB_OSNewLine() +;
                    "   Date, Time: " + DToC( Date() ) + " " + Time() + HB_OSNewLine() +;
                    "       Output: " + s_cFileName + HB_OSNewLine() +;
                    "Shortcut opt.: " + iif( s_lShortcut, "ON", "OFF" ) + HB_OSNewLine() +;
                    "     Switches: " + cParam + HB_OSNewLine() +;
                    "===========================================================================" + HB_OSNewLine() )

   FWrite( s_nFhnd, PadR( "R", TEST_RESULT_COL1_WIDTH ) + " " +;
                    PadR( "No.  Line", TEST_RESULT_COL2_WIDTH ) + " " +;
                    PadR( "TestCall()", TEST_RESULT_COL3_WIDTH ) + " -> " +;
                    PadR( "Result", TEST_RESULT_COL4_WIDTH ) + " | " +;
                    PadR( "Expected", TEST_RESULT_COL5_WIDTH ) + HB_OSNewLine() +;
                    "---------------------------------------------------------------------------" + HB_OSNewLine() )

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

   rddSetDefault( "DBFNTX" )

   dbCreate( "!TEMP!.DBF",;
      { { "TYPE_C"   , "C", 15, 0 } ,;
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

   USE ( "!TEMP!.DBF" ) NEW ALIAS w_TEST EXCLUSIVE

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

   RETURN NIL

FUNCTION TEST_CALL( cBlock, bBlock, xResultExpected )
   LOCAL xResult
   LOCAL oError
   LOCAL bOldError
   LOCAL lPPError
   LOCAL lFailed
   LOCAL lSkipped

   s_nCount++

   IF !( ValType( cBlock ) == "C" )
      cBlock := "!! Preprocessor error !!"
      lPPError := .T.
   ELSE
      lPPError := .F.
   ENDIF

   lSkipped := aScan( s_aSkipList, s_nCount ) > 0

   IF lSkipped

      lFailed := .F.
      xResult := "!! Skipped !!"

   ELSE

      bOldError := ErrorBlock( {|oError| Break( oError ) } )

      BEGIN SEQUENCE
         xResult := Eval( bBlock )
      RECOVER USING oError
         xResult := ErrorMessage( oError )
      END SEQUENCE

      ErrorBlock( bOldError )

      IF !( ValType( xResult ) == ValType( xResultExpected ) )
         IF ValType( xResultExpected) == "C" .AND. ValType( xResult ) $ "ABMO"
            lFailed := !( XToStr( xResult ) == xResultExpected )
         ELSE
            lFailed := .T.
         ENDIF
      ELSE
         lFailed := !( xResult == xResultExpected )
      ENDIF

   ENDIF

   IF s_lShowAll .OR. lFailed .OR. lSkipped .OR. lPPError

      FWrite( s_nFhnd, PadR( iif( lFailed, "!", iif( lSkipped, "S", " " ) ), TEST_RESULT_COL1_WIDTH ) + " " +;
                       PadR( Str( s_nCount, 4 ) + " " + ProcName( 1 ) + "(" + LTrim( Str( ProcLine( 1 ), 5 ) ) + ")", TEST_RESULT_COL2_WIDTH ) + " " +;
                       PadR( cBlock, TEST_RESULT_COL3_WIDTH ) + " -> " +;
                       PadR( XToStr( xResult ), TEST_RESULT_COL4_WIDTH ) + " | " +;
                       PadR( XToStr( xResultExpected ), TEST_RESULT_COL5_WIDTH ) +;
                       HB_OSNewLine() )

   ENDIF

   IF lFailed
      s_nFail++
   ELSE
      s_nPass++
   ENDIF

   RETURN NIL

FUNCTION TEST_OPT_Z()
   RETURN s_lShortCut

STATIC FUNCTION TEST_END()

   dbSelectArea( "w_TEST" )
   dbCloseArea()
   fErase( "!TEMP!.DBF" )
   fErase( "!TEMP!.DBT" )

   s_nEndTime := Seconds()

   FWrite( s_nFhnd, "===========================================================================" + HB_OSNewLine() +;
                    "Test calls passed: " + Str( s_nPass ) + " ( " + LTrim( Str( ( 1 - ( s_nFail / s_nPass ) ) * 100, 6, 2 ) ) + " % )" + HB_OSNewLine() +;
                    "Test calls failed: " + Str( s_nFail ) + " ( " + LTrim( Str( ( s_nFail / s_nPass ) * 100, 6, 2 ) ) + " % )" + HB_OSNewLine() +;
                    "                   ----------" + HB_OSNewLine() +;
                    "            Total: " + Str( s_nPass + s_nFail ) +;
                    " ( Time elapsed: " + LTrim( Str( s_nEndTime - s_nStartTime ) ) + " seconds )" + HB_OSNewLine() +;
                    HB_OSNewLine() )

   IF s_nFail != 0
      IF "CLIPPER (R)" $ Upper( Version() )
         FWrite( s_nFhnd, "WARNING ! Failures detected using CA-Clipper." + HB_OSNewLine() +;
                          "Please fix those expected results which are not bugs in CA-Clipper itself." + HB_OSNewLine() )
      ELSE
         FWrite( s_nFhnd, "WARNING ! Failures detected" + HB_OSNewLine() )
      ENDIF
   ENDIF

   ErrorLevel( iif( s_nFail != 0, 1, 0 ) )

   RETURN NIL

FUNCTION XToStr( xValue )
   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr(0), '"+Chr(0)+"' )
      xValue := StrTran( xValue, Chr(9), '"+Chr(9)+"' )
      xValue := StrTran( xValue, Chr(10), '"+Chr(10)+"' )
      xValue := StrTran( xValue, Chr(13), '"+Chr(13)+"' )
      xValue := StrTran( xValue, Chr(26), '"+Chr(26)+"' )

      RETURN '"' + xValue + '"'

   CASE cType == "N" ; RETURN LTrim( Str( xValue ) )
   CASE cType == "D" ; RETURN 'SToD("' + DToS( xValue ) + '")'
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN '{||...}'
   CASE cType == "A" ; RETURN '{.[' + LTrim( Str( Len( xValue ) ) ) + '].}'
   CASE cType == "M" ; RETURN 'M:"' + xValue + '"'
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
   IF ValType( oError:subsystem ) == "C"
      cMessage += oError:subsystem() + " "
   ENDIF
   IF ValType( oError:subCode ) == "N"
      cMessage += LTrim( Str( oError:subCode ) ) + " "
   ENDIF
   IF ValType( oError:description ) == "C"
      cMessage += oError:description + " "
   ENDIF
   IF !Empty( oError:operation )
      cMessage += oError:operation + " "
   ENDIF
   IF !Empty( oError:filename )
      cMessage += oError:filename + " "
   ENDIF

#ifdef _COMMENT_
   IF ValType( oError:Args ) == "A"
      cMessage += "A:"
      FOR tmp := 1 TO Len( oError:Args )
         cMessage += ValType( oError:Args[ tmp ] )
//       cMessage += XToStr( oError:Args[ tmp ] )
//       IF tmp < Len( oError:Args )
//          cMessage += ";"
//       ENDIF
      NEXT
      cMessage += " "
   ENDIF
#endif

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

   IF !Empty( cString )
      DO WHILE ( nPos := At( ",", cString ) ) > 0
         aAdd( aArray, Val( AllTrim( Left( cString, nPos - 1 ) ) ) )
         cString := SubStr( cString, nPos + 1 )
      ENDDO

      aAdd( aArray, Val( AllTrim( cString ) ) )
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

#ifndef HAVE_HBCLIP
#ifndef __HARBOUR__
#ifndef __XPP__

FUNCTION SToD( cDate )
   LOCAL cOldDateFormat
   LOCAL dDate

   IF ValType( cDate ) == "C" .AND. !Empty( cDate )
      cOldDateFormat := Set( _SET_DATEFORMAT, "yyyy/mm/dd" )

      dDate := CToD( SubStr( cDate, 1, 4 ) + "/" +;
                     SubStr( cDate, 5, 2 ) + "/" +;
                     SubStr( cDate, 7, 2 ) )

      Set( _SET_DATEFORMAT, cOldDateFormat )
   ELSE
      dDate := CToD( "" )
   ENDIF

   RETURN dDate

#endif
#endif
#endif

/* Don't change the position of this #include. */
#include "rt_init.ch"

