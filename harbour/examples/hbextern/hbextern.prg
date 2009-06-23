/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbextern.ch generator
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* NOTE: The process is not completely automatical, the generated output should
         be edited by hand after extraction. */

#include "directry.ch"
#include "fileio.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

// Remark this line when BEGINDUMP/ENDDUMP #pragma's
// are not used anymore in Harbour core and RTL .prg files:
// #define PRG_CAN_HAVE_HB_FUNC

#ifdef __HARBOUR__
   #define EOL  hb_OSNewLine()
#else
   #define EOL  ( Chr( 13 ) + Chr( 10 ) )
#endif

#ifdef __PLATFORM__UNIX
   #define PATH_SEPARATOR "/"
   #define BASE_DIR "../../"
#else
   #define PATH_SEPARATOR "\"
   #define BASE_DIR "..\..\"
#endif

// List of known files which does not contain any real public function.
STATIC s_aSkipFiles := { "profiler.prg" }
STATIC s_aSkipDirs := { "tests", "example", "examples", "sample", "samples", ".svn", "%HB_ARCHITECTURE%", "codepage" }
STATIC s_aSkipNames  := { "MAIN" }   // Init with names you want to skip
STATIC s_aMethodAsProcedure := {}
STATIC s_aConditions := {}

#define sw_BaseDir                  1
#define sw_Target                   2
#define sw_Verbose                  3
#define sw_Recursive                4
#define sw_Case                     5
#define sw_ExcludeDuplicates        6
#define sw_ExcludeEmptyFiles        7
#define sw_ExcludeClasses           8
#define sw_ExcludeClassMethods      9
#define sw_ExcludeConditionals     10
#define sw_ConditionalDepth        11
#define sw_MimicHBExtern           12
#define sw_ExcludeParams           13
#define sw_SwitchesN               13

STATIC s_aSwitches[ sw_SwitchesN ]

PROCEDURE MAIN( ... )
   LOCAL aArgs := hb_AParams()
   LOCAL aDirs, i, nOutput, arg, cArgName

   SET DATE FORMAT TO "yyyy-mm-dd"

   s_aSwitches[ sw_BaseDir ] := BASE_DIR
   s_aSwitches[ sw_Target ] := "hbextern.ch_"
   s_aSwitches[ sw_Verbose ] := 2
   s_aSwitches[ sw_Recursive ] := .F.
   s_aSwitches[ sw_Case ] := 1
   s_aSwitches[ sw_ExcludeDuplicates ] := .T.
   s_aSwitches[ sw_ExcludeEmptyFiles ] := .F.
   s_aSwitches[ sw_ExcludeClasses ] := .T.
   s_aSwitches[ sw_ExcludeClassMethods ] := .T.
   s_aSwitches[ sw_ExcludeConditionals ] := .T.
   s_aSwitches[ sw_ConditionalDepth ] := 0
   s_aSwitches[ sw_MimicHBExtern ] := .T.
   s_aSwitches[ sw_ExcludeParams ] := .T.

   FOR EACH arg IN aArgs
      IF .NOT. Empty(arg)
         IF ( i := At( "=", arg ) ) == 0
            cArgName = arg
            arg = ""
         ELSE
            cArgName = SubStr(arg, 1, i - 1)
            arg = SubStr( arg, i + 1)
         ENDIF

         DO CASE
         CASE cArgName == "-?" .OR. cArgName == "--help"
            ShowHelp()
            RETURN
         CASE cArgName == "-source" ;           s_aSwitches[ sw_BaseDir ] := arg + IIf(SubStr(arg, -1, 1) == PATH_SEPARATOR, "", PATH_SEPARATOR)
         CASE cArgName == "-target" ;           s_aSwitches[ sw_Target ] := arg
         CASE cArgName == "-recurse" .OR. ;
              cArgName == "-recursive" ;        s_aSwitches[ sw_Recursive ] := ( arg == "+" .OR. arg == "1" .OR. arg == "yes" )
         CASE cArgName == "-skipdirs"
            s_aSkipDirs := FileToArray( arg )
         CASE cArgName == "-skipfiles";      s_aSkipFiles := FileToArray( arg )
         CASE cArgName == "-silent" ;        s_aSwitches[ sw_Verbose ] := 0
         CASE cArgName == "-verbose"
            DO CASE
            CASE arg == "silent";            s_aSwitches[ sw_Verbose ] := 0
            CASE arg == "minimum";           s_aSwitches[ sw_Verbose ] := 1
            CASE arg == "maximum";           s_aSwitches[ sw_Verbose ] := 2
            OTHERWISE ;                      s_aSwitches[ sw_Verbose ] := 2
            END CASE
         CASE cArgName == "-case"
            DO CASE
            CASE arg == "upper";             s_aSwitches[ sw_Case ] := 1
            CASE arg == "lower";             s_aSwitches[ sw_Case ] := 2
            CASE arg == "unchanged";         s_aSwitches[ sw_Case ] := 0
            OTHERWISE ;                      s_aSwitches[ sw_Case ] := 1
            END CASE
         CASE cArgName == "-exclude" .OR.  cArgName == "-include"
            DO CASE
            CASE arg == "all"
               s_aSwitches[ sw_ExcludeDuplicates ] := ( cArgName == "-exclude" )
               s_aSwitches[ sw_ExcludeEmptyFiles ] := ( cArgName == "-exclude" )
               s_aSwitches[ sw_ExcludeClasses ] := ( cArgName == "-exclude" )
               s_aSwitches[ sw_ExcludeClassMethods ] := ( cArgName == "-exclude" )
               s_aSwitches[ sw_ExcludeConditionals ] := ( cArgName == "-exclude" )
               s_aSwitches[ sw_ExcludeParams ] := ( cArgName == "-exclude" )
            CASE arg == "duplicates" ;       s_aSwitches[ sw_ExcludeDuplicates ] := ( cArgName == "-exclude" )
            CASE arg == "empty-files" ;      s_aSwitches[ sw_ExcludeEmptyFiles ] := ( cArgName == "-exclude" )
            CASE arg == "classes" ;          s_aSwitches[ sw_ExcludeClasses ] := ( cArgName == "-exclude" )
            CASE arg == "class-methods" ;    s_aSwitches[ sw_ExcludeClassMethods ] := ( cArgName == "-exclude" )
            CASE arg == "conditionals" ;     s_aSwitches[ sw_ExcludeConditionals ] := ( cArgName == "-exclude" )
            CASE arg == "params" ;           s_aSwitches[ sw_ExcludeParams ] := ( cArgName == "-exclude" )
            OTHERWISE
            END CASE
         OTHERWISE
         END CASE
      ENDIF
   NEXT

   FOR i := 1 to Len(s_aSkipDirs)
      IF SubStr( s_aSkipDirs[i], 1, 1 ) == "%"
         s_aSkipDirs[i] := GetEnv( SubStr( s_aSkipDirs[i], 2, Len( s_aSkipDirs[i] ) - 1 ) )
      ENDIF
   NEXT

   AAdd( s_aSkipDirs, "." )
   AAdd( s_aSkipDirs, ".." )

   s_aSwitches[ sw_MimicHBExtern ] := ;
      ( ;
         s_aSwitches[ sw_ExcludeClasses ] .AND. ;
         s_aSwitches[ sw_ExcludeClassMethods ] ;
      )

   IF s_aSwitches[ sw_ExcludeClasses ] .AND. .NOT. s_aSwitches[ sw_ExcludeClassMethods ]
      s_aSwitches[ sw_ExcludeClassMethods ] := .T.
   ENDIF

   IF s_aSwitches[ sw_Recursive ]

      aDirs :={ ;
         s_aSwitches[ sw_BaseDir ] + "source", ;
         s_aSwitches[ sw_BaseDir ] + "contrib", ;
      }

   ELSE

      aDirs :={ ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "debug", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "pp", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "dbfcdx", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "dbffpt", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "dbfnsx", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "dbfntx", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "hbsix", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "hsx", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "nulsys", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rdd" + PATH_SEPARATOR + "usrrdd", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "rtl", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "vm", ;
      }

   ENDIF

   IF s_aSwitches[ sw_Verbose ] > 0; ? "Processing files:" ; ENDIF

#define YesOrNo(b) IIf((b), "Yes", "No")

   nOutput := FCreate( s_aSwitches[ sw_Target ] )
   IF nOutput > 0
      FWrite( nOutput, "// NOTE: Machine generated on: " + DTOC( DATE() ) + EOL + ;
                       "//       This output should be edited by hand after extraction." + EOL + EOL )
      FWrite( nOutput, ;
         "/*"  + EOL + ;
         "  BaseDir" + "=" + s_aSwitches[ sw_BaseDir ] + EOL + ;
         "  Recursive" + "=" + YesOrNo( s_aSwitches[ sw_Recursive ] ) + EOL + ;
         "  Verbose" + "=" + { "silent", "minimum", "maximum" }[ s_aSwitches[ sw_Verbose ] + 1 ] + EOL + ;
         "  Case" + "=" + { "unchanged", "upper", "lower" }[ s_aSwitches[ sw_Case ] + 1 ] + EOL + ;
         "  Exclude duplicates" + "=" + YesOrNo( s_aSwitches[ sw_ExcludeDuplicates ] ) + EOL + ;
         "  Exclude empty files" + "=" + YesOrNo( s_aSwitches[ sw_ExcludeEmptyFiles ] ) + EOL + ;
         "  Exclude classes" + "=" + YesOrNo( s_aSwitches[ sw_ExcludeClasses ] ) + EOL + ;
         "  Exclude class methods" + "=" + YesOrNo( s_aSwitches[ sw_ExcludeClassMethods ] ) + EOL + ;
         "  Exclude conditionals" + "=" + YesOrNo( s_aSwitches[ sw_ExcludeConditionals ] ) + EOL + ;
         "  Exclude params" + "=" + YesOrNo( s_aSwitches[ sw_ExcludeParams ] ) + EOL + ;
         "  Mimic HBExtern" + "=" + YesOrNo( s_aSwitches[ sw_MimicHBExtern ] ) + EOL + ;
         "*/" + EOL + ;
         EOL ;
      )
      FOR i := 1 TO LEN( aDirs )
         IF .NOT. Empty(aDirs[i])
            ProcessDir( nOutput, aDirs[i] )
         ENDIF
      NEXT
      FWrite( nOutput, EOL + "// " + Replicate( "-", 60 ) + EOL + EOL )

      IF Len(s_aMethodAsProcedure) > 0
         FWrite( nOutput, "/*" + EOL + "Class methods defined as 'procedure':" + EOL )
         AEval(s_aMethodAsProcedure, {|ac| FWrite( nOutput, "  " + ac[1] + " " + ac[2] + EOL ) } )
         FWrite( nOutput, "*/" + EOL + EOL )
      ENDIF

      FClose( nOutput )
   ENDIF

   IF s_aSwitches[ sw_Verbose ] > 0 ; ? "Done." ; ? ; END

   RETURN

STATIC PROCEDURE ShowHelp()
   LOCAL aHelp := { ;
      "Syntax: ", ;
      "  hbextern [options]", ;
      "options:", ;
      "  -source=<folder> // source folder, default is .." + PATH_SEPARATOR + "..", ;
      "  -target=<filename> // target file, default is hbextern.ch_", ;
      "  -recurse=[yes|no] // perform recursively, default is no", ;
      "  -skipdirs=<filename> // configuration file of folders to bypass, default:", ;
      {|| AEval( s_aSkipDirs, {|c| OutStd( IIf( Empty(c), "", "    " + c ) + hb_osNewLine() ) } ) }, ;
      "  -skipfiles=<filename> // configuration file of files to bypass, default:", ;
      {|| AEval( s_aSkipFiles, {|c| OutStd( IIf( Empty(c), "", "    " + c ) + hb_osNewLine() ) } ) }, ;
      "  -verbose=[silent|minimum|maximum] // verbose operation, default is maximum", ;
      "  -silent // synonym for verbose=silent", ;
      "  -case=[upper|lower|unchanged] // output case, default is upper", ;
      "  -include=<switch> // sets <switch> on", ;
      "  -exclude=<switch> // sets <switch> off", ;
      "switches:", ;
      "  all // all switches are set", ;
      "  duplicates // default is on", ;
      "  empty-files // default is off", ;
      "  classes // default is off", ;
      "  class-methods // default is off", ;
      "  conditionals // default is off", ;
      "  params // default is off", ;
/*       "  ", */ ;
   }

   // using hbmk2 style
   AEval( aHelp, {|x| IIf( ValType( x ) == "B", Eval( x ), OutStd( IIf( Empty(x), "", x ) + hb_osNewLine() ) ) } )

   RETURN

STATIC PROCEDURE ProcessDir( nOutput, cDir )
   LOCAL i, nLen, aFiles

   IF s_aSwitches[ sw_Verbose ] > 0 ; ? cDir ; ENDIF

   IF .NOT. s_aSwitches[ sw_ExcludeEmptyFiles ]
      FWrite( nOutput, EOL + "// " + Replicate( "-", 60 ) + EOL )
      FWrite( nOutput, "// Files from: " + cDir + EOL + EOL )
   ENDIF

   cDir += PATH_SEPARATOR

   aFiles := Directory( cDir + "*.*", "D" )
   IF ( nLen := LEN( aFiles ) ) > 0
      // Sort C files before PRG files before folders; this mimics HBExtern
      ASort( aFiles,,, {|x,y| ;
            IIf( x[ F_ATTR ] == "D", Chr(255), SubStr( x[ F_NAME ], At( ".", x[ F_NAME ] ) ) ) + x[ F_NAME ] < IIf( y[ F_ATTR ] == "D", Chr(255), SubStr( y[ F_NAME ], At( ".", y[ F_NAME ] ) )  ) + y[ F_NAME ] ;
         } )
      FOR i := 1 TO nLen
         IF aFiles[ i ][F_ATTR ] == "D"
            IF s_aSwitches[ sw_Recursive ] .AND. ;
               AScan( s_aSkipDirs, {|d| Lower(d) == Lower( aFiles[ i ][ F_NAME ] ) } ) == 0
               ProcessDir( nOutput, cDir + aFiles[ i ][ F_NAME ] )
            ENDIF
         ELSEIF Upper( SubStr( aFiles[ i ][ F_NAME ], -4 ) ) == ".PRG"
            ProcessFile( nOutput, cDir + aFiles[ i ][ F_NAME ], .T. )
         ELSEIF Upper( SubStr( aFiles[ i ][ F_NAME ], -2 ) ) == ".C"
            ProcessFile( nOutput, cDir + aFiles[ i ][ F_NAME ], .F. )
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION FileToArray( cFile )
   LOCAL nH
   LOCAL aResult := {}

   nH := FOpen( cFile )
   IF nH > 0
      FileEval( nH, 255, {|c| AAdd( aResult, c ) } )
      FClose( nH )
   ELSE
      IF s_aSwitches[ sw_Verbose ] > 0 ; ? "Could not open [" + cFile + "], error" + hb_ntos( nH ) ; ENDIF
   END

   RETURN aResult

STATIC PROCEDURE ProcessFile( nOutput, cFile, lPRG )
   LOCAL nH
   LOCAL bOutputHeader
   LOCAL cHeader

   // Skip known files which does not contain any real public function
   IF AScan( s_aSkipFiles, {|c| lower(c) $ lower( cFile ) } ) > 0
      RETURN
   ENDIF

   IF s_aSwitches[ sw_Verbose ] > 1 ; ? cFile ; ENDIF

   cHeader := "//" + EOL + "// symbols from file: " + cFile + EOL + "//" + EOL

   IF .NOT. s_aSwitches[ sw_ExcludeEmptyFiles ]
      FWrite( nOutput, cHeader )
      bOutputHeader := .F.
   ELSE
      bOutputHeader := .T.
   ENDIF

   nH := FOpen( cFile )
   IF nH > 0
      FileEval( nH, 255, {|c| ProcessLine( nOutput, cFile, c, lPRG, @bOutputHeader, @cHeader ) } )
      FClose( nH )
      FWrite( nOutput, Replicate( "#endif" + EOL, s_aSwitches[ sw_ConditionalDepth ] ) )
      s_aSwitches[ sw_ConditionalDepth ] := 0
   ELSE
      IF s_aSwitches[ sw_Verbose ] > 0 ; ? "Could not process [" + cFile + "], error " + hb_ntos(nH) ; ENDIF
   ENDIF

   ASize( s_aConditions, 0 )

   RETURN

STATIC FUNCTION Parse(cVar, xDelimiter)
   LOCAL cResult
   LOCAL idx

   IF ValType(xDelimiter) == "N"
      cResult := SubStr( cVar, 1, xDelimiter )
      cVar := SubStr( cVar, xDelimiter + 1 )
   ELSE
      IF ( idx := At( xDelimiter, cVar ) ) == 0
         cResult := cVar
         cVar := ""
      ELSE
         cResult := SubStr( cVar, 1, idx - 1 )
         cVar := SubStr( cVar, idx + Len(xDelimiter) )
      ENDIF
   ENDIF

   RETURN cResult

STATIC FUNCTION Split(cVar, cDelimiter)
   LOCAL aResult := {}
   LOCAL clVar := cVar

   DO WHILE Len(clVar) > 0
      AAdd(aResult, Parse( @clVar, cDelimiter ) )
   ENDDO

   RETURN aResult

STATIC FUNCTION RemoveComments(cLine)
   LOCAL cResult := Trim(cLine)
   LOCAL idx
   LOCAL idx2

   IF SubStr( cLine, 1, 1 ) == "*" .OR. SubStr( cLine, 1, 2 ) == "&&"
      cResult := ""
   ELSEIF ( idx := At( "//", cResult ) ) > 0
      cResult := SubStr( cResult, 1, idx - 1 )
   END

   DO WHILE ( idx := At( "/*", cResult ) ) > 0
      IF ( idx2 := At( "*/", cResult ) ) == 0 ; idx2 := Len(cResult) ; ENDIF
      cResult := SubStr( cResult, 1, idx - 1 ) + SubStr( cResult, idx2 + 2 )
   ENDDO

   RETURN cResult


STATIC PROCEDURE ProcessLine( nOutput, cFile, cLine, lPRG, bOutputHeader, cHeader )
STATIC s_cMethodType, s_lVisible, s_FullLine := ""
   LOCAL nPos, nLen, aUnits

   cLine := AllTrim( cLine )
   IF Empty( cLine )
      RETURN
   ENDIF

   IF SubStr( cLine, -1, 1 ) == IIf( lPRG, ";", "\" )
      s_FullLine += AllTrim( SubStr( cLine, 1, Len( cLine ) - 1 ) )
      RETURN
   ENDIF

   cLine := s_FullLine + cLine
   s_FullLine := ""

   IF lPRG                                // PRG source file (FUNC, PROC, CLASS)

      IF .NOT. s_aSwitches[ sw_ExcludeConditionals ] .AND. SubStr( cLine, 1, 1 ) == "#"
         IF SubStr( cLine, 1, 2 ) == "# "
            cLine := "#" + LTrim( SubStr( cLine, 2 ) )
         ENDIF

         IF .F.
/*
         ELSEIF Lower( SubStr( cLine, 1, Len( "#include" ) ) ) == "#include"
         ELSEIF Lower( SubStr( cLine, 1, Len( "#define" ) ) ) == "#define"
         ELSEIF Lower( SubStr( cLine, 1, Len( "#undef" ) ) ) == "#undef"
         ELSEIF Lower( SubStr( cLine, 1, Len( "#pragma " ) ) ) == "#pragma "
         ELSEIF Lower( SubStr( cLine, 1, Len( "#command" ) ) ) == "#command"
         ELSEIF Lower( SubStr( cLine, 1, Len( "#xcommand" ) ) ) == "#xcommand"
         ELSEIF Lower( SubStr( cLine, 1, Len( "#translate" ) ) ) == "#translate"
         ELSEIF Lower( SubStr( cLine, 1, Len( "#xtrans" ) ) ) == "#xtrans"
         ELSEIF Lower( SubStr( cLine, 1, Len( "#error " ) ) ) == "#error "
*/
         ELSEIF Lower( SubStr( cLine, 1, Len( "#if" ) ) ) == "#if"
            AAdd( s_aConditions, cLine )
         ELSEIF Lower( SubStr( cLine, 1, Len( "#elif" ) ) ) == "#elif"
            s_aConditions[ Len( s_aConditions ) ] := cLine
         ELSEIF Lower( SubStr( cLine, 1, Len( "#else" ) ) ) == "#else"
            IF s_aSwitches[ sw_ConditionalDepth ] > 0
               FWrite( nOutput, "#else" + EOL )
            ENDIF
            s_aConditions[ Len( s_aConditions ) ] := cLine
         ELSEIF Lower( SubStr( cLine, 1, Len( "#endif" ) ) ) == "#endif"
            IF s_aSwitches[ sw_ConditionalDepth ] > 0
               FWrite( nOutput, "#endif" + EOL )
               s_aSwitches[ sw_ConditionalDepth ]--
            ENDIF
            ASize( s_aConditions, Len( s_aConditions ) - 1 )
      //~ ELSE
         //~ ? cLine
         ENDIF
      ENDIF

      IF Upper( SubStr( cLine, 1, Len( "FUNC " ) ) ) == "FUNC " .OR. ;
         Upper( SubStr( cLine, 1, Len( "PROC " ) ) ) == "PROC " .OR. ;
         Upper( SubStr( cLine, 1, Len( "FUNCTION " ) ) ) == "FUNCTION " .OR. ;
         Upper( SubStr( cLine, 1, Len( "PROCEDURE " ) ) ) == "PROCEDURE " .OR. ;
         .NOT. s_aSwitches[ sw_ExcludeClasses ] .AND. ;
         ( ;
            Upper( SubStr( cLine, 1, Len( "CLASS " ) ) ) == "CLASS " .OR. ;
            Upper( SubStr( cLine, 1, Len( "CREATE CLASS " ) ) ) == "CREATE CLASS " ;
         ) .OR. ;
         .F.

         DO CASE
         CASE Upper( SubStr( cLine, 1, Len( "CLASS " ) ) ) == "CLASS "
            s_cMethodType := "CLASS"
            s_lVisible := .T. // TODO: what is the default visibility of methods
         CASE Upper( SubStr( cLine, 1, Len( "CREATE CLASS " ) ) ) == "CREATE CLASS "
            s_cMethodType := "CLASS"
            cLine := SubStr( cLine, Len("CREATE ") + 1 )
            s_lVisible := .T. // TODO: what is the default visibility of methods
         OTHERWISE
            s_cMethodType = Upper( SubStr( cLine, 1, 4 ) )
         END

         aUnits := Split(RemoveComments(cLine), " ")
         IF Upper( SubStr( cLine, 1, Len( "PROC" ) ) ) == "PROC" .AND. Len(aUnits) > 1 .AND. Upper( aUnits[ Len( aUnits ) - 1 ] ) == "CLASS"
            AAdd(s_aMethodAsProcedure, { cFile, cLine } )
         ELSEIF ( nPos := AT( " ", cLine ) ) > 4
            cLine := LTrim( SubStr( cLine, nPos ) )
            nLen  := len( cLine )
            FOR nPos := 1 TO nLen
                IF SubStr( cLine, nPos, 1 ) $ " (;/&" + Chr( 9 )
                   --nPos
                   EXIT
                ENDIF
            NEXT
            WriteSymbol( nOutput, SubStr( cLine, 1, nPos ), s_cMethodType + " " + RemoveComments( SubStr( cLine, nPos + 1 ) ), @bOutputHeader, @cHeader )
         ENDIF
      ELSEIF s_cMethodType == "CLASS"
         cLine := RemoveComments(cLine)
         IF Upper( SubStr( cLine, 1, Len( "HIDDEN:" ) ) ) == "HIDDEN:"
            s_lVisible := .F.
         ELSEIF Upper( SubStr( cLine, 1, Len( "EXPORTED:" ) ) ) == "EXPORTED:"
            s_lVisible := .T.
         ELSEIF Upper( SubStr( cLine, 1, Len( "VAR " ) ) ) == "VAR " .OR. ;
            Upper( SubStr( cLine, 1, Len( "DATA " ) ) ) == "DATA " .OR. ;
            Upper( SubStr( cLine, 1, Len( "METHOD " ) ) ) == "METHOD " .OR. ;
            Upper( SubStr( cLine, 1, Len( "ACCESS " ) ) ) == "ACCESS " .OR. ;
            Upper( SubStr( cLine, 1, Len( "ASSIGN " ) ) ) == "ASSIGN " .OR. ;
            Upper( SubStr( cLine, 1, Len( "FRIEND " ) ) ) == "FRIEND "
            IF s_lVisible .AND. .NOT. s_aSwitches[ sw_ExcludeClassMethods ]
               cLine := Parse( @cLine, " INIT " )
               cLine := Parse( @cLine, " INLINE " )
               cLine := Trim( cLine )
               WriteSymbol( nOutput, "   " + cLine, ""/*s_cMethodType*/, @bOutputHeader, @cHeader ) // SubStr( cLine, 1, nPos ), s_cMethodType )
            ENDIF
         ELSEIF Upper( SubStr( cLine, 1, Len( "ENDCLASS" ) ) ) == "ENDCLASS"
            s_cMethodType := ""
         ENDIF

      ENDIF

#ifdef PRG_CAN_HAVE_HB_FUNC
   ENDIF
#else
   ELSE                                   // C source file (HB_FUNC)
#ENDIF

      IF SubStr( cLine, 1, 8 ) == "HB_FUNC("
         cLine := SubStr( cLine, 9 )
         IF ( nPos := AT( ")", cLine ) ) > 0
            WriteSymbol( nOutput, AllTrim( SubStr( cLine, 1, nPos - 1 ) ), "C function", @bOutputHeader, @cHeader )
         ENDIF
      ENDIF

#ifndef PRG_CAN_HAVE_HB_FUNC
   ENDIF
#ENDIF

   RETURN

STATIC PROCEDURE FileEval( nHandle, nMaxLine, bBlock )
   LOCAL cBuffer

   FSeek( nHandle, 0 )

   DO WHILE FReadLn( nHandle, @cBuffer, nMaxLine )
      Eval( bBlock, cBuffer )
   ENDDO

   RETURN

STATIC FUNCTION FReadLn( nHandle, cBuffer, nMaxLine )
STATIC s_aEOL := { chr(13) + chr(10), chr(10), chr(13) }
   LOCAL cLine, nSavePos, nEol, nNumRead, nLenEol, idx

   cBuffer := ""

   nSavePos := FSeek( nHandle, 0, FS_RELATIVE )
   cLine := space( nMaxLine )
   nNumRead := FRead( nHandle, @cLine, Len( cLine ) )
   cLine := SubStr( cLine, 1, nNumRead )

   nEol := 0
   FOR idx := 1 To Len(s_aEOL)
      // TOFIX: if the last character is CR check if next character is LF
      IF ( nEol := At( s_aEOL[ idx ], cLine ) ) > 0
         nLenEol := Len( s_aEOL[ idx ] ) - 1
         Exit
      ENDIF
   NEXT

   IF nEol == 0
     cBuffer := cLine
   ELSE
     cBuffer := SubStr( cLine, 1, nEol - 1 )
     FSeek( nHandle, nSavePos + nEol + nLenEol, FS_SET )
   ENDIF

   RETURN nNumRead != 0

STATIC PROCEDURE WriteSymbol( nOutput, cLine, cMethodType, bOutputHeader, cHeader )
   IF Len( cLine ) > 0
      IF s_aSwitches[ sw_Case ] == 1
         cLine := Upper( cLine )
      ELSEIF s_aSwitches[ sw_Case ] == 2
         cLine := Lower( cLine )
      ENDIF
      IF AScan( s_aSkipNames , {|c| Upper(c) == Upper(cLine) } ) == 0
         IF bOutputHeader
            FWrite( nOutput, cHeader )
            bOutputHeader := .F.
         ENDIF

         IF s_aSwitches[ sw_ExcludeDuplicates ]
            AAdd( s_aSkipNames, cLine )
         ENDIF

         IF cMethodType <> "C function"
            IF s_aSwitches[ sw_ConditionalDepth ] == 0
               AEval( s_aConditions, {|c| s_aSwitches[ sw_ConditionalDepth ]++, FWrite( nOutput, c + EOL ) } )
            ENDIF
         ENDIF

         IF s_aSwitches[ sw_MimicHBExtern ].OR. Empty(cMethodType) .OR. s_aSwitches[ sw_ExcludeClassMethods ]
            cMethodType = ""
         ELSEIF .NOT. s_aSwitches[ sw_ExcludeClassMethods ]
            cMethodType = " // " + cMethodType
         ENDIF

         FWrite( nOutput, "EXTERNAL " + cLine + cMethodType + EOL )
      ENDIF
   ENDIF

   RETURN
