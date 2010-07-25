/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbextern.ch et al generator
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* NOTE: The process is not completely automatical, the generated output should
         be edited by hand after extraction. */

#include "directry.ch"
#include "fileio.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

// Remark this line when BEGINDUMP/ENDDUMP #pragma's
// are not used anymore in Harbour core and RTL .prg files:
// #define PRG_CAN_HAVE_HB_FUNC

#define BASE_DIR ".." + hb_ps() + ".." + hb_ps()

// List of known files which does not contain any real public function.
STATIC s_aSkipFiles := { "profiler.prg", "msg_tpl.c" }
STATIC s_aSkipDirs := { "tests", "examples", "sample", "samples", ".svn", "obj" }

STATIC s_aSkipNames  := { "MAIN" }   // Init with method names you want to skip
STATIC s_aDirsProcessed := {}
STATIC s_aMethodAsProcedure
STATIC s_aConditions := {}

#define sw_BaseDir                  1
#define sw_Verbose                  2
#define sw_Case                     3
#define sw_ExcludeDuplicates        4
#define sw_ExcludeEmptyFiles        5
#define sw_ExcludeClasses           6
#define sw_ExcludeClassMethods      7
#define sw_ExcludeConditionals      8
#define sw_ConditionalDepth         9
#define sw_MimicHBExtern           10
#define sw_ExcludeParams           11
#define sw_SwitchesN               11

STATIC s_aSwitches[ sw_SwitchesN ]

#xcommand DEFAULT <p> TO <v> => IF <p> == NIL ; <p> := <v> ; ENDIF

PROCEDURE MAIN( ... )
   LOCAL aArgs := hb_AParams()
   LOCAL i, nOutput, arg, cArgName
   LOCAL ao, ar, aa
   LOCAL cDescription
   LOCAL aActions

   SET DATE FORMAT TO "yyyy-mm-dd"
   SET EXACT ON

   s_aSwitches[ sw_BaseDir ] := BASE_DIR
   s_aSwitches[ sw_Verbose ] := 2
   s_aSwitches[ sw_Case ] := 1
   s_aSwitches[ sw_ExcludeDuplicates ] := .T.
   s_aSwitches[ sw_ExcludeEmptyFiles ] := .F.
   s_aSwitches[ sw_ExcludeClasses ] := .F.
   s_aSwitches[ sw_ExcludeClassMethods ] := .T.
   s_aSwitches[ sw_ExcludeConditionals ] := .F.
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
         CASE cArgName == "-source" ;           s_aSwitches[ sw_BaseDir ] := arg + IIf(SubStr(arg, -1, 1) == hb_ps(), "", hb_ps())
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
         s_aSkipDirs[i] := GetEnv( SubStr( s_aSkipDirs[i], 2, Len( s_aSkipDirs[i] ) - 2 ) )
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

#define ca51            "/* CA-Cl*pper compatible standard functions */"
#define ca51int         "/* CA-Cl*pper compatible internal functions */"
#define ca52und         "/* CA-Cl*pper 5.2 compatible undocumented functions */"
#define ca53            "/* CA-Cl*pper 5.3 compatible functions */"
#define ca53X52         "/* CA-Cl*pper 5.3 compatible, CA-Cl*pper 5.2 undocumented, functions */"
#define harb            "/* Harbour extensions */"
#define harbXcp         "/* Harbour extensions, codepage support */"
#define harbXns         "/* Harbour extensions violating extension namespace rules." + hb_eol() + "   See reasons in source. */"
#define dbgr            "/* The debugger interface */"
#define rdd             "/* RDD related symbols */"
#define rddSx           "/* RDD SX related symbols */"
#define hiper           "/* HiPer-SEEK compatible functions */"
#define cfts            "/* CFTS compatible functions */"
#define i18n            "/* i18n */"
#define cdpsp           "/* Codepage support */"
#define langsp          "/* lang support */"
#define scalar          "/* Scalar objects */"
#define xbasepp         "/* Xbase++ compatible functions */"
#define dosunkn         "/* DOS (?) */"
#define flagshp         "/* FlagShip extension */"

#define capt_Desc 1
#define capt_Cond 2
#define capt_Repository 3
#define Capture( desc, cond ) { desc, cond, {} }

   // store all entries that match the codeblock but build a sub-array of entries per conditionals
#define STDACTIONS ;
   { ;
      Capture( ca51   , {|a,b,c,f| a,b,c,f, .T. } ), ;
      Capture( ca51int, {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "__") ) == "__" } ), ;
      Capture( harb   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_") ) == "HB_" } ), ;
      Capture( dbgr   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "__DBG") ) == "__DBG" } ), ;
      Capture( rdd    , {|a,b,c,f| a,b,c, AT( hb_ps() + "rdd" + hb_ps(), f) > 0 } ), ;
      Capture( rddSx  , {|a,b,c,f| a,b, (SubStr(c, 1, 2) == "SX" .OR. SubStr(c, 1, 3) == "_SX" .OR. .F.) .AND. AT( hb_ps() + "rdd" + hb_ps(), f) > 0 } ), ;
      Capture( hiper  , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HS_") ) == "HS_" } ), ;
      Capture( cfts   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "CFTS") ) == "CFTS" } ), ;
      Capture( i18n   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_I18N") ) == "HB_I18N" } ), ;
   }

#define CDPACTIONS ;
   { ;
      Capture( cdpsp  , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_CODEPAGE_") ) == "HB_CODEPAGE_" } ), ;
   }

#define LNGACTIONS ;
   { ;
      Capture( langsp , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_LANG_") ) == "HB_LANG_" } ), ;
   }

#define ACN_OUTPUT 1
#define ACN_HEADER 2
#define ACN_FOLDER 3
#define ACN_ACTIONS 4

   // action codepage first so the folder will be ignored when actioning source
   aActions := {;
      { "hbextcdp.ch_", "HB_EXTCDP_CH_", "source" + hb_ps() + "codepage", CDPACTIONS }, ;
      { "hbextlng.ch_", "HB_EXTLNG_CH_", "source" + hb_ps() + "lang", LNGACTIONS }, ;
      { "hbextern.ch_", "HB_EXTERN_CH_", "source", STDACTIONS }, ;
   }

   AEval( ;
      Directory( s_aSwitches[ sw_BaseDir ] + "contrib" + hb_ps() + "*", "D" ), ;
      {|ad| IIf( HB_AScan( s_aSkipDirs, ad[ F_NAME ] ) == 0 .AND. ad[ F_ATTR ] == "D", ;
              AAdd( aActions, ;
               { ad[ F_NAME ] + ".ch_", ;
                 "HB_CONTRIB_EXTERN_" + Upper( ad[ F_NAME ] ) + "_CH_", ;
                 "contrib" + hb_ps() + ad[ F_NAME ], ;
                 { Capture( "", {|a_,b_,c_,f_| a_,b_,c_,f_, .T. } ) } } ), ;
               ) ;
      } ;
   )

   IF s_aSwitches[ sw_Verbose ] > 0; ? "Processing files:" ; ENDIF

   FOR EACH aa IN aActions
      IF .NOT. EMPTY( aa )

         IF s_aSwitches[ sw_Verbose ] > 0
            ?
            ? "Output to: " + aa[ ACN_OUTPUT ]
            ? "Folder is: " + aa[ ACN_FOLDER ]
            ?
         ENDIF

         IF ( nOutput := FCreate( aa[ ACN_OUTPUT ] ) ) > 0

            IF SubStr( aa[ ACN_FOLDER ], 1, Len( "contrib" ) ) == "contrib"
               CopyGenericCopyrightToTarget( StrTran( aa[ ACN_OUTPUT ], ".ch_", ".ch" ), nOutput )
            ELSE
               CopyExistingSourceToTarget( StrTran( aa[ ACN_OUTPUT ], ".ch_", ".ch" ), nOutput )
            ENDIF

            FWrite( nOutput, ;
               "// NOTE: Machine generated on: " + DTOC( DATE() ) + hb_eol() + ;
               "//       This output should be edited by hand after extraction." + hb_eol() + ;
               hb_eol() + ;
               "#ifndef " + aa[ ACN_HEADER ] + hb_eol() + ;
               "#define " + aa [ ACN_HEADER ] + hb_eol() + ;
               hb_eol() ;
            )

            s_aMethodAsProcedure := {}

            ProcessDir( s_aSwitches[ sw_BaseDir ] + aa[ ACN_FOLDER ], aa[ ACN_ACTIONS ] )

            IF Len(s_aMethodAsProcedure) > 0
               FWrite( nOutput, "/*" + hb_eol() + "Class methods defined as 'procedure':" + hb_eol() )
               AEval(s_aMethodAsProcedure, {|ac| FWrite( nOutput, "  " + ac[1] + " " + ac[2] + hb_eol() ) } )
               FWrite( nOutput, "*/" + hb_eol() + hb_eol() )
            ENDIF

            FOR EACH ao IN aa[ 4 ]
               IF ao != NIL
                  IF Len( ao[ capt_Desc ] ) > 0
                     FWrite( nOutput, ao[ capt_Desc ] + hb_eol() )
                  ENDIF
                  IF LEn( ao[ capt_Repository ] ) == 0
                     FWrite( nOutput, "/* empty */" + hb_eol() )
                  ELSE
                     FWrite( nOutput, hb_eol() )
                     FOR EACH ar in ao[ capt_Repository ]
                        IF ar[ capt_Cond ] != NIL .AND. Len( ar[ capt_Cond ] ) > 0 ; FWrite( nOutput, ar[ capt_Cond ] + hb_eol() ); ENDIF

                        ASort( ar[ capt_Repository ] )
                        AEval( ar[ capt_Repository ], {|a| FWrite( nOutput, "EXTERNAL " + a + hb_eol() ) } )

                        IF ar[ capt_Cond ] != NIL .AND. Len( ar[ capt_Cond ] ) > 0
                           cDescription := ""
                           DO WHILE Len( ar[ capt_Cond ] ) > 0
                              cDescription := "#endif /* " + Parse( @ar[ capt_Cond ], hb_eol() ) + " */" + hb_eol() + cDescription
                           ENDDO
                           FWrite( nOutput, cDescription )
                        ENDIF

                        FWrite( nOutput, hb_eol() )
                     NEXT
                  ENDIF
                  IF Len( ao[ capt_Desc ] ) > 0
                     FWrite( nOutput, Stuff( ao[ capt_Desc ], 4, 0, "End of ") + hb_eol() + hb_eol() )
                  ELSE
                     FWrite( nOutput, hb_eol() )
                  ENDIF
               ENDIF
            NEXT

            FWrite( nOutput, ;
               "#endif /* " + aa [ ACN_HEADER ] + " */" + hb_eol() ;
            )

            FClose( nOutput )
         ELSE
            ? "error condition: " + HB_NTOS( Abs( nOutput ) )
            ? "Could not create " + aa[ ACN_OUTPUT ]
         ENDIF
      ENDIF
   NEXT

   IF s_aSwitches[ sw_Verbose ] > 0 ; ? "Done." ; ? ; END

   RETURN

STATIC PROCEDURE ShowHelp()
#define OnOrOff(b) IIf( b, "excluded", "included" )

   LOCAL aHelp := { ;
      "Syntax: ", ;
      "  hbextern [options]", ;
      "options:", ;
      "  -source=<folder> // source folder, default is .." + hb_ps() + "..", ;
      "  -skipdirs=<filename> // configuration file of folders to bypass, default:", ;
      {|| AEval( s_aSkipDirs, {|c| OutStd( IIf( Empty(c), "", "    " + c ) + hb_eol() ) } ) }, ;
      "  -skipfiles=<filename> // configuration file of files to bypass, default:", ;
      {|| AEval( s_aSkipFiles, {|c| OutStd( IIf( Empty(c), "", "    " + c ) + hb_eol() ) } ) }, ;
      "  -verbose=[silent|minimum|maximum] // verbose operation, default is maximum", ;
      "  -silent // synonym for verbose=silent", ;
      "  -case=[upper|lower|unchanged] // output case, default is upper", ;
      "  -include=<switch> // sets <switch> on", ;
      "  -exclude=<switch> // sets <switch> off", ;
      "switches:", ;
      "  all // all switches are set", ;
      "  duplicates // default is " + OnOrOff( s_aSwitches[ sw_ExcludeDuplicates ] ), ;
      "  empty-files // default is " + OnOrOff( s_aSwitches[ sw_ExcludeEmptyFiles ] ), ;
      "  classes // default is " + OnOrOff( s_aSwitches[ sw_ExcludeClasses ] ), ;
      "  class-methods // default is " + OnOrOff( s_aSwitches[ sw_ExcludeClassMethods ] ), ;
      "  conditionals // default is " + OnOrOff( s_aSwitches[ sw_ExcludeConditionals ] ), ;
      "  params // default is " + OnOrOff( s_aSwitches[ sw_ExcludeParams ] ), ;
/*       "  ", */ ;
   }
#undef OnOrOff

   // using hbmk2 style
   AEval( aHelp, {|x| IIf( ValType( x ) == "B", Eval( x ), OutStd( IIf( Empty(x), "", x ) + hb_eol() ) ) } )

   RETURN

STATIC PROCEDURE ProcessDir( cDir, aOutput )
   LOCAL i, nLen, aFiles

   // check for and prevent re-processing a folder
   IF HB_AScan( s_aDirsProcessed, {|c| c == Lower( cDir ) } ) > 0
      RETURN
   ENDIF
   AAdd( s_aDirsProcessed, Lower( cDir ) )

   IF s_aSwitches[ sw_Verbose ] > 0 ; ? cDir ; ENDIF

   cDir += hb_ps()

   aFiles := Directory( cDir + "*.*", "D" )
   IF ( nLen := LEN( aFiles ) ) > 0
      // Sort C files before PRG files before folders; this mimics HBExtern
      ASort( aFiles,,, {|x,y| ;
            IIf( x[ F_ATTR ] == "D", Chr(255), SubStr( x[ F_NAME ], 1 + HB_RAt( ".", x[ F_NAME ] ) ) ) + x[ F_NAME ] < IIf( y[ F_ATTR ] == "D", Chr(255), SubStr( y[ F_NAME ], 1 + HB_RAt( ".", y[ F_NAME ] ) ) ) + y[ F_NAME ] ;
         } )
      FOR i := 1 TO nLen
         IF aFiles[ i ][F_ATTR ] == "D"
            IF HB_AScan( s_aSkipDirs, {|d| Lower(d) == Lower( aFiles[ i ][ F_NAME ] ) } ) == 0
               ProcessDir( cDir + aFiles[ i ][ F_NAME ], aOutput )
            ENDIF
         ELSEIF Upper( SubStr( aFiles[ i ][ F_NAME ], -4 ) ) == ".PRG"
            ProcessFile( cDir + aFiles[ i ][ F_NAME ], .T., aOutput )
         ELSEIF Upper( SubStr( aFiles[ i ][ F_NAME ], -2 ) ) == ".C"
            ProcessFile( cDir + aFiles[ i ][ F_NAME ], .F., aOutput )
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION FileToArray( cFile )
   LOCAL nH
   LOCAL aResult := {}

   IF ( nH := FOpen( cFile ) ) > 0
      FileEval( nH, {|c| AAdd( aResult, c ) }, 255 )
      FClose( nH )
   ELSE
      IF s_aSwitches[ sw_Verbose ] > 0 ; ? "Could not open [" + cFile + "], error" + HB_NTOS( nH ) ; ENDIF
   END

   RETURN aResult

STATIC PROCEDURE ProcessFile( cFile, lPRG, aOutput )
   LOCAL nH
   LOCAL bOutputHeader
   LOCAL cHeader

   // Skip known files which does not contain any real public function
   IF HB_AScan( s_aSkipFiles, {|c| lower(c) $ lower( cFile ) } ) > 0
      RETURN
   ENDIF

   IF s_aSwitches[ sw_Verbose ] > 1 ; ? cFile ; ENDIF

   cHeader := "//" + hb_eol() + "// symbols from file: " + cFile + hb_eol() + "//" + hb_eol()

   IF .NOT. s_aSwitches[ sw_ExcludeEmptyFiles ]
      bOutputHeader := .F.
   ELSE
      bOutputHeader := .T.
   ENDIF

   IF ( nH := FOpen( cFile ) ) > 0
      FileEval( nH, {|c| ProcessLine( cFile, c, lPRG, @bOutputHeader, @cHeader, aOutput ) }, 255 )
      FClose( nH )
      s_aSwitches[ sw_ConditionalDepth ] := 0
   ELSE
      IF s_aSwitches[ sw_Verbose ] > 0 ; ? "Could not process [" + cFile + "], error " + HB_NTOS(nH) ; ENDIF
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


STATIC PROCEDURE ProcessLine( cFile, cLine, lPRG, bOutputHeader, cHeader, aOutput )
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

            //~ s_aConditions[ Len( s_aConditions ) ] := cLine
            DO CASE
            CASE SubStr( ATail( s_aConditions ), 1, Len( "#if " ) ) == "#if "
               s_aConditions[ Len( s_aConditions ) ]  := "#if !(" + SubStr( ATail( s_aConditions ), Len( "#if " ) + 1 ) + ")"
            CASE SubStr( ATail( s_aConditions ), 1, Len( "#ifdef " ) ) == "#ifdef "
               s_aConditions[ Len( s_aConditions ) ]  := "#ifndef " + SubStr( ATail( s_aConditions ), Len( "#ifdef " ) + 1 )
            CASE SubStr( ATail( s_aConditions ), 1, Len( "#ifndef " ) ) == "#ifndef "
               s_aConditions[ Len( s_aConditions ) ]  := "#ifdef " + SubStr( ATail( s_aConditions ), Len( "#ifndef " ) + 1 )
            OTHERWISE
               // TODO: print a more useful error message
               ? "Error:", cLine, ATail(s_aConditions)
            ENDCASE
         ENDIF
      ELSEIF Lower( SubStr( cLine, 1, Len( "#endif" ) ) ) == "#endif"
         IF s_aSwitches[ sw_ConditionalDepth ] > 0
            s_aSwitches[ sw_ConditionalDepth ]--
         ENDIF
         ASize( s_aConditions, Len( s_aConditions ) - 1 )
   //~ ELSE
      //~ ? cLine
      ENDIF
   ENDIF

   IF lPRG                                // PRG source file (FUNC, PROC, CLASS)

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
         ENDCASE

         aUnits := Split(RemoveComments(cLine), " ")

         IF s_cMethodType == "CLASS"
            // TOFIX: dependant upon there being no double spaces in the source line
            IF Len(aUnits) > 1 .AND. Upper( aUnits[ Len( aUnits ) - 1 ] ) == "FUNCTION"
               cLine := s_cMethodType + " " + aUnits[ Len( aUnits ) ]
            ENDIF
         ENDIF

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
            WriteSymbol( cFile, SubStr( cLine, 1, nPos ), s_cMethodType + " " + RemoveComments( SubStr( cLine, nPos + 1 ) ), @bOutputHeader, @cHeader, aOutput )
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
               WriteSymbol( cFile, "   " + cLine, ""/*s_cMethodType*/, @bOutputHeader, @cHeader, aOutput ) // SubStr( cLine, 1, nPos ), s_cMethodType )
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

      IF ;
         SubStr( cLine, 1, Len( "HB_FUNC(" ) ) == "HB_FUNC(" .OR. ;
         SubStr( cLine, 1, Len( "HB_CODEPAGE_INIT(" ) ) == "HB_CODEPAGE_INIT(" .OR. ;
         SubStr( cLine, 1, Len( "HB_LANG_ANNOUNCE(" ) ) == "HB_LANG_ANNOUNCE(" .OR. ;
         .F.

         DO CASE
         CASE SubStr( cLine, 1, Len( "HB_FUNC(" ) ) == "HB_FUNC(" ; s_cMethodType := ""
         CASE SubStr( cLine, 1, Len( "HB_CODEPAGE_INIT(" ) ) == "HB_CODEPAGE_INIT(" ; s_cMethodType := "HB_CODEPAGE_"
         CASE SubStr( cLine, 1, Len( "HB_LANG_ANNOUNCE(" ) ) == "HB_LANG_ANNOUNCE(" ; s_cMethodType := "HB_LANG_"
         OTHERWISE
         END

         Parse( @cLine, "(" )
         IF ( nPos := AT( ")", cLine ) ) > 0
            WriteSymbol( cFile, s_cMethodType + AllTrim( SubStr( cLine, 1, nPos - 1 ) ), "C function", @bOutputHeader, @cHeader, aOutput )
         ENDIF
      ENDIF

#ifndef PRG_CAN_HAVE_HB_FUNC
   ENDIF
#ENDIF

   RETURN

STATIC PROCEDURE FileEval( cnFile, bBlock, nMaxLine )
   LOCAL nHandle
   LOCAL cBuffer
   LOCAL lCloseFile := .F.

   DEFAULT nMaxLine TO 256

   IF ValType( cnFile ) == "C"
      lCloseFile := .T.
      IF ( nHandle := FOpen( cnFile ) ) < 0
         RETURN
      ENDIF
   ELSE
      nHandle := cnFile
   ENDIF

   FSeek( nHandle, 0 )

   DO WHILE FReadLn( nHandle, @cBuffer, nMaxLine )
      IF SubStr( LTrim( cBuffer ), 1, 2 ) == "/*"
         FSeek( nHandle, -( Len( cBuffer ) - 2 ), FS_RELATIVE )
         IF .NOT. FReadUntil( nHandle, "*/" )
            EXIT
         ENDIF
      ELSE
         Eval( bBlock, cBuffer )
      ENDIF
   ENDDO

   IF lCloseFile
      FClose( nHandle )
   ENDIF

   RETURN

STATIC FUNCTION FReadUntil( nHandle, cMatch, cResult )
   LOCAL cBuffer, nSavePos, nIdxMatch, nNumRead

   DEFAULT cResult TO ""

   DO WHILE nNumRead != 0
      nSavePos := FSeek( nHandle, 0, FS_RELATIVE )
      cBuffer := Space( 255 )
      IF ( nNumRead := FRead( nHandle, @cBuffer, Len( cBuffer ) ) ) == 0
         RETURN .F.
      ENDIF
      cBuffer := SubStr( cBuffer, 1, nNumRead )

      IF ( nIdxMatch := At( cMatch, cBuffer ) ) > 0
         cResult += SubStr( cBuffer, 1, nIdxMatch + Len( cMatch ) - 1 )
         FSeek( nHandle, nSavePos + nIdxMatch + Len( cMatch ) - 1, FS_SET )
         RETURN nNumRead != 0
      ELSE
         cResult += SubStr( cBuffer, 1, nNumRead - Len( cMatch ) )
         FSeek( nHandle, -Len( cMatch ), FS_RELATIVE )
      ENDIF
   ENDDO

   RETURN nNumRead != 0

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

STATIC PROCEDURE WriteSymbol( cFile, cLine, cMethodType, bOutputHeader, cHeader, aOutput )
   LOCAL idxOutput, idxRepository
   LOCAL cConditions

   DEFAULT cHeader TO ""

   IF Len( cLine ) > 0
      IF s_aSwitches[ sw_Case ] == 1
         cLine := Upper( cLine )
      ELSEIF s_aSwitches[ sw_Case ] == 2
         cLine := Lower( cLine )
      ENDIF

      IF HB_AScan( s_aSkipNames , {|c| Upper(c) == Upper(cLine) } ) == 0
         IF bOutputHeader
            bOutputHeader := .F.
         ENDIF

         IF s_aSwitches[ sw_ExcludeDuplicates ]
            AAdd( s_aSkipNames, cLine )
         ENDIF

         IF s_aSwitches[ sw_ConditionalDepth ] == 0
            AEval( s_aConditions, {|/* c */| s_aSwitches[ sw_ConditionalDepth ]++/* , FWrite( nOutput, c + hb_eol() ) */ } )
         ENDIF

         IF s_aSwitches[ sw_MimicHBExtern ].OR. Empty(cMethodType) .OR. s_aSwitches[ sw_ExcludeClassMethods ]
            cMethodType = ""
         ELSEIF .NOT. s_aSwitches[ sw_ExcludeClassMethods ]
            cMethodType = " // " + cMethodType
         ENDIF

         cConditions := ""
         AEval( s_aConditions, {|c| cConditions += c + hb_eol() } )
         IF Len( cConditions ) > Len( hb_eol() ) ; cConditions := SubStr( cConditions, 1, Len( cConditions ) - Len( hb_eol() ) ) ; ENDIF

         // the first entry has a hard-coded TRUE value so one will always be found
         idxOutput := HB_RAScan( aOutput, {|ao| ;
                         ao != NIL .AND. ;
                         Eval( ao[ capt_Cond ], ao, cConditions, cLine, cFile ) ;
                      } )
         idxRepository := HB_AScan( aOutput[ idxOutput ][ capt_Repository ], {|ar| ;
                           ar[ capt_Cond ] == cConditions ;
                          } )
         IF idxRepository == 0
            AAdd( aOutput[ idxOutput ][ capt_Repository ], Capture( NIL, cConditions ) )
            idxRepository := Len( aOutput[ idxOutput ][ capt_Repository ] )
         ENDIF
         AAdd( aOutput[ idxOutput ][ capt_Repository ][ idxRepository ][ capt_Repository ], cLine /* + cHeader */ )

         //~ FWrite( nOutput, "EXTERNAL " + cLine + cMethodType + hb_eol() )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE CopyGenericCopyrightToTarget( cSource, nOutput )
   LOCAL cFile := s_aSwitches[ sw_BaseDir ] + "doc" + hb_ps() + "hdr_tpl.txt"
   LOCAL nInput
   LOCAL cBuffer2

   FWrite( nOutput, ;
            "/*" + hb_eol() + ;
            " * $" + "Id" + "$" + hb_eol() + ;
            "*/" + hb_eol() + ;
            hb_eol() )

   IF ( nInput := FOpen( cFile ) ) > 0
      // assume there are two comment blocks seperated by a blank line (svn header and copyright)
      IF FReadUntil( nInput, "*/", "" /* discard ID comment */ ) .AND. ;
         FReadUntil( nInput, "*/", "" /* discard note comment */ ) .AND. ;
         FReadUntil( nInput, "/*", "" /* discard text comment */ ) .AND. ;
         FReadUntil( nInput, "*/", @cBuffer2 )
         cBuffer2 := StrTran( cBuffer2, " 2001", "" )
         cBuffer2 := StrTran( cBuffer2, "{list of individual authors and e-mail addresses}", "- automaticallyt generated by hbextern; do not edit" )
         cBuffer2 := StrTran( cBuffer2, "{one-liner description about the purpose of this source file}", "Harbour " + cSource + " contrib external header" )
         FWrite( nOutput, "/*" + cBuffer2 + hb_eol() + hb_eol() )
      ENDIF
      FClose( nInput )
   ELSE
      ? "error condition: " + HB_NTOS( Abs( nInput ) )
      ? "Could not open " + cFile
   ENDIF

STATIC PROCEDURE CopyExistingSourceToTarget( cSource, nOutput )
   LOCAL cFile := s_aSwitches[ sw_BaseDir ] + "include" + hb_ps() + cSource
   LOCAL nInput
   LOCAL cBuffer1, cBuffer2

   IF Empty( Directory( cFile ) )
#if 0
      FWrite( nOutput, ;
               "/*" + hb_eol() + ;
               " * $" + "Id" + "$" + hb_eol() + ;
               "*/" + hb_eol() + ;
               hb_eol() )
      cFile := s_aSwitches[ sw_BaseDir ] + "doc" + hb_ps() + "hdr_tpl.txt"
      IF ( nInput := FOpen( cFile ) ) > 0
         // assume there are two comment blocks seperated by a blank line (svn header and copyright)
         IF FReadUntil( nInput, "*/", "" /* discard ID comment */ ) .AND. ;
            FReadUntil( nInput, "*/", "" /* discard note comment */ ) .AND. ;
            FReadUntil( nInput, "/*", "" /* discard text comment */ ) .AND. ;
            FReadUntil( nInput, "*/", @cBuffer2 )
            cBuffer2 := StrTran( cBuffer2, " 2001", "" )
            cBuffer2 := StrTran( cBuffer2, "{list of individual authors and e-mail addresses}", "- automaticallyt generated by hbextern; do not edit" )
            cBuffer2 := StrTran( cBuffer2, "{one-liner description about the purpose of this source file}", "Harbour " + cSource + " contrib external header" )
            FWrite( nOutput, "/*" + cBuffer2 + hb_eol() + hb_eol() )
         ENDIF
         FClose( nInput )
      ENDIF
 #endif
   ELSEIF ( nInput := FOpen( cFile ) ) > 0
      // assume there are two comment blocks seperated by a blank line (svn header and copyright)
      IF FReadUntil( nInput, "*/", @cBuffer1 ) .AND. FReadUntil( nInput, "*/", @cBuffer2 )
         FWrite( nOutput, cBuffer1 + cBuffer2 + hb_eol() + hb_eol() )
      ENDIF
      FClose( nInput )
   ELSE
      ? "error condition: " + HB_NTOS( Abs( nInput ) )
      ? "Could not open " + cFile
   ENDIF
