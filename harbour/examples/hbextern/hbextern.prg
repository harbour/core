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
STATIC s_aSkipFiles := { "profiler.prg", "msg_tpl.c" }
STATIC s_aSkipDirs := { "tests", "examples", "sample", "samples", ".svn", "obj" }

STATIC s_aSkipNames  := { "MAIN" }   // Init with method names you want to skip
STATIC s_aDirsProcessed := {}
STATIC s_aMethodAsProcedure := {}
STATIC s_aConditions := {}

#define capt_Desc 1
#define capt_Cond 2
#define capt_Repository 3
#define Capture( desc, cond ) { desc, cond, {} }
STATIC s_aOutput // initialized within MAIN()

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
#define sw_ExcludeContrib          14
#define sw_SwitchesN               14

STATIC s_aSwitches[ sw_SwitchesN ]

#xcommand DEFAULT <p> TO <v> => IF <p> == NIL ; <p> := <v> ; ENDIF

PROCEDURE MAIN( ... )
   LOCAL aArgs := hb_AParams()
   LOCAL aDirs, i, nOutput, arg, cArgName
   LOCAL ao, ar
   LOCAL cDescription

   SET DATE FORMAT TO "yyyy-mm-dd"

   s_aSwitches[ sw_BaseDir ] := BASE_DIR
   s_aSwitches[ sw_Target ] := "hbextern.ch_"
   s_aSwitches[ sw_Verbose ] := 2
   s_aSwitches[ sw_Recursive ] := .T.
   s_aSwitches[ sw_Case ] := 1
   s_aSwitches[ sw_ExcludeDuplicates ] := .T.
   s_aSwitches[ sw_ExcludeEmptyFiles ] := .F.
   s_aSwitches[ sw_ExcludeClasses ] := .F.
   s_aSwitches[ sw_ExcludeClassMethods ] := .T.
   s_aSwitches[ sw_ExcludeConditionals ] := .F.
   s_aSwitches[ sw_ConditionalDepth ] := 0
   s_aSwitches[ sw_MimicHBExtern ] := .T.
   s_aSwitches[ sw_ExcludeParams ] := .T.
   s_aSwitches[ sw_ExcludeContrib ] := .T.

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
#define harbXns         "/* Harbour extensions violating extension namespace rules." + EOL + "   See reasons in source. */"
#define dbgr            "/* The debugger interface */"
#define rdd             "/* RDD related symbols */"
#define rddSx           "/* RDD SX related symbols */"
#define hiper           "/* HiPer-SEEK compatible functions */"
#define cfts            "/* CFTS compatible functions */"
#define i18n            "/* i18n */"
#define cpsp            "/* Codepage support */"
#define langsp          "/* lang support */"
#define scalar          "/* Scalar objects */"
#define xbasepp         "/* Xbase++ compatible functions */"
#define dosunkn         "/* DOS (?) */"
#define flagshp         "/* FlagShip extension */"

   // store all entries that match the codeblock but build a sub-array of entries per conditionals
   s_aOutput := { ;
      Capture( ca51   , {|a,b,c,f| a,b,c,f, .T. } ), ;
      Capture( ca51int, {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "__") ) == "__" } ), ;
      Capture( harb   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_") ) == "HB_" } ), ;
      Capture( dbgr   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "__DBG") ) == "__DBG" } ), ;
      Capture( rdd    , {|a,b,c,f| a,b,c, AT( PATH_SEPARATOR + "rdd" + PATH_SEPARATOR, f) > 0 } ), ;
      Capture( rddSx  , {|a,b,c,f| a,b, (SubStr(c, 1, 2) == "SX" .OR. SubStr(c, 1, 3) == "_SX" .OR. .F.) .AND. AT( PATH_SEPARATOR + "rdd" + PATH_SEPARATOR, f) > 0 } ), ;
      Capture( hiper  , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HS_") ) == "HS_" } ), ;
      Capture( cfts   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "CFTS") ) == "CFTS" } ), ;
      Capture( i18n   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_I18N") ) == "HB_I18N" } ), ;
      Capture( cpsp   , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_CODEPAGE_") ) == "HB_CODEPAGE_" } ), ;
      Capture( langsp , {|a,b,c,f| a,b,f, SubStr(c, 1, Len( "HB_LANG_") ) == "HB_LANG_" } ), ;
   }

   IF s_aSwitches[ sw_Recursive ]

      aDirs :={ ;
         s_aSwitches[ sw_BaseDir ] + "source", ;
         IIf( s_aSwitches[ sw_ExcludeContrib ], NIL, s_aSwitches[ sw_BaseDir ] + "contrib" ), ;
      }

   ELSE

      aDirs :={ ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "codepage", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "debug", ;
         s_aSwitches[ sw_BaseDir ] + "source" + PATH_SEPARATOR + "lang", ;
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
      CopyExistingTargetTo( nOutput )

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
#undef YesOrNo

      FWrite( nOutput, ;
         "#ifndef HB_EXTERN_CH_" + EOL + ;
         "#define HB_EXTERN_CH_" + EOL + ;
         EOL ;
      )

      FOR i := 1 TO LEN( aDirs )
         IF .NOT. Empty(aDirs[i])
            ProcessDir( nOutput, aDirs[i] )
         ENDIF
      NEXT

      IF Len(s_aMethodAsProcedure) > 0
         FWrite( nOutput, "/*" + EOL + "Class methods defined as 'procedure':" + EOL )
         AEval(s_aMethodAsProcedure, {|ac| FWrite( nOutput, "  " + ac[1] + " " + ac[2] + EOL ) } )
         FWrite( nOutput, "*/" + EOL + EOL )
      ENDIF

      FOR EACH ao IN s_aOutput
         IF ao != NIL
            FWrite( nOutput, ao[ capt_Desc ] + EOL )
            IF LEn( ao[ capt_Repository ] ) == 0
               FWrite( nOutput, "/* empty */" + EOL )
            ELSE
               FWrite( nOutput, EOL )
               FOR EACH ar in ao[ capt_Repository ]
                  IF ar[ capt_Cond ] != NIL .AND. Len( ar[ capt_Cond ] ) > 0 ; FWrite( nOutput, ar[ capt_Cond ] + EOL ); ENDIF

                  ASort( ar[ capt_Repository ] )
                  AEval( ar[ capt_Repository ], {|a| FWrite( nOutput, "EXTERNAL " + a + EOL ) } )

                  IF ar[ capt_Cond ] != NIL .AND. Len( ar[ capt_Cond ] ) > 0
                     cDescription := ""
                     DO WHILE Len( ar[ capt_Cond ] ) > 0
                        cDescription := "#endif /* " + Parse( @ar[ capt_Cond ], EOL ) + " */" + EOL + cDescription
                     ENDDO
                     FWrite( nOutput, cDescription )
                  ENDIF

                  FWrite( nOutput, EOL )
               NEXT
            ENDIF
            FWrite( nOutput, Stuff( ao[ capt_Desc ], 4, 0, "End of ") + EOL + EOL )
         ENDIF
      NEXT

      FWrite( nOutput, ;
         "#endif /* HB_EXTERN_CH_ */" + EOL ;
      )

      FClose( nOutput )
   ENDIF

   IF s_aSwitches[ sw_Verbose ] > 0 ; ? "Done." ; ? ; END

   RETURN

STATIC PROCEDURE ShowHelp()
#define OnOrOff(b) IIf( b, "excluded", "included" )

   LOCAL aHelp := { ;
      "Syntax: ", ;
      "  hbextern [options]", ;
      "options:", ;
      "  -source=<folder> // source folder, default is .." + PATH_SEPARATOR + "..", ;
      "  -target=<filename> // target file, default is hbextern.ch_", ;
      "  -recurse=[yes|no] // perform recursively, default is no", ;
      "  -skipdirs=<filename> // configuration file of folders to bypass, default:", ;
      {|| AEval( s_aSkipDirs, {|c| OutStd( IIf( Empty(c), "", "    " + c ) + EOL ) } ) }, ;
      "  -skipfiles=<filename> // configuration file of files to bypass, default:", ;
      {|| AEval( s_aSkipFiles, {|c| OutStd( IIf( Empty(c), "", "    " + c ) + EOL ) } ) }, ;
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
   AEval( aHelp, {|x| IIf( ValType( x ) == "B", Eval( x ), OutStd( IIf( Empty(x), "", x ) + EOL ) ) } )

   RETURN

STATIC PROCEDURE ProcessDir( nOutput, cDir )
   LOCAL i, nLen, aFiles

   // check for and prevent re-processing a folder
   IF HB_AScan( s_aDirsProcessed, Lower( cDir ) ) > 0
      RETURN
   ENDIF
   AAdd( s_aDirsProcessed, Lower( cDir ) )

   IF s_aSwitches[ sw_Verbose ] > 0 ; ? cDir ; ENDIF

   //~ IF .NOT. s_aSwitches[ sw_ExcludeEmptyFiles ]
      //~ FWrite( nOutput, "// Files from: " + cDir + EOL + EOL )
   //~ ENDIF

   cDir += PATH_SEPARATOR

   aFiles := Directory( cDir + "*.*", "D" )
   IF ( nLen := LEN( aFiles ) ) > 0
      // Sort C files before PRG files before folders; this mimics HBExtern
      ASort( aFiles,,, {|x,y| ;
            IIf( x[ F_ATTR ] == "D", Chr(255), SubStr( x[ F_NAME ], 1 + HB_RAt( ".", x[ F_NAME ] ) ) ) + x[ F_NAME ] < IIf( y[ F_ATTR ] == "D", Chr(255), SubStr( y[ F_NAME ], 1 + HB_RAt( ".", y[ F_NAME ] ) ) ) + y[ F_NAME ] ;
         } )
      FOR i := 1 TO nLen
         IF aFiles[ i ][F_ATTR ] == "D"
            IF s_aSwitches[ sw_Recursive ] .AND. ;
               HB_AScan( s_aSkipDirs, {|d| Lower(d) == Lower( aFiles[ i ][ F_NAME ] ) } ) == 0
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

   IF ( nH := FOpen( cFile ) ) > 0
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
   IF HB_AScan( s_aSkipFiles, {|c| lower(c) $ lower( cFile ) } ) > 0
      RETURN
   ENDIF

   IF s_aSwitches[ sw_Verbose ] > 1 ; ? cFile ; ENDIF

   cHeader := "//" + EOL + "// symbols from file: " + cFile + EOL + "//" + EOL

   IF .NOT. s_aSwitches[ sw_ExcludeEmptyFiles ]
      //~ FWrite( nOutput, cHeader )
      bOutputHeader := .F.
   ELSE
      bOutputHeader := .T.
   ENDIF

   nH := FOpen( cFile )
   IF nH > 0
      FileEval( nH, 255, {|c| ProcessLine( nOutput, cFile, c, lPRG, @bOutputHeader, @cHeader ) } )
      FClose( nH )
      //~ FWrite( nOutput, Replicate( "#endif" + EOL, s_aSwitches[ sw_ConditionalDepth ] ) )
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
            //~ FWrite( nOutput, "#else" + EOL )

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
            //~ FWrite( nOutput, "#endif" + EOL )
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
            WriteSymbol( nOutput, cFile, SubStr( cLine, 1, nPos ), s_cMethodType + " " + RemoveComments( SubStr( cLine, nPos + 1 ) ), @bOutputHeader, @cHeader )
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
               WriteSymbol( nOutput, cFile, "   " + cLine, ""/*s_cMethodType*/, @bOutputHeader, @cHeader ) // SubStr( cLine, 1, nPos ), s_cMethodType )
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
            WriteSymbol( nOutput, cFile, s_cMethodType + AllTrim( SubStr( cLine, 1, nPos - 1 ) ), "C function", @bOutputHeader, @cHeader )
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
      IF SubStr( LTrim( cBuffer ), 1, 2 ) == "/*"
         FSeek( nHandle, -( Len( cBuffer ) - 2 ), FS_RELATIVE )
         IF .NOT. FReadUntil( nHandle, "*/" )
            EXIT
         ENDIF
      ELSE
         Eval( bBlock, cBuffer )
      ENDIF
   ENDDO

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

STATIC PROCEDURE WriteSymbol( nOutput, cFile, cLine, cMethodType, bOutputHeader, cHeader )
   LOCAL idxOutput, idxRepository
   LOCAL cConditions

   DEFAULT nOutput TO 0
   DEFAULT cHeader TO ""

   IF Len( cLine ) > 0
      IF s_aSwitches[ sw_Case ] == 1
         cLine := Upper( cLine )
      ELSEIF s_aSwitches[ sw_Case ] == 2
         cLine := Lower( cLine )
      ENDIF

      IF HB_AScan( s_aSkipNames , {|c| Upper(c) == Upper(cLine) } ) == 0
         IF bOutputHeader
            //~ FWrite( nOutput, cHeader )
            bOutputHeader := .F.
         ENDIF

         IF s_aSwitches[ sw_ExcludeDuplicates ]
            AAdd( s_aSkipNames, cLine )
         ENDIF

         IF s_aSwitches[ sw_ConditionalDepth ] == 0
            AEval( s_aConditions, {|/* c */| s_aSwitches[ sw_ConditionalDepth ]++/* , FWrite( nOutput, c + EOL ) */ } )
         ENDIF

         IF s_aSwitches[ sw_MimicHBExtern ].OR. Empty(cMethodType) .OR. s_aSwitches[ sw_ExcludeClassMethods ]
            cMethodType = ""
         ELSEIF .NOT. s_aSwitches[ sw_ExcludeClassMethods ]
            cMethodType = " // " + cMethodType
         ENDIF

         cConditions := ""
         AEval( s_aConditions, {|c| cConditions += c + EOL } )
         IF Len( cConditions ) > Len( EOL ) ; cConditions := SubStr( cConditions, 1, Len( cConditions ) - Len( EOL ) ) ; ENDIF

         // the first entry has a hard-coded TRUE value so one will always be found
         idxOutput := HB_RAScan( s_aOutput, {|ao| ;
                         ao != NIL .AND. ;
                         Eval( ao[ capt_Cond ], ao, cConditions, cLine, cFile ) ;
                      } )
         idxRepository := HB_AScan( s_aOutput[ idxOutput ][ capt_Repository ], {|ar| ;
                           ar[ capt_Cond ] == cConditions ;
                          } )
         IF idxRepository == 0
            AAdd( s_aOutput[ idxOutput ][ capt_Repository ], Capture( NIL, cConditions ) )
            idxRepository := Len( s_aOutput[ idxOutput ][ capt_Repository ] )
         ENDIF
         AAdd( s_aOutput[ idxOutput ][ capt_Repository ][ idxRepository ][ capt_Repository ], cLine /* + cHeader */ )

         //~ FWrite( nOutput, "EXTERNAL " + cLine + cMethodType + EOL )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE CopyExistingTargetTo( nOutput )
   LOCAL nInput
   LOCAL cBuffer1, cBuffer2

   IF ( nInput := FOpen( s_aSwitches[ sw_BaseDir ] + PATH_SEPARATOR + "include" + PATH_SEPARATOR + "hbextern.ch" ) ) > 0
      // assume there are two comment blocks seperated by a blank line (svn header and copyright)
      IF FReadUntil( nInput, "*/", @cBuffer1 ) .AND. FReadUntil( nInput, "*/", @cBuffer2 )
         FWrite( nOutput, cBuffer1 + cBuffer2 + EOL + EOL )
      ENDIF
      FClose( nInput )
   ENDIF
