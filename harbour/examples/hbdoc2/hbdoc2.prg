/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Document generator
 *
 * Copyright 2009 April White <april users.sourceforge.net>
 * www - http://www.harbour-project.org
 *
 * Portions of this project are based on hbdoc
 *    Copyright 1999-2003 Luiz Rafael Culik <culikr@uol.com.br>
 *
 *    <TODO: list gen... methods used>
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

/* note to self (from howtosvn.txt)
Run these commands and commit:
svn propset svn:keywords "Author Date Id Revision" "filename"
svn propset svn:eol-style native "filename"
*/

/*

todo
   - handle preformatted text / code / etc
   - include links back to index
   - include jumps to 'top'
   - 'coverage' to have links to corresponding file
   - 'Filename' must return the same file name all of the time for the same source
      - one method to retrieve, one to add?
      - key-value pair [hash table?]

todo - treat '<fixed>' / </fixed> as an non-conformance condition
   ntf: this may be okay for EXAMPLES and TESTS but this is also used
        within other sections, much like <table>

todo - look for embedded 'fixed'

these files have <tables>
   *..\..\doc\en-en\cmdline.txt    txt\lineutility.txt
   ..\..\doc\en-en\dbstrux.txt    txt\dbstrux.txt
   *..\..\doc\en-en\file.txt       txt\file.txt
   ..\..\doc\en-en\input.txt      txt\input.txt
   ..\..\doc\en-en\lang.txt       txt\lang.txt
   ..\..\doc\en-en\menu.txt       txt\menu.txt
   ..\..\doc\en-en\objfunc.txt    txt\objfunc.txt
   ..\..\doc\en-en\rdddb.txt      txt\rdddb.txt
   ..\..\doc\en-en\sayget.txt     txt\sayget.txt
   ..\..\doc\en-en\set.txt        txt\set.txt
   ..\..\doc\en-en\setmode.txt    txt\setmode.txt
   ..\..\doc\en-en\string.txt     txt\string.txt
   ..\..\doc\en-en\var.txt        txt\var.txt

done - recognize and accept </par>; see macro.txt output esp. hb_setmacro
done - list 'compliance' and 'platforms' within help
done - list 'category' and 'subcategory' types on help screen
done - load into memory (class and) method template
done - minimize these to the barest
done - build a list of 'categories' and validate against; see what 'classdoc' uses
done - validate sources against these templates
*/

#include "directry.ch"
#include "fileio.ch"
#include "hbdoc2.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

#ifndef __HARBOUR__
   #define HB_OSNewLine() ( Chr( 13 ) + Chr( 10 ) )
#endif

#ifdef __PLATFORM__UNIX
   #define PATH_SEPARATOR "/"
   #define BASE_DIR "../../"
#else
   #define PATH_SEPARATOR "\"
   #define BASE_DIR "..\..\"
#endif

STATIC s_aExclusions := { "class_tp.txt", "hdr_tpl.txt" }

PROCEDURE Main( ... )
   LOCAL aArgs := HB_AParams()
   LOCAL idx, idx2 //, idx3
   LOCAL arg
   LOCAL cArgName
   LOCAL oFormat
   LOCAL aContent

   PUBLIC p_aNonConformingSources := {}

   PUBLIC p_hsSwitches := HB_Hash( ;
      /* configuration settings, values, etc */ ;
      "basedir", BASE_DIR, ;
      "doc", .T., ;
      "source", .F., ;
      "contribs", .F., ;
      "format", {}, ;
      "by-", "method", ;
      "include-doc-source", .F., ;
      "include-doc-version", .F., ;
      ;
      /* internal settings, values, etc */ ;
      "PATH_SEPARATOR", PATH_SEPARATOR, ;
      "DELIMITER", "$", ;
      "format-list", { "text", "ascii", "html", "html2", "xml", "rtf", "hpc", "ngi", "os2", "chm", "ch2", "pdf", "trf", "doc", "dbf", "all" }, ;
      "hbextern.ch", {}, ;
      "in hbextern", {}, ;
      "not in hbextern", {}, ;
      "<eol>", nil ;
   )

   // remove formats that have not been implemented yet
   FOR idx := Len( p_hsSwitches[ "format-list" ] ) TO 1 STEP -1
      IF p_hsSwitches[ "format-list" ][ idx ] == "all"

      ELSEIF Type( "Generate" + p_hsSwitches[ "format-list" ][ idx ] + "()" ) != "UI"
         ASize( ADel( p_hsSwitches[ "format-list" ], idx), Len( p_hsSwitches[ "format-list" ] ) - 1 )
      ENDIF
   NEXT

   IF Len( aArgs ) == 0 .OR. aArgs[ 1 ] == "-?" .OR. aArgs[ 1 ] == "/?" .OR. aArgs[ 1 ] == "--help"
      ShowHelp( , aArgs )
      RETURN
   ENDIF

   FOR EACH arg IN aArgs
      IF .NOT. Empty(arg)
         IF ( idx := At( "=", arg ) ) == 0
            cArgName = arg
            arg = ""
         ELSE
            cArgName = SubStr(arg, 1, idx - 1)
            arg = SubStr( arg, idx + 1)
         ENDIF

         DO CASE
         CASE cArgName == "-source" ;           p_hsSwitches[ "basedir" ] := arg + IIf(SubStr(arg, -1, 1) == PATH_SEPARATOR, "", PATH_SEPARATOR)
         CASE cArgName == "-format"
            IF arg == "" .OR. HB_AScan( p_hsSwitches[ "format-list" ], arg, , , .T. ) == 0
               ShowHelp( "Unknown format option '" + arg + "'" )
               RETURN
            ELSE
               IF arg == "all"
                  p_hsSwitches[ "format" ] := p_hsSwitches[ "format-list" ]
               ELSE
                  AAdd( p_hsSwitches[ "format" ], arg )
               ENDIF
            END
         CASE cArgName == "-by-category" ;        p_hsSwitches[ "by-" ] := "category"
         CASE cArgName == "-by-method" ;          p_hsSwitches[ "by-" ] := "method"
         CASE cArgName == "-include-doc-source" ;     p_hsSwitches[ "include-doc-source" ] := .T.
         CASE cArgName == "-include-doc-version" ;     p_hsSwitches[ "include-doc-version" ] := .T.
         OTHERWISE
            IF HB_AScan( p_hsSwitches[ "format-list" ], SubStr( cArgName, 2 ), , , .T. ) > 0
               IF SubStr( cArgName, 2 ) == "all"
                  p_hsSwitches[ "format" ] := p_hsSwitches[ "format-list" ]
               ELSE
                  AAdd( p_hsSwitches[ "format" ], SubStr( cArgName, 2 ) )
               ENDIF
            ELSE
               ShowHelp( "Unknown option:" + cArgName + IIf( Len(arg) > 0, "=" + arg, "") )
               RETURN
            ENDIF
         END CASE
      ENDIF
   NEXT

   // load hbextern.ch
   FileEval( p_hsSwitches[ "basedir" ] + "include" + PATH_SEPARATOR + "hbextern.ch", ;
      {|c| IIf( SubStr( c, 1, Len( "EXTERNAL " ) ) == "EXTERNAL ", ;
                AAdd( p_hsSwitches[ "hbextern.ch" ], SubStr( c, Len( "EXTERNAL " ) + 1 ) ), ;
                ) } )
   ASort( p_hsSwitches[ "hbextern.ch" ] )

   aContent := {}
   AEval( ;
      {;
         p_hsSwitches[ "basedir" ] + "doc", ;
         p_hsSwitches[ "basedir" ] + "doc" + PATH_SEPARATOR + "en-en", ;
         IIf( p_hsSwitches[ "source" ], p_hsSwitches[ "basedir" ] + "source", NIL ), ;
         IIf( p_hsSwitches[ "contribs" ], p_hsSwitches[ "basedir" ] + "contrib", NIL ), ;
      }, ;
      {|c| IIf( .NOT. Empty( c ), ProcessFolder( c, @aContent ), ) } )

   ? HB_NTOS( Len( aContent ) ) + " items found"
   ?

   ASort( aContent, , , {|hsL,hsR| ;
      HB_HGetDef( hsL, "CATEGORY", "" ) + Chr(1) + HB_HGetDef( hsL, "SUBCATEGORY", "" ) + Chr(1)  + HB_HGetDef( hsL, "NAME", "" ) + Chr(1) ;
      <= ;
      HB_HGetDef( hsR, "CATEGORY", "" ) + Chr(1) + HB_HGetDef( hsR, "SUBCATEGORY", "" ) + Chr(1)  + HB_HGetDef( hsR, "NAME", "" ) + Chr(1) ;
      } )

   FOR idx := 1 TO Len( p_aCategories )
      IF .NOT. Empty( p_aCategories[ idx ] )
         IF Len( p_aCategories[ idx ] ) == 3 // category, list of subcategory, list of entries
            IF ValType( p_aCategories[ idx ][ 3 ] ) == "A"
               FOR idx2 := Len( p_aCategories[ idx ][ 3 ] ) TO 1 STEP -1
                  IF ValType( p_aCategories[ idx ][ 3 ][ idx2 ] ) == "A"
                     ASort( p_aCategories[ idx ][ 3 ][ idx2 ], ;
                        {|hsL,hsR| ;
                           HB_HGetDef( hsL, "CATEGORY", "" )  + HB_HGetDef( hsL, "SUBCATEGORY", "" )  + HB_HGetDef( hsL, "NAME", "" ) ;
                           <= ;
                           HB_HGetDef( hsR, "CATEGORY", "" )  + HB_HGetDef( hsR, "SUBCATEGORY", "" )  + HB_HGetDef( hsR, "NAME", "" ) ;
                           } )
                  ELSEIF p_aCategories[ idx ][ 2 ][ idx2 ] != NIL
                     //~ ? p_aCategories[ idx ][ 1 ] + " " + p_aCategories[ idx ][ 2 ][ idx2 ] + " has no subcategories populated"
                     ASize( ADel( p_aCategories[ idx ][ 2 ], idx2 ), Len( p_aCategories[ idx ][ 2 ] ) - 1 )
                     ASize( ADel( p_aCategories[ idx ][ 3 ], idx2 ), Len( p_aCategories[ idx ][ 3 ] ) - 1 )
                  ELSE
                     ASize( ADel( p_aCategories[ idx ][ 2 ], idx2 ), Len( p_aCategories[ idx ][ 2 ] ) - 1 )
                     ASize( ADel( p_aCategories[ idx ][ 3 ], idx2 ), Len( p_aCategories[ idx ][ 3 ] ) - 1 )
                  ENDIF
               NEXT
            ELSE
               //~ ? "expecting an array, found", ValType( p_aCategories[ idx ][ 3 ] )
            ENDIF
         ELSE
            //~ ? "Index", idx, " is not length 3 but rather", Len( p_aCategories[ idx ] )
         ENDIF
      ENDIF
   NEXT

   IF Len( p_hsSwitches[ "format" ] ) == 0
      p_hsSwitches[ "format" ] := { "text" }
   ENDIF

   DO CASE
   CASE p_hsSwitches[ "by-" ] == "category"

   CASE p_hsSwitches[ "by-" ] == "method"

      FOR idx2 := 1 TO Len( p_hsSwitches[ "format" ] )
         IF p_hsSwitches[ "format" ][ idx2 ] != "all"
            ? "Output as " + p_hsSwitches[ "format" ][ idx2 ]
            FOR idx := 1 TO Len( aContent )
               arg := aContent[ idx ]

               IF .NOT. HB_HHasKey( arg, "filename" ) .OR. Empty( arg[ "filename" ] )
                  arg[ "filename" ] := Filename( arg[ "NAME" ] )
               ENDIF

      //~ ? "<< " + arg[ "NAME" ], arg[ "filename"
               // create an instance of the class using the format as part of the class name
               oFormat := &("Generate" + p_hsSwitches[ "format" ][ idx2 ] + "()")
               oFormat:New( p_hsSwitches[ "format" ][ idx2 ], arg[ "filename" ], "Harbour Reference Guide", arg[ "NAME" ] )
               AEval( p_hsTemplates[ "orderby" ], {|c| IIf( HB_HHasKey( arg, c ), oFormat:AddEntry( c, Decode( c, arg, arg[ c ] ) ), NIL ) } )
               IF p_hsSwitches[ "include-doc-source" ]
                  oFormat:AddEntry( "SOURCE", arg[ "source-file" ] )
                  IF p_hsSwitches[ "include-doc-version" ]
                     oFormat:AddEntry( "SOURCEVERSION", arg[ "source-file-version" ] )
                  ENDIF
               ENDIF
               oFormat:Generate()
               oFormat:Close()
            NEXT
            oFormat := NIL
         ENDIF
      NEXT

   OTHERWISE
   ENDCASE

/*
   IF Len( p_aNonConformingSources ) > 0
      ? "Non-conforming sources:"
      AEval( p_aNonConformingSources, {|a| qout( a[ 1 ] + ":" + HB_NTOS( a[ 3 ] ) + ": " +a[ 2 ] ) } )
   ENDIF
*/

   ?

   RETURN

STATIC PROCEDURE ProcessFolder( cFolder, aContent )
   LOCAL aFiles
   LOCAL nLen
   LOCAL idx

   //~ ? "> " + cFolder

   cFolder += PATH_SEPARATOR

   aFiles := Directory( cFolder + "*.*", "D" )
   IF ( nLen := LEN( aFiles ) ) > 0
      FOR idx := 1 TO nLen
         IF aFiles[ idx ][F_ATTR ] == "D"
            IF ( p_hsSwitches[ "source" ] .OR. p_hsSwitches[ "contribs" ] ) /*.AND. ;
               HB_AScan( s_aSkipDirs, {|d| Lower(d) == Lower( aFiles[ idx ][ F_NAME ] ) } ) == 0*/
               ProcessFolder( cFolder + aFiles[ idx ][ F_NAME ], @aContent )
            ENDIF
         ELSEIF HB_AScan( s_aExclusions, {|f| Lower(f) == Lower( aFiles[ idx ][ F_NAME ] ) } ) == 0
            IF .NOT. ProcessFile( cFolder + aFiles[ idx ][ F_NAME ], @aContent )
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION ProcessFile( cFile, aContent )
   LOCAL aHandle := { 0, 0 }
   LOCAL cSectionName
   LOCAL cVersion

// debug code to keep output small
//~ if .not. "1st" $ lower(cFile)
//~ return .t.
//~ endif

   ? "> " + cFile

   IF ( aHandle[ 1 ] := FOpen( cFile ) ) < 0
      ? "error: could not open " + cFile + ", " + HB_NTOS( Abs( aHandle[ 1 ] ) )
      RETURN .F.
   ENDIF

   IF .NOT. FReadLn( @aHandle, "" ) // assume first line is ID comment prefix
      //~ FClose( aHandle[ 1 ] )
      //~ RETURN .F.
   ENDIF

   IF .NOT. FReadLn( @aHandle, @cVersion ) // assume second line is ID
      //~ FClose( aHandle[ 1 ] )
      //~ RETURN .F.
   ENDIF

   DO WHILE FReadSection( aHandle, @cSectionName, , p_hsTemplates[ "Template" ] )
      IF HB_HHasKey( p_hsTemplates[ "Template" ], cSectionName ) .AND. ;
         p_hsTemplates[ "Template" ][ cSectionName ][ 1 ] == TPL_START
         ProcessBlock( aHandle, @aContent, cFile, cSectionName, @cVersion )
      ENDIF
   ENDDO
   FClose( aHandle[ 1 ] )

   RETURN .T.

STATIC PROCEDURE ProcessBlock( aHandle, aContent, cFile, cType, cVersion )
   LOCAL cSectionName
   LOCAL cSection
   LOCAL hsBlock
   LOCAL lAccepted := .T.
   LOCAL cSource
   LOCAL idxCategory := -1
   LOCAL idxSubCategory := -1
   LOCAL idx
   LOCAL hsTemplate := p_hsTemplates[ "Template" ]
   LOCAL cSourceFile

#ifdef __PLATFORM__UNIX
      cSourceFile := "../" + cFile /* SubStr( cFile, Len( p_hsSwitches[ "basedir" ] + PATH_SEPARATOR ) ) */
#else
      cSourceFile := StrTran( "../" + cFile /* SubStr( cFile, Len( p_hsSwitches[ "basedir" ] + PATH_SEPARATOR ) ) */, "\", "/" )
#endif

   hsBlock := HB_Hash( ;
      "type", cType, ;
      "source-file", cSourceFile, ;
      "source-file-version", cVersion ;
   )

   DO WHILE FReadSection( aHandle, @cSectionName, @cSection, hsTemplate )
      idx := HB_HPos( hsTemplate, cSectionName )

      DO CASE
      CASE cSectionName == "TEMPLATE"

         IF HB_HHasKey( p_hsTemplates, cSection )
            hsTemplate := p_hsTemplates[ cSection ]

            FOR idx := 1 TO Len( hsTemplate )
               IF HB_HValueAt( hsTemplate, idx ) != NIL
                  DO CASE
                  CASE HB_HKeyAt( hsTemplate, idx ) == "TEMPLATE"
                     hsBlock[ HB_HKeyAt( hsTemplate, idx ) ] := HB_HValueAt( hsTemplate, idx )[ 2 ][ 1 ]
                  CASE HB_BitAnd( HB_HValueAt( hsTemplate, idx )[ 1 ], TPL_REQUIRED ) == TPL_REQUIRED
                     hsBlock[ HB_HKeyAt( hsTemplate, idx ) ] := NIL
                  OTHERWISE
                     hsBlock[ HB_HKeyAt( hsTemplate, idx ) ] := ""
                  ENDCASE
               ENDIF
            NEXT
         ELSE
            AddErrorCondition( cFile, "Unknown TYPE '" + cSection + "'" ) // + "' (line " + HB_NTOS( aHandle[ 2 ] ) + ")" // exclude link number, it reports tonnes of entries
            lAccepted := .F.
            EXIT
         ENDIF

      CASE hsTemplate == NIL

? "should not be here"
         //?

      OTHERWISE

         IF Len( cSectionName ) == 0

         ELSEIF HB_HHasKey( hsBlock, cSectionName )

            DO CASE
            CASE hsTemplate[ cSectionName ][ 1 ] == TPL_START

               AddErrorCondition( cFile, "Encountered another section '" + cSection, aHandle[ 2 ] )
               lAccepted := .F.
               EXIT

            CASE hsTemplate[ cSectionName ][ 1 ] == TPL_END

               EXIT

            CASE .NOT. Empty( hsBlock[ cSectionName ] )

               AddErrorCondition( cFile, "Duplicate " + cSectionName, aHandle[ 2 ] )
               lAccepted := .F.

            CASE cSectionName == "CATEGORY"

               IF ( idxCategory := HB_AScan( p_aCategories, {|c| .NOT. Empty( c ) .AND. ( IIf( ValType( c ) == "C", Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) ) } ) ) == 0
                  AddErrorCondition( cFile, "Unknown CATEGORY '" + cSection + "' for template '" + hsTemplate[ "TEMPLATE" ][ 2 ], aHandle[ 2 ] )
                  lAccepted := .F.
               ENDIF

            CASE HB_HHasKey( hsTemplate, "SUBCATEGORY" ) .AND. cSectionName == "SUBCATEGORY"

               IF idxCategory <= 0 .OR. hsBlock[ "CATEGORY" ] == ""

                  AddErrorCondition( cFile, "SUBCATEGORY '" + cSection + "' defined before CATEGORY", aHandle[ 2 ] )
                  lAccepted := .F.

               ELSEIF ValType( p_aCategories[ idxCategory ] ) == "C"

                  AddErrorCondition( cFile, "CATEGORY '" + p_aCategories[ idxCategory ] + " has no SUBCATEGORY listed, found '" + cSection, aHandle[ 2 ] )
                  lAccepted := .F.

               ELSEIF ( idxSubCategory := HB_AScan( p_aCategories[ idxCategory ][ 2 ], {|c| .NOT. ( c == NIL ) .AND. ( IIf( ValType( c ) == "C", Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) ) } ) ) == 0

                  AddErrorCondition( cFile, "Unknown SUBCATEGORY '" + p_aCategories[ idxCategory ][ 1 ] + "-" + cSection, aHandle[ 2 ] )
                  lAccepted := .F.

               ENDIF

            CASE .NOT. HB_HHasKey( hsTemplate, "RETURNS" ) .AND. cSectionName == "RETURNS"

               AddErrorCondition( cFile, "'" + hsBlock[ "NAME" ] + "' is identified as template FUNCTION but has no RETURNS value", aHandle[ 2 ] )
               lAccepted := .F.

            CASE HB_HHasKey( hsTemplate, "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
                     Empty( cSection ) .OR. ;
                     Lower( cSection ) == "nil" .OR. ;
                     Lower( cSection ) == "none" .OR. ;
                     Lower( cSection ) == "none." )

               AddErrorCondition( cFile, "'" + hsBlock[ "NAME" ] + "' is identified as template FUNCTION but has no RETURNS value", aHandle[ 2 ] - 1 )
               lAccepted := .F.

            CASE HB_BitAnd( HB_HValueAt( hsTemplate, idx )[ 1 ], TPL_CONSTRAINTLIST ) == TPL_CONSTRAINTLIST .AND. ;
                 HB_AScan( HB_HValueAt( hsTemplate, idx )[ 2 ], cSection ) == 0 .AND. ;
                 HB_AScan( HB_HValueAt( hsTemplate, idx )[ 2 ], Parse( ( cSection ), "," ) ) == 0

               cSource := HB_HKeyAt( hsTemplate, idx ) + " is '" + IIf( Len( cSection ) <= 20, cSection, SubStr( StrTran( cSection, HB_OSNewLine(), "" ), 1, 20 ) + "..." ) + "', should be one of: "
               //~ cSource := HB_HKeyAt( hsTemplate, idx ) + " should be one of: "
               AEval( HB_HValueAt( hsTemplate, idx )[ 2 ], {|c,n| cSource += IIf( n == 1, "", "," ) + c } )
               AddErrorCondition( cFile, cSource, aHandle[ 2 ] - 1 )

            OTHERWISE

            ENDCASE

            IF lAccepted
            //~ IF ATail( hsTemplate[ cSectionName ] ) > 0
               hsBlock[ cSectionName ] := cSection
            //~ ELSE
            //~ ? "bypassing " + cSectionName + ": " + SubStr(cSection, 1, 30 )
            //~ ENDIF
            ENDIF

         ELSE

            AddErrorCondition( cFile, "Using template '" + hsTemplate[ "TEMPLATE" ][ 2 ] + "' encountered an unexpected section '" + cSectionName, aHandle[ 2 ] )
            lAccepted := .F.

         ENDIF
      END CASE
   ENDDO

   IF lAccepted
      cSource := ""
      FOR idx := 1 TO Len( hsBlock )
         IF .NOT. HB_HHasKey( hsTemplate, HB_HKeyAt( hsBlock, idx ) ) .OR. ;
            hsTemplate[ HB_HKeyAt( hsBlock, idx ) ][ 1 ] == TPL_START .OR. hsTemplate[ HB_HKeyAt( hsBlock, idx ) ][ 1 ] == TPL_END

         ELSEIF HB_HValueAt( hsBlock, idx ) == NIL
            cSource += "," + HB_HKeyAt( hsBlock, idx )
         ENDIF
      NEXT
      IF Len( cSource ) > 0
         AddErrorCondition( cFile, "Missing sections: '" + SubStr( cSource, 2 ), aHandle[ 2 ] )
         lAccepted := .F.
      ENDIF
   ENDIF

   IF .NOT. lAccepted

   ELSEIF Len( hsBlock[ "NAME" ] ) == 0

      AddErrorCondition( cFile, "Section NAME missing", aHandle[ 2 ] )
      //~ lAccepted := .F.

   //~ ELSEIF hsTemplate[ "TEMPLATE" ][ 2 ][ 1 ] == "Function" .AND. ( ;
   ELSEIF hsBlock[ "TEMPLATE" ] == "Function" .AND. ( ;
                     Empty( hsBlock[ "RETURNS" ] ) .OR. ;
                     Lower( hsBlock[ "RETURNS" ] ) == "nil" .OR. ;
                     Lower( hsBlock[ "RETURNS" ] ) == "none" .OR. ;
                     Lower( hsBlock[ "RETURNS" ] ) == "none." )

      AddErrorCondition( cFile, "'" + hsBlock[ "NAME" ] + "' is identified as template FUNCTION but has no RETURNS value", aHandle[ 2 ] )
      //~ lAccepted := .F.

   ELSE

      IF .NOT. ( ;
         /* Lower( hsBlock[ "CATEGORY" ] ) == "document" .OR. */ ;
         /* .NOT. ( hsBlock[ "SUBCODE" ] == "" ) .OR. */ ;
         .F. )

         cSectionName := Upper( hsBlock[ "NAME" ] )

         //~ cSectionName := Upper( Decode( "Name", hsBlock ) )
         //~ Parse( @cSectionName, " " )

         cSectionName := Parse( cSectionName, "(" )

         IF HB_AScan( p_hsSwitches[ "hbextern.ch" ], cSectionName, , , .T. ) > 0
            AAdd( p_hsSwitches[ "in hbextern" ], cSectionName )
         ELSE
            AAdd( p_hsSwitches[ "not in hbextern" ], cSectionName + "; " + cFile )
         ENDIF

         //~ ? "    > " + cSectionName

      ENDIF

      AAdd( aContent, hsBlock )

      IF .NOT. HB_HHasKey( hsTemplate, "SUBCATEGORY" ) .OR. ;
         HB_BitAnd( hsTemplate[ "SUBCATEGORY" ][ 1 ], TPL_REQUIRED ) == 0 //.AND. idxSubCategory == -1
         idxSubCategory := 1
      ENDIF

//~ if 1 <= idxCategory .and. idxCategory < Len( p_aCategories )

   //~ ? "matching category", idxCategory, Len( p_aCategories ), idxSubCategory
   //~ if 1 <= idxSubCategory .and. idxSubCategory <= len( p_aCategories[ idxCategory ][ 3 ] )

      //~ ? "matching category and subcategory", idxCategory, Len( p_aCategories ), idxSubCategory, len( p_aCategories[ idxCategory ][ 3 ] )
      //~ ?

   //~ else

      //~ ? "matching category, not matching subcategory", idxCategory, Len( p_aCategories ), idxSubCategory
      //~ ?

   //~ endif

//~ else

   //~ ? "not matching category", idxCategory
   //~ ?

//~ endif

      IF ValType( p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ] ) != "A"
         p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ] := {}
      ENDIF
      AAdd( p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ], hsBlock )

   ENDIF

   RETURN

STATIC FUNCTION FReadSection( aHandle, cSectionName, cSection, hsTemplate )
   LOCAL nPosition
   LOCAL cBuffer
   LOCAL nLastIndent
   LOCAL lPreformatted := .F.
   LOCAL lLastPreformatted

   cSectionName := cSection := ""

   IF FReadLn( @aHandle, @cSectionName )
      cSectionName := LTrim( SubStr( cSectionName, 3 ) )
      IF SubStr( cSectionName, 1, 1 ) == p_hsSwitches[ "DELIMITER" ] .AND. ;
         SubStr( cSectionName, -1, 1 ) == p_hsSwitches[ "DELIMITER" ]

         cSectionName := SubStr( cSectionName, 1 + Len( p_hsSwitches[ "DELIMITER" ] ), Len( cSectionName ) - ( 2 * Len( p_hsSwitches[ "DELIMITER" ] ) ) )
         IF HB_HHasKey( hsTemplate, cSectionName )
            lLastPreformatted := lPreformatted := ( HB_BitAnd( hsTemplate[ cSectionName ][ 1 ], TPL_PREFORMATTED ) != 0 )
            nLastIndent := -1
            DO WHILE ( nPosition := FSeek( aHandle[ 1 ], 0, FS_RELATIVE ) ), FReadLn( @aHandle, @cBuffer )
               // TOFIX: this assumes that every line starts with " *"
               cBuffer := RTrim( SubStr( cBuffer, 3 ) )
               IF SubStr( LTrim( cBuffer ), 1, 1 ) == p_hsSwitches[ "DELIMITER" ] ;
                  .AND. SubStr( cBuffer, -1, 1 ) == p_hsSwitches[ "DELIMITER" ]
                  FSeek( aHandle[ 1 ], nPosition, FS_SET )
                  aHandle[ 2 ]-- // decrement the line number when rewinding the file
                  Exit
               ELSEIF Len( AllTrim( cBuffer ) ) == 0
                  IF SubStr( cSection, -Len( HB_OSNewLine() ) ) != HB_OSNewLine()
                     cSection += HB_OSNewLine()
                  ENDIF
                  nLastIndent := -1
               ELSEIF AllTrim( cBuffer ) == "<table>"
                  IF SubStr( cSection, -Len( HB_OSNewLine() ) ) != HB_OSNewLine() .OR. lPreformatted
                     cSection += HB_OSNewLine()
                  ENDIF
                  cSection += "<table>" //+ HB_OSNewLine()
                  lLastPreformatted := lPreformatted
                  lPreformatted := .T.
               ELSEIF AllTrim( cBuffer ) == "</table>"
                  IF SubStr( cSection, -Len( HB_OSNewLine() ) ) != HB_OSNewLine() .OR. lPreformatted
                     cSection += HB_OSNewLine()
                  ENDIF
                  cSection += "</table>" + HB_OSNewLine()
                  lPreformatted := lLastPreformatted
               ELSEIF nLastIndent != ( Len( cBuffer ) - Len( LTrim( cBuffer ) ) ) .OR. lPreformatted .OR. SubStr( cBuffer, -Len( "</par>" ) ) == "</par>"
                  IF SubStr( cBuffer, -Len( "</par>" ) ) == "</par>"
                     cBuffer := SubStr( cBuffer, 1, Len( cBuffer ) - Len( "</par>" ) )
                  ENDIF
                  nLastIndent := ( Len( cBuffer ) - Len( LTrim( cBuffer ) ) )
                  IF SubStr( cSection, -Len( HB_OSNewLine() ) ) != HB_OSNewLine()
                     cSection += HB_OSNewLine()
                  ENDIF
                  cSection += IIf( lPreformatted, cBuffer, AllTrim( cBuffer ) )
               ELSE
                  cSection += " " + AllTrim( cBuffer )
               ENDIF
            ENDDO
         ENDIF

      ENDIF
   ELSE
      RETURN .F.
   ENDIF

   DO WHILE SubStr( cSection, 1, Len( HB_OSNewLine() ) ) == HB_OSNewLine()
      cSection := SubStr( cSection, Len( HB_OSNewLine() ) + 1 )
   ENDDO

   DO WHILE SubStr( cSection, -Len( HB_OSNewLine() ) ) == HB_OSNewLine()
      cSection := SubStr( cSection, 1, Len( cSection ) - Len( HB_OSNewLine() ) )
   ENDDO

   IF lPreformatted .AND. Lower( SubStr( cSection, -Len( "</fixed>" ) ) ) == "</fixed>"
      cSection := SubStr( cSection, 1, Len( cSection ) - Len( "</fixed>" ) )
      DO WHILE SubStr( cSection, -Len( HB_OSNewLine() ) ) == HB_OSNewLine()
         cSection := SubStr( cSection, 1, Len( cSection ) - Len( HB_OSNewLine() ) )
      ENDDO
   ENDIF

   RETURN .T.

STATIC PROCEDURE FileEval( acFile, bBlock, nMaxLine )
   LOCAL aHandle := { 0, 0 }
   LOCAL cBuffer
   LOCAL lCloseFile := .F.
   LOCAL xResult

   DEFAULT nMaxLine TO 256

   IF ValType( acFile ) == "C"
      lCloseFile := .T.
      IF ( aHandle[ 1 ] := FOpen( acFile ) ) < 0
         RETURN
      ENDIF
   ELSEIF ValType( acFile ) == "N"
      aHandle[ 1 ] := acFile
   ELSE
      aHandle := acFile
   ENDIF

   //~ FSeek( nHandle, 0 )

   DO WHILE FReadLn( @aHandle, @cBuffer, nMaxLine )
      //~ IF SubStr( LTrim( cBuffer ), 1, 2 ) == "/*"
         //~ FSeek( nHandle, -( Len( cBuffer ) - 2 ), FS_RELATIVE )
         //~ IF .NOT. FReadUntil( nHandle, "*/" )
            //~ EXIT
         //~ ENDIF
      //~ ELSE
         xResult := Eval( bBlock, cBuffer )
         IF xResult != NIL .AND. ValType( xResult ) == "L" .AND. .NOT. xResult
            EXIT
         ENDIF
      //~ ENDIF
   ENDDO

   IF lCloseFile
      FClose( aHandle )
   ENDIF

   RETURN

STATIC FUNCTION FReadUntil( aHandle, cMatch, cResult )
   LOCAL cBuffer, nSavePos, nIdxMatch, nNumRead

   DEFAULT cResult TO ""

   DO WHILE nNumRead != 0
      nSavePos := FSeek( aHandle[ 1 ], 0, FS_RELATIVE )
      cBuffer := Space( 255 )
      IF ( nNumRead := FRead( aHandle[ 1 ], @cBuffer, Len( cBuffer ) ) ) == 0
         RETURN .F.
      ENDIF
      cBuffer := SubStr( cBuffer, 1, nNumRead )

      IF ( nIdxMatch := At( cMatch, cBuffer ) ) > 0
         cResult += SubStr( cBuffer, 1, nIdxMatch + Len( cMatch ) - 1 )
         FSeek( aHandle[ 1 ], nSavePos + nIdxMatch + Len( cMatch ) - 1, FS_SET )
         RETURN nNumRead != 0
      ELSE
         cResult += SubStr( cBuffer, 1, nNumRead - Len( cMatch ) )
         FSeek( aHandle[ 1 ], -Len( cMatch ), FS_RELATIVE )
      ENDIF
   ENDDO

   RETURN nNumRead != 0

STATIC FUNCTION FReadLn( aHandle, cBuffer, nMaxLine )
STATIC s_aEOL := { chr(13) + chr(10), chr(10), chr(13) }
   LOCAL cLine, nSavePos, nEol, nNumRead, nLenEol, idx

   DEFAULT nMaxLine TO 256

   cBuffer := ""

   nSavePos := FSeek( aHandle[ 1 ], 0, FS_RELATIVE )
   cLine := Space( nMaxLine )
   nNumRead := FRead( aHandle[ 1 ], @cLine, Len( cLine ) )
   cLine := SubStr( cLine, 1, nNumRead )

   nEol := 0
   FOR idx := 1 To Len(s_aEOL)
      IF ( nEol := At( s_aEOL[ idx ], cLine ) ) > 0
         nLenEol := Len( s_aEOL[ idx ] ) - 1
         Exit
      ENDIF
   NEXT

   IF nEol == 0
      cBuffer := cLine
   ELSE
      cBuffer := SubStr( cLine, 1, nEol - 1 )
      FSeek( aHandle[ 1 ], nSavePos + nEol + nLenEol, FS_SET )
   ENDIF

   aHandle[ 2 ]++
//~ ? hb_ntos(aHandle[ 2 ]) + ": " + cBuffer

   RETURN nNumRead != 0

FUNCTION Decode( cType, hsBlock, cKey )
   LOCAL cCode
   LOCAL cResult

   IF cKey != NIL .AND. hsBlock != NIL .AND. HB_HHasKey( hsBlock, cKey )
      cCode := hsBlock[ cKey ]
   ELSE
      cCode := cKey
   ENDIF

   DO CASE
   CASE cType == "STATUS"
      IF LEN( cCode ) > 1 .OR. Empty( cCode )
         RETURN cCode
      ELSEIF cCode == "R"
         RETURN "Ready"
      ELSEIF cCode == "S"
         RETURN "Started"
      ELSEIF Len( cCode ) > 0
         RETURN "Unknown 'STATUS': '" + cCode + '"'
      ELSE
         RETURN "Not started"
      ENDIF

   CASE cType == "PLATFORMS"
      IF "," $ cCode .AND. HB_AScan( p_aPlatforms, Parse( ( cCode ), "," ) ) > 0
         cResult := ""
         DO WHILE LEN( cCode ) > 0
            cResult += HB_OSNewLine() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, LEN( HB_OSNewLine() ) + 1 )
      ENDIF

      DO CASE
      CASE Empty( cCode ) ;      RETURN cCode
      CASE cCode == "All" ;      RETURN "This is available on all platforms"
      CASE cCode == "All(64K)" ; RETURN "This is available on all platforms though some platforms have a string length limit of 64KB"
      CASE cCode == "All(GT)" ;  RETURN "This part of the GT API and supported only by some platforms."
      CASE cCode == "All(LFN)" ;  RETURN ;
         "This is available on all platforms." + HB_OSNewLine() + ;
         "If long file names are available Harbour will use/display the first 15 characters " +;
         "else Harbour will use/display a 8.3 file name consistent with CA-Cl*pper"
      CASE cCode == "Linux(GT)" ;  RETURN "Under Linux the number of columns avaliable depends of the current Terminal screen size."
      CASE cCode == "OS2(GT)" ;  RETURN "Under OS/2 the number of columns avaliable depends of the current Terminal screen size."
      CASE cCode == "Win(GT)" ;  RETURN "Under Windows, the return value of MAXROW() function is only affected if called after an SETMODE() function"
      CASE cCode == "BSD" ;      RETURN "This is available on the BSD platform"
      CASE cCode == "DARWIN" ;   RETURN "This is available on the DARWIN platform"
      CASE cCode == "DOS" ;      RETURN "This is available on the MS-DOS platform"
      CASE cCode == "HPUX" ;     RETURN "This is available on the HPUX platform"
      CASE cCode == "LINUX" ;    RETURN "This is available on the LINUX platform"
      CASE cCode == "OS2" ;      RETURN "This is available on the OS/2 platform"
      CASE cCode == "SUNOS" ;    RETURN "This is available on the SUNOS platform"
      CASE cCode == "Unix" ;     RETURN "This is available on the Unix platform(s)"
      CASE cCode == "Win" ;      RETURN "This is available on the MS-Windows platform(s)"
      CASE cCode == "Win32" ;    RETURN "This is available on the MS-Windows (32-bit) platform(s)"
      CASE cCode == "Win64" ;    RETURN "This is available on the MS-Windows (64-bit) platform(s)"
      CASE cCode == "WinCE" ;    RETURN "This is available on the MS-Windows-CE platform"
      OTHERWISE ;                RETURN "Unknown 'PLATFORMS' code: '" + cCode + "'"
      ENDCASE

   CASE cType == "COMPLIANCE"
      IF "," $ cCode .AND. HB_AScan( p_aCompliance, Parse( ( cCode ), "," ) ) > 0
         cResult := ""
         DO WHILE LEN( cCode ) > 0
            cResult += HB_OSNewLine() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, LEN( HB_OSNewLine() ) + 1 )
      ENDIF

      DO CASE
      CASE Empty( cCode ) ;      RETURN cCode
      CASE cCode == "C" ;        RETURN "This is CA-Cl*pper v5.2 compliant"
      CASE cCode == "C(array)" ; RETURN "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements"
      CASE cCode == "C(menu)" ;  RETURN "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements"
      CASE cCode == "C52U" ;     RETURN "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_CLP_UNDOC flag"
      CASE cCode == "C52S" ;     RETURN "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_CLP_STRICT flag"
      CASE cCode == "C53" ;      RETURN "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag"
      CASE cCode == "FS" ;       RETURN "This a Flagship compatibility function and is only visible if source was compiled with the HB_COMPAT_FLAGSHIP flag"
      CASE cCode == "H" ;        RETURN "This is Harbour specific"
      CASE cCode == "NA" ;       RETURN "Not applicable"
      CASE cCode == "XPP" ;      RETURN "This an Xbase++ compatibility function and is only visible if source was compiled with the HB_COMPAT_XPP flag"
      OTHERWISE ;                RETURN "Unknown 'COMPLIANCE' code: '" + cCode + "'"
      ENDCASE

   CASE cType == "NAME"
      IF .NOT. HB_HHasKey( hsBlock, "RETURNS" )
         RETURN hsBlock[ "NAME" ]
      ELSEIF Empty( hsBlock[ "RETURNS" ] ) .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "nil" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none."

         hsBlock[ "RETURNS" ] = ""

         DO CASE
         CASE Lower( hsBlock[ "CATEGORY" ] ) == "document"
            RETURN hsBlock[ "NAME" ]
         OTHERWISE
            IF Lower( hsBlock[ "TEMPLATE" ] ) == "function" .OR. Lower( hsBlock[ "TEMPLATE" ] ) == "procedure"
               RETURN "Procedure " + hsBlock[ "NAME" ]
            ELSE
               RETURN LTrim( hsBlock[ "SUBCATEGORY" ] + " " ) + hsBlock[ "CATEGORY" ] + " " + hsBlock[ "NAME" ]
            ENDIF
         ENDCASE
      ELSE
         DO CASE
         CASE .NOT. Empty( hsBlock[ "NAME" ] )
            RETURN "Function " + hsBlock[ "NAME" ]
         OTHERWISE
            RETURN "unknown: " + hsBlock[ "CATEGORY" ]
         ENDCASE
      ENDIF

   ENDCASE

   RETURN /* cType + "=" +  */cCode

PROCEDURE ShowSubHelp( xLine, nMode, nIndent, n )
   LOCAL cIndent := Space( nIndent )

   IF xLine != NIL
      DO CASE
      CASE ValType( xLine ) == "N"
         nMode := xLine
      CASE ValType( xLine ) == "B"
         Eval( xLine )
      CASE ValType( xLine ) == "A"
         IF nMode == 2
            OutStd( cIndent + Space( 2 ) )
         ENDIF
         AEval( xLine, {|x,n| ShowSubHelp( x, @nMode, nIndent + 2, n ) } )
         IF nMode == 2
            OutStd( HB_OSNewLine() )
         ENDIF
      OTHERWISE
         DO CASE
         CASE nMode == 1         ; OutStd( cIndent + xLine ) ; OutStd( HB_OSNewLine() )
         CASE nMode == 2         ; OutStd( IIf( n > 1, ", ", "") + xLine )
         OTHERWISE               ; OutStd( "(" + HB_NTOS( nMode ) + ") " ) ; OutStd( xLine ) ; OutStd( HB_OSNewLine() )
         ENDCASE
      ENDCASE
   ENDIF

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

PROCEDURE ShowHelp( cExtraMessage, aArgs )
   LOCAL nMode := 1

#define OnOrOff(b) IIf( b, "excluded", "included" )
#define YesOrNo(b) IIf( b, "yes", "no" )
#define IsDefault(b) IIf( b, "; default", "" )

   LOCAL aHelp

   DO CASE
   CASE Empty( aArgs ) .OR. Len( aArgs ) <= 1 .OR. Empty( aArgs[ 1 ] )
      aHelp = { ;
         cExtraMessage, ;
         "Harbour Document Extractor (hbdoc2) " + HBRawVersion(), ;
         "Copyright (c) 1999-2009, http://www.harbour-project.org/", ;
         "", ;
         "Syntax:", ;
         "", ;
         { "hbdoc2 [options]" }, ;
         "", ;
         "Options:", ;
         { ;
            "-? or --help // this screen", ;
            "-? <option> or --help <option> // help on <option>, <option> is one of:", ;
            2, ;
            { "Categories", "Templates", "Compliance", "Platforms" }, ;
            1, ;
            "-[format=]<type> // output type, default is text, or one of:", ;
            2, ;
            p_hsSwitches[ "format-list" ], ;
            1, ;
            "-by-method // output is one file per method (function, command, etc)" + IsDefault( p_hsSwitches[ "by-" ] == "method" ), ;
            "-by-category // output is one file per category" + IsDefault( p_hsSwitches[ "by-" ] == "category" ), ;
            "-source=<folder> // source folder, default is .." + PATH_SEPARATOR + "..", ;
            "-include-doc-source // output is to indicate the document source file name", ;
            "-include-doc-version // output is to indicate the document source file version", ;
         } ;
      }

   CASE aArgs[ 2 ] == "Categories"
      aHelp := { ;
         "Defined categories and sub-categories are:", ;
         p_aCategories, ;
      }

   CASE aArgs[ 2 ] == "Templates"
      aHelp := { ;
         IIf( Len( aArgs ) >= 3, aArgs[ 3 ] + " template is:", "Defined templates are:" ), ;
         "", ;
         {|| ShowTemplatesHelp( IIf( Len( aArgs ) >= 3, aArgs[ 3 ], NIL ) ) } ;
      }

   CASE aArgs[ 2 ] == "Compliance"
      aHelp := { ;
         "Defined 'COMPLIANCE' are:", ;
         "", ;
         {|| ShowComplianceHelp() } ;
      }

   CASE aArgs[ 2 ] == "Platforms"
      aHelp := { ;
         "Defined 'PLATFORMS' are:", ;
         "", ;
         {|| ShowPlatformsHelp() } ;
      }

   OTHERWISE

      ShowHelp( "Unknown help option" )
      RETURN

   ENDCASE

#undef OnOrOff
#undef YesOrNo

   // using hbmk2 style
   AEval( aHelp, {|x| ShowSubHelp( x, @nMode, 0 ) } )

   RETURN

FUNCTION Parse(cVar, xDelimiter)
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

FUNCTION Split(cVar, cDelimiter)
   LOCAL aResult := {}
   LOCAL clVar := cVar

   DO WHILE Len(clVar) > 0
      AAdd(aResult, Parse( @clVar, cDelimiter ) )
   ENDDO

   RETURN aResult

FUNCTION Join(aVar, cDelimiter)
   LOCAL cResult := ""

   AEval( aVar, {|c,n| cResult += IIf( n > 1, cDelimiter, "" ) + c } )

   RETURN cResult

FUNCTION ProperCase( cSource )
   RETURN Upper( SubStr( cSource, 1, 1 ) ) + Lower( SubStr( cSource, 2 ) )

STATIC PROCEDURE AddErrorCondition( cFile, cMessage, nLine )
   If HB_AScan( p_aNonConformingSources, {|a| a[ 1 ] == cFile .AND. a[ 2 ] == cMessage .AND. a[ 3 ] == nLine } ) == 0
      AAdd( p_aNonConformingSources, { cFile, cMessage, nLine } )
   ENDIF
   RETURN

FUNCTION Indent( cText, nLeftMargin, nWidth, lRaw )
   LOCAL cResult := ""
   LOCAL idx
   LOCAL cLine
   LOCAL aText

   DEFAULT lRaw TO .F.

   IF nWidth == 0 .or. lRaw
      aText := Split( cText, HB_OSNewLine() )
      idx := 99999
      AEval( aText, {|c| IIf( Empty(c), , idx := Min( idx, Len( c ) - Len( LTrim( c ) ) ) ) } )
      AEval( aText, {|c,n| aText[ n ] := Space( nLeftMargin ) + SubStr( c, idx + 1 ) } )
      cResult := Join( aText, HB_OSNewLine() ) + HB_OSNewLine() + HB_OSNewLine()
   ELSE
      DO WHILE Len( cText ) > 0
         cLine := Parse( @cText, HB_OSNewLine() )

         IF cLine == "<table>"
            lRaw := .T.
         ELSEIF cLine == "</table>"
            cResult += HB_OSNewLine()
            lRaw := .F.
         ELSEIF lRaw
            cResult += Space( nLeftMargin ) + LTrim( cLine ) + HB_OSNewLine()
         ELSE
            DO WHILE Len( cLine ) > nWidth
               idx := nWidth + 1
               DO WHILE idx > 0
                  idx--
                  DO CASE
                  CASE At( SubStr( cLine, idx, 1 ), " ,;.!?" ) == 0
                     //
                  CASE Upper( SubStr( cLine, idx, 3 ) ) == ".T." .OR. Upper( SubStr( cLine, idx, 3 ) ) == ".F."
                     idx--
                  CASE Upper( SubStr( cLine, idx - 2, 3 ) ) == ".T." .OR. Upper( SubStr( cLine, idx - 1, 3 ) ) == ".F."
                     idx -= 3
                  CASE Upper( SubStr( cLine, idx, 5 ) ) == ".AND." .OR. Upper( SubStr( cLine, idx, 5 ) ) == ".NOT."
                     idx--
                  CASE Upper( SubStr( cLine, idx - 4, 5 ) ) == ".AND." .OR. Upper( SubStr( cLine, idx - 4, 5 ) ) == ".NOT."
                     idx -= 5
                  CASE Upper( SubStr( cLine, idx, 4 ) ) == ".OR."
                     idx--
                  CASE Upper( SubStr( cLine, idx - 3, 4 ) ) == ".OR."
                     idx -= 4
                  CASE Upper( SubStr( cLine, idx - 1, 4 ) ) == "i.e."
                     idx -= 2
                  CASE Upper( SubStr( cLine, idx - 3, 4 ) ) == "i.e."
                     idx -= 4
                  CASE Upper( SubStr( cLine, idx - 1, 4 ) ) == "e.g."
                     idx -= 2
                  CASE Upper( SubStr( cLine, idx - 3, 4 ) ) == "e.g."
                     idx -= 4
                  CASE Upper( SubStr( cLine, idx - 1, 2 ) ) == "*."
                     idx -= 2
                  OTHERWISE
                     EXIT
                  END SELECT
               ENDDO
               IF idx <= 0
                  idx := nWidth
               ENDIF

               cResult += Space( nLeftMargin ) + SubStr( cLine, 1, idx - IIf( SubStr( cLine, idx, 1 ) == " ", 1, 0 ) ) + HB_OSNewLine()
               cLine := LTrim( SubStr( cLine, idx + 1 ) )
            ENDDO

            IF Len( cLine ) > 0
               cResult += Space( nLeftMargin ) + cLine + HB_OSNewLine()
            ENDIF

            cResult += hb_OSNewLine()
         ENDIF
      ENDDO
   ENDIF

   RETURN cResult

FUNCTION Filename( cFile, cFormat, nLength )
STATIC s_Files := {}
   LOCAL cResult := ""
   LOCAL idx
   LOCAL char

   DEFAULT cFormat TO "alnum"

#ifdef __PLATFORM__DOS
   DEFAULT nLength TO 8
#else
   DEFAULT nLength TO 0
#endif

   DO CASE
   CASE Lower( cFormat ) == "alnum"

      FOR idx := 1 TO Len( cFile )
         char := Lower( SubStr( cFile, idx, 1 ) )
         IF "0" <= char .AND. char <= "9" .or. "a" <= char .AND. char <= "z" .or. char == "_"
            cResult += char
            IF nLength > 0 .AND. Len( cResult ) == nLength
               EXIT
            ENDIF
         ENDIF
      NEXT

   OTHERWISE
      cResult := cFile

   ENDCASE

   IF HB_AScan( s_Files, cResult, , , .T. ) == 0
      AAdd( s_Files, cResult )
   ELSE
#ifdef __PLATFORM__DOS
      cResult := SubStr( cResult, 1, Len( cResult ) - 3 )
#endif
      idx := 0
      DO WHILE HB_AScan( s_Files, cResult + PadL( HB_NTOS( ++idx ), 3, "0" ), , , .T. ) > 0
      ENDDO
      cResult += PadL( HB_NTOS( idx ), 3, "0" )
      AAdd( s_Files, cResult )
   ENDIF

   RETURN cResult
