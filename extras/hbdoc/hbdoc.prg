/*
 * Harbour Project source code:
 * Document generator
 *
 * Copyright 2009 April White <april users.sourceforge.net>
 * www - http://harbour-project.org
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
  *..\..\doc\en\cmdline.txt    txt\lineutility.txt
   ..\..\doc\en\dbstrux.txt    txt\dbstrux.txt
  *..\..\doc\en\file.txt       txt\file.txt
   ..\..\doc\en\input.txt      txt\input.txt
   ..\..\doc\en\lang.txt       txt\lang.txt
   ..\..\doc\en\menu.txt       txt\menu.txt
   ..\..\doc\en\objfunc.txt    txt\objfunc.txt
   ..\..\doc\en\rdddb.txt      txt\rdddb.txt
   ..\..\doc\en\sayget.txt     txt\sayget.txt
   ..\..\doc\en\set.txt        txt\set.txt
   ..\..\doc\en\setmode.txt    txt\setmode.txt
   ..\..\doc\en\string.txt     txt\string.txt
   ..\..\doc\en\var.txt        txt\var.txt

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
#include "hbver.ch"

#include "hbdoc.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

#define BASE_DIR ".." + hb_ps() + ".." + hb_ps()

#define OnOrOff( b )   iif( b, "excluded", "included" )
#define YesOrNo( b )   iif( b, "yes", "no" )
#define IsDefault( b ) iif( b, "; default", "" )

STATIC sc_aExclusions := { "class_tp.txt", "hdr_tpl.txt" }

MEMVAR p_hsSwitches

PROCEDURE Main( ... )

   LOCAL aArgs := hb_AParams()
   LOCAL idx, idx2, idx3, idx4
   LOCAL arg
   LOCAL cArgName
   LOCAL cFormat
   LOCAL oDocument, oIndex
   LOCAL aContent

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   init_Templates()

   PUBLIC p_hsSwitches := { ;
      /* configuration settings, values, etc */ ;
      "basedir"             => BASE_DIR, ;
      "doc"                 => .T., ;
      "source"              => .F., ;
      "contribs"            => .T., ;
      "format"              => {}, ;
      "output"              => "category", ;
      "include-doc-source"  => .F., ;
      "include-doc-version" => .F., ;
      "immediate-errors"    => .F., ;
      /* internal settings, values, etc */ ;
      "DELIMITER"           => "$", ;
      "format-list"         => { "text", "ascii", "html", "html2", "xml", "rtf", "hpc", "ngi", "os2", "chm", "ch2", "pdf", "trf", "doc", "dbf", "all" }, ;
      "hbextern.ch"         => {}, ;
      "in hbextern"         => {}, ;
      "not in hbextern"     => {}, ;
      "<eol>"               => NIL }

   // remove formats that have not been implemented yet
   FOR idx := Len( p_hsSwitches[ "format-list" ] ) TO 1 STEP -1
      IF p_hsSwitches[ "format-list" ][ idx ] == "all"
      ELSEIF ! hb_IsFunction( "Generate" + p_hsSwitches[ "format-list" ][ idx ] )
         hb_ADel( p_hsSwitches[ "format-list" ], idx, .T. )
      ENDIF
   NEXT

   IF Len( aArgs ) == 0 .OR. aArgs[ 1 ] == "-?" .OR. aArgs[ 1 ] == "/?" .OR. aArgs[ 1 ] == "--help"
      ShowHelp( , aArgs )
      RETURN
   ENDIF

   FOR EACH arg IN aArgs
      IF ! Empty( arg )
         IF ( idx := At( "=", arg ) ) == 0
            cArgName := arg
            arg := ""
         ELSE
            cArgName := Left( arg, idx - 1 )
            arg := SubStr( arg, idx + 1 )
         ENDIF

         DO CASE
         CASE cArgName == "-source" ; p_hsSwitches[ "basedir" ] := arg + iif( Right( arg, 1 ) == hb_ps(), "", hb_ps() )
         CASE cArgName == "-format"
            IF arg == "" .OR. hb_AScan( p_hsSwitches[ "format-list" ], arg, , , .T. ) == 0
               ShowHelp( "Unknown format option '" + arg + "'" )
               RETURN
            ELSE
               IF arg == "all"
                  p_hsSwitches[ "format" ] := p_hsSwitches[ "format-list" ]
               ELSE
                  AAdd( p_hsSwitches[ "format" ], arg )
               ENDIF
            ENDIF
         CASE cArgName == "-output-single" ;          p_hsSwitches[ "output" ] := "single"
         CASE cArgName == "-output-category" ;        p_hsSwitches[ "output" ] := "category"
         CASE cArgName == "-output-entry" ;           p_hsSwitches[ "output" ] := "entry"
         CASE cArgName == "-include-doc-source" ;     p_hsSwitches[ "include-doc-source" ] := .T.
         CASE cArgName == "-include-doc-version" ;    p_hsSwitches[ "include-doc-version" ] := .T.
         OTHERWISE
            IF hb_AScan( p_hsSwitches[ "format-list" ], SubStr( cArgName, 2 ), , , .T. ) > 0
               IF SubStr( cArgName, 2 ) == "all"
                  p_hsSwitches[ "format" ] := p_hsSwitches[ "format-list" ]
               ELSE
                  AAdd( p_hsSwitches[ "format" ], SubStr( cArgName, 2 ) )
               ENDIF
            ELSE
               ShowHelp( "Unknown option:" + cArgName + iif( Len( arg ) > 0, "=" + arg, "" ) )
               RETURN
            ENDIF
         ENDCASE
      ENDIF
   NEXT

   // load hbextern.ch
   FileEval( p_hsSwitches[ "basedir" ] + "include" + hb_ps() + "hbextern.ch", ;
      {| c | iif( Left( c, Len( "EXTERNAL " ) ) == "EXTERNAL ", ;
                  AAdd( p_hsSwitches[ "hbextern.ch" ], SubStr( c, Len( "EXTERNAL " ) + 1 ) ), ;
                  ) } )
   ASort( p_hsSwitches[ "hbextern.ch" ] )

   aContent := {}
   AEval( ;
      {;
         p_hsSwitches[ "basedir" ] + "doc" + hb_ps() + "en", ;
         iif( p_hsSwitches[ "source" ], p_hsSwitches[ "basedir" ] + "src", NIL ), ;
         iif( p_hsSwitches[ "contribs" ], p_hsSwitches[ "basedir" ] + "contrib", NIL ), ;
      }, ;
      {| c | iif( ! Empty( c ), ProcessFolder( c, @aContent ), ) } )

   OutStd( hb_ntos( Len( aContent ) ) + " items found" + hb_eol() )
   OutStd( hb_eol() )

   ASort( aContent, , , {| oL, oR | ;
      hb_ntos( oL:CategoryIndex( oL:Category ) ) + " " + hb_ntos( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + Chr( 1 ) + oL:Name + " " ;
      <= ;
      hb_ntos( oR:CategoryIndex( oR:Category ) ) + " " + hb_ntos( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + Chr( 1 ) + oR:Name + " " ;
      } )

   // TODO: what is this for?  it is sorting the category sub-arrays and removing empty (?) sub-arrays, but why?
   FOR idx := 1 TO Len( p_aCategories )
      IF ! Empty( p_aCategories[ idx ] )
         IF Len( p_aCategories[ idx ] ) == 4 // category, list of subcategory, list of entries, handle
            FOR idx2 := Len( p_aCategories[ idx ][ 3 ] ) TO 1 STEP -1
               IF HB_ISARRAY( p_aCategories[ idx ][ 3 ][ idx2 ] )
                  ASort( p_aCategories[ idx ][ 3 ][ idx2 ], , , ;
                     {| oL, oR | ;
                        hb_ntos( oL:CategoryIndex( oL:Category ) ) + " " + hb_ntos( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + " " + oL:Name ;
                        <= ;
                        hb_ntos( oR:CategoryIndex( oR:Category ) ) + " " + hb_ntos( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + " " + oR:Name ;
                        } )
               ELSE
                  hb_ADel( p_aCategories[ idx ][ 2 ], idx2, .T. )
                  hb_ADel( p_aCategories[ idx ][ 3 ], idx2, .T. )
               ENDIF
            NEXT
         ELSE
            OutStd( "Index", idx, " is not length 4 but rather", Len( p_aCategories[ idx ] ), hb_eol() )
         ENDIF
      ENDIF
   NEXT

   IF Len( p_hsSwitches[ "format" ] ) == 0
      p_hsSwitches[ "format" ] := { "text" }
   ENDIF

   FOR idx2 := 1 TO Len( p_hsSwitches[ "format" ] )
      cFormat := p_hsSwitches[ "format" ][ idx2 ]
      IF !( cFormat == "all" )
         OutStd( "Output as " + cFormat + hb_eol() )

         DO CASE
         CASE p_hsSwitches[ "output" ] == "single"

            oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, "harbour", "Harbour Reference Guide" )

            FOR idx := 1 TO Len( aContent )
               IF Right( aContent[ idx ]:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt"
                  oDocument:AddEntry( aContent[ idx ] )
                  idx := Len( aContent )
               ENDIF
            NEXT

            FOR idx := 1 TO Len( aContent )
               IF !( Right( aContent[ idx ]:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt" )
                  oDocument:AddEntry( aContent[ idx ] )
               ENDIF
            NEXT

            oDocument:Generate()
            oDocument := NIL

         CASE p_hsSwitches[ "output" ] == "category"

            oIndex := &( "Generate" + cFormat + "()" ):NewIndex( cFormat, "harbour", "Harbour Reference Guide" )

            FOR idx := 1 TO Len( aContent )
               IF Right( aContent[ idx ]:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt"
                  IF oIndex != NIL
                     oIndex:AddEntry( aContent[ idx ] )
                  ENDIF
                  idx := Len( aContent )
               ENDIF
            NEXT

            FOR idx3 := 1 TO Len( p_aCategories )
               IF ! Empty( p_aCategories[ idx3 ] )
                  p_aCategories[ idx3 ][ 4 ] := Filename( p_aCategories[ idx3 ][ 1 ] )
                  // ~ oIndex:BeginSection( p_aCategories[ idx3 ][ 1 ], p_aCategories[ idx3 ][ 4 ] )
                  // ~ oIndex:EndSection( p_aCategories[ idx3 ][ 1 ], p_aCategories[ idx3 ][ 4 ] )
               ENDIF
            NEXT

            FOR idx3 := 1 TO Len( p_aCategories )
               IF ! Empty( p_aCategories[ idx3 ] )
                  oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, p_aCategories[ idx3 ][ 4 ], "Harbour Reference Guide - " + p_aCategories[ idx3 ][ 1 ] )

                  IF oIndex != NIL
                     oIndex:BeginSection( p_aCategories[ idx3 ][ 1 ], oDocument:cFilename )
                  ENDIF
                  oDocument:BeginSection( p_aCategories[ idx3 ][ 1 ], oDocument:cFilename )

                  FOR idx := 1 TO Len( p_aCategories[ idx3 ][ 3 ] )
                     IF ! Empty( p_aCategories[ idx3 ][ 3 ][ idx ] )
                        ASort( p_aCategories[ idx3 ][ 3 ][ idx ], , , {| oL, oR | oL:Name <= oR:Name } )
                        IF Len( p_aCategories[ idx3 ][ 2 ][ idx ] ) > 1 .OR. Len( p_aCategories[ idx3 ][ 2 ][ idx ] ) > 0
                           IF oIndex != NIL
                              oIndex:BeginSection( p_aCategories[ idx3 ][ 2 ][ idx ], oDocument:cFilename )
                           ENDIF
                           oDocument:BeginSection( p_aCategories[ idx3 ][ 2 ][ idx ], oDocument:cFilename )
                        ENDIF
                        FOR idx4 := 1 TO Len( p_aCategories[ idx3 ][ 3 ][ idx ] )
                           IF ! Empty( p_aCategories[ idx3 ][ 3 ][ idx ][ idx4 ] )
                              IF Right( p_aCategories[ idx3 ][ 3 ][ idx ][ idx4 ]:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt"
                              ELSE
                                 IF oIndex != NIL
                                    oIndex:AddReference( p_aCategories[ idx3 ][ 3 ][ idx ][ idx4 ] )
                                 ENDIF
                                 oDocument:AddEntry( p_aCategories[ idx3 ][ 3 ][ idx ][ idx4 ] )
                                 IF oIndex != NIL
                                    oDocument:AddReference( "Index", oIndex:cFilename )
                                    // this kind of works; the reference is outputed but it is not what I meant
                                    oDocument:AddReference( p_aCategories[ idx3 ][ 1 ], oIndex:cFilename, p_aCategories[ idx3 ][ 4 ] )
                                 ENDIF
                              ENDIF
                           ENDIF
                        NEXT
                        IF Len( p_aCategories[ idx3 ][ 2 ][ idx ] ) > 1 .OR. Len( p_aCategories[ idx3 ][ 2 ][ idx ] ) > 0
                           IF oIndex != NIL
                              oIndex:EndSection( p_aCategories[ idx3 ][ 2 ][ idx ], oDocument:cFilename )
                           ENDIF
                           oDocument:EndSection( p_aCategories[ idx3 ][ 2 ][ idx ], oDocument:cFilename )
                        ENDIF
                     ENDIF
                  NEXT
                  IF oIndex != NIL
                     oIndex:EndSection( p_aCategories[ idx3 ][ 1 ], oDocument:cFilename )
                  ENDIF
                  oDocument:EndSection( p_aCategories[ idx3 ][ 1 ], oDocument:cFilename )
                  oDocument:Generate()
               ENDIF
            NEXT

         CASE p_hsSwitches[ "output" ] == "entry"

            FOR idx := 1 TO Len( aContent )
               oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, aContent[ idx ]:filename, "Harbour Reference Guide" )
               IF oIndex != NIL
                  oIndex:AddEntry( aContent[ idx ] )
               ENDIF
               oDocument:AddEntry( aContent[ idx ] )
               oDocument:Generate()
            NEXT

         ENDCASE

         oDocument := NIL

         IF oIndex != NIL
            oIndex:Generate()
            oIndex := NIL
         ENDIF

      ENDIF
   NEXT

   OutStd( hb_eol() )

   RETURN

STATIC PROCEDURE ProcessFolder( cFolder, aContent ) // this is a recursive procedure

   LOCAL aFiles
   LOCAL nLen
   LOCAL idx
   LOCAL cExt

   // ~ OutStd( ">>> " + cFolder + hb_eol() )

   cFolder += hb_ps()

   aFiles := Directory( cFolder + hb_osFileMask(), "D" )
   IF ( nLen := Len( aFiles ) ) > 0
      FOR idx := 1 TO nLen
         IF aFiles[ idx ][ F_ATTR ] == "D"
            IF !( aFiles[ idx ][ F_NAME ] == "." ) .AND. ;
               !( aFiles[ idx ][ F_NAME ] == ".." )

               IF p_hsSwitches[ "source" ] .OR. p_hsSwitches[ "contribs" ]
                  /* .AND. AScan( s_aSkipDirs, {| d | Lower( d ) == Lower( aFiles[ idx ][ F_NAME ] ) } ) == 0 */
                  ProcessFolder( cFolder + aFiles[ idx ][ F_NAME ], @aContent )
               ENDIF
            ENDIF
         ELSEIF AScan( sc_aExclusions, {| f | Lower( f ) == Lower( aFiles[ idx ][ F_NAME ] ) } ) == 0
            hb_FNameSplit( aFiles[ idx ][ F_NAME ], , , @cExt )
            IF Lower( cExt ) == ".txt"
               IF ! ProcessFile( cFolder + aFiles[ idx ][ F_NAME ], @aContent )
                  EXIT
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION ProcessFile( cFile, aContent )

   LOCAL aHandle := { F_ERROR, 0 } // file handle and position
   LOCAL cSectionName
   LOCAL cVersion
   LOCAL o
   LOCAL nOldContentLen := Len( aContent )

   IF ( aHandle[ 1 ] := FOpen( cFile ) ) == F_ERROR
      OutErr( "error: could not open " + cFile + ", " + hb_ntos( aHandle[ 1 ] ) + hb_eol() )
      RETURN .F.
   ENDIF

   cVersion := ""

   o := Entry():New( "Template" )

   DO WHILE FReadSection( aHandle, @cSectionName, , o )
      IF o:IsField( @cSectionName, TPL_START )
         o := Entry():New( "Template" )
         ProcessBlock( aHandle, @aContent, cFile, cSectionName, @cVersion, @o )
      ENDIF
   ENDDO
   FClose( aHandle[ 1 ] )

   IF ( Len( aContent ) - nOldContentLen ) > 0
      OutStd( "> " + cFile + " (" + hb_ntos( Len( aContent ) - nOldContentLen ) + " items)" + hb_eol() )
   ENDIF

   RETURN .T.

STATIC PROCEDURE ProcessBlock( aHandle, aContent, cFile, cType, cVersion, o )

   LOCAL cSectionName
   LOCAL cSection
   LOCAL lAccepted := .T.
   LOCAL cSource
   LOCAL idxCategory := -1
   LOCAL idxSubCategory := -1
   LOCAL cSourceFile

   cSourceFile := StrTran( ".." + hb_ps() + cFile /* SubStr( cFile, Len( p_hsSwitches[ "basedir" ] + hb_ps() ) ) */, iif( hb_ps() == "\", "/", "\" ), hb_ps() )

   o:type_ := cType
   o:sourcefile_ := cSourceFile
   o:sourcefileversion_ := cVersion
   o:Name := "?NAME?"
   o:SetTemplate( "Function" )

   DO WHILE FReadSection( aHandle, @cSectionName, @cSection, @o )

      DO CASE
      CASE cSectionName == "TEMPLATE"
         IF o:IsTemplate( cSection )
            o:SetTemplate( cSection )
         ELSE
            AddErrorCondition( cFile, "Unknown TEMPLATE '" + cSection + "'" ) // + "' (line " + hb_ntos( aHandle[ 2 ] ) + ")" // exclude link number, it reports tonnes of entries
            lAccepted := .F.
            EXIT
         ENDIF

      OTHERWISE

         IF Len( cSectionName ) == 0

         ELSEIF o:IsField( cSectionName )

            DO CASE
            CASE o:IsField( cSectionName, TPL_START )

               AddErrorCondition( cFile, "Encountered another section '" + cSection, aHandle[ 2 ] )
               lAccepted := .F.
               EXIT

            CASE o:IsField( cSectionName, TPL_END )

               EXIT

            CASE ! Empty( o:&cSectionName )

               AddErrorCondition( cFile, "Duplicate " + cSectionName, aHandle[ 2 ] )
               lAccepted := .F.

            CASE cSectionName == "CATEGORY"

               IF ( idxCategory := AScan( p_aCategories, {| c | ! Empty( c ) .AND. ( iif( HB_ISCHAR( c ), Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) ) } ) ) == 0
                  AddErrorCondition( cFile, "Unknown CATEGORY '" + cSection + "' for template '" + o:Template, aHandle[ 2 ] )
                  // lAccepted := .F.
               ENDIF

            CASE cSectionName == "SUBCATEGORY" .AND. o:IsField( "SUBCATEGORY" )

               IF idxCategory <= 0 .OR. o:Category == ""

                  AddErrorCondition( cFile, "SUBCATEGORY '" + cSection + "' defined before CATEGORY", aHandle[ 2 ] )
                  // lAccepted := .F.

               ELSEIF ( idxSubCategory := AScan( p_aCategories[ idxCategory ][ 2 ], {| c | c != NIL .AND. ( iif( HB_ISCHAR( c ), Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) ) } ) ) == 0

                  AddErrorCondition( cFile, "Unknown SUBCATEGORY '" + p_aCategories[ idxCategory ][ 1 ] + "-" + cSection, aHandle[ 2 ] )
                  // lAccepted := .F.

               ENDIF

            CASE o:IsField( "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
                     Empty( cSection ) .OR. ;
                     Lower( cSection ) == "nil" .OR. ;
                     Lower( cSection ) == "none" .OR. ;
                     Lower( cSection ) == "none." )

               AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + cSection + ")", aHandle[ 2 ] - 1 )
               // lAccepted := .F.

            CASE ! o:IsConstraint( cSectionName, cSection )

               cSource := cSectionName + " is '" + iif( Len( cSection ) <= 20, cSection, Left( StrTran( cSection, hb_eol() ), 20 ) + "..." ) + "', should be one of: "
               // ~ cSource := hb_HKeyAt( hsTemplate, idx ) + " should be one of: "
               AEval( &( "p_a" + cSectionName ), {| c, n | cSource += iif( n == 1, "", "," ) + c } )
               AddErrorCondition( cFile, cSource, aHandle[ 2 ] - 1 )

            OTHERWISE

            ENDCASE

            IF lAccepted
               o:&cSectionName := Decode( cSectionName, , cSection )
            ENDIF

         ELSE

            AddErrorCondition( cFile, "Using template '" + o:Template + "' encountered an unexpected section '" + cSectionName, aHandle[ 2 ] )
            lAccepted := .F.

         ENDIF
      ENDCASE
   ENDDO

   IF lAccepted
      IF ! o:IsComplete( @cSource )
         AddErrorCondition( cFile, "Missing sections: '" + cSource + "'", aHandle[ 2 ] )
         // lAccepted := .F.
      ENDIF
   ENDIF

   IF ! lAccepted
   ELSEIF o:Template == "Function" .AND. ( ;
                     Empty( o:Returns ) .OR. ;
                     Lower( o:Returns ) == "nil" .OR. ;
                     Lower( o:Returns ) == "none" .OR. ;
                     Lower( o:Returns ) == "none." )

      AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + o:Returns + ")", aHandle[ 2 ] )
      // ~ lAccepted := .F.

   ELSE

      IF ! ( ;
         /* Lower( hsBlock[ "CATEGORY" ] ) == "document" .OR. */ ;
         /* ! ( hsBlock[ "SUBCODE" ] == "" ) .OR. */ ;
         .F. )

         cSectionName := Parse( Upper( o:Name ), "(" )

         IF hb_AScan( p_hsSwitches[ "hbextern.ch" ], cSectionName, , , .T. ) > 0
            AAdd( p_hsSwitches[ "in hbextern" ], cSectionName )
         ELSE
            AAdd( p_hsSwitches[ "not in hbextern" ], cSectionName + "; " + cFile )
         ENDIF

         // ~ OutStd( "    > " + cSectionName + hb_eol() )

      ENDIF

      IF p_hsSwitches[ "include-doc-source" ]
         o:Files += hb_eol() + o:sourcefile_ + iif( p_hsSwitches[ "include-doc-version" ], " (" + o:sourcefileversion_ + ")", "" )
      ENDIF

      o:filename := Filename( o:Name )

      AAdd( aContent, o )

      IF idxSubCategory == -1 .AND. ( ! o:IsField( "SUBCATEGORY" ) .OR. ! o:IsRequired( "SUBCATEGORY" ) ) // .AND. idxSubCategory == -1
         idxSubCategory := o:SubcategoryIndex( o:Category, "" )
      ENDIF

      IF idxCategory > 0 .AND. idxSubCategory > 0
         IF ! HB_ISARRAY( p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ] )
            p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ] := {}
         ENDIF
         AAdd( p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ], o )
      ENDIF

   ENDIF

   RETURN

STATIC FUNCTION FReadSection( aHandle, cSectionName, cSection, o )

   LOCAL nPosition
   LOCAL cBuffer
   LOCAL nLastIndent
   LOCAL lPreformatted := .F.
   LOCAL lLastPreformatted

   cSectionName := cSection := ""

   IF FReadLn( @aHandle, @cSectionName )
      cSectionName := LTrim( SubStr( cSectionName, 3 ) )
      IF Left( cSectionName, 1 ) == p_hsSwitches[ "DELIMITER" ] .AND. ;
         Right( cSectionName, 1 ) == p_hsSwitches[ "DELIMITER" ]

         cSectionName := SubStr( cSectionName, 1 + Len( p_hsSwitches[ "DELIMITER" ] ), Len( cSectionName ) - ( 2 * Len( p_hsSwitches[ "DELIMITER" ] ) ) )
         IF o:IsField( cSectionName )
            lLastPreformatted := lPreformatted := o:IsPreformatted( cSectionName )
            nLastIndent := -1
            DO WHILE ( nPosition := FSeek( aHandle[ 1 ], 0, FS_RELATIVE ) ), FReadLn( @aHandle, @cBuffer )
               // TOFIX: this assumes that every line starts with " *"
               cBuffer := RTrim( SubStr( cBuffer, 3 ) )
               IF Left( LTrim( cBuffer ), 1 ) == p_hsSwitches[ "DELIMITER" ] .AND. ;
                  Right( cBuffer, 1 ) == p_hsSwitches[ "DELIMITER" ]
                  FSeek( aHandle[ 1 ], nPosition, FS_SET )
                  aHandle[ 2 ]-- // decrement the line number when rewinding the file
                  EXIT
               ELSEIF Len( AllTrim( cBuffer ) ) == 0
                  IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
                     cSection += hb_eol()
                  ENDIF
                  nLastIndent := -1
               ELSEIF AllTrim( cBuffer ) == "<table>"
                  IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
                     cSection += hb_eol()
                  ENDIF
                  cSection += "<table>" // + hb_eol()
                  lLastPreformatted := lPreformatted
                  lPreformatted := .T.
               ELSEIF AllTrim( cBuffer ) == "</table>"
                  IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
                     cSection += hb_eol()
                  ENDIF
                  cSection += "</table>" + hb_eol()
                  lPreformatted := lLastPreformatted
               ELSEIF nLastIndent != ( Len( cBuffer ) - Len( LTrim( cBuffer ) ) ) .OR. lPreformatted .OR. Right( cBuffer, Len( "</par>" ) ) == "</par>"
                  IF Right( cBuffer, Len( "</par>" ) ) == "</par>"
                     cBuffer := Left( cBuffer, Len( cBuffer ) - Len( "</par>" ) )
                  ENDIF
                  nLastIndent := ( Len( cBuffer ) - Len( LTrim( cBuffer ) ) )
                  IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
                     cSection += hb_eol()
                  ENDIF
                  cSection += iif( lPreformatted, cBuffer, AllTrim( cBuffer ) )
               ELSE
                  cSection += " " + AllTrim( cBuffer )
               ENDIF
            ENDDO
         ENDIF

      ENDIF
   ELSE
      RETURN .F.
   ENDIF

   DO WHILE Left( cSection, Len( hb_eol() ) ) == hb_eol()
      cSection := SubStr( cSection, Len( hb_eol() ) + 1 )
   ENDDO

   DO WHILE Right( cSection, Len( hb_eol() ) ) == hb_eol()
      cSection := Left( cSection, Len( cSection ) - Len( hb_eol() ) )
   ENDDO

   IF lPreformatted .AND. Lower( Right( cSection, Len( "</fixed>" ) ) ) == "</fixed>"
      cSection := Left( cSection, Len( cSection ) - Len( "</fixed>" ) )
      DO WHILE Right( cSection, Len( hb_eol() ) ) == hb_eol()
         cSection := Left( cSection, Len( cSection ) - Len( hb_eol() ) )
      ENDDO
   ENDIF

   RETURN .T.

STATIC PROCEDURE FileEval( acFile, bBlock, nMaxLine )

   LOCAL aHandle := { F_ERROR, 0 }
   LOCAL cBuffer
   LOCAL lCloseFile := .F.
   LOCAL xResult

   hb_default( @nMaxLine, 256 )

   IF HB_ISSTRING( acFile )
      lCloseFile := .T.
      IF ( aHandle[ 1 ] := FOpen( acFile ) ) == F_ERROR
         RETURN
      ENDIF
   ELSEIF HB_ISNUMERIC( acFile )
      aHandle[ 1 ] := acFile
   ELSE
      aHandle := acFile
   ENDIF

   DO WHILE FReadLn( @aHandle, @cBuffer, nMaxLine )
      xResult := Eval( bBlock, cBuffer )
      IF xResult != NIL .AND. HB_ISLOGICAL( xResult ) .AND. ! xResult
         EXIT
      ENDIF
   ENDDO

   IF lCloseFile
      FClose( aHandle )
   ENDIF

   RETURN

STATIC FUNCTION FReadLn( aHandle, cBuffer, nMaxLine )

   STATIC s_aEOL := { Chr( 13 ) + Chr( 10 ), Chr( 10 ), Chr( 13 ) }
   LOCAL cLine, nSavePos, nEol, nNumRead, nLenEol, idx

   hb_default( @nMaxLine, 256 )

   cBuffer := ""

   nSavePos := FSeek( aHandle[ 1 ], 0, FS_RELATIVE )
   cLine := Space( nMaxLine )
   nNumRead := FRead( aHandle[ 1 ], @cLine, hb_BLen( cLine ) )
   cLine := hb_BLeft( cLine, nNumRead )

   nEol := 0
   FOR idx := 1 TO Len( s_aEOL )
      IF ( nEol := At( s_aEOL[ idx ], cLine ) ) > 0
         nLenEol := hb_BLen( s_aEOL[ idx ] ) - 1
         EXIT
      ENDIF
   NEXT

   IF nEol == 0
      cBuffer := cLine
   ELSE
      cBuffer := Left( cLine, nEol - 1 )
      FSeek( aHandle[ 1 ], nSavePos + hb_BLen( cBuffer ) + 1 + nLenEol, FS_SET )
   ENDIF

   aHandle[ 2 ]++

   RETURN nNumRead != 0

FUNCTION Decode( cType, hsBlock, cKey )

   LOCAL cCode
   LOCAL cResult
   LOCAL idx

   IF cKey != NIL .AND. hsBlock != NIL .AND. hb_HHasKey( hsBlock, cKey )
      cCode := hsBlock[ cKey ]
   ELSE
      cCode := cKey
   ENDIF

   DO CASE
   CASE cType == "STATUS"
      IF "," $ cCode .AND. hb_AScan( p_aStatus, Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE Len( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( p_aStatus, {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN p_aStatus[ idx ][ 2 ]
      ELSEIF Len( cCode ) > 1
         RETURN cCode
      ELSEIF Len( cCode ) > 0
         RETURN "Unknown 'STATUS' code: '" + cCode + "'"
      ELSE
         RETURN ATail( p_aStatus )[ 2 ]
      ENDIF

   CASE cType == "PLATFORMS"
      IF "," $ cCode .AND. hb_AScan( p_aPlatforms, Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE Len( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( p_aPlatforms, {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN p_aPlatforms[ idx ][ 2 ]
      ELSE
         RETURN cCode
      ENDIF

   CASE cType == "COMPLIANCE"
      IF "," $ cCode .AND. hb_AScan( p_aCompliance, Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE Len( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( p_aCompliance, {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN p_aCompliance[ idx ][ 2 ]
      ELSE
         RETURN cCode
      ENDIF
      DO CASE
      CASE cCode == "C" ;        RETURN "This is CA-Cl*pper v5.2 compliant"
      CASE cCode == "C(array)" ; RETURN "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements"
      CASE cCode == "C(menu)" ;  RETURN "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements"
      CASE cCode == "C52U" ;     RETURN "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_CLP_UNDOC flag"
      CASE cCode == "C52S" ;     RETURN "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_CLP_STRICT flag"
      CASE cCode == "C53" ;      RETURN "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag"
      CASE cCode == "H" ;        RETURN "This is Harbour specific"
      CASE cCode == "NA" ;       RETURN "Not applicable"
      OTHERWISE ;                RETURN cCode
      ENDCASE

   CASE cType == "NAME"
      IF hsBlock == NIL
         RETURN cCode
      ELSEIF ! hb_HHasKey( hsBlock, "RETURNS" )
         RETURN hsBlock[ "NAME" ]
      ELSEIF Empty( hsBlock[ "RETURNS" ] ) .OR. ;
         Upper( hsBlock[ "RETURNS" ] ) == "NIL" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none."

         hsBlock[ "RETURNS" ] := ""

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
         CASE ! Empty( hsBlock[ "NAME" ] )
            RETURN "Function " + hsBlock[ "NAME" ]
         OTHERWISE
            RETURN "Unknown 'CATEGORY': " + hsBlock[ "CATEGORY" ]
         ENDCASE
      ENDIF

   ENDCASE

   RETURN /* cType + "=" +  */cCode

PROCEDURE ShowSubHelp( xLine, nMode, nIndent, n )

   LOCAL cIndent := Space( nIndent )

   IF xLine != NIL
      DO CASE
      CASE HB_ISNUMERIC( xLine )
         nMode := xLine
      CASE HB_ISBLOCK( xLine )
         Eval( xLine )
      CASE HB_ISARRAY( xLine )
         IF nMode == 2
            OutStd( cIndent + Space( 2 ) )
         ENDIF
         AEval( xLine, {| x, n | ShowSubHelp( x, @nMode, nIndent + 2, n ) } )
         IF nMode == 2
            OutStd( hb_eol() )
         ENDIF
      OTHERWISE
         DO CASE
         CASE nMode == 1 ; OutStd( cIndent + xLine ) ; OutStd( hb_eol() )
         CASE nMode == 2 ; OutStd( iif( n > 1, ", ", "" ) + xLine )
         OTHERWISE       ; OutStd( "(" + hb_ntos( nMode ) + ") " ) ; OutStd( xLine ) ; OutStd( hb_eol() )
         ENDCASE
      ENDCASE
   ENDIF

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN hb_StrFormat( "%d.%d.%d%s (r%d)", ;
      hb_Version( HB_VERSION_MAJOR ), ;
      hb_Version( HB_VERSION_MINOR ), ;
      hb_Version( HB_VERSION_RELEASE ), ;
      hb_Version( HB_VERSION_STATUS ), ;
      hb_Version( HB_VERSION_REVISION ) )

PROCEDURE ShowHelp( cExtraMessage, aArgs )

   LOCAL nMode := 1

   LOCAL aHelp

   DO CASE
   CASE Empty( aArgs ) .OR. Len( aArgs ) <= 1 .OR. Empty( aArgs[ 1 ] )
      aHelp := { ;
         cExtraMessage, ;
         "Harbour Document Compiler (hbdoc) " + HBRawVersion(), ;
         "Copyright (c) 1999-2010, http://harbour-project.org/", ;
         "", ;
         "Syntax:", ;
         "", ;
         { "hbdoc [options]" }, ;
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
            "-output-single // output is one file" + IsDefault( p_hsSwitches[ "output" ] == "single" ), ;
            "-output-category // output is one file per category" + IsDefault( p_hsSwitches[ "output" ] == "category" ), ;
            "-output-entry // output is one file per entry (function, command, etc)" + IsDefault( p_hsSwitches[ "output" ] == "entry" ), ;
            "-source=<folder> // source folder, default is .." + hb_ps() + "..", ;
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
         iif( Len( aArgs ) >= 3, aArgs[ 3 ] + " template is:", "Defined templates are:" ), ;
         "", ;
         {|| ShowTemplatesHelp( iif( Len( aArgs ) >= 3, aArgs[ 3 ], NIL ), p_hsSwitches[ "DELIMITER" ] ) } ;
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

   // using hbmk2 style
   AEval( aHelp, {| x | ShowSubHelp( x, @nMode, 0 ) } )

   RETURN

FUNCTION Parse( cVar, xDelimiter )

   LOCAL cResult
   LOCAL idx

   IF HB_ISNUMERIC( xDelimiter )
      cResult := Left( cVar, xDelimiter )
      cVar := SubStr( cVar, xDelimiter + 1 )
   ELSE
      IF ( idx := At( xDelimiter, cVar ) ) == 0
         cResult := cVar
         cVar := ""
      ELSE
         cResult := Left( cVar, idx - 1 )
         cVar := SubStr( cVar, idx + Len( xDelimiter ) )
      ENDIF
   ENDIF

   RETURN cResult

FUNCTION Join( aVar, cDelimiter )

   LOCAL cResult := ""

   AEval( aVar, {| c, n | cResult += iif( n > 1, cDelimiter, "" ) + c } )

   RETURN cResult

STATIC PROCEDURE AddErrorCondition( cFile, cMessage, nLine )

   IF p_hsSwitches[ "immediate-errors" ]
      OutStd( cFile + ":" + hb_ntos( nLine ) + ": " + cMessage + hb_eol() )
   ENDIF

   RETURN

FUNCTION Indent( cText, nLeftMargin, nWidth, lRaw )

   LOCAL cResult := ""
   LOCAL idx
   LOCAL cLine
   LOCAL aText

   hb_default( @lRaw, .F. )

   IF nWidth == 0 .OR. lRaw
      aText := hb_ATokens( cText, hb_eol() )
      idx := 99999
      AEval( aText, {| c | iif( Empty( c ), , idx := Min( idx, Len( c ) - Len( LTrim( c ) ) ) ) } )
      AEval( aText, {| c, n | aText[ n ] := Space( nLeftMargin ) + SubStr( c, idx + 1 ) } )
      cResult := Join( aText, hb_eol() ) + hb_eol() + hb_eol()
   ELSE
      DO WHILE Len( cText ) > 0
         cLine := Parse( @cText, hb_eol() )

         IF cLine == "<table>"
            lRaw := .T.
         ELSEIF cLine == "</table>"
            cResult += hb_eol()
            lRaw := .F.
         ELSEIF lRaw
            cResult += Space( nLeftMargin ) + LTrim( cLine ) + hb_eol()
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
                  ENDCASE
               ENDDO
               IF idx <= 0
                  idx := nWidth
               ENDIF

               cResult += Space( nLeftMargin ) + Left( cLine, idx - iif( SubStr( cLine, idx, 1 ) == " ", 1, 0 ) ) + hb_eol()
               cLine := LTrim( SubStr( cLine, idx + 1 ) )
            ENDDO

            IF Len( cLine ) > 0
               cResult += Space( nLeftMargin ) + cLine + hb_eol()
            ENDIF

            cResult += hb_eol()
         ENDIF
      ENDDO
   ENDIF

   RETURN cResult

FUNCTION Filename( cFile, cFormat, nLength )

   STATIC s_Files := {}

   LOCAL cResult := ""
   LOCAL idx
   LOCAL char

   hb_default( @cFormat, "alnum" )

#ifdef __PLATFORM__DOS
   hb_default( @nLength, 8 )
#else
   hb_default( @nLength, 0 )
#endif

   DO CASE
   CASE Lower( cFormat ) == "alnum"

      FOR idx := 1 TO Len( cFile )
         char := Lower( SubStr( cFile, idx, 1 ) )
         IF "0" <= char .AND. char <= "9" .OR. "a" <= char .AND. char <= "z" .OR. char == "_"
            cResult += char
            IF nLength > 0 .AND. Len( cResult ) == nLength
               EXIT
            ENDIF
         ENDIF
      NEXT

   OTHERWISE
      cResult := cFile

   ENDCASE

   IF hb_AScan( s_Files, cResult, , , .T. ) == 0
      AAdd( s_Files, cResult )
   ELSE
#ifdef __PLATFORM__DOS
      cResult := Left( cResult, Len( cResult ) - 3 )
#endif
      idx := 0
      DO WHILE hb_AScan( s_Files, cResult + PadL( hb_ntos( ++idx ), 3, "0" ), , , .T. ) > 0
      ENDDO
      cResult += PadL( hb_ntos( idx ), 3, "0" )
      AAdd( s_Files, cResult )
   ENDIF

   RETURN cResult

#if defined( __HBSCRIPT__HBSHELL )
SET PROCEDURE TO "_tmplate.prg"
SET PROCEDURE TO "_genbase.prg"
SET PROCEDURE TO "_gentxt.prg"
SET PROCEDURE TO "_genhtml.prg"
SET PROCEDURE TO "_genxml.prg"
#endif
