/*
 * $Id$
 */

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
#include "simpleio.ch"

#include "hbdoc.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

#define BASE_DIR ".." + hb_ps() + ".." + hb_ps()

STATIC s_aExclusions := { "class_tp.txt", "hdr_tpl.txt" }

PROCEDURE Main( ... )
   LOCAL aArgs := HB_AParams()
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

   PUBLIC p_hsSwitches := HB_Hash( ;
      /* configuration settings, values, etc */ ;
      "basedir", BASE_DIR, ;
      "doc", .T., ;
      "source", .F., ;
      "contribs", .T., ;
      "format", {}, ;
      "output", "category", ;
      "include-doc-source", .F., ;
      "include-doc-version", .F., ;
      "immediate-errors", .F., ;
      ;
      /* internal settings, values, etc */ ;
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
      ELSEIF ! hb_IsFunction( "Generate" + p_hsSwitches[ "format-list" ][ idx ] )
         hb_ADel( p_hsSwitches[ "format-list" ], idx, .T. )
      ENDIF
   NEXT

   IF Len( aArgs ) == 0 .OR. aArgs[ 1 ] == "-?" .OR. aArgs[ 1 ] == "/?" .OR. aArgs[ 1 ] == "--help"
      ShowHelp( , aArgs )
      RETURN
   ENDIF

   FOR EACH arg IN aArgs
      IF ! Empty(arg)
         IF ( idx := At( "=", arg ) ) == 0
            cArgName := arg
            arg := ""
         ELSE
            cArgName := SubStr(arg, 1, idx - 1)
            arg := SubStr( arg, idx + 1)
         ENDIF

         DO CASE
         CASE cArgName == "-source" ;           p_hsSwitches[ "basedir" ] := arg + iif(SubStr(arg, -1, 1) == hb_ps(), "", hb_ps())
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
         CASE cArgName == "-output-single" ;          p_hsSwitches[ "output" ] := "single"
         CASE cArgName == "-output-category" ;        p_hsSwitches[ "output" ] := "category"
         CASE cArgName == "-output-entry" ;           p_hsSwitches[ "output" ] := "entry"
         CASE cArgName == "-include-doc-source" ;     p_hsSwitches[ "include-doc-source" ] := .T.
         CASE cArgName == "-include-doc-version" ;    p_hsSwitches[ "include-doc-version" ] := .T.
         OTHERWISE
            IF HB_AScan( p_hsSwitches[ "format-list" ], SubStr( cArgName, 2 ), , , .T. ) > 0
               IF SubStr( cArgName, 2 ) == "all"
                  p_hsSwitches[ "format" ] := p_hsSwitches[ "format-list" ]
               ELSE
                  AAdd( p_hsSwitches[ "format" ], SubStr( cArgName, 2 ) )
               ENDIF
            ELSE
               ShowHelp( "Unknown option:" + cArgName + iif( Len(arg) > 0, "=" + arg, "") )
               RETURN
            ENDIF
         ENDCASE
      ENDIF
   NEXT

   // load hbextern.ch
   FileEval( p_hsSwitches[ "basedir" ] + "include" + hb_ps() + "hbextern.ch", ;
      {|c| iif( SubStr( c, 1, Len( "EXTERNAL " ) ) == "EXTERNAL ", ;
                AAdd( p_hsSwitches[ "hbextern.ch" ], SubStr( c, Len( "EXTERNAL " ) + 1 ) ), ;
                ) } )
   ASort( p_hsSwitches[ "hbextern.ch" ] )

   aContent := {}
   AEval( ;
      {;
         p_hsSwitches[ "basedir" ] + "doc", ;
         p_hsSwitches[ "basedir" ] + "doc" + hb_ps() + "en", ;
         iif( p_hsSwitches[ "source" ], p_hsSwitches[ "basedir" ] + "source", NIL ), ;
         iif( p_hsSwitches[ "contribs" ], p_hsSwitches[ "basedir" ] + "contrib", NIL ), ;
      }, ;
      {|c| iif( ! Empty( c ), ProcessFolder( c, @aContent ), ) } )

   ? HB_NTOS( Len( aContent ) ) + " items found"
   ?

   ASort( aContent, , , {|oL,oR| ;
      HB_NTOS( oL:CategoryIndex( oL:Category ) ) + " " + HB_NTOS( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + Chr(1) + oL:Name + " " ;
      <= ;
      HB_NTOS( oR:CategoryIndex( oR:Category ) ) + " " + HB_NTOS( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + Chr(1) + oR:Name + " " ;
      } )

   // TODO: what is this for?  it is sorting the category sub-arrays and removing empty (?) sub-arrays, but why?
   FOR idx := 1 TO Len( p_aCategories )
      IF ! Empty( p_aCategories[ idx ] )
         IF Len( p_aCategories[ idx ] ) == 4 // category, list of subcategory, list of entries, handle
            FOR idx2 := Len( p_aCategories[ idx ][ 3 ] ) TO 1 STEP -1
               IF ValType( p_aCategories[ idx ][ 3 ][ idx2 ] ) == "A"
                  ASort( p_aCategories[ idx ][ 3 ][ idx2 ], , , ;
                     {|oL,oR| ;
                        HB_NTOS( oL:CategoryIndex( oL:Category ) ) + " " + HB_NTOS( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + " " + oL:Name ;
                        <= ;
                        HB_NTOS( oR:CategoryIndex( oR:Category ) ) + " " + HB_NTOS( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + " " + oR:Name ;
                        } )
               ELSE
                  ASize( ADel( p_aCategories[ idx ][ 2 ], idx2 ), Len( p_aCategories[ idx ][ 2 ] ) - 1 )
                  ASize( ADel( p_aCategories[ idx ][ 3 ], idx2 ), Len( p_aCategories[ idx ][ 3 ] ) - 1 )
               ENDIF
            NEXT
         ELSE
            ? "Index", idx, " is not length 4 but rather", Len( p_aCategories[ idx ] )
         ENDIF
      ENDIF
   NEXT

   IF Len( p_hsSwitches[ "format" ] ) == 0
      p_hsSwitches[ "format" ] := { "text" }
   ENDIF

   FOR idx2 := 1 TO Len( p_hsSwitches[ "format" ] )
      cFormat := p_hsSwitches[ "format" ][ idx2 ]
      IF cFormat != "all"
         ? "Output as " + cFormat

         DO CASE
         CASE p_hsSwitches[ "output" ] == "single"

            oDocument := &("Generate" + cFormat + "()"):NewDocument( cFormat, "harbour", "Harbour Reference Guide" )

            FOR idx := 1 TO Len( aContent )
               IF SubStr( aContent[ idx ]:sourcefile_, -Len( "1stread.txt" ) ) == "1stread.txt"
                  oDocument:AddEntry( aContent[ idx ] )
                  idx := Len( aContent )
               ENDIF
            NEXT

            FOR idx := 1 TO Len( aContent )
               IF SubStr( aContent[ idx ]:sourcefile_, -Len( "1stread.txt" ) ) == "1stread.txt"
               ELSE
                  oDocument:AddEntry( aContent[ idx ] )
               ENDIF
            NEXT

            oDocument:Generate()
            oDocument := NIL

         CASE p_hsSwitches[ "output" ] == "category"

            oIndex := &("Generate" + cFormat + "()"):NewIndex( cFormat, "harbour", "Harbour Reference Guide" )

            FOR idx := 1 TO Len( aContent )
               IF SubStr( aContent[ idx ]:sourcefile_, -Len( "1stread.txt" ) ) == "1stread.txt"
                  IF oIndex != NIL
                     oIndex:AddEntry( aContent[ idx ] )
                  ENDIF
                  idx := Len( aContent )
               ENDIF
            NEXT

            FOR idx3 := 1 TO Len( p_aCategories )
               IF ! Empty( p_aCategories[ idx3 ] )
                  p_aCategories[ idx3 ][ 4 ] := Filename( p_aCategories[ idx3 ][ 1 ] )
                  //~ oIndex:BeginSection( p_aCategories[ idx3 ][ 1 ], p_aCategories[ idx3 ][ 4 ] )
                  //~ oIndex:EndSection( p_aCategories[ idx3 ][ 1 ], p_aCategories[ idx3 ][ 4 ] )
               ENDIF
            NEXT

            FOR idx3 := 1 TO Len( p_aCategories )
               IF ! Empty( p_aCategories[ idx3 ] )
                  oDocument := &("Generate" + cFormat + "()"):NewDocument( cFormat, p_aCategories[ idx3 ][ 4 ], "Harbour Reference Guide - " + p_aCategories[ idx3 ][ 1 ] )

                  IF oIndex != NIL
                     oIndex:BeginSection( p_aCategories[ idx3 ][ 1 ], oDocument:cFilename )
                  ENDIF
                  oDocument:BeginSection( p_aCategories[ idx3 ][ 1 ], oDocument:cFilename )

                  FOR idx := 1 TO Len( p_aCategories[ idx3 ][ 3 ] )
                     IF ! Empty( p_aCategories[ idx3 ][ 3 ][ idx ] )
                        ASort( p_aCategories[ idx3 ][ 3 ][ idx ], , , {|oL,oR| oL:Name <= oR:Name } )
                        IF Len( p_aCategories[ idx3 ][ 2 ][ idx ] ) > 1 .OR. Len( p_aCategories[ idx3 ][ 2 ][ idx ] ) > 0
                           IF oIndex != NIL
                              oIndex:BeginSection( p_aCategories[ idx3 ][ 2 ][ idx ], oDocument:cFilename )
                           ENDIF
                           oDocument:BeginSection( p_aCategories[ idx3 ][ 2 ][ idx ], oDocument:cFilename )
                        ENDIF
                        FOR idx4 := 1 TO Len( p_aCategories[ idx3 ][ 3 ][ idx ] )
                           IF ! Empty( p_aCategories[ idx3 ][ 3 ][ idx ][ idx4 ] )
                              IF SubStr( p_aCategories[ idx3 ][ 3 ][ idx ][ idx4 ]:sourcefile_, -Len( "1stread.txt" ) ) == "1stread.txt"
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
               oDocument := &("Generate" + cFormat + "()"):NewDocument( cFormat, aContent[ idx ]:filename, "Harbour Reference Guide" )
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

   ?

   RETURN

STATIC PROCEDURE ProcessFolder( cFolder, aContent ) // this is a recursive procedure
   LOCAL aFiles
   LOCAL nLen
   LOCAL idx
   LOCAL cExt

   //~ ? ">>> " + cFolder

   cFolder += hb_ps()

   aFiles := Directory( cFolder + hb_osFileMask(), "D" )
   IF ( nLen := LEN( aFiles ) ) > 0
      FOR idx := 1 TO nLen
         IF aFiles[ idx ][F_ATTR ] == "D"
            IF !( aFiles[ idx ][ F_NAME ] == "." ) .AND. ;
               !( aFiles[ idx ][ F_NAME ] == ".." )

               IF ( p_hsSwitches[ "source" ] .OR. p_hsSwitches[ "contribs" ] ) /* .AND. ;
                  HB_AScan( s_aSkipDirs, {|d| LOWER(d) == LOWER( aFiles[ idx ][ F_NAME ] ) } ) == 0 */
                  ProcessFolder( cFolder + aFiles[ idx ][ F_NAME ], @aContent )
               ENDIF
            ENDIF
         ELSEIF HB_AScan( s_aExclusions, {|f| LOWER(f) == LOWER( aFiles[ idx ][ F_NAME ] ) } ) == 0
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
   LOCAL aHandle := { 0, 0 } // file handle and position
   LOCAL cSectionName
   LOCAL cVersion
   LOCAL o
   LOCAL nOldContentLen := Len( aContent )

   IF ( aHandle[ 1 ] := FOpen( cFile ) ) < 0
      ? "error: could not open " + cFile + ", " + HB_NTOS( Abs( aHandle[ 1 ] ) )
      RETURN .F.
   ENDIF

   IF ! FReadLn( @aHandle, "" ) // assume first line is ID comment prefix
      //~ FClose( aHandle[ 1 ] )
      //~ RETURN .F.
   ENDIF

   IF ! FReadLn( @aHandle, @cVersion ) // assume second line is ID
      //~ FClose( aHandle[ 1 ] )
      //~ RETURN .F.
   ENDIF

   o := Entry():New( "Template" )

   DO WHILE FReadSection( aHandle, @cSectionName, , o )
      IF o:IsField( @cSectionName, TPL_START )
         o := Entry():New( "Template" )
         ProcessBlock( aHandle, @aContent, cFile, cSectionName, @cVersion, @o )
      ENDIF
   ENDDO
   FClose( aHandle[ 1 ] )

   IF ( Len( aContent ) - nOldContentLen ) > 0
      ? "> " + cFile
      ?? " (" + HB_NTOS( Len( aContent ) - nOldContentLen ) + " items)"
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
   o:Template := "?TEMPLATE?"
   o:Name := "?NAME?"

   DO WHILE FReadSection( aHandle, @cSectionName, @cSection, @o )

      DO CASE
      CASE cSectionName == "TEMPLATE"
         IF o:IsTemplate( cSection )
            o:SetTemplate( cSection )
         ELSE
            AddErrorCondition( cFile, "Unknown TEMPLATE '" + cSection + "'" ) // + "' (line " + HB_NTOS( aHandle[ 2 ] ) + ")" // exclude link number, it reports tonnes of entries
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

               IF ( idxCategory := HB_AScan( p_aCategories, {|c| ! Empty( c ) .AND. ( iif( ValType( c ) == "C", LOWER( c ) == LOWER( cSection ), LOWER( c[ 1 ] ) == LOWER( cSection ) ) ) } ) ) == 0
                  AddErrorCondition( cFile, "Unknown CATEGORY '" + cSection + "' for template '" + o:Template, aHandle[ 2 ] )
                  lAccepted := .F.
               ENDIF

            CASE cSectionName == "SUBCATEGORY" .AND. o:IsField( "SUBCATEGORY" )

               IF idxCategory <= 0 .OR. o:Category == ""

                  AddErrorCondition( cFile, "SUBCATEGORY '" + cSection + "' defined before CATEGORY", aHandle[ 2 ] )
                  lAccepted := .F.

               ELSEIF ( idxSubCategory := HB_AScan( p_aCategories[ idxCategory ][ 2 ], {|c| ! ( c == NIL ) .AND. ( iif( ValType( c ) == "C", LOWER( c ) == LOWER( cSection ), LOWER( c[ 1 ] ) == LOWER( cSection ) ) ) } ) ) == 0

                  AddErrorCondition( cFile, "Unknown SUBCATEGORY '" + p_aCategories[ idxCategory ][ 1 ] + "-" + cSection, aHandle[ 2 ] )
                  lAccepted := .F.

               ENDIF

            CASE o:IsField( "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
                     Empty( cSection ) .OR. ;
                     LOWER( cSection ) == "nil" .OR. ;
                     LOWER( cSection ) == "none" .OR. ;
                     LOWER( cSection ) == "none." )

               AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + cSection + ")", aHandle[ 2 ] - 1 )
               lAccepted := .F.

            CASE ! o:IsConstraint( cSectionName, cSection )

               cSource := cSectionName + " is '" + iif( Len( cSection ) <= 20, cSection, SubStr( StrTran( cSection, hb_eol() ), 1, 20 ) + "..." ) + "', should be one of: "
               //~ cSource := HB_HKeyAt( hsTemplate, idx ) + " should be one of: "
               AEval( &( "p_a" + cSectionName ), {|c,n| cSource += iif( n == 1, "", "," ) + c } )
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
      lAccepted := o:IsComplete( @cSource )
      IF ! lAccepted
         AddErrorCondition( cFile, "Missing sections: '" + cSource + "'", aHandle[ 2 ] )
         lAccepted := .F.
      ENDIF
   ENDIF

   IF ! lAccepted

   ELSEIF o:Template == "Function" .AND. ( ;
                     Empty( o:Returns ) .OR. ;
                     LOWER( o:Returns ) == "nil" .OR. ;
                     LOWER( o:Returns ) == "none" .OR. ;
                     LOWER( o:Returns ) == "none." )

      AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + o:Returns + ")", aHandle[ 2 ] )
      //~ lAccepted := .F.

   ELSE

      IF ! ( ;
         /* LOWER( hsBlock[ "CATEGORY" ] ) == "document" .OR. */ ;
         /* ! ( hsBlock[ "SUBCODE" ] == "" ) .OR. */ ;
         .F. )

         cSectionName := Parse( UPPER( o:Name ), "(" )

         IF HB_AScan( p_hsSwitches[ "hbextern.ch" ], cSectionName, , , .T. ) > 0
            AAdd( p_hsSwitches[ "in hbextern" ], cSectionName )
         ELSE
            AAdd( p_hsSwitches[ "not in hbextern" ], cSectionName + "; " + cFile )
         ENDIF

         //~ ? "    > " + cSectionName

      ENDIF

      IF p_hsSwitches[ "include-doc-source" ]
         o:Files += hb_eol() + o:sourcefile_ + iif( p_hsSwitches[ "include-doc-version" ], " (" + o:sourcefileversion_ + ")", "" )
      ENDIF

      o:filename := Filename( o:Name )

      AAdd( aContent, o )

      IF idxSubCategory == -1 .AND. ( ! o:IsField( "SUBCATEGORY" ) .OR. ! o:IsRequired( "SUBCATEGORY" ) ) //.AND. idxSubCategory == -1
         idxSubCategory := o:SubcategoryIndex( o:Category, "" )
         IF idxSubCategory == -1
            idxSubCategory := 1
         ENDIF
      ENDIF

      IF ValType( p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ] ) != "A"
         p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ] := {}
      ENDIF
      AAdd( p_aCategories[ idxCategory ][ 3 ][ idxSubCategory ], o )

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
      IF SubStr( cSectionName, 1, 1 ) == p_hsSwitches[ "DELIMITER" ] .AND. ;
         SubStr( cSectionName, -1, 1 ) == p_hsSwitches[ "DELIMITER" ]

         cSectionName := SubStr( cSectionName, 1 + Len( p_hsSwitches[ "DELIMITER" ] ), Len( cSectionName ) - ( 2 * Len( p_hsSwitches[ "DELIMITER" ] ) ) )
         IF o:IsField( cSectionName )
            lLastPreformatted := lPreformatted := o:IsPreformatted( cSectionName )
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
                  IF SubStr( cSection, -Len( hb_eol() ) ) != hb_eol()
                     cSection += hb_eol()
                  ENDIF
                  nLastIndent := -1
               ELSEIF AllTrim( cBuffer ) == "<table>"
                  IF SubStr( cSection, -Len( hb_eol() ) ) != hb_eol() .OR. lPreformatted
                     cSection += hb_eol()
                  ENDIF
                  cSection += "<table>" //+ hb_eol()
                  lLastPreformatted := lPreformatted
                  lPreformatted := .T.
               ELSEIF AllTrim( cBuffer ) == "</table>"
                  IF SubStr( cSection, -Len( hb_eol() ) ) != hb_eol() .OR. lPreformatted
                     cSection += hb_eol()
                  ENDIF
                  cSection += "</table>" + hb_eol()
                  lPreformatted := lLastPreformatted
               ELSEIF nLastIndent != ( Len( cBuffer ) - Len( LTrim( cBuffer ) ) ) .OR. lPreformatted .OR. SubStr( cBuffer, -Len( "</par>" ) ) == "</par>"
                  IF SubStr( cBuffer, -Len( "</par>" ) ) == "</par>"
                     cBuffer := SubStr( cBuffer, 1, Len( cBuffer ) - Len( "</par>" ) )
                  ENDIF
                  nLastIndent := ( Len( cBuffer ) - Len( LTrim( cBuffer ) ) )
                  IF SubStr( cSection, -Len( hb_eol() ) ) != hb_eol()
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

   DO WHILE SubStr( cSection, 1, Len( hb_eol() ) ) == hb_eol()
      cSection := SubStr( cSection, Len( hb_eol() ) + 1 )
   ENDDO

   DO WHILE SubStr( cSection, -Len( hb_eol() ) ) == hb_eol()
      cSection := SubStr( cSection, 1, Len( cSection ) - Len( hb_eol() ) )
   ENDDO

   IF lPreformatted .AND. LOWER( SubStr( cSection, -Len( "</fixed>" ) ) ) == "</fixed>"
      cSection := SubStr( cSection, 1, Len( cSection ) - Len( "</fixed>" ) )
      DO WHILE SubStr( cSection, -Len( hb_eol() ) ) == hb_eol()
         cSection := SubStr( cSection, 1, Len( cSection ) - Len( hb_eol() ) )
      ENDDO
   ENDIF

   RETURN .T.

STATIC PROCEDURE FileEval( acFile, bBlock, nMaxLine )
   LOCAL aHandle := { 0, 0 }
   LOCAL cBuffer
   LOCAL lCloseFile := .F.
   LOCAL xResult

   hb_default( @nMaxLine, 256 )

   IF HB_ISSTRING( acFile )
      lCloseFile := .T.
      IF ( aHandle[ 1 ] := FOpen( acFile ) ) < 0
         RETURN
      ENDIF
   ELSEIF HB_ISNUMERIC( acFile )
      aHandle[ 1 ] := acFile
   ELSE
      aHandle := acFile
   ENDIF

   DO WHILE FReadLn( @aHandle, @cBuffer, nMaxLine )
      xResult := Eval( bBlock, cBuffer )
      IF xResult != NIL .AND. HB_ISLOGICAL( ValType( xResult ) ) .AND. ! xResult
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
   FOR idx := 1 To Len( s_aEOL )
      IF ( nEol := At( s_aEOL[ idx ], cLine ) ) > 0
         nLenEol := hb_BLen( s_aEOL[ idx ] ) - 1
         Exit
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

   IF cKey != NIL .AND. hsBlock != NIL .AND. HB_HHasKey( hsBlock, cKey )
      cCode := hsBlock[ cKey ]
   ELSE
      cCode := cKey
   ENDIF

   DO CASE
   CASE cType == "STATUS"
      IF "," $ cCode .AND. HB_AScan( p_aStatus, Parse( ( cCode ), "," ) ) > 0
         cResult := ""
         DO WHILE LEN( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, LEN( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := HB_AScan( p_aStatus, {|a| a[1] == cCode } ) ) > 0
         RETURN p_aStatus[ idx ][ 2 ]
      ELSEIF LEN( cCode ) > 1
         RETURN cCode
      ELSEIF Len( cCode ) > 0
         RETURN "Unknown 'STATUS' code: '" + cCode + "'"
      ELSE
         RETURN ATail( p_aStatus )[ 2 ]
      ENDIF

   CASE cType == "PLATFORMS"
      IF "," $ cCode .AND. HB_AScan( p_aPlatforms, Parse( ( cCode ), "," ) ) > 0
         cResult := ""
         DO WHILE LEN( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, LEN( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := HB_AScan( p_aPlatforms, {|a| a[1] == cCode } ) ) > 0
         RETURN p_aPlatforms[ idx ][ 2 ]
      ELSE
         RETURN "Unknown 'PLATFORMS' code: '" + cCode + "'"
      ENDIF

   CASE cType == "COMPLIANCE"
      IF "," $ cCode .AND. HB_AScan( p_aCompliance, Parse( ( cCode ), "," ) ) > 0
         cResult := ""
         DO WHILE LEN( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, LEN( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := HB_AScan( p_aCompliance, {|a| a[1] == cCode } ) ) > 0
         RETURN p_aCompliance[ idx ][ 2 ]
      ELSE
         RETURN "Unknown 'COMPLIANCE' code: '" + cCode + "'"
      ENDIF
      DO CASE
      CASE Empty( cCode ) ;      RETURN cCode
      CASE cCode == "C" ;        RETURN "This is CA-Cl*pper v5.2 compliant"
      CASE cCode == "C(array)" ; RETURN "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements"
      CASE cCode == "C(menu)" ;  RETURN "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements"
      CASE cCode == "C52U" ;     RETURN "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_CLP_UNDOC flag"
      CASE cCode == "C52S" ;     RETURN "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_CLP_STRICT flag"
      CASE cCode == "C53" ;      RETURN "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag"
      CASE cCode == "H" ;        RETURN "This is Harbour specific"
      CASE cCode == "NA" ;       RETURN "Not applicable"
      OTHERWISE ;                RETURN "Unknown 'COMPLIANCE' code: '" + cCode + "'"
      ENDCASE

   CASE cType == "NAME"
      IF hsBlock == NIL
         RETURN cCode
      ELSEIF ! HB_HHasKey( hsBlock, "RETURNS" )
         RETURN hsBlock[ "NAME" ]
      ELSEIF Empty( hsBlock[ "RETURNS" ] ) .OR. ;
         LOWER( hsBlock[ "RETURNS" ] ) == "nil" .OR. ;
         LOWER( hsBlock[ "RETURNS" ] ) == "none" .OR. ;
         LOWER( hsBlock[ "RETURNS" ] ) == "none."

         hsBlock[ "RETURNS" ] := ""

         DO CASE
         CASE LOWER( hsBlock[ "CATEGORY" ] ) == "document"
            RETURN hsBlock[ "NAME" ]
         OTHERWISE
            IF LOWER( hsBlock[ "TEMPLATE" ] ) == "function" .OR. LOWER( hsBlock[ "TEMPLATE" ] ) == "procedure"
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
         AEval( xLine, {|x,n| ShowSubHelp( x, @nMode, nIndent + 2, n ) } )
         IF nMode == 2
            OutStd( hb_eol() )
         ENDIF
      OTHERWISE
         DO CASE
         CASE nMode == 1         ; OutStd( cIndent + xLine ) ; OutStd( hb_eol() )
         CASE nMode == 2         ; OutStd( iif( n > 1, ", ", "") + xLine )
         OTHERWISE               ; OutStd( "(" + HB_NTOS( nMode ) + ") " ) ; OutStd( xLine ) ; OutStd( hb_eol() )
         ENDCASE
      ENDCASE
   ENDIF

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

PROCEDURE ShowHelp( cExtraMessage, aArgs )
   LOCAL nMode := 1

#define OnOrOff(b) iif( b, "excluded", "included" )
#define YesOrNo(b) iif( b, "yes", "no" )
#define IsDefault(b) iif( b, "; default", "" )

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
         {|| ShowTemplatesHelp( iif( Len( aArgs ) >= 3, aArgs[ 3 ], NIL ) ) } ;
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

FUNCTION Split(cVar, xDelimiter)
   LOCAL aResult := {}
   LOCAL clVar := cVar

   DO WHILE Len(clVar) > 0
      AAdd(aResult, Parse( @clVar, xDelimiter ) )
   ENDDO

   RETURN aResult

FUNCTION Join(aVar, cDelimiter)
   LOCAL cResult := ""

   AEval( aVar, {|c,n| cResult += iif( n > 1, cDelimiter, "" ) + c } )

   RETURN cResult

STATIC PROCEDURE AddErrorCondition( cFile, cMessage, nLine )
   IF p_hsSwitches[ "immediate-errors" ]
      qout( cFile + ":" + HB_NTOS( nLine ) + ": " + cMessage )
   ENDIF
   RETURN

FUNCTION Indent( cText, nLeftMargin, nWidth, lRaw )
   LOCAL cResult := ""
   LOCAL idx
   LOCAL cLine
   LOCAL aText

   hb_default( @lRaw, .F. )

   IF nWidth == 0 .or. lRaw
      aText := Split( cText, hb_eol() )
      idx := 99999
      AEval( aText, {|c| iif( Empty(c), , idx := Min( idx, Len( c ) - Len( LTrim( c ) ) ) ) } )
      AEval( aText, {|c,n| aText[ n ] := Space( nLeftMargin ) + SubStr( c, idx + 1 ) } )
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
                  CASE UPPER( SubStr( cLine, idx, 3 ) ) == ".T." .OR. UPPER( SubStr( cLine, idx, 3 ) ) == ".F."
                     idx--
                  CASE UPPER( SubStr( cLine, idx - 2, 3 ) ) == ".T." .OR. UPPER( SubStr( cLine, idx - 1, 3 ) ) == ".F."
                     idx -= 3
                  CASE UPPER( SubStr( cLine, idx, 5 ) ) == ".AND." .OR. UPPER( SubStr( cLine, idx, 5 ) ) == ".NOT."
                     idx--
                  CASE UPPER( SubStr( cLine, idx - 4, 5 ) ) == ".AND." .OR. UPPER( SubStr( cLine, idx - 4, 5 ) ) == ".NOT."
                     idx -= 5
                  CASE UPPER( SubStr( cLine, idx, 4 ) ) == ".OR."
                     idx--
                  CASE UPPER( SubStr( cLine, idx - 3, 4 ) ) == ".OR."
                     idx -= 4
                  CASE UPPER( SubStr( cLine, idx - 1, 4 ) ) == "i.e."
                     idx -= 2
                  CASE UPPER( SubStr( cLine, idx - 3, 4 ) ) == "i.e."
                     idx -= 4
                  CASE UPPER( SubStr( cLine, idx - 1, 4 ) ) == "e.g."
                     idx -= 2
                  CASE UPPER( SubStr( cLine, idx - 3, 4 ) ) == "e.g."
                     idx -= 4
                  CASE UPPER( SubStr( cLine, idx - 1, 2 ) ) == "*."
                     idx -= 2
                  OTHERWISE
                     EXIT
                  END SELECT
               ENDDO
               IF idx <= 0
                  idx := nWidth
               ENDIF

               cResult += Space( nLeftMargin ) + SubStr( cLine, 1, idx - iif( SubStr( cLine, idx, 1 ) == " ", 1, 0 ) ) + hb_eol()
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
   CASE LOWER( cFormat ) == "alnum"

      FOR idx := 1 TO Len( cFile )
         char := LOWER( SubStr( cFile, idx, 1 ) )
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
