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

REQUEST HB_CODEPAGE_UTF8EX

#define BASE_DIR        ".." + hb_ps() + ".." + hb_ps()

#define OnOrOff( b )    iif( b, "excluded", "included" )
#define YesOrNo( b )    iif( b, "yes", "no" )
#define IsDefault( b )  iif( b, "; default", "" )

STATIC sc_aExclusions := { "class_tp.txt", "hdr_tpl.txt" }
STATIC sc_hConstraint
STATIC s_hSwitches

PROCEDURE Main( ... )

   LOCAL aArgs := hb_AParams()
   LOCAL idx, idx2, item, item4
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

   s_hSwitches := { ;
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

   /* remove formats that have not been implemented yet */
   FOR EACH item IN s_hSwitches[ "format-list" ] DESCEND
      IF item == "all"
      ELSEIF ! hb_IsFunction( "Generate" + item )
         hb_ADel( item:__enumBase(), item:__enumIndex(), .T. )
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
         CASE cArgName == "-source" ; s_hSwitches[ "basedir" ] := arg + iif( Right( arg, 1 ) == hb_ps(), "", hb_ps() )
         CASE cArgName == "-format"
            IF arg == "" .OR. hb_AScan( s_hSwitches[ "format-list" ], arg, , , .T. ) == 0
               ShowHelp( "Unknown format option '" + arg + "'" )
               RETURN
            ELSE
               IF arg == "all"
                  s_hSwitches[ "format" ] := s_hSwitches[ "format-list" ]
               ELSE
                  AAdd( s_hSwitches[ "format" ], arg )
               ENDIF
            ENDIF
         CASE cArgName == "-output-single" ;          s_hSwitches[ "output" ] := "single"
         CASE cArgName == "-output-category" ;        s_hSwitches[ "output" ] := "category"
         CASE cArgName == "-output-entry" ;           s_hSwitches[ "output" ] := "entry"
         CASE cArgName == "-include-doc-source" ;     s_hSwitches[ "include-doc-source" ] := .T.
         CASE cArgName == "-include-doc-version" ;    s_hSwitches[ "include-doc-version" ] := .T.
         OTHERWISE
            IF hb_AScan( s_hSwitches[ "format-list" ], SubStr( cArgName, 2 ), , , .T. ) > 0
               IF SubStr( cArgName, 2 ) == "all"
                  s_hSwitches[ "format" ] := s_hSwitches[ "format-list" ]
               ELSE
                  AAdd( s_hSwitches[ "format" ], SubStr( cArgName, 2 ) )
               ENDIF
            ELSE
               ShowHelp( "Unknown option:" + cArgName + iif( Len( arg ) > 0, "=" + arg, "" ) )
               RETURN
            ENDIF
         ENDCASE
      ENDIF
   NEXT

   /* load hbextern.ch */
   FileEval( s_hSwitches[ "basedir" ] + "include" + hb_ps() + "hbextern.ch", ;
      {| c | iif( hb_LeftEq( c, "EXTERNAL " ), ;
                  AAdd( s_hSwitches[ "hbextern.ch" ], SubStr( c, Len( "EXTERNAL " ) + 1 ) ), ;
                  ) } )
   ASort( s_hSwitches[ "hbextern.ch" ] )

   aContent := {}
   AEval( ;
      {;
         s_hSwitches[ "basedir" ] + "doc" + hb_ps() + "en", ;
         iif( s_hSwitches[ "source" ], s_hSwitches[ "basedir" ] + "src", NIL ), ;
         iif( s_hSwitches[ "contribs" ], s_hSwitches[ "basedir" ] + "contrib", NIL ), ;
      }, ;
      {| c | iif( ! Empty( c ), ProcessFolder( c, @aContent ), ) } )

   OutStd( hb_ntos( Len( aContent ) ), "items found" + hb_eol() )
   OutStd( hb_eol() )

   ASort( aContent, , , {| oL, oR | ;
      hb_ntos( oL:CategoryIndex( oL:Category ) ) + " " + hb_ntos( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + Chr( 1 ) + oL:Name + " " ;
      <= ;
      hb_ntos( oR:CategoryIndex( oR:Category ) ) + " " + hb_ntos( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + Chr( 1 ) + oR:Name + " " ;
      } )

   /* TODO: what is this for?  it is sorting the category sub-arrays and removing empty (?) sub-arrays, but why? */
   FOR EACH item IN sc_hConstraint[ "categories" ]
      IF ! Empty( item )
         IF Len( item ) == 4  /* category, list of subcategory, list of entries, handle */
            FOR idx2 := Len( item[ 3 ] ) TO 1 STEP -1
               IF HB_ISARRAY( item[ 3 ][ idx2 ] )
                  ASort( item[ 3 ][ idx2 ], , , ;
                     {| oL, oR | ;
                        hb_ntos( oL:CategoryIndex( oL:Category ) ) + " " + hb_ntos( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + " " + oL:Name ;
                        <= ;
                        hb_ntos( oR:CategoryIndex( oR:Category ) ) + " " + hb_ntos( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + " " + oR:Name ;
                        } )
               ELSE
                  hb_ADel( item[ 2 ], idx2, .T. )
                  hb_ADel( item[ 3 ], idx2, .T. )
               ENDIF
            NEXT
         ELSE
            OutStd( "Index", item:__enumIndex(), "is not length 4 but rather", Len( item ), hb_eol() )
         ENDIF
      ENDIF
   NEXT

   IF Len( s_hSwitches[ "format" ] ) == 0
      s_hSwitches[ "format" ] := { "text" }
   ENDIF

   FOR EACH cFormat IN s_hSwitches[ "format" ]
      IF !( cFormat == "all" )
         OutStd( "Output as", cFormat + hb_eol() )

         DO CASE
         CASE s_hSwitches[ "output" ] == "single"

            oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, "harbour", "Harbour Reference Guide" )

            FOR EACH item IN aContent
               IF Right( item:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt"
                  oDocument:AddEntry( item )
                  EXIT
               ENDIF
            NEXT

            FOR EACH item IN aContent
               IF !( Right( item:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt" )
                  oDocument:AddEntry( item )
               ENDIF
            NEXT

            oDocument:Generate()
            oDocument := NIL

         CASE s_hSwitches[ "output" ] == "category"

            oIndex := &( "Generate" + cFormat + "()" ):NewIndex( cFormat, "harbour", "Harbour Reference Guide" )

            FOR EACH item IN aContent
               IF Right( item:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt"
                  IF oIndex != NIL
                     oIndex:AddEntry( item )
                  ENDIF
                  EXIT
               ENDIF
            NEXT

            FOR EACH item IN sc_hConstraint[ "categories" ]
               IF ! Empty( item )
                  item[ 4 ] := Filename( item[ 1 ] )
#if 0
                  oIndex:BeginSection( item[ 1 ], item[ 4 ] )
                  oIndex:EndSection( item[ 1 ], item[ 4 ] )
#endif
               ENDIF
            NEXT

            FOR EACH item IN sc_hConstraint[ "categories" ]
               IF ! Empty( item )
                  oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, item[ 4 ], "Harbour Reference Guide - " + item[ 1 ] )

                  IF oIndex != NIL
                     oIndex:BeginSection( item[ 1 ], oDocument:cFilename )
                  ENDIF
                  oDocument:BeginSection( item[ 1 ], oDocument:cFilename )

                  FOR idx := 1 TO Len( item[ 3 ] )
                     IF ! Empty( item[ 3 ][ idx ] )
                        ASort( item[ 3 ][ idx ], , , {| oL, oR | oL:Name <= oR:Name } )
                        IF Len( item[ 2 ][ idx ] ) > 1 .OR. Len( item[ 2 ][ idx ] ) > 0
                           IF oIndex != NIL
                              oIndex:BeginSection( item[ 2 ][ idx ], oDocument:cFilename )
                           ENDIF
                           oDocument:BeginSection( item[ 2 ][ idx ], oDocument:cFilename )
                        ENDIF
                        FOR EACH item4 IN item[ 3 ][ idx ]
                           IF ! Empty( item4 )
                              IF !( Right( item4:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt" )
                                 IF oIndex != NIL
                                    oIndex:AddReference( item4 )
                                 ENDIF
                                 oDocument:AddEntry( item4 )
                                 IF oIndex != NIL
                                    oDocument:AddReference( "Index", oIndex:cFilename )
                                    /* this kind of works; the reference is outputed but it is not what I meant */
                                    oDocument:AddReference( item[ 1 ], oIndex:cFilename, item[ 4 ] )
                                 ENDIF
                              ENDIF
                           ENDIF
                        NEXT
                        IF Len( item[ 2 ][ idx ] ) > 1 .OR. Len( item[ 2 ][ idx ] ) > 0
                           IF oIndex != NIL
                              oIndex:EndSection( item[ 2 ][ idx ], oDocument:cFilename )
                           ENDIF
                           oDocument:EndSection( item[ 2 ][ idx ], oDocument:cFilename )
                        ENDIF
                     ENDIF
                  NEXT
                  IF oIndex != NIL
                     oIndex:EndSection( item[ 1 ], oDocument:cFilename )
                  ENDIF
                  oDocument:EndSection( item[ 1 ], oDocument:cFilename )
                  oDocument:Generate()
               ENDIF
            NEXT

         CASE s_hSwitches[ "output" ] == "entry"

            FOR EACH item IN aContent
               oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, item:filename, "Harbour Reference Guide" )
               IF oIndex != NIL
                  oIndex:AddEntry( item )
               ENDIF
               oDocument:AddEntry( item )
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

STATIC PROCEDURE ProcessFolder( cFolder, aContent )  /* this is a recursive procedure */

   LOCAL file

#if 0
   OutStd( ">>> " + cFolder + hb_eol() )
#endif

   cFolder += hb_ps()

   FOR EACH file IN Directory( cFolder + hb_osFileMask(), "D" )
      IF file[ F_ATTR ] == "D"
         IF !( file[ F_NAME ] == "." ) .AND. ;
            !( file[ F_NAME ] == ".." )

            IF s_hSwitches[ "source" ] .OR. s_hSwitches[ "contribs" ]
               /* .AND. AScan( s_aSkipDirs, {| d | Lower( d ) == Lower( file[ F_NAME ] ) } ) == 0 */
               ProcessFolder( cFolder + file[ F_NAME ], @aContent )
            ENDIF
         ENDIF
      ELSEIF AScan( sc_aExclusions, {| f | Lower( f ) == Lower( file[ F_NAME ] ) } ) == 0
         IF Lower( hb_FNameExt( file[ F_NAME ] ) ) == ".txt" .AND. ;
            ! ProcessFile( cFolder + file[ F_NAME ], @aContent )
            EXIT
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION ProcessFile( cFile, aContent )

   LOCAL aHandle := { F_ERROR, 0 } /* file handle and position */
   LOCAL cSectionName
   LOCAL cVersion
   LOCAL o
   LOCAL nOldContentLen := Len( aContent )

   IF ( aHandle[ 1 ] := FOpen( cFile ) ) == F_ERROR
      OutErr( "error: could not open " + cFile + ", " + hb_ntos( aHandle[ 1 ] ) + hb_eol() )
      RETURN .F.
   ENDIF

   cVersion := ""

   o := Entry():New( "Template", sc_hConstraint )

   DO WHILE FReadSection( aHandle, @cSectionName, , o )
      IF o:IsField( @cSectionName, TPL_START )
         o := Entry():New( "Template", sc_hConstraint )
         ProcessBlock( aHandle, @aContent, cFile, cSectionName, @cVersion, @o )
      ENDIF
   ENDDO
   FClose( aHandle[ 1 ] )

   IF Len( aContent ) > nOldContentLen
      OutStd( ">", cFile, "(" + hb_ntos( Len( aContent ) - nOldContentLen ) + " items)" + hb_eol() )
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

   cSourceFile := StrTran( ".." + hb_ps() + cFile /* SubStr( cFile, Len( s_hSwitches[ "basedir" ] + hb_ps() ) ) */, iif( hb_ps() == "\", "/", "\" ), hb_ps() )

   o:type_ := cType
   o:sourcefile_ := cSourceFile
   o:sourcefileversion_ := cVersion
   o:Name := "?NAME?"
   o:SetTemplate( "Function" )

   DO WHILE FReadSection( aHandle, @cSectionName, @cSection, @o )

      IF cSectionName == "TEMPLATE"
         IF o:IsTemplate( cSection )
            o:SetTemplate( cSection )
         ELSE
            AddErrorCondition( cFile, "Unknown TEMPLATE '" + cSection + "'" ) // + "' (line " + hb_ntos( aHandle[ 2 ] ) + ")" // exclude link number, it reports tonnes of entries
            lAccepted := .F.
            EXIT
         ENDIF

      ELSEIF Len( cSectionName ) == 0

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

            IF ( idxCategory := AScan( sc_hConstraint[ "categories" ], {| c | ! Empty( c ) .AND. iif( HB_ISCHAR( c ), Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) } ) ) == 0
               AddErrorCondition( cFile, "Unknown CATEGORY '" + cSection + "' for template '" + o:Template, aHandle[ 2 ] )
#if 0
               lAccepted := .F.
#endif
            ENDIF

         CASE cSectionName == "SUBCATEGORY" .AND. o:IsField( "SUBCATEGORY" )

            IF idxCategory <= 0 .OR. o:Category == ""

               AddErrorCondition( cFile, "SUBCATEGORY '" + cSection + "' defined before CATEGORY", aHandle[ 2 ] )
#if 0
               lAccepted := .F.
#endif

            ELSEIF ( idxSubCategory := AScan( sc_hConstraint[ "categories" ][ idxCategory ][ 2 ], {| c | c != NIL .AND. iif( HB_ISCHAR( c ), Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) } ) ) == 0

               AddErrorCondition( cFile, "Unknown SUBCATEGORY '" + sc_hConstraint[ "categories" ][ idxCategory ][ 1 ] + "-" + cSection, aHandle[ 2 ] )
#if 0
               lAccepted := .F.
#endif

            ENDIF

         CASE o:IsField( "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
                  Empty( cSection ) .OR. ;
                  Lower( cSection ) == "nil" .OR. ;
                  Lower( cSection ) == "none" .OR. ;
                  Lower( cSection ) == "none." )

            AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + cSection + ")", aHandle[ 2 ] - 1 )
#if 0
            lAccepted := .F.
#endif

         CASE ! o:IsConstraint( cSectionName, cSection )

            cSource := cSectionName + " is '" + iif( Len( cSection ) <= 20, cSection, Left( StrTran( cSection, hb_eol() ), 20 ) + "..." ) + "', should be one of: "
#if 0
            cSource := hb_HKeyAt( hsTemplate, idx ) + " should be one of: "
#endif
            AEval( sc_hConstraint[ cSectionName ], {| c, n | cSource += iif( n == 1, "", "," ) + c } )
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
   ENDDO

   IF lAccepted .AND. ! o:IsComplete( @cSource )
      AddErrorCondition( cFile, "Missing sections: '" + cSource + "'", aHandle[ 2 ] )
#if 0
      lAccepted := .F.
#endif
   ENDIF

   IF ! lAccepted
   ELSEIF o:Template == "Function" .AND. ( ;
                     Empty( o:Returns ) .OR. ;
                     Lower( o:Returns ) == "nil" .OR. ;
                     Lower( o:Returns ) == "none" .OR. ;
                     Lower( o:Returns ) == "none." )

      AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + o:Returns + ")", aHandle[ 2 ] )
#if 0
      lAccepted := .F.
#endif

   ELSE

      IF ! ( ;
         /* Lower( hsBlock[ "CATEGORY" ] ) == "document" .OR. */ ;
         /* ! ( hsBlock[ "SUBCODE" ] == "" ) .OR. */ ;
         .F. )

         cSectionName := Parse( Upper( o:Name ), "(" )

         IF hb_AScan( s_hSwitches[ "hbextern.ch" ], cSectionName, , , .T. ) > 0
            AAdd( s_hSwitches[ "in hbextern" ], cSectionName )
         ELSE
            AAdd( s_hSwitches[ "not in hbextern" ], cSectionName + "; " + cFile )
         ENDIF

#if 0
         OutStd( "    > " + cSectionName + hb_eol() )
#endif

      ENDIF

      IF s_hSwitches[ "include-doc-source" ]
         o:Files += hb_eol() + o:sourcefile_ + iif( s_hSwitches[ "include-doc-version" ], " (" + o:sourcefileversion_ + ")", "" )
      ENDIF

      o:filename := Filename( o:Name )

      AAdd( aContent, o )

      IF idxSubCategory == -1 .AND. ( ! o:IsField( "SUBCATEGORY" ) .OR. ! o:IsRequired( "SUBCATEGORY" ) ) // .AND. idxSubCategory == -1
         idxSubCategory := o:SubcategoryIndex( o:Category, "" )
      ENDIF

      IF idxCategory > 0 .AND. idxSubCategory > 0
         IF ! HB_ISARRAY( sc_hConstraint[ "categories" ][ idxCategory ][ 3 ][ idxSubCategory ] )
            sc_hConstraint[ "categories" ][ idxCategory ][ 3 ][ idxSubCategory ] := {}
         ENDIF
         AAdd( sc_hConstraint[ "categories" ][ idxCategory ][ 3 ][ idxSubCategory ], o )
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
      IF Left( cSectionName, 1 ) == s_hSwitches[ "DELIMITER" ] .AND. ;
         Right( cSectionName, 1 ) == s_hSwitches[ "DELIMITER" ]

         cSectionName := SubStr( cSectionName, 1 + Len( s_hSwitches[ "DELIMITER" ] ), Len( cSectionName ) - ( 2 * Len( s_hSwitches[ "DELIMITER" ] ) ) )
         IF o:IsField( cSectionName )
            lLastPreformatted := lPreformatted := o:IsPreformatted( cSectionName )
            nLastIndent := -1
            DO WHILE ( nPosition := FSeek( aHandle[ 1 ], 0, FS_RELATIVE ) ), FReadLn( @aHandle, @cBuffer )
               /* TOFIX: this assumes that every line starts with " *" */
               cBuffer := RTrim( SubStr( cBuffer, 3 ) )
               IF Left( LTrim( cBuffer ), 1 ) == s_hSwitches[ "DELIMITER" ] .AND. ;
                  Right( cBuffer, 1 ) == s_hSwitches[ "DELIMITER" ]
                  FSeek( aHandle[ 1 ], nPosition, FS_SET )
                  aHandle[ 2 ]-- /* decrement the line number when rewinding the file */
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
                     cBuffer := hb_StrShrink( cBuffer, Len( "</par>" ) )
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

   DO WHILE hb_LeftEq( cSection, hb_eol() )
      cSection := SubStr( cSection, Len( hb_eol() ) + 1 )
   ENDDO

   DO WHILE Right( cSection, Len( hb_eol() ) ) == hb_eol()
      cSection := hb_StrShrink( cSection, Len( hb_eol() ) )
   ENDDO

   IF lPreformatted .AND. Lower( Right( cSection, Len( "</fixed>" ) ) ) == "</fixed>"
      cSection := hb_StrShrink( cSection, Len( "</fixed>" ) )
      DO WHILE Right( cSection, Len( hb_eol() ) ) == hb_eol()
         cSection := hb_StrShrink( cSection, Len( hb_eol() ) )
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

   LOCAL cLine, nSavePos, nEol, nNumRead, nLenEol, cEOL

   hb_default( @nMaxLine, 256 )

   cBuffer := ""

   nSavePos := FSeek( aHandle[ 1 ], 0, FS_RELATIVE )
   cLine := Space( nMaxLine )
   nNumRead := FRead( aHandle[ 1 ], @cLine, hb_BLen( cLine ) )
   cLine := hb_BLeft( cLine, nNumRead )

   nEol := 0
   FOR EACH cEOL IN s_aEOL
      IF ( nEol := hb_BAt( cEOL, cLine ) ) > 0
         nLenEol := hb_BLen( cEOL ) - 1
         EXIT
      ENDIF
   NEXT

   IF nEol == 0
      cBuffer := cLine
   ELSE
      cBuffer := hb_BLeft( cLine, nEol - 1 )
      FSeek( aHandle[ 1 ], nSavePos + hb_BLen( cBuffer ) + 1 + nLenEol, FS_SET )
   ENDIF

   aHandle[ 2 ]++

   RETURN nNumRead != 0

FUNCTION Decode( cType, hsBlock, cKey )

   LOCAL cCode
   LOCAL cResult
   LOCAL idx

   IF cKey != NIL .AND. hsBlock != NIL .AND. cKey $ hsBlock
      cCode := hsBlock[ cKey ]
   ELSE
      cCode := cKey
   ENDIF

   SWITCH cType
   CASE "STATUS"
      IF "," $ cCode .AND. hb_AScan( sc_hConstraint[ "status" ], Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE Len( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( sc_hConstraint[ "status" ], {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN sc_hConstraint[ "status" ][ idx ][ 2 ]
      ELSEIF Len( cCode ) > 1
         RETURN cCode
      ELSEIF Len( cCode ) > 0
         RETURN "Unknown 'STATUS' code: '" + cCode + "'"
      ELSE
         RETURN ATail( sc_hConstraint[ "status" ] )[ 2 ]
      ENDIF

   CASE "PLATFORMS"
      IF "," $ cCode .AND. hb_AScan( sc_hConstraint[ "platforms" ], Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE Len( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( sc_hConstraint[ "platforms" ], {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN sc_hConstraint[ "platforms" ][ idx ][ 2 ]
      ELSE
         RETURN cCode
      ENDIF

   CASE "COMPLIANCE"
      IF "," $ cCode .AND. hb_AScan( sc_hConstraint[ "compliance" ], Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE Len( cCode ) > 0
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( sc_hConstraint[ "compliance" ], {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN sc_hConstraint[ "compliance" ][ idx ][ 2 ]
      ELSE
         RETURN cCode
      ENDIF

      SWITCH cCode
      CASE "C" ;        RETURN "This is CA-Cl*pper v5.2 compliant"
      CASE "C(array)" ; RETURN "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements"
      CASE "C(menu)" ;  RETURN "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements"
      CASE "C52U" ;     RETURN "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_CLP_UNDOC flag"
      CASE "C52S" ;     RETURN "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_CLP_STRICT flag"
      CASE "C53" ;      RETURN "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag"
      CASE "H" ;        RETURN "This is Harbour specific"
      CASE "NA" ;       RETURN "Not applicable"
      OTHERWISE ;       RETURN cCode
      ENDSWITCH

   CASE "NAME"
      IF hsBlock == NIL
         RETURN cCode
      ELSEIF !( "RETURNS" $ hsBlock )
         RETURN hsBlock[ "NAME" ]
      ELSEIF Empty( hsBlock[ "RETURNS" ] ) .OR. ;
         Upper( hsBlock[ "RETURNS" ] ) == "NIL" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none."

         hsBlock[ "RETURNS" ] := ""

         DO CASE
         CASE Lower( hsBlock[ "CATEGORY" ] ) == "document"
            RETURN hsBlock[ "NAME" ]
         CASE Lower( hsBlock[ "TEMPLATE" ] ) == "function" .OR. ;
              Lower( hsBlock[ "TEMPLATE" ] ) == "procedure"
            RETURN "Procedure " + hsBlock[ "NAME" ]
         OTHERWISE
            RETURN LTrim( hsBlock[ "SUBCATEGORY" ] + " " ) + hsBlock[ "CATEGORY" ] + " " + hsBlock[ "NAME" ]
         ENDCASE
      ELSE
         DO CASE
         CASE ! Empty( hsBlock[ "NAME" ] )
            RETURN "Function " + hsBlock[ "NAME" ]
         OTHERWISE
            RETURN "Unknown 'CATEGORY': " + hsBlock[ "CATEGORY" ]
         ENDCASE
      ENDIF

   ENDSWITCH

   RETURN /* cType + "=" + */ cCode

PROCEDURE ShowSubHelp( xLine, nMode, nIndent, n )

   LOCAL cIndent := Space( nIndent )

   DO CASE
   CASE xLine == NIL
   CASE HB_ISNUMERIC( xLine )
      nMode := xLine
   CASE HB_ISEVALITEM( xLine )
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
      CASE nMode == 1 ; OutStd( cIndent + xLine ); OutStd( hb_eol() )
      CASE nMode == 2 ; OutStd( iif( n > 1, ", ", "" ) + xLine )
      OTHERWISE       ; OutStd( "(" + hb_ntos( nMode ) + ") " ); OutStd( xLine ); OutStd( hb_eol() )
      ENDCASE
   ENDCASE

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN hb_StrFormat( "%d.%d.%d%s (%s) (%s)", ;
      hb_Version( HB_VERSION_MAJOR ), ;
      hb_Version( HB_VERSION_MINOR ), ;
      hb_Version( HB_VERSION_RELEASE ), ;
      hb_Version( HB_VERSION_STATUS ), ;
      hb_Version( HB_VERSION_ID ), ;
      "20" + Transform( hb_Version( HB_VERSION_REVISION ), "99-99-99 99:99" ) )

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
            "-? or --help                    this screen", ;
            "-? <option> or --help <option>  help on <option>, <option> is one of:", ;
            2, ;
            { "Categories", "Templates", "Compliance", "Platforms" }, ;
            1, ;
            "-[format=]<type>                output type, default is text, or one of:", ;
            2, ;
            s_hSwitches[ "format-list" ], ;
            1, ;
            "-output-single                  output is one file" + IsDefault( s_hSwitches[ "output" ] == "single" ), ;
            "-output-category                output is one file per category" + IsDefault( s_hSwitches[ "output" ] == "category" ), ;
            "-output-entry                   output is one file per entry (function, command, etc)" + IsDefault( s_hSwitches[ "output" ] == "entry" ), ;
            "-source=<folder>                source folder, default is .." + hb_ps() + "..", ;
            "-include-doc-source             output is to indicate the document source file name", ;
            "-include-doc-version            output is to indicate the document source file version" ;
         } }

   CASE aArgs[ 2 ] == "Categories"
      aHelp := { ;
         "Defined categories and sub-categories are:", ;
         sc_hConstraint[ "categories" ] }

   CASE aArgs[ 2 ] == "Templates"
      aHelp := { ;
         iif( Len( aArgs ) >= 3, aArgs[ 3 ] + " template is:", "Defined templates are:" ), ;
         "", ;
         {|| ShowTemplatesHelp( iif( Len( aArgs ) >= 3, aArgs[ 3 ], NIL ), s_hSwitches[ "DELIMITER" ] ) } }

   CASE aArgs[ 2 ] == "Compliance"
      aHelp := { ;
         "Defined 'COMPLIANCE' are:", ;
         "", ;
         {|| ShowComplianceHelp() } }

   CASE aArgs[ 2 ] == "Platforms"
      aHelp := { ;
         "Defined 'PLATFORMS' are:", ;
         "", ;
         {|| ShowPlatformsHelp() } }

   OTHERWISE

      ShowHelp( "Unknown help option" )
      RETURN

   ENDCASE

   /* using hbmk2 style */
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

   IF s_hSwitches[ "immediate-errors" ]
      OutStd( cFile + ":" + hb_ntos( nLine ) + ":", cMessage + hb_eol() )
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
                  CASE ! SubStr( cLine, idx, 1 ) $ " ,;.!?"
                     /* do nothing */
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

#ifdef __PLATFORM__DOS
   hb_default( @nLength, 8 )
#else
   hb_default( @nLength, 0 )
#endif

   IF Lower( hb_defaultValue( cFormat, "alnum" ) ) == "alnum"
      FOR idx := 1 TO Len( cFile )
         char := Lower( SubStr( cFile, idx, 1 ) )
         IF ( "0" <= char .AND. char <= "9" ) .OR. ;
            ( "a" <= char .AND. char <= "z" ) .OR. ;
            char == "_"
            cResult += char
            IF nLength > 0 .AND. Len( cResult ) == nLength
               EXIT
            ENDIF
         ENDIF
      NEXT
   ELSE
      cResult := cFile
   ENDIF

   IF hb_AScan( s_Files, cResult, , , .T. ) == 0
      AAdd( s_Files, cResult )
   ELSE
#ifdef __PLATFORM__DOS
      cResult := hb_StrShrink( cResult, 3 )
#endif
      idx := 0
      DO WHILE hb_AScan( s_Files, cResult + PadL( hb_ntos( ++idx ), 3, "0" ), , , .T. ) > 0
      ENDDO
      cResult += PadL( hb_ntos( idx ), 3, "0" )
      AAdd( s_Files, cResult )
   ENDIF

   RETURN cResult

PROCEDURE init_Templates()

   LOCAL item
   LOCAL aSubCategories := { ;
      "Application", ;
      "Array", ;
      "Classes", ;
      "Conversion", ;
      "Database", ;
      "Date/Time", ;
      "Environment", ;
      "Error", ;
      "Events", ;
      "Execute and execution", ; /* replace w/ "Environment"? */
      "Extend", ;
      "FileSys", ;
      "Fixed memory", ;
      "Garbage collector", ;
      "Hash table", ;
      "Idle states", ;
      "INET", ;
      "Internal", ;
      "Item", ;
      "Language and Nation", ;
      "Legacy", ;
      "Macro", ;
      "Math", ;
      "Objects", ;
      "Printer", ;
      "RDD", ;
      "Strings", ;
      "Terminal", ;
      "Undocumented", ;
      "User interface", ;
      "Variable management", ;
      "Virtual machine" }

   LOCAL aCategories := { ;
      { "Document", { "License", "Compiler", "" } }, ;
      { "API", AClone( aSubCategories ) }, ;
      { "C level API", AClone( aSubCategories ) }, ;
      { "C level API compatability", AClone( aSubCategories ) }, ;
      { "Class", { ;
            "", ;
            "Access", ;
            "Assign", ;
            "Constructor", ;
            "Data", ;
            "Definition", ;
            "Destructor", ;
            "Method", ;
            "Var" } }, ;
      { "Command", AClone( aSubCategories ) }, ;
      /* { "Compile time errors", { {} } }, */ ;
      { "Run time errors", { "" } } }

   LOCAL aCompliance := { ;
      { "",         "" }, ;
      { "C",        "This is CA-Cl*pper v5.2 compliant" }, ;
      { "C(array)", "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements" }, ;
      { "C(menu)",  "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements" }, ;
      { "C(arrayblock)",  "Codeblock calling frequency and order differs from  CA-Cl*pper, since Harbour uses a different (faster) sorting algorithm (quicksort)" }, ;
      { "C52S",     "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_C52_STRICT flag" }, ;
      { "C52U",     "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_C52_UNDOC flag" }, ;
      { "C53",      "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag" }, ;
      { "H",        "This is Harbour specific" }, ;
      { "NA",       "Not applicable" } }

   LOCAL aPlatforms := { ;
      { "",          "" }, ;
      { "All",       "This is available on all platforms" }, ;
      { "All(GT)",   "This part of the GT API and supported only by some platforms." }, ;
      { "All(LFN)",  "This is available on all platforms." + hb_eol() + ;
                     "If long file names are available Harbour will use/display the first 15 characters " +;
                     "else Harbour will use/display a 8.3 file name consistent with CA-Cl*pper" }, ;
      { "Linux(GT)", "Under Linux the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "OS2(GT)",   "Under OS/2 the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "Win(GT)",   "Under Windows, the return value of MaxRow() function is only affected if called after an SetMode() function" }, ;
      { "BSD",       "This is available on the BSD platform" }, ;
      { "DARWIN",    "This is available on the Darwin platform" }, ;
      { "DOS",       "This is available on the MS-DOS platform" }, ;
      { "HPUX",      "This is available on the HPUX platform" }, ;
      { "LINUX",     "This is available on the Linux platform" }, ;
      { "OS2",       "This is available on the OS/2 platform" }, ;
      { "SUNOS",     "This is available on the SunOS platform" }, ;
      { "Unix",      "This is available on the Unix platform(s)" }, ;
      { "Win",       "This is available on the Windows platform(s)" }, ;
      { "WinCE",     "This is available on the Windows CE platform" } }

   LOCAL aStatus := { ;
      { "",  "" }, ;
      { "R", "Ready" }, ;
      { "S", "Started" }, ;
      { "N", "Not started" } }

   FOR EACH item IN aCategories
      IF ! Empty( item )
         AAdd( item, Array( Len( item[ 2 ] ) ) )  /* holder array of sub-category entries */
         AAdd( item, "" )  /* holder for sub-category file name */
      ENDIF
   NEXT

   sc_hConstraint := { ;
      "categories" => aCategories, ;
      "compliance" => aCompliance, ;
      "platforms" => aPlatforms, ;
      "status" => aStatus }

   hb_HCaseMatch( sc_hConstraint, .F. )

   RETURN

STATIC PROCEDURE ShowTemplatesHelp( cTemplate, cDelimiter )

   LOCAL o := Entry():New( , sc_hConstraint )
   LOCAL idxTemplates, nFrom := 1, nTo := Len( o:Templates )
   LOCAL idx

   IF ! Empty( cTemplate ) .AND. !( cTemplate == "Template" )
      IF o:IsTemplate( cTemplate )
         nFrom := nTo := AScan( o:Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } )
      ELSE
         ShowHelp( "Unknown template '" + cTemplate + "'" )
         RETURN
      ENDIF
   ENDIF

   FOR idxTemplates := nFrom TO nTo
      IF ! Empty( o:Templates[ idxTemplates ] ) .AND. ;
         ! Empty( o:Templates[ idxTemplates ][ 1 ] ) .AND. ;
         !( o:Templates[ idxTemplates ][ 1 ] == "Template" )

#if 0
         IF nFrom != nTo
            ShowSubHelp( o:Templates[ idxTemplates ][ 1 ], 1, 0 )
         ENDIF
#endif

         o:SetTemplate( o:Templates[ idxTemplates ][ 1 ] )

         FOR idx := 1 TO Len( o:Fields )
            IF o:Group[ idx ] != 0
               ShowSubHelp( iif( idx == 1, "/", " " ) + "*  " + cDelimiter + o:Fields[ idx ][ 1 ] + cDelimiter, 1, 0 )
               IF o:Fields[ idx ][ 1 ] == "TEMPLATE"
                  ShowSubHelp( " *      " + o:Template, 1, 0 )
               ELSEIF o:Group[ idx ] != TPL_START .AND. o:Group[ idx ] != TPL_END .AND. .T.
                  ShowSubHelp( " *      " + iif( o:IsRequired( o:Fields[ idx ][ 1 ] ), "<required>", "<optional>" ), 1, 0 )
               ENDIF
            ENDIF
         NEXT
         ShowSubHelp( " */", 1, 0 )
         ShowSubHelp( "", 1, 0 )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE ShowComplianceHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "compliance" ]
      ShowSubHelp( item[ 1 ], 1, 0, item:__enumIndex() )
      ShowSubHelp( Decode( "COMPLIANCE", NIL, item[ 1 ] ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

STATIC PROCEDURE ShowPlatformsHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "platforms" ]
      ShowSubHelp( item[ 1 ], 1, 0, item:__enumIndex() )
      ShowSubHelp( Decode( "PLATFORMS", NIL, item[ 1 ] ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

#if defined( __HBSCRIPT__HBSHELL )
SET PROCEDURE TO "_tmplate.prg"
SET PROCEDURE TO "_genbase.prg"
SET PROCEDURE TO "_gentxt.prg"
SET PROCEDURE TO "_genhtml.prg"
SET PROCEDURE TO "_genxml.prg"
#endif
