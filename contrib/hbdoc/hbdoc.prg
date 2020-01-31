/*
 * Document generator
 *
 * Copyright 2009 April White <bright.tigra gmail.com>
 * Copyright 1999-2003 Luiz Rafael Culik <culikr@uol.com.br> (Portions of this project are based on hbdoc)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/* TODO:
   - handle preformatted text / code / etc
   - include links back to index
   - include jumps to 'top'
   - 'coverage' to have links to corresponding file
   - 'Filename' must return the same file name all of the time for the same source
      - one method to retrieve, one to add?
      - key-value pair [hash table?]

   TODO: - treat '<fixed>' / </fixed> as an non-conformance condition
   ntf: this may be okay for EXAMPLES and TESTS but this is also used
        within other sections, much like <table>

   TODO: - look for embedded 'fixed'

   done - recognize and accept </par>; see macro.txt output esp. hb_SetMacro()
   done - list 'compliance' and 'platforms' within help
   done - list 'category' and 'subcategory' types on help screen
   done - load into memory (class and) method template
   done - minimize these to the barest
   done - build a list of 'categories' and validate against; see what 'classdoc' uses
   done - validate sources against these templates
*/

#include "directry.ch"
#include "hbclass.ch"
#include "hbver.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

REQUEST HB_CODEPAGE_UTF8EX

#define BASE_DIR        ".." + hb_ps() + ".." + hb_ps()

#define OnOrOff( b )    iif( b, "excluded", "included" )
#define YesOrNo( b )    iif( b, "yes", "no" )
#define IsDefault( b )  iif( b, "; default", "" )

#define TPL_START            1
#define TPL_END              2
#define TPL_REQUIRED         4  // intentionally has a 'required' and 'optional' flag
#define TPL_OPTIONAL         8
#define TPL_PREFORMATTED     16
#define TPL_CONSTRAINTLIST   32
#define TPL_TEMPLATE         64
#define TPL_OUTPUT           128

STATIC sc_aExclusions := { "class_tp.txt", "hdr_tpl.txt" }
STATIC sc_hFields
STATIC sc_hTemplates
STATIC sc_hConstraint
STATIC s_hSwitches
STATIC s_hComponent := { => }
STATIC s_generators

PROCEDURE Main( ... )

   LOCAL aArgs := hb_AParams()
   LOCAL idx, item, item4
   LOCAL arg, tmp
   LOCAL cArgName
   LOCAL cFormat
   LOCAL oDocument, oIndex
   LOCAL aContent

   LOCAL generatorClass

   s_generators := { ;
      "all"   =>, ;
      "html"  => @GenerateHTML(), ;
      "ascii" => @GenerateAscii(), ;
      "text"  => @GenerateText(), ;
      "xml"   => @GenerateXML() }

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   init_Templates()

   /* configuration settings, values, etc */ ;
   s_hSwitches := { ;
      "basedir"             => BASE_DIR, ;
      "lang"                => "en", ;
      "doc"                 => .T., ;
      "source"              => .F., ;
      "contribs"            => .T., ;
      "format"              => {}, ;
      "output"              => "single", ;
      "include-doc-source"  => .F., ;
      "immediate-errors"    => .F., ;
      /* internal settings, values, etc */ ;
      "DELIMITER"           => "$", ;
      "hHBX"                => {}, ;
      "in hbextern"         => {}, ;
      "not in hbextern"     => {} }

   IF Empty( aArgs ) .OR. ;
      aArgs[ 1 ] == "-h" .OR. ;
      aArgs[ 1 ] == "--help"
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
         CASE cArgName == "-source" ; s_hSwitches[ "basedir" ] := hb_DirSepAdd( arg )
         CASE cArgName == "-lang" ; s_hSwitches[ "lang" ] := Lower( arg )
         CASE cArgName == "-format"
            IF arg == "" .OR. ! arg $ s_generators
               ShowHelp( "Unrecognized format option '" + arg + "'" )
               RETURN
            ELSEIF arg == "all"
               s_hSwitches[ "format" ] := hb_HKeys( s_generators )
            ELSE
               AAdd( s_hSwitches[ "format" ], arg )
            ENDIF
         CASE hb_LeftEq( cArgName, "-output-" )
            s_hSwitches[ "output" ] := SubStr( cArgName, Len( "-output-" ) + 1 )
         CASE cArgName == "-include-doc-source" ;     s_hSwitches[ "include-doc-source" ] := .T.
         OTHERWISE
            IF SubStr( cArgName, 2 ) $ s_generators
               IF SubStr( cArgName, 2 ) == "all"
                  s_hSwitches[ "format" ] := hb_HKeys( s_generators )
               ELSE
                  AAdd( s_hSwitches[ "format" ], SubStr( cArgName, 2 ) )
               ENDIF
            ELSE
               ShowHelp( "Unrecognized option:" + cArgName + iif( Len( arg ) > 0, "=" + arg, "" ) )
               RETURN
            ENDIF
         ENDCASE
      ENDIF
   NEXT

   s_hSwitches[ "hHBX" ] := { => }
   hb_HCaseMatch( s_hSwitches[ "hHBX" ], .F. )
   aContent := ProcessDirs( s_hSwitches[ "hHBX" ] )

#if 0
   hb_MemoWrit( "hbx.json", hb_jsonEncode( s_hSwitches[ "hHBX" ], .T. ) )
   hb_MemoWrit( "cats.json", hb_jsonencode( sc_hConstraint[ "categories" ], .T. ) )
#endif

   OutStd( hb_ntos( Len( aContent ) ), "items found" + hb_eol() )
   OutStd( hb_eol() )

   ASort( aContent,,, {| oL, oR | ;
         PadR( SortWeight( oL:fld[ "CATEGORY" ] ), 20 ) + ;
         PadR( SortWeight( oL:fld[ "SUBCATEGORY" ] ), 20 ) + ;
         PadR( oL:fld[ "NAME" ], 50 ) ;
      <= ;
         PadR( SortWeight( oR:fld[ "CATEGORY" ] ), 20 ) + ;
         PadR( SortWeight( oR:fld[ "SUBCATEGORY" ] ), 20 ) + ;
         PadR( oR:fld[ "NAME" ], 50 ) ;
      } )

   FOR EACH cFormat IN s_hSwitches[ "format" ]

      IF HB_ISEVALITEM( generatorClass := hb_HGetDef( s_generators, Lower( cFormat ) ) )

         OutStd( "Output as", cFormat + hb_eol() )

         DO CASE
         CASE s_hSwitches[ "output" ] == "single"

            oDocument := Eval( generatorClass ):NewDocument( cFormat, "harbour", "Harbour Reference Guide", s_hSwitches[ "lang" ] )

            FOR EACH item IN aContent
               IF item:_type == "harbour"
                  oDocument:AddEntry( item )
               ENDIF
            NEXT

            FOR EACH tmp IN ASort( hb_HKeys( s_hComponent ) )
               IF !( tmp == "harbour" )
                  FOR EACH item IN aContent
                     IF item:_type == tmp .AND. ;
                        oDocument:AddEntry( item )
                     ENDIF
                  NEXT
               ENDIF
            NEXT

            oDocument:Generate()
            oDocument := NIL

         CASE s_hSwitches[ "output" ] == "component"

            oDocument := Eval( generatorClass ):NewDocument( cFormat, "harbour", "Harbour Reference Guide", s_hSwitches[ "lang" ] )

            FOR EACH item IN aContent
               IF item:_type == "harbour"
                  oDocument:AddEntry( item )
               ENDIF
            NEXT

            oDocument:Generate()

            FOR EACH tmp IN ASort( hb_HKeys( s_hComponent ) )
               IF !( tmp == "harbour" )
                  oDocument := Eval( generatorClass ):NewDocument( cFormat, tmp, hb_StrFormat( "Harbour Reference Guide — %1$s", tmp ), s_hSwitches[ "lang" ] )

                  FOR EACH item IN aContent
                     IF item:_type == tmp .AND. ;
                        oDocument:AddEntry( item )
                     ENDIF
                  NEXT

                  oDocument:Generate()
               ENDIF
            NEXT

            oDocument := NIL

         CASE s_hSwitches[ "output" ] == "category"

            oIndex := Eval( generatorClass ):NewIndex( cFormat, "harbour", "Harbour Reference Guide" )

            FOR EACH item IN aContent
               IF Right( item:_sourcefile, Len( "1stread.txt" ) ) == "1stread.txt"
                  IF oIndex != NIL
                     oIndex:AddEntry( item )
                  ENDIF
                  EXIT
               ENDIF
            NEXT

            FOR EACH item IN sc_hConstraint[ "categories" ]
               item[ 3 ] := Filename( item:__enumKey() )
            NEXT

            FOR EACH item IN sc_hConstraint[ "categories" ]

               oDocument := Eval( generatorClass ):NewDocument( cFormat, item[ 3 ], hb_StrFormat( "Harbour Reference Guide — %1$s", item:__enumKey() ), s_hSwitches[ "lang" ] )

               IF oIndex != NIL
                  oIndex:BeginSection( item:__enumKey(), oDocument:cFilename )
               ENDIF
               oDocument:BeginSection( item:__enumKey(), oDocument:cFilename )

               FOR idx := 1 TO Len( item[ 2 ] )
                  IF ! Empty( item[ 2 ][ idx ] )
                     ASort( item[ 2 ][ idx ], , , {| oL, oR | oL:fld[ "NAME" ] <= oR:fld[ "NAME" ] } )
                     IF Len( item[ 1 ][ idx ] ) > 0
                        IF oIndex != NIL
                           oIndex:BeginSection( item[ 1 ][ idx ], oDocument:cFilename )
                        ENDIF
                        oDocument:BeginSection( item[ 1 ][ idx ], oDocument:cFilename )
                     ENDIF
                     FOR EACH item4 IN item[ 2 ][ idx ]
                        IF ! Empty( item4 )
                           IF !( Right( item4:_sourcefile, Len( "1stread.txt" ) ) == "1stread.txt" )
                              IF oIndex != NIL
                                 oIndex:AddReference( item4 )
                              ENDIF
                              oDocument:AddEntry( item4 )
                              IF oIndex != NIL
                                 oDocument:AddReference( "Index", oIndex:cFilename )
                                 /* this kind of works; the reference is outputed but it is not what I meant */
                                 oDocument:AddReference( item:__enumKey(), oIndex:cFilename, item[ 3 ] )
                              ENDIF
                           ENDIF
                        ENDIF
                     NEXT
                     IF Len( item[ 1 ][ idx ] ) > 0
                        IF oIndex != NIL
                           oIndex:EndSection( item[ 1 ][ idx ], oDocument:cFilename )
                        ENDIF
                        oDocument:EndSection( item[ 1 ][ idx ], oDocument:cFilename )
                     ENDIF
                  ENDIF
               NEXT
               IF oIndex != NIL
                  oIndex:EndSection( item:__enumKey(), oDocument:cFilename )
               ENDIF
               oDocument:EndSection( item:__enumKey(), oDocument:cFilename )
               oDocument:Generate()
            NEXT

         CASE s_hSwitches[ "output" ] == "entry"

            FOR EACH item IN aContent
               oDocument := Eval( generatorClass ):NewDocument( cFormat, item:_filename, "Harbour Reference Guide", s_hSwitches[ "lang" ] )
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

STATIC FUNCTION SortWeight( cString )

   SWITCH hb_defaultValue( cString, "" )
   CASE "Document" ; RETURN Chr( 31 ) + "001" + cString  /* category */
   CASE "Intro"    ; RETURN Chr( 31 ) + "001" + cString  /* subcategory */
   CASE "License"  ; RETURN Chr( 31 ) + "002" + cString  /* subcategory */
   CASE "Compiler" ; RETURN Chr( 31 ) + "003" + cString  /* subcategory */
   ENDSWITCH

   RETURN cString

STATIC FUNCTION ProcessDirs( hAll )

   LOCAL aContent := {}
   LOCAL cDir
   LOCAL file

   DirLoadHBX( s_hSwitches[ "basedir" ] + "include", hAll )

   ProcessDocDir( s_hSwitches[ "basedir" ], "harbour", @aContent )

   IF s_hSwitches[ "contribs" ]

      cDir := s_hSwitches[ "basedir" ] + "contrib"

      FOR EACH file IN hb_DirScan( cDir,, "D" )
         IF file[ F_ATTR ] == "D" .AND. ;
            !( hb_FNameName( hb_DirSepDel( file[ F_NAME ] ) ) == "." ) .AND. ;
            !( hb_FNameName( hb_DirSepDel( file[ F_NAME ] ) ) == ".." )

            DirLoadHBX( cDir + hb_ps() + file[ F_NAME ], hAll )

            IF ! ProcessDocDir( cDir + hb_ps() + file[ F_NAME ], hb_FNameName( file[ F_NAME ] ), @aContent )
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN aContent

STATIC FUNCTION ProcessDocDir( cDir, cComponent, aContent )

   LOCAL aErrMsg := {}
   LOCAL aEntry := __hbdoc_LoadDir( cDir, cComponent, aErrMsg )

   LOCAL hEntry
   LOCAL nOldContentLen, tmp

   FOR EACH tmp IN aErrMsg
      AddErrorCondition( cDir, tmp )
   NEXT

   IF ! Empty( aEntry )

#if 1
      hb_MemoWrit( "_" + aEntry[ 1 ][ "_COMPONENT" ] + ".json", hb_jsonEncode( aEntry, .t. ) )
#endif

      nOldContentLen := Len( aContent )

      FOR EACH hEntry IN aEntry
         IF Lower( hEntry[ "_LANG" ] ) == s_hSwitches[ "lang" ]
            ProcessBlock( hEntry, aContent )
         ENDIF
      NEXT

      IF Len( aContent ) > nOldContentLen
         OutStd( ">", cDir, "(" + hb_ntos( Len( aContent ) - nOldContentLen ), "items)" + hb_eol() )
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION NewLineVoodoo( cSectionIn )

   LOCAL cSection := ""
   LOCAL lPreformatted := .F.
   LOCAL lLastPreformatted := .F.
   LOCAL nLastIndent := -1

   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( cSectionIn, .T. )

      IF Len( AllTrim( cLine ) ) == 0
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
            cSection += hb_eol()
         ENDIF
         nLastIndent := -1
      ELSEIF hb_LeftEq( AllTrim( cLine ), "<table" ) .OR. AllTrim( cLine ) == "<fixed>" .OR. hb_LeftEq( AllTrim( cLine ), '```' )
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
            cSection += hb_eol()
         ENDIF
         cSection += AllTrim( cLine )  // + hb_eol()
         lLastPreformatted := lPreformatted
         lPreformatted := .T.
      ELSEIF AllTrim( cLine ) == "</table>" .OR. AllTrim( cLine ) == "</fixed>"
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
            cSection += hb_eol()
         ENDIF
         cSection += AllTrim( cLine ) + hb_eol()
         lPreformatted := lLastPreformatted
      ELSEIF nLastIndent != ( Len( cLine ) - Len( LTrim( cLine ) ) ) .OR. lPreformatted .OR. Right( cLine, Len( "</par>" ) ) == "</par>"
         IF Right( cLine, Len( "</par>" ) ) == "</par>"
            cLine := hb_StrShrink( cLine, Len( "</par>" ) )
         ENDIF
         nLastIndent := Len( cLine ) - Len( LTrim( cLine ) )
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
            cSection += hb_eol()
         ENDIF
         cSection += iif( lPreformatted, cLine, AllTrim( cLine ) )
      ELSE
         cSection += " " + AllTrim( cLine )
      ENDIF
   NEXT

   IF hb_LeftEq( cSection, hb_eol() )
      cSection := SubStr( cSection, Len( hb_eol() ) + 1 )
   ENDIF
   IF Right( cSection, Len( hb_eol() ) ) == hb_eol()
      cSection := hb_StrShrink( cSection, Len( hb_eol() ) )
   ENDIF

   RETURN cSection

STATIC PROCEDURE ProcessBlock( hEntry, aContent )

   LOCAL cFile := hEntry[ "_DOCSOURCE" ]
   LOCAL cComponent := hEntry[ "_COMPONENT" ]

   LOCAL cSectionName
   LOCAL cSection
   LOCAL lAccepted := .T.
   LOCAL cSource
   LOCAL idxCategory := NIL
   LOCAL idxSubCategory := -1
   LOCAL item

   LOCAL cSourceFile := StrTran( ".." + hb_ps() + cFile, iif( hb_ps() == "\", "/", "\" ), hb_ps() )

   LOCAL o

   /* Set template */
   IF ! "TEMPLATE" $ hEntry
      hEntry[ "TEMPLATE" ] := "Function"
   ENDIF
   IF ! hEntry[ "TEMPLATE" ] $ sc_hTemplates
      hEntry[ "TEMPLATE" ] := "Function"
      AddErrorCondition( cFile, "Unrecognized TEMPLATE '" + hEntry[ "TEMPLATE" ] + "'", .T. )
      lAccepted := .F.
   ENDIF

   o := Entry():New( hEntry[ "TEMPLATE" ] )
   o:_type := cComponent
   o:_sourcefile := cSourceFile

   /* Merge category/subcategory into tag list */
   o:_tags := { => }
   FOR EACH item IN hb_ATokens( ;
      hb_HGetDef( hEntry, "TAGS", "" ) + ;
      ", " + hb_HGetDef( hEntry, "CATEGORY", "" ) + ;
      ", " + hb_HGetDef( hEntry, "SUBCATEGORY", "" ), "," )

      IF ! HB_ISNULL( item := AllTrim( item ) )
         o:_tags[ item ] := NIL
      ENDIF
   NEXT
   hEntry[ "TAGS" ] := ""
   FOR EACH item IN hb_HKeys( o:_tags )
      hEntry[ "TAGS" ] += item
      IF ! item:__enumIsLast()
         hEntry[ "TAGS" ] += ", "
      ENDIF
   NEXT

   IF "CATEGORY" $ hEntry
      IF hEntry[ "CATEGORY" ] $ sc_hConstraint[ "categories" ]
         idxCategory := hEntry[ "CATEGORY" ]
      ELSE
         AddErrorCondition( cFile, "Unrecognized CATEGORY '" + hEntry[ "CATEGORY" ] + "' for template '" + o:fld[ "TEMPLATE" ] )
      ENDIF
   ENDIF

   FOR EACH item IN hEntry

      cSectionName := item:__enumKey()
      cSection := StrTran( item, Chr( 13 ) + Chr( 10 ), hb_eol() )

      IF !( cSectionName == "EXAMPLES" ) .AND. ;
         !( cSectionName == "TESTS" )
         cSection := NewLineVoodoo( cSection )  /* Decides which EOLs to keep and which to drop */
      ENDIF

      cSection := StrTran( cSection, hb_eol(), Chr( 10 ) )

      IF hb_LeftEq( cSectionName, "_" ) .OR. ;
         cSectionName == "TEMPLATE"

         /* do nothing */

      ELSEIF o:IsField( cSectionName )

         DO CASE
         CASE o:IsField( cSectionName, TPL_START )

            AddErrorCondition( cFile, "Encountered another section '" + cSection, .T. )
            lAccepted := .F.
            EXIT

         CASE o:IsField( cSectionName, TPL_END )

            EXIT

         CASE ! Empty( o:fld[ cSectionName ] )

            AddErrorCondition( cFile, "Duplicate " + cSectionName, .T. )
            lAccepted := .F.

         CASE cSectionName == "SUBCATEGORY" .AND. o:IsField( "SUBCATEGORY" )

            IF idxCategory != NIL .AND. ;
               ( idxSubCategory := AScan( sc_hConstraint[ "categories" ][ idxCategory ][ 1 ], {| c | c != NIL .AND. iif( HB_ISSTRING( c ), Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) } ) ) == 0
               AddErrorCondition( cFile, "Unrecognized SUBCATEGORY '" + idxCategory + "'-" + cSection )
            ENDIF

         CASE o:IsField( "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
               Empty( cSection ) .OR. ;
               Lower( cSection ) == "nil" .OR. ;
               Lower( cSection ) == "none" .OR. ;
               Lower( cSection ) == "none." )

            AddErrorCondition( cFile, "'" + o:fld[ "NAME" ] + "' is identified as template " + hEntry[ "TEMPLATE" ] + " but has no RETURNS value (" + cSection + ")" )

         CASE ! o:IsConstraint( cSectionName, cSection )

            cSource := cSectionName + " is '" + iif( Len( cSection ) <= 20, cSection, Left( StrTran( cSection, hb_eol() ), 20 ) + "..." ) + "', should be one of: "
#if 0
            cSource := hb_HKeyAt( hsTemplate, idx ) + " should be one of: "
#endif
            AEval( sc_hConstraint[ cSectionName ], {| c, n | cSource += iif( n == 1, "", "," ) + c } )
            AddErrorCondition( cFile, cSource )

         ENDCASE

         IF lAccepted
            o:fld[ cSectionName ] := ExpandAbbrevs( cSectionName, cSection )
         ENDIF
      ELSE
         AddErrorCondition( cFile, "Using template '" + hEntry[ "TEMPLATE" ] + "' encountered an unexpected section '" + cSectionName + "'", .T. )
         lAccepted := .F.
      ENDIF
   NEXT

   /* Verify entry-wide constraints */
   IF lAccepted

      DO CASE
      CASE ! o:IsComplete( @cSource )
         AddErrorCondition( cFile, "Missing sections: '" + cSource + "'" )
#if 0
         lAccepted := .F.
#endif
      CASE hEntry[ "TEMPLATE" ] == "Function" .AND. ( ;
         Empty( o:fld[ "RETURNS" ] ) .OR. ;
         Lower( o:fld[ "RETURNS" ] ) == "nil" .OR. ;
         Lower( o:fld[ "RETURNS" ] ) == "none" .OR. ;
         Lower( o:fld[ "RETURNS" ] ) == "none." )

         AddErrorCondition( cFile, "'" + o:fld[ "NAME" ] + "' is identified as template " + hEntry[ "TEMPLATE" ] + " but has no RETURNS value (" + o:fld[ "RETURNS" ] + ")" )
#if 0
         lAccepted := .F.
#endif
      ENDCASE
   ENDIF

   IF lAccepted

      IF !( Lower( hEntry[ "CATEGORY" ] ) == "document" )
         cSectionName := Parse( o:fld[ "NAME" ], "(" )
         IF ! cSectionName $ s_hSwitches[ "hHBX" ]
            AddErrorCondition( cFile, "Not found in HBX: " + cSectionName + " " + cComponent )
         ENDIF
      ENDIF

      IF s_hSwitches[ "include-doc-source" ]
         o:Files += hb_eol() + o:_sourcefile
      ENDIF

      o:_filename := Filename( o:fld[ "NAME" ] )

      AAdd( aContent, o )

      IF ! cComponent $ s_hComponent
         s_hComponent[ cComponent ] := NIL
      ENDIF

      IF idxCategory != NIL
         IF idxSubCategory == -1 .AND. ( ! o:IsField( "SUBCATEGORY" ) .OR. ! o:IsRequired( "SUBCATEGORY" ) )
            idxSubCategory := o:SubcategoryIndex( o:fld[ "CATEGORY" ], "" )
         ENDIF
         IF idxSubCategory > 0
            AAdd( sc_hConstraint[ "categories" ][ idxCategory ][ 2 ][ idxSubCategory ], o )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION ExpandAbbrevs( cSectionName, cCode )

   LOCAL cResult

   SWITCH cSectionName
   CASE "STATUS"
      IF "," $ cCode .AND. Parse( cCode, "," ) $ sc_hConstraint[ "status" ]
         cResult := ""
         DO WHILE ! HB_ISNULL( cCode )
            cResult += hb_eol() + ExpandAbbrevs( cSectionName, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF cCode $ sc_hConstraint[ "status" ]
         RETURN sc_hConstraint[ "status" ][ cCode ]
      ELSEIF Len( cCode ) > 1
         RETURN cCode
      ELSEIF ! HB_ISNULL( cCode )
         RETURN "Unrecognized 'STATUS' code: '" + cCode + "'"
      ELSE
         RETURN sc_hConstraint[ "status" ][ "N" ]
      ENDIF

   CASE "PLATFORMS"
      cResult := ""
      FOR EACH cCode IN hb_ATokens( cCode, "," )
         IF ! HB_ISNULL( cCode := AllTrim( cCode ) )
            cResult += hb_eol() + hb_HGetDef( sc_hConstraint[ "platforms" ], cCode, cCode )
         ENDIF
      NEXT
      RETURN SubStr( cResult, Len( hb_eol() ) + 1 )

   CASE "COMPLIANCE"
      IF "," $ cCode .AND. Parse( cCode, "," ) $ sc_hConstraint[ "compliance" ]
         cResult := ""
         DO WHILE ! HB_ISNULL( cCode )
            cResult += hb_eol() + ExpandAbbrevs( cSectionName, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      RETURN hb_HGetDef( sc_hConstraint[ "compliance" ], cCode, cCode )

   ENDSWITCH

   RETURN cCode

STATIC PROCEDURE ShowSubHelp( xLine, /* @ */ nMode, nIndent, n )

   DO CASE
   CASE xLine == NIL
   CASE HB_ISNUMERIC( xLine )
      nMode := xLine
   CASE HB_ISEVALITEM( xLine )
      Eval( xLine )
   CASE HB_ISARRAY( xLine )
      IF nMode == 2
         OutStd( Space( nIndent ) + Space( 2 ) )
      ENDIF
      AEval( xLine, {| x, n | ShowSubHelp( x, @nMode, nIndent + 2, n ) } )
      IF nMode == 2
         OutStd( hb_eol() )
      ENDIF
   OTHERWISE
      DO CASE
      CASE nMode == 1 ; OutStd( Space( nIndent ) + xLine + hb_eol() )
      CASE nMode == 2 ; OutStd( iif( n > 1, ", ", "" ) + xLine )
      OTHERWISE       ; OutStd( "(" + hb_ntos( nMode ) + ") " + xLine + hb_eol() )
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

STATIC PROCEDURE ShowHelp( cExtraMessage, aArgs )

   LOCAL nMode := 1

   LOCAL aHelp

   DO CASE
   CASE Empty( aArgs ) .OR. Len( aArgs ) <= 1 .OR. Empty( aArgs[ 1 ] )
      aHelp := { ;
         cExtraMessage, ;
         "Harbour Document Compiler (hbdoc) " + HBRawVersion(), ;
         "Copyright (c) 1999-2020, " + hb_Version( HB_VERSION_URL_BASE ), ;
         "", ;
         "Syntax:", ;
         "", ;
         { "hbdoc [options]" }, ;
         "", ;
         "Options:", ;
         { ;
            "-h or --help                    this screen", ;
            "-h <option> or --help <option>  help on <option>, <option> is one of:", ;
            2, ;
            { "Categories", "Templates", "Compliance", "Platforms" }, ;
            1, ;
            "-[format=]<type>                output type, default is text, or one of:", ;
            2, ;
            hb_HKeys( s_generators ), ;
            1, ;
            "-output-single                  output is one file" + IsDefault( s_hSwitches[ "output" ] == "single" ), ;
            "-output-category                output is one file per category" + IsDefault( s_hSwitches[ "output" ] == "category" ), ;
            "-output-entry                   output is one file per entry (function, command, etc)" + IsDefault( s_hSwitches[ "output" ] == "entry" ), ;
            "-source=<directory>             source directory, default is .." + hb_ps() + "..", ;
            "-include-doc-source             output is to indicate the document source file name", ;
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

      ShowHelp( "Unrecognized help option" )
      RETURN

   ENDCASE

   /* using hbmk2 style */
   AEval( aHelp, {| x | ShowSubHelp( x, @nMode, 0 ) } )

   RETURN

FUNCTION Parse( /* @ */ cVar, xDelimiter )

   LOCAL cResult
   LOCAL idx

   IF ( idx := At( xDelimiter, cVar ) ) > 0
      cResult := Left( cVar, idx - 1 )
      cVar := SubStr( cVar, idx + Len( xDelimiter ) )
   ELSE
      cResult := cVar
      cVar := ""
   ENDIF

   RETURN cResult

STATIC FUNCTION Join( aVar, cDelimiter )

   LOCAL cResult := ""

   AEval( aVar, {| c, n | cResult += iif( n > 1, cDelimiter, "" ) + c } )

   RETURN cResult

STATIC PROCEDURE AddErrorCondition( cFile, cMessage, lFatal )

   IF s_hSwitches[ "immediate-errors" ] .OR. hb_defaultValue( lFatal, .F. )
      OutStd( cFile + ":", cMessage + hb_eol() )
   ENDIF

   RETURN

FUNCTION Indent( cText, nLeftMargin, nWidth, lRaw, lForceRaw )

   LOCAL cResult := ""
   LOCAL idx
   LOCAL cLine

   LOCAL aText := hb_ATokens( cText, .T. )

   hb_default( @lRaw, .F. )
   hb_default( @lForceRaw, .F. )

   IF nWidth == 0 .OR. lRaw
      idx := 99999
      AEval( aText, {| c | iif( Empty( c ), , idx := Min( idx, Len( c ) - Len( LTrim( c ) ) ) ) } )
      AEval( aText, {| c, n | aText[ n ] := Space( nLeftMargin ) + SubStr( c, idx + 1 ) } )
      cResult := Join( aText, hb_eol() ) + hb_eol() + hb_eol()
   ELSE
      FOR EACH cLine IN aText
         IF hb_LeftEq( cLine, "<table" ) .OR. cLine == "<fixed>"
            lRaw := .T.
         ELSEIF cLine == "</table>" .OR. cLine == "</fixed>"
            cResult += hb_eol()
            lRaw := .F.
         ELSEIF lRaw .OR. lForceRaw
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

            IF ! HB_ISNULL( cLine )
               cResult += Space( nLeftMargin ) + cLine + hb_eol()
            ENDIF

            cResult += hb_eol()
         ENDIF
      NEXT
   ENDIF

   RETURN cResult

STATIC FUNCTION Filename( cFile )

   STATIC s_hFiles := { => }

   LOCAL cResult := ""
   LOCAL idx, tmp

   cFile := Lower( cFile )

   FOR idx := 1 TO Len( cFile )
      tmp := SubStr( cFile, idx, 1 )
      IF hb_asciiIsDigit( tmp ) .OR. hb_asciiIsAlpha( tmp ) .OR. tmp == "_"
         cResult += tmp
      ENDIF
   NEXT

   IF cResult $ s_hFiles
      idx := 0
      DO WHILE ( tmp := cResult + "_" + StrZero( ++idx, 3 ) ) $ s_hFiles
      ENDDO
      cResult := tmp
   ENDIF

   s_hFiles[ cResult ] := NIL

   RETURN cResult

/* a class that will hold one entry */
CREATE CLASS Entry

   METHOD New( cTemplate )
   METHOD IsField( cField, nType )
   METHOD IsConstraint( cField, cSection )
   METHOD IsComplete( cIncompleteFieldsList )
   METHOD IsPreformatted( cField )
   METHOD IsRequired( cField )
   METHOD IsOptional( cField )
   METHOD IsOutput( cField )
   METHOD SubcategoryIndex( cCategory, cSubcategory )

   VAR fld AS HASH

   VAR _group AS ARRAY
   VAR _filename AS STRING
   VAR _type AS STRING
   VAR _sourcefile AS STRING
   VAR _tags AS HASH

ENDCLASS

METHOD New( cTemplate ) CLASS Entry

   LOCAL item, key, idx

   ::fld := { => }
   hb_HCaseMatch( ::fld, .F. )
   hb_HEval( sc_hFields, {| k | ::fld[ k ] := "" } )

   ::_group := sc_hTemplates[ cTemplate ]

   FOR EACH item IN sc_hFields
      key := item:__enumKey()
      idx := item:__enumIndex()
      ::fld[ key ] := iif( key == "TEMPLATE", cTemplate, iif( ::_group[ idx ] == TPL_REQUIRED,, "" ) )
   NEXT

   RETURN self

METHOD IsField( cField, nType ) CLASS Entry

   LOCAL idx

   IF ( idx := hb_HPos( sc_hFields, cField ) ) > 0
      IF ::_group[ idx ] == 0
      ELSEIF HB_ISNUMERIC( nType ) .AND. hb_bitAnd( ::_group[ idx ], nType ) != nType
      ELSE
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

METHOD IsConstraint( cField, cSection ) CLASS Entry

   IF hb_bitAnd( ::_group[ hb_HPos( sc_hFields, cField ) ], hb_bitAnd( TPL_REQUIRED, TPL_OPTIONAL ) ) == 0
      RETURN .T.
   ELSEIF cField $ sc_hConstraint
      RETURN ;
         cSection $ sc_hConstraint[ cField ] .OR. ;
         Parse( cSection, "," ) $ sc_hConstraint[ cField ]
   ENDIF

   RETURN .T.

METHOD IsComplete( cIncompleteFieldsList ) CLASS Entry

   LOCAL lResult := .T.
   LOCAL idx, key

   cIncompleteFieldsList := ""

   FOR idx := 1 TO Len( sc_hFields )
      key := hb_HKeyAt( sc_hFields, idx )
      IF hb_bitAnd( ::_group[ idx ], TPL_REQUIRED ) != 0 .AND. Empty( ::fld[ key ] )
         cIncompleteFieldsList += "," + key
         lResult := .F.
      ENDIF
   NEXT

   cIncompleteFieldsList := SubStr( cIncompleteFieldsList, 2 )

   RETURN lResult

METHOD IsPreformatted( cField ) CLASS Entry
   LOCAL nGroup := hb_HPos( sc_hFields, cField )
   RETURN nGroup > 0 .AND. hb_bitAnd( ::_group[ nGroup ], TPL_PREFORMATTED ) != 0

METHOD IsRequired( cField ) CLASS Entry
   RETURN hb_bitAnd( ::_group[ hb_HPos( sc_hFields, cField ) ], TPL_REQUIRED ) != 0

METHOD IsOptional( cField ) CLASS Entry
   RETURN hb_bitAnd( ::_group[ hb_HPos( sc_hFields, cField ) ], TPL_OPTIONAL ) != 0

METHOD IsOutput( cField ) CLASS Entry
   RETURN hb_bitAnd( ::_group[ hb_HPos( sc_hFields, cField ) ], TPL_OUTPUT ) != 0

METHOD SubcategoryIndex( cCategory, cSubcategory ) CLASS Entry
   RETURN iif( cCategory $ sc_hConstraint[ "categories" ], ;
      hb_AScan( sc_hConstraint[ "categories" ][ cCategory ][ 1 ], cSubcategory, , , .T. ), ;
      0 )

FUNCTION FieldIDList()
   RETURN hb_HKeys( sc_hFields )

FUNCTION FieldCaption( cName )
   RETURN sc_hFields[ cName ]

STATIC PROCEDURE init_Templates()

   LOCAL item, tmp
   LOCAL aSubCategories := { ;
      "", ;
      "Application", ;
      "Array", ;
      "Classes", ;
      "Conversion", ;
      "Database", ;
      "Date/Time", ;
      "Environment", ;
      "Error", ;
      "Events", ;
      "Execute and execution", ;  /* replace w/ "Environment"? */
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

   sc_hConstraint := { => }
   hb_HCaseMatch( sc_hConstraint, .F. )

   sc_hConstraint[ "categories" ] := { ;
      "Document"                  => { { "License", "Compiler", "" } }, ;
      "API"                       => { AClone( aSubCategories ) }, ;
      "C level API"               => { AClone( aSubCategories ) }, ;
      "C level API compatability" => { AClone( aSubCategories ) }, ;
      "Class"                     => { { ;
          "", ;
          "Access", ;
          "Assign", ;
          "Constructor", ;
          "Data", ;
          "Definition", ;
          "Destructor", ;
          "Method", ;
          "Var" } }, ;
      "Command"                   => { AClone( aSubCategories ) }, ;
      /* "Compile time errors"    => { { "" } }, */ ;
      "Run time errors"           => { { "" } } }

   hb_HCaseMatch( sc_hConstraint[ "categories" ], .F. )

   FOR EACH item IN sc_hConstraint[ "categories" ]
      AAdd( item, Array( Len( item[ 1 ] ) ) )  /* holder array of sub-category entries */
      FOR EACH tmp IN ATail( item )
         tmp := {}
      NEXT
      AAdd( item, "" )  /* holder for sub-category file name */
   NEXT

   sc_hConstraint[ "compliance" ] := { ;
      "C"       => "CA-Cl*pper v5.x compatible", ;
      "C52S"    => "CA-Cl*pper v5.x compatible in builds with HB_CLP_STRICT option enabled", ;
      "C52U"    => "Undocumented CA-Cl*pper v5.x function only available in builds with HB_CLP_UNDOC option enabled (default)", ;
      "C53"     => "CA-Cl*pper v5.3 function only available in builds with HB_COMPAT_C53 option enabled (default)", ;
      "H"       => "Harbour specific", ;
      "NA"      => "Not applicable" }

   sc_hConstraint[ "platforms" ] := { ;
      "All"     => "Available on all platforms", ;
      "Unix"    => "Available on Unix platform(s)", ;
      "Linux"   => "Available on Linux", ;
      "Windows" => "Available on Windows", ;
      "OS2"     => "Available on OS/2", ;
      "DOS"     => "Available on MS-DOS" }

   sc_hConstraint[ "status" ] := { ;
      "R" => "Ready", ;
      "S" => "Started", ;
      "N" => "Not started" }

   sc_hFields := { ;
      "DOC"           => "Doc", ;
      "TEMPLATE"      => "Template", ;
      "NAME"          => "", ;
      "ONELINER"      => "", ;
      "CATEGORY"      => "Category", ;
      "SUBCATEGORY"   => "Sub category", ;
      "SYNTAX"        => "Syntax", ;
      "ARGUMENTS"     => "Argument(s)", ;
      "RETURNS"       => "Returns", ;
      "DESCRIPTION"   => "Description", ;
      "DATALINK"      => "Data link", ;
      "DATANOLINK"    => "Data no link", ;
      "METHODSLINK"   => "Methods link", ;
      "METHODSNOLINK" => "Methods no link", ;
      "EXAMPLES"      => "Example(s)", ;
      "TESTS"         => "Test(s)", ;
      "STATUS"        => "Status", ;       /* sc_hConstraint[ "status" ] is the constraint list */
      "COMPLIANCE"    => "Compliance", ;   /* sc_hConstraint[ "compliance" ] is the constraint list */
      "PLATFORMS"     => "Platform(s)", ;  /* sc_hConstraint[ "platforms" ] is the constraint list */
      "FILES"         => "File(s)", ;
      "TAGS"          => "Tag(s)", ;
      "SEEALSO"       => "See also", ;
      "END"           => "End" }

   hb_HCaseMatch( sc_hFields, .F. )

   #define _S TPL_START
   #define _E TPL_END
   #define _T TPL_TEMPLATE
   #define _R TPL_REQUIRED
   #define _O TPL_OPTIONAL
   #define _P TPL_PREFORMATTED
   #define _U TPL_OUTPUT

   /* The columns of this array correspond to the elements of sc_hFields */
   sc_hTemplates := { ;
      "Template"       => { _S, _T,  0+_U,  0+_U,  0, _O,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U, _E }, ;
      "Document"       => { _S, _T, _R+_U, _O+_U, _R, _O,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Function"       => { _S, _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "C Function"     => { _S, _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Procedure"      => { _S, _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U,     0, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Command"        => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Class"          => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U, _R+_U, _R+_U, _O+_U, _O+_U, _O+_U, _O+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Class method"   => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _O+_U, _E }, ;
      "Class data"     => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _O+_U, _E }, ;
      "Run time error" => { _S, _T, _R+_U, _O+_U, _R,  0,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U, _O+_U,  0+_U,  0+_U, _O+_U, _O+_U, _E } }

   RETURN

STATIC PROCEDURE ShowTemplatesHelp( cTemplate, cDelimiter )

   LOCAL idxTemplates, nFrom := 1, nTo := Len( sc_hTemplates )
   LOCAL idx, key, fldkey, o

   IF ! Empty( cTemplate ) .AND. !( cTemplate == "Template" )
      nFrom := nTo := hb_HPos( sc_hTemplates, cTemplate )
      IF nFrom == 0
         ShowHelp( "Unrecognized template '" + cTemplate + "'" )
         RETURN
      ENDIF
   ENDIF

   FOR idxTemplates := nFrom TO nTo

      key := hb_HKeyAt( sc_hTemplates, idxTemplates )

      IF !( key == "Template" )

#if 0
         IF nFrom != nTo
            ShowSubHelp( key, 1, 0 )
         ENDIF
#endif

         o := Entry():New( key )

         FOR idx := 1 TO Len( sc_hFields )
            fldkey := hb_HKeyAt( sc_hFields, idx )
            IF o:_group[ idx ] != 0
               ShowSubHelp( iif( idx == 1, "/", " " ) + "*  " + cDelimiter + fldkey + cDelimiter, 1, 0 )
               IF fldkey == "TEMPLATE"
                  ShowSubHelp( " *      " + o:fld[ "TEMPLATE" ], 1, 0 )
               ELSEIF o:_group[ idx ] != TPL_START .AND. o:_group[ idx ] != TPL_END .AND. .T.
                  ShowSubHelp( " *      " + iif( o:IsRequired( fldkey ), "<required>", "<optional>" ), 1, 0 )
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
      ShowSubHelp( item:__enumKey(), 1, 0, item:__enumIndex() )
      ShowSubHelp( ExpandAbbrevs( "COMPLIANCE", item:__enumKey() ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

STATIC PROCEDURE ShowPlatformsHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "platforms" ]
      ShowSubHelp( item:__enumKey(), 1, 0, item:__enumIndex() )
      ShowSubHelp( ExpandAbbrevs( "PLATFORMS", item:__enumKey() ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

STATIC PROCEDURE DirLoadHBX( cDir, hAll )

   LOCAL aFile
   LOCAL cFileName

   cDir := hb_DirSepAdd( cDir )

   FOR EACH aFile IN hb_vfDirectory( cDir + "*.hbx" )
      IF hb_vfExists( cFileName := cDir + aFile[ F_NAME ] )
         LoadHBX( cFileName, hAll )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION LoadHBX( cFileName, hAll )

   LOCAL cName := StrTran( cFileName, "\", "/" )

   LOCAL cFile
   LOCAL pRegex
   LOCAL tmp
   LOCAL aDynamic := {}
   LOCAL cFilter

   IF ! HB_ISNULL( cFile := hb_MemoRead( cFileName ) )

      FOR EACH cFilter IN { ;
         "^DYNAMIC ([a-zA-Z0-9_]*)$", ;
         "ANNOUNCE ([a-zA-Z0-9_]*)$" }

         IF ! Empty( pRegex := hb_regexComp( cFilter, .T., .T. ) )
            FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
               IF tmp[ 2 ] $ hAll
                  hAll[ tmp[ 2 ] ] += "," + cName
               ELSE
                  hAll[ tmp[ 2 ] ] := cName
               ENDIF
            NEXT
         ENDIF
      NEXT
   ENDIF

   RETURN aDynamic

#if defined( __HBSCRIPT__HBSHELL )
SET PROCEDURE TO "_base.prg"
SET PROCEDURE TO "_txt.prg"
SET PROCEDURE TO "_html.prg"
SET PROCEDURE TO "_xml.prg"
#endif
