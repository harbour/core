/*
 * Document generator - HTML output
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

#include "hbclass.ch"

#ifdef __PLATFORM__DOS
   #define EXTENSION ".htm"
#else
   #define EXTENSION ".html"
#endif

#define STYLEFILE "hbdoc.css"

CREATE CLASS GenerateHTML2 INHERIT GenerateHTML

   METHOD NewIndex( cDir, cFilename, cTitle )
   METHOD NewDocument( cDir, cFilename, cTitle )

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle ) CLASS GenerateHTML2

   ::super:NewDocument( cDir, cFilename, cTitle, EXTENSION )

   RETURN self

METHOD NewIndex( cDir, cFilename, cTitle ) CLASS GenerateHTML2

   ::super:NewIndex( cDir, cFilename, cTitle, EXTENSION )

   RETURN self

CREATE CLASS GenerateHTML INHERIT TPLGenerate

   HIDDEN:
   METHOD RecreateStyleDocument( cStyleFile )
   METHOD OpenTag( cText, ... )
   METHOD Tagged( cText, cTag, ... )
   METHOD CloseTag( cText )
   METHOD Append( cText, cFormat )
   METHOD Newline() INLINE hb_vfWrite( ::hFile, "<br>" + hb_eol() ), self

   CLASS VAR lCreateStyleDocument AS LOGICAL INIT .T.
   VAR TargetFilename AS STRING INIT ""

   EXPORTED:
   METHOD NewFile() HIDDEN
   METHOD NewIndex( cDir, cFilename, cTitle )
   METHOD NewDocument( cDir, cFilename, cTitle )
   METHOD AddEntry( oEntry )
   METHOD AddReference( oEntry, cReference, cSubReference )
   METHOD BeginSection( cSection, cFilename )
   METHOD EndSection( cSection, cFilename )
   METHOD Generate()

   METHOD WriteEntry( cField, oEntry, lPreformatted ) HIDDEN

ENDCLASS

METHOD NewFile() CLASS GenerateHTML

   hb_vfWrite( ::hFile, "<!DOCTYPE html>" + hb_eol() )

   ::OpenTag( "html", "lang", "en" )

   ::OpenTag( "meta", "charset", "utf-8" )
   ::OpenTag( "meta", "name", "generator", "content", "hbdoc" )
   ::OpenTag( "meta", "name", "keywords", "content", "Harbour, Clipper, xBase, database, Free Software, GPL, compiler, cross platform, 32-bit, 64-bit" )

   IF ::lCreateStyleDocument
      ::lCreateStyleDocument := .F.
      ::RecreateStyleDocument( STYLEFILE )
   ENDIF

   ::Append( ::cTitle /* + iif( Empty( ::cDescription ), "", " - " + ::cDescription ) */, "title" )

   ::OpenTag( "link", "rel", "stylesheet", "href", STYLEFILE )

   ::Append( ::cTitle, "h1" )

   RETURN self

METHOD NewDocument( cDir, cFilename, cTitle ) CLASS GenerateHTML

   ::super:NewDocument( cDir, cFilename, cTitle, EXTENSION )
   ::NewFile()

   RETURN self

METHOD NewIndex( cDir, cFilename, cTitle ) CLASS GenerateHTML

   ::super:NewIndex( cDir, cFilename, cTitle, EXTENSION )
   ::NewFile()

   RETURN self

METHOD BeginSection( cSection, cFilename ) CLASS  GenerateHTML

   cSection := SymbolToHTMLID( cSection )

   IF ::IsIndex()
      IF cFilename == ::cFilename
         ::OpenTag( "div", "id", cSection ):Append( cSection, "h" + hb_ntos( ::Depth + 2 ) ):CloseTag( "div" )
      ELSE
         ::OpenTag( "a", "href", cFilename + ::cExtension + "#" + cSection ):Append( cSection, "h" + hb_ntos( ::Depth + 2 ) ):CloseTag( "a" )
      ENDIF
   ELSE
      ::OpenTag( "div", "id", cSection ):Append( cSection, "h" + hb_ntos( ::Depth + 2 ) ):CloseTag( "div" )
   ENDIF
   ::TargetFilename := cFilename
   ::Depth++

   RETURN self

METHOD EndSection( cSection, cFilename ) CLASS  GenerateHTML

   HB_SYMBOL_UNUSED( cSection )
   HB_SYMBOL_UNUSED( cFilename )
   ::Depth--

   RETURN self

METHOD AddReference( oEntry, cReference, cSubReference ) CLASS GenerateHTML

   IF HB_ISOBJECT( oEntry ) .AND. oEntry:ClassName() == "ENTRY"
      ::OpenTag( "a", "href", ::TargetFilename + ::cExtension + "#" + oEntry:Filename ):Append( oEntry:Name ):CloseTag( "a" ):Append( oEntry:OneLiner ):Newline()
   ELSE
      IF HB_ISSTRING( cSubReference )
         ::OpenTag( "a", "href", cReference + ::cExtension + "#" + cSubReference ):Append( oEntry ):CloseTag( "a" ):Newline()
      ELSE
         ::OpenTag( "a", "href", cReference + ::cExtension /* + "#" + oEntry:Filename */ ):Append( oEntry ):CloseTag( "a" ):Newline()
      ENDIF
   ENDIF

   RETURN self

METHOD AddEntry( oEntry ) CLASS GenerateHTML

   LOCAL item

   FOR EACH item IN oEntry:Fields
      IF item[ 1 ] == "NAME"
         ::OpenTag( "div", "id", SymbolToHTMLID( oEntry:filename ) ):OpenTag( "h4" ):Append( oEntry:Name ):CloseTag( "h4" ):CloseTag( "div" )
      ELSEIF oEntry:IsField( item[ 1 ] ) .AND. oEntry:IsOutput( item[ 1 ] ) .AND. Len( oEntry:&( item[ 1 ] ) ) > 0
         ::WriteEntry( item[ 1 ], oEntry, oEntry:IsPreformatted( item[ 1 ] ) )
      ENDIF
   NEXT

   RETURN self

METHOD Generate() CLASS GenerateHTML
   RETURN self

METHOD PROCEDURE WriteEntry( cField, oEntry, lPreformatted ) CLASS GenerateHTML

   LOCAL cCaption := oEntry:FieldName( cField )
   LOCAL cEntry := oEntry:&( cField )

   /* TODO: change this to search the CSS document itself */
   LOCAL cTagClass := iif( Lower( cField ) + "|" $ "name|oneliner|examples|tests|", Lower( cField ), "itemtext" )

   IF ! Empty( cEntry )

#if 0
      hb_default( @lPreformatted, .F. )
      hb_default( @cTagClass, "itemtext" )
#endif

      hb_default( @cCaption, "" )
      IF ! HB_ISNULL( cCaption )
         ::Tagged( cCaption, "div", "class", "itemtitle" )
      ENDIF

      IF lPreformatted
         ::OpenTag( "pre", "class", cTagClass )
         DO WHILE ! HB_ISNULL( cEntry )
            IF Lower( cField ) + "|" $ "examples|tests|"
               ::Append( SubStr( Parse( @cEntry, hb_eol() ), 5 ), "" )
            ELSE
               ::Append( Indent( Parse( @cEntry, hb_eol() ), 0, , .T. ), "" )
            ENDIF
#if 0
            IF ! HB_ISNULL( cEntry ) .AND. ! lPreformatted
               hb_vfWrite( ::hFile, hb_eol() )
            ENDIF
#endif
         ENDDO
         ::CloseTag( "pre" )
      ELSE
         DO WHILE ! HB_ISNULL( cEntry )
            ::OpenTag( "div", "class", cTagClass )
            ::Append( Indent( Parse( @cEntry, hb_eol() ), 0, 70 ), "" ):Newline()
            ::CloseTag( "div" )
         ENDDO
      ENDIF
   ENDIF

   RETURN

METHOD OpenTag( cText, ... ) CLASS GenerateHTML

   LOCAL aArgs := hb_AParams()
   LOCAL idx

   FOR idx := 2 TO Len( aArgs ) STEP 2
      cText += " " + aArgs[ idx ] + "=" + '"' + aArgs[ idx + 1 ] + '"'
   NEXT

   hb_vfWrite( ::hFile, "<" + cText + ">" + hb_eol() )

   RETURN self

METHOD Tagged( cText, cTag, ... ) CLASS GenerateHTML

   LOCAL aArgs := hb_AParams()
   LOCAL cResult := ""
   LOCAL idx

   FOR idx := 3 TO Len( aArgs ) STEP 2
      cResult += " " + aArgs[ idx ] + "=" + '"' + aArgs[ idx + 1 ] + '"'
   NEXT

   hb_vfWrite( ::hFile, "<" + cTag + cResult + ">" + cText + "</" + cTag + ">" + hb_eol() )

   RETURN self

METHOD CloseTag( cText ) CLASS GenerateHTML

   hb_vfWrite( ::hFile, "</" + cText + ">" + hb_eol() )

   IF cText == "html"
      hb_vfClose( ::hFile )
      ::hFile := NIL
   ENDIF

   RETURN self

METHOD Append( cText, cFormat ) CLASS GenerateHTML

   LOCAL idx

   IF ! HB_ISNULL( cText )

      cText := hb_StrReplace( cText, { ;
         "&" => "&amp;", ;
         '"' => "&quot;", ;
         "<" => "&lt;", ;
         ">" => "&gt;" } )

      FOR EACH idx IN hb_ATokens( hb_defaultValue( cFormat, "" ), "," ) DESCEND
         IF ! Empty( idx )
            cText := "<" + idx + ">" + cText + "</" + idx + ">"
         ENDIF
      NEXT

      DO WHILE Right( cText, Len( hb_eol() ) ) == hb_eol()
         cText := hb_StrShrink( cText, Len( hb_eol() ) )
      ENDDO

      hb_vfWrite( ::hFile, cText + hb_eol() )

   ENDIF

   RETURN self

METHOD RecreateStyleDocument( cStyleFile ) CLASS GenerateHTML

   LOCAL cString

   #pragma __streaminclude "hbdoc.css" | cString := %s

   IF ! hb_MemoWrit( ::cDir + hb_ps() + cStyleFile, cString )
      /* TODO: raise an error, could not create style file */
   ENDIF

   RETURN self

STATIC FUNCTION SymbolToHTMLID( cID )
   RETURN Lower( hb_StrReplace( cID, "_ ", "--" ) )
