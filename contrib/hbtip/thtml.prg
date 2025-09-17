/*
 * HTML Classes
 *
 * Copyright 2007 Hannes Ziegler <hz/at/knowleXbase.com>
 * Copyright 2022 Marinaldo de Jesus <marinaldo.jesus/at/gmail.com>
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

#include "error.ch"
#include "hbclass.ch"
#include "thtml.ch"

#DEFINE THTML_HB_EOL "__THTML_HB_EOL__"

// A Html document can have more than 16 nesting levels.
// The current implementation of FOR EACH is not suitable for the HTML classes

// Directives for a light weight html parser
#xtrans P_PARSER( <c> )       => { <c>, 0, Len( <c> ), 0 }
#xtrans :p_str                => \[ 1 ]  // the string to parse
#xtrans :p_pos                => \[ 2 ]  // current parser position
#xtrans :p_len                => \[ 3 ]  // length of string
#xtrans :p_end                => \[ 4 ]  // last parser position

#xtrans P_SEEK( <a>, <c> )    => ( <a>:p_end := <a>:p_pos, <a>:p_pos := hb_At( <c>, <a>:p_str, <a>:p_end + 1 ) )
#xtrans P_SEEKI( <a>, <c> )   => ( <a>:p_end := <a>:p_pos, <a>:p_pos := hb_AtI( <c>, <a>:p_str, <a>:p_end + 1 ) )
#xtrans P_PEEK( <a>, <c> )    => ( <a>:p_end := <a>:p_pos, hb_LeftEqI( SubStr( <a>:p_str, <a>:p_pos ), <c> ) )
#xtrans P_NEXT( <a> )         => ( <a>:p_end := <a>:p_pos, SubStr( <a>:p_str, ++<a>:p_pos, 1 ) )
#xtrans P_PREV( <a> )         => ( <a>:p_end := <a>:p_pos, SubStr( <a>:p_str, --<a>:p_pos, 1 ) )

// Directives for a light weight stack
#define S_DATA                1  // array holding data elements
#define S_NUM                 2  // number of occupied data elements
#define S_SIZE                3  // total size of data array
#define S_STEP                4  // number of elements for auto sizing

#xtrans S_STACK()             => S_STACK( 64 )
#xtrans S_STACK( <n> )        => { Array( <n> ), 0, <n>, Max( 32, Int( <n> / 2 ) ) }
#xtrans S_GROW( <a> )         => ( iif( ++<a>\[S_NUM] > <a>\[S_SIZE], ASize( <a>\[S_DATA], ( <a>\[S_SIZE] += <a>\[S_STEP] ) ), <a> ) )
#xtrans S_SHRINK( <a> )       => ( iif( <a>\[S_NUM] > 0 .AND. --<a>\[S_NUM] \< <a>\[S_SIZE] - <a>\[S_STEP], ASize( <a>\[S_DATA], <a>\[S_SIZE] -= <a>\[S_STEP] ), <a> ) )
#xtrans S_COMPRESS( <a> )     => ( ASize( <a>\[S_DATA], <a>\[S_SIZE] := <a>\[S_NUM] ) )
#xtrans S_PUSH( <a>, <x> )    => ( S_GROW( <a> ), <a>\[S_DATA, <a>\[S_NUM]] := <x> )
#xtrans S_POP( <a>, @<x> )    => ( <x> := <a>\[S_DATA, <a>\[S_NUM]], <a>\[S_DATA, <a>\[S_NUM]] := NIL, S_SHRINK( <a> ) )
#xtrans S_POP( <a> )          => ( <a>\[S_DATA, <a>\[S_NUM]] := NIL, S_SHRINK( <a> ) )
#xtrans S_TOP( <a> )          => ( <a>\[S_DATA, <a>\[S_NUM]] )


THREAD STATIC t_aHA                // data for HTML attributes
THREAD STATIC t_aHAAria            // data for HTML Aria attributes
THREAD STATIC t_hHT                // data for HTML tags
THREAD STATIC t_aGlbAttr           // data for Global HTML attributes
THREAD STATIC t_cHtmlCP := ""
THREAD STATIC t_aHtmlUnicEntities  // HTML character entities
THREAD STATIC t_cHtmlUnicChars
#ifdef HB_LEGACY_LEVEL4
THREAD STATIC t_aHtmlAnsiEntities  // HTML character entities (ANSI character set)
THREAD STATIC t_cHtmlAnsiChars
#endif
THREAD STATIC t_lInit := .F.       // initialization flag for HTML data

#ifdef _DEBUG_
#xtranslate HIDDEN: => EXPORTED:   // debugger cannot see HIDDEN iVars
#endif

/* Class for handling an entire HTML document */
CREATE CLASS THtmlDocument MODULE FRIENDLY

   HIDDEN:
   VAR oIterator
   VAR nodes

   EXPORTED:
   VAR root    READONLY
   VAR head    READONLY
   VAR body    READONLY
   VAR changed INIT .T.

   METHOD new( cHtmlString )
   METHOD readFile( cFileName )
   METHOD writeFile( cFileName , nIndent , nIncIndent )

   METHOD collect()
   METHOD toString( nIndent , nIncIndent )
   METHOD getNode( cTagName )
   METHOD getNodes( cTagName )
   METHOD findFirst( cName, cAttrib, cValue, cData )
   METHOD findFirstRegex( cName, cAttrib, cValue, cData )
   METHOD findNext() INLINE ::oIterator:Next()

ENDCLASS

// accepts a HTML formatted string
METHOD new( cHtmlString ) CLASS THtmlDocument

   LOCAL oSubNode, oErrNode, aHead, aBody, nMode := 0
   LOCAL cEmptyHtmlDoc
   
#pragma __cstream | cEmptyHtmlDoc:=%s
<!DOCTYPE html>
<html>
    <head>
    </head>
    <body>
        <main>
        </main>
        <footer>
        </footer>
    </body>
</html>
#pragma __endtext

   cEmptyHtmlDoc:=strTran(cEmptyHtmlDoc,CHR(10),"")
   cEmptyHtmlDoc+=hb_eol()

   IF !HB_ISSTRING( cHtmlString )
      ::root := THtmlNode():new( cEmptyHtmlDoc )
   ELSEIF "<html" $ Lower( Left( cHtmlString, 4096 ) )
      ::root := THtmlNode():new( cHtmlString )
   ELSE
      ::root := THtmlNode():new( cEmptyHtmlDoc )
      nMode := 1
   ENDIF

   ::root:document := Self
   ::head := ::getNode( "head" )
   ::body := ::getNode( "body" )

   IF ::head == NIL .AND. ::body == NIL
      // A HTML document consists of <html>, <head> and <body> tags
      // Although they are optional, the THtmlDocument class enforces them
      // so that the instance variables :head and :body are always available
      aHead := {}
      aBody := {}
      FOR EACH oSubNode IN ::root:htmlContent
         IF oSubNode:isType( CM_HEAD )
            AAdd( aHead, oSubNode )
         ELSE
            AAdd( aBody, oSubNode )
         ENDIF
      NEXT

      ::root    := THtmlNode():new( cEmptyHtmlDoc )
      ::root:document := Self
      ::changed := .T.
      ::head    := ::getNode( "head" )
      ::body    := ::getNode( "body" )

      FOR EACH oSubNode IN aHead
         IF oSubNode:isType( CM_HEAD )
            ::head:addNode( oSubNode )
         ELSE
            ::body:addNode( oSubNode )
         ENDIF
      NEXT

      FOR EACH oSubNode IN aBody
         IF Lower( oSubNode:htmlTagName ) $ "html,head,body"
            // This node is an error in the HTML string.
            // We gracefully add its subnodes to the <body> tag
            FOR EACH oErrNode IN oSubNode:htmlContent
               ::body:addNode( oErrNode )
            NEXT
         ELSE
            IF oSubNode:isType( CM_HEAD )
               oSubNode:delete()
               ::head:addNode( oSubNode )
            ELSE
               ::body:addNode( oSubNode )
            ENDIF
         ENDIF
      NEXT

   ELSEIF ::head == NIL
      ::head := ::body:insertBefore( THtmlNode():new( ::body, "head" ) )

   ELSEIF ::body == NIL
      ::head := ::head:insertAfter( THtmlNode():new( ::head, "body" ) )

   ENDIF

   IF nMode == 1
      FOR EACH oSubNode IN THtmlNode():new( cHtmlString ):htmlContent
         IF oSubNode:isType( CM_HEAD )
            ::head:addNode( oSubNode )
         ELSE
            ::body:addNode( oSubNode )
         ENDIF
      NEXT
   ENDIF

   RETURN Self

// Builds a HTML formatted string
METHOD toString( nIndent , nIncIndent ) CLASS THtmlDocument
   RETURN ::root:toString( nIndent , nIncIndent )

// reads HTML file and parses it into tree of objects
METHOD readFile( cFileName ) CLASS THtmlDocument

   IF hb_vfExists( cFileName )
      ::changed := .T.
      ::new( hb_MemoRead( cFileName ) )

      RETURN .T.
   ENDIF

   RETURN .F.

// writes the entire tree of HTML objects into a file
METHOD writeFile( cFileName , nIndent , nIncIndent ) CLASS THtmlDocument

   LOCAL lSuccess := hb_MemoWrit( cFileName, ::toString( nIndent , nIncIndent ) )

   IF lSuccess
      ::changed := .F.
   ENDIF

   RETURN lSuccess

// builds a one dimensional array of all nodes contained in the HTML document
METHOD collect() CLASS THtmlDocument

   IF ::changed
      ::nodes   := ::root:collect()
      ::changed := .F.
   ENDIF

   RETURN ::nodes

// returns the first tag matching the passed tag name
METHOD getNode( cTagName ) CLASS THtmlDocument

   LOCAL oNode
   LOCAL cTagNameToLower

   IF ::changed
      ::collect()
   ENDIF

   cTagNameToLower:=Lower( cTagName )
   FOR EACH oNode IN ::nodes
      IF Lower( oNode:htmlTagName ) == cTagNameToLower
         RETURN oNode
      ENDIF
   NEXT

   RETURN NIL

// returns all tags matching the passed tag name
METHOD getNodes( cTagName ) CLASS THtmlDocument

   LOCAL oNode, stack := S_STACK()
   LOCAL cTagNameToLower

   IF ::changed
      ::collect()
   ENDIF

   cTagNameToLower:=Lower( cTagName )
   FOR EACH oNode IN ::nodes
      IF Lower( oNode:htmlTagName ) == cTagNameToLower
         S_PUSH( stack, oNode )
      ENDIF
   NEXT

   S_COMPRESS( stack )

   RETURN stack[ S_DATA ]

// finds the first HTML tag matching the search criteria
METHOD findFirst( cName, cAttrib, cValue, cData ) CLASS THtmlDocument

   ::oIterator := THtmlIteratorScan():New( Self )

   RETURN ::oIterator:Find( cName, cAttrib, cValue, cData )

// finds the first HTML tag matching the RegEx search criteria
METHOD findFirstRegex( cName, cAttrib, cValue, cData ) CLASS THtmlDocument

   ::oIterator := THtmlIteratorRegex():New( Self )

   RETURN ::oIterator:Find( cName, cAttrib, cValue, cData )

/* Abstract super class for THtmlIteratorScan and THtmlIteratorScanRegEx
   (Adopted from TXMLIterator -> contrib/xhb/txml.prg) */

CREATE CLASS THtmlIterator MODULE FRIENDLY

   METHOD New( oHtml ) CONSTRUCTOR
   METHOD Next()
   METHOD Rewind()
   METHOD Find( cName, cAttribute, cValue, cData )
   METHOD GetNode() INLINE ::oNode
   METHOD SetContext()
   METHOD Clone()

   HIDDEN:

   VAR cName
   VAR cAttribute
   VAR cValue
   VAR cData
   VAR oNode
   VAR oTop
   VAR aNodes
   VAR nCurrent
   VAR nLast

   METHOD MatchCriteria()

ENDCLASS

// accepts a THtmlNode or THtmlDocument object
METHOD New( oHtml ) CLASS THtmlIterator

   IF oHtml:isDerivedFrom ( "THtmlDocument" )
      ::oNode := oHtml:root
      ::aNodes := oHtml:nodes
   ELSE
      ::oNode := oHtml
      ::aNodes := ::oNode:collect()
   ENDIF

   ::oTop     := ::oNode
   ::nCurrent := 1
   ::nLast    := Len( ::aNodes )

   RETURN Self

METHOD rewind() CLASS THtmlIterator

   ::oNode := ::oTop
   ::nCurrent := 0

   RETURN Self

METHOD Clone() CLASS THtmlIterator

   LOCAL oRet

   oRet            := THtmlIterator():New( ::oTop )
   oRet:cName      := ::cName
   oRet:cAttribute := ::cAttribute
   oRet:cValue     := ::cValue
   oRet:cData      := ::cData
   oRet:nCurrent   := 0
   oRet:nLast      := Len( ::aNodes )
   oRet:aNodes     := ::aNodes

   RETURN oRet

METHOD SetContext() CLASS THtmlIterator

   ::oTop          := ::oNode
   ::aNodes        := ::oNode:collect()
   ::nCurrent      := 0
   ::nLast         := Len( ::aNodes )

   RETURN Self

METHOD Find( cName, cAttribute, cValue, cData ) CLASS THtmlIterator

   ::cName         := cName
   ::cAttribute    := cAttribute
   ::cValue        := cValue
   ::cData         := cData

   IF ::nLast == 0
      ::nCurrent := 0
      RETURN NIL
   ENDIF

   IF ::MatchCriteria( ::oNode )
      RETURN ::oNode
   ENDIF

   RETURN ::Next()

METHOD Next() CLASS THtmlIterator

   LOCAL oFound, lExit := .F.

   DO WHILE ! lExit
      BEGIN SEQUENCE WITH __BreakBlock()
         oFound := ::aNodes[ ++::nCurrent ]
         IF ::MatchCriteria( oFound )
            ::oNode := oFound
            lExit := .T.
         ENDIF
      RECOVER
         lExit      := .T.
         oFound     := NIL
         ::nCurrent := 0
      END SEQUENCE
   ENDDO

   RETURN oFound

METHOD MatchCriteria() CLASS THtmlIterator
   RETURN .T.

/* Iterator scan class */

CREATE CLASS THtmlIteratorScan INHERIT THtmlIterator MODULE FRIENDLY

   METHOD New( oNodeTop ) CONSTRUCTOR

   HIDDEN:

   METHOD MatchCriteria( oFound )

ENDCLASS

METHOD New( oNodeTop ) CLASS THtmlIteratorScan

   ::Super:New( oNodeTop )

   RETURN Self

METHOD MatchCriteria( oFound ) CLASS THtmlIteratorScan

   LOCAL xData

   IF ::cName != NIL .AND. ! Lower( ::cName ) == Lower( oFound:htmlTagName )
      RETURN .F.
   ENDIF

   IF ::cAttribute != NIL .AND. ! ::cAttribute $ oFound:getAttributes()
      RETURN .F.
   ENDIF

   IF ::cValue != NIL
      xData := oFound:getAttributes()
      IF hb_HScan( xData, {| xKey, cValue | HB_SYMBOL_UNUSED( xKey ), Lower( ::cValue ) == Lower( cValue ) } ) == 0
         RETURN .F.
      ENDIF
   ENDIF

   IF ::cData != NIL
      xData := oFound:getText( " " )
      /* NOTE: != changed to ! == */
      IF Empty( xData ) .OR. ! AllTrim( ::cData ) == AllTrim( xData )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

/* Iterator regex class */

CREATE CLASS THtmlIteratorRegex INHERIT THtmlIterator MODULE FRIENDLY

   METHOD New( oNodeTop ) CONSTRUCTOR
   HIDDEN:
   METHOD MatchCriteria( oFound )

ENDCLASS

METHOD New( oNodeTop ) CLASS THtmlIteratorRegex

   ::Super:New( oNodeTop )

   RETURN Self

METHOD MatchCriteria( oFound ) CLASS THtmlIteratorRegex

   LOCAL xData

   IF ::cName != NIL .AND. ! hb_regexLike( Lower( oFound:htmlTagName ), Lower( ::cName ) )
      RETURN .F.
   ENDIF

   IF ::cAttribute != NIL .AND. ;
      hb_HScan( oFound:getAttributes(), {| cKey | hb_regexLike( Lower( ::cAttribute ), cKey ) } ) == 0
      RETURN .F.
   ENDIF

   IF ::cValue != NIL .AND. ;
      hb_HScan( oFound:getAttributes(), {| xKey, cValue | HB_SYMBOL_UNUSED( xKey ), hb_regexLike( ::cValue, cValue ) } ) == 0
      RETURN .F.
   ENDIF

   IF ::cData != NIL
      xData := oFound:getText( " " )
      IF Empty( xData ) .OR. ! hb_regexHas( AllTrim( ::cData ), AllTrim( xData ) )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

/* Class representing a HTML node tree.
   It parses a HTML formatted string */

CREATE CLASS THtmlNode MODULE FRIENDLY

   HIDDEN:

   VAR root
   VAR _document
   VAR parent
   VAR htmlContent

   METHOD parseHtml( parser )
   METHOD parseHtmlFixed( parser )
   METHOD _getTextNode()
   METHOD _setTextNode( cText )
   METHOD keepFormatting()

   EXPORTED:

   VAR htmlTagName     READONLY
   VAR htmlEndTagName  READONLY
   VAR htmlTagType     READONLY
   VAR htmlAttributes  READONLY

   METHOD New( oParent, cTagName, cAttrib, cContent )

   METHOD isType( nType )
   ACCESS isEmpty()
   ACCESS isInline()
   ACCESS isOptional()
   ACCESS isNode()
   ACCESS isBlock()

   METHOD addNode( oTHtmlNode )
   METHOD insertAfter( oTHtmlNode )
   METHOD insertBefore( oTHtmlNode )
   METHOD Delete()

   // Messages from TXmlNode
   MESSAGE insertBelow METHOD addNode
   MESSAGE unlink      METHOD Delete

   METHOD firstNode( lRoot )
   METHOD lastNode( lRoot )

   ACCESS nextNode()
   ACCESS prevNode()

   ACCESS siblingNodes()  INLINE iif( ::parent == NIL, NIL, ::parent:htmlContent )
   ACCESS childNodes()    INLINE iif( ::isNode(), ::htmlContent, NIL )
   ACCESS parentNode()    INLINE ::parent
   ACCESS document()      INLINE iif( ::root == NIL, NIL, ::root:_document )

   METHOD toString( nIndent , nIncIndent )
   METHOD __toString( nIndent , nIncIndent )
   METHOD attrToString()

   METHOD collect( oEndNode )
   METHOD getText( cEOL )

   METHOD getAttribute( cName )
   METHOD getAttributes()

   METHOD setAttribute( cName, cValue )
   METHOD setAttributes( xHtml )

   METHOD delAttribute( cName )
   METHOD delAttributes()

   METHOD isAttribute( cName )

   ACCESS TEXT      INLINE ::_getTextNode()
   ASSIGN TEXT( x ) INLINE ::_setTextNode( x )

   ACCESS attr      INLINE ::getAttributes()
   ASSIGN attr( x ) INLINE ::setAttributes( x )

   METHOD pushNode  OPERATOR +
   METHOD popNode   OPERATOR -

   OPERATOR "+=" INLINE ::pushNode()
   OPERATOR "-=" INLINE ::popNode()

   METHOD findNodeByTagName
   METHOD findNodesByTagName

   ERROR HANDLER noMessage

   METHOD noAttribute

ENDCLASS

METHOD new( oParent, cTagName, cAttrib, cContent ) CLASS THtmlNode

   IF ! t_lInit
      THtmlInit( .T. )
   ENDIF

   IF HB_ISSTRING( oParent )
      // a HTML string is passed -> build new tree of objects
      oParent := StrTran( oParent, Chr( 9 ), " " )
      ::root           := Self
      ::htmlTagName    := "_root_"
      ::htmlTagType    := THtmlTagType( "_root_" )
      ::htmlContent    := {}
      ::parseHtml( P_PARSER( oParent ) )
   ELSEIF HB_ISOBJECT( oParent )
      // a HTML object is passed -> we are in the course of building an object tree
      ::root        := oParent:root
      ::parent      := oParent
      IF HB_ISSTRING( cAttrib )
         IF Right( cAttrib, 1 ) == "/"
            ::htmlEndTagName := "/"
            ::htmlAttributes := hb_StrShrink( cAttrib )
         ELSE
            ::htmlAttributes := cAttrib
         ENDIF
      ELSE
         ::htmlAttributes := cAttrib
      ENDIF
      ::htmlTagName := cTagName
      ::htmlTagType := THtmlTagType( cTagName )
      ::htmlContent := iif( cContent == NIL, {}, cContent )
   ELSE
      RETURN ::error( "Parameter error", ::className(), ":new()", EG_ARG, hb_AParams() )
   ENDIF

   RETURN Self

METHOD isType( nType ) CLASS THtmlNode

   LOCAL lRet

   BEGIN SEQUENCE WITH __BreakBlock()
      lRet := hb_bitAnd( ::htmlTagType[ 2 ], nType ) != 0
   RECOVER
      lRet := .F.
   END SEQUENCE

   RETURN lRet

// checks if this is a node that is always empty and never has HTML text, e.g. <img>,<link>,<meta>
METHOD isEmpty() CLASS THtmlNode
   RETURN hb_bitAnd( ::htmlTagType[ 2 ], CM_EMPTY ) != 0

// checks if this is a node that may occur inline, eg. <b>,<font>
METHOD isInline() CLASS THtmlNode
   RETURN hb_bitAnd( ::htmlTagType[ 2 ], CM_INLINE ) != 0

// checks if this is a node that may appear without a closing tag, eg. <p>,<tr>,<td>
METHOD isOptional() CLASS THtmlNode
   RETURN hb_bitAnd( ::htmlTagType[ 2 ], CM_OPT ) != 0

// checks if this is a node (leafs contain no further nodes, e.g. <br />,<hr>,_text_)
METHOD isNode() CLASS THtmlNode
   RETURN HB_ISARRAY( ::htmlContent ) .AND. Len( ::htmlContent ) > 0

// checks if this is a block node that must be closed with an ending tag: eg: <table></table>, <ul></ul>
METHOD isBlock() CLASS THtmlNode
   RETURN hb_bitAnd( ::htmlTagType[ 2 ], CM_BLOCK ) != 0

// checks if this is a node whose text line formatting must be preserved: <pre>,<script>,<textarea>
METHOD keepFormatting() CLASS THtmlNode
   RETURN "<" + Lower( ::htmlTagName ) + ">" $ "<pre>,<script>,<textarea>"

// parses a HTML string and builds a tree of THtmlNode objects
METHOD parseHtml( parser ) CLASS THtmlNode

   LOCAL nLastPos := parser:p_pos
   LOCAL lRewind  := .F.
   LOCAL oThisTag, oNextTag, oLastTag
   LOCAL cTagName, cAttr, nStart, nEnd, nPos, cText

   IF ! "<" $ parser:p_Str
      // Plain text
      ::addNode( THtmlNode():new( Self, "_text_", , parser:p_Str ) )
      RETURN Self
   ENDIF

   oThisTag := Self

   DO WHILE P_SEEK( parser, "<" ) > 0
      nStart   := parser:p_pos
      P_SEEK( parser, ">" )
      nEnd     := parser:p_pos
      cAttr    := SubStr( parser:p_Str, nStart, nEnd - nStart + 1 )
      cText    := LTrim( SubStr( parser:p_str, nLastPos + 1, nStart - nLastPos - 1 ) )
      cTagName := CutStr( " ", @cAttr )

      IF ! cText == ""
         IF hb_LeftEq( cText, "</" )
            // ending tag of previous node
            cText := Lower( AllTrim( SubStr( CutStr( ">", @cText ), 3 ) ) )
            oLastTag := oThisTag:parent
            DO WHILE oLastTag != NIL .AND. ! Lower( oLastTag:htmlTagName ) == cText  /* NOTE: != changed to ! == */
               oLastTag := oLastTag:parent
            ENDDO
            IF oLastTag != NIL
               oLastTag:htmlEndTagName := "/" + oLastTag:htmlTagName
            ENDIF

         ELSEIF Chr( 10 ) $ cText
            cText := RTrim( cText )
            nPos := Len( cText ) + 1
            DO WHILE nPos > 0 .AND. SubStr( cText, --nPos, 1 ) $ Chr( 9 ) + Chr( 10 ) + Chr( 13 )
            ENDDO
            oThisTag:addNode( THtmlNode():new( oThisTag, "_text_", , Left( cText, nPos ) ) )
         ELSE
            oThisTag:addNode( THtmlNode():new( oThisTag, "_text_", , cText ) )
         ENDIF
      ENDIF

      IF cTagName == "<"
         // <  tagName>
         cAttr    := LTrim( cAttr )
         cTagName += CutStr( " ", @cAttr )
      ENDIF
      cTagName := StrTran( cTagName, ">" )
      cTagName := AllTrim( SubStr( cTagName, 2 ) )

      SWITCH Left( cTagName, 1 )
      CASE "!"
         // comment or PI
         oThisTag:addNode( THtmlNode():new( oThisTag, cTagName, hb_StrShrink( cAttr ) ) )
         EXIT

      CASE "/"
         // end tag
         IF Lower( "/" + oThisTag:htmlTagName ) == Lower( cTagName )
            oThisTag:htmlEndTagName := cTagName

         ELSE

            oNextTag := oThisTag:parent
            DO WHILE oNextTag != NIL .AND. ! Lower( oNextTag:htmlTagName ) == Lower( SubStr( cTagName, 2 ) )  /* NOTE: != changed to ! == */
               oNextTag := oNextTag:parent
            ENDDO

            IF oNextTag == NIL
               // orphaned end tag with no opening tag
               LOOP
            ELSE
               // node that opened the end tag
               oNextTag:htmlEndTagName := cTagName
               oThisTag := oNextTag
            ENDIF

         ENDIF

         lRewind := .T.
         EXIT

      OTHERWISE

         IF oThisTag:isOptional()
            // this tag has no closing tag
            // a new opening tag is found
            DO CASE
            CASE Lower( cTagName ) == Lower( oThisTag:htmlTagName )
               // the next tag is the same like this tag
               // ( e.g. <p>|<tr>|<td>|<li>)
               lRewind := .T.
            CASE Lower( cTagName ) == Lower( oThisTag:parent:htmlTagName ) .AND. ! oThisTag:isType( CM_LIST )
               // the next tag is the same like the parent tag
               // ( e.g. this is <td> and the next tag is <tr> )
               lRewind := .T.
            CASE Lower( ::htmlTagName ) $ "dd,dt"
               // <dl><dt><dd> is a unique special case
               IF Lower( cTagName ) $ "dd,dt"
                  // next tag is <dt> or <dd>
                  lRewind := .T.
               ENDIF
            ENDCASE

            IF lRewind
               // go back to previous node
               parser:p_pos := nStart - 1
            ENDIF
         ENDIF

         IF ! lRewind
            IF cAttr == ""
               // tag has no attributes
               oNextTag := THtmlNode():new( oThisTag, cTagName )
            ELSE
               // attribute string has ">" at the end. Remove ">"
               oNextTag := THtmlNode():new( oThisTag, cTagName, hb_StrShrink( cAttr ) )
            ENDIF

            oThisTag:addNode( oNextTag )

            IF ! oThisTag:isOptional() .AND. Lower( oThisTag:htmlTagName ) == Lower( ctagName )
               oThisTag:htmlEndTagName := "/" + oThisTag:htmlTagName
            ENDIF

            IF oNextTag:keepFormatting()
               // do not spoil formatting of Html text
               oNextTag:parseHtmlFixed( parser )

            ELSEIF ! oNextTag:isEmpty()
               // parse into node list of new tag
               oThisTag := oNextTag

            ENDIF
         ENDIF
      ENDSWITCH

      IF lRewind
         oThisTag := oThisTag:parent
         lRewind := .F.

         IF oThisTag == NIL
            oThisTag := Self
            nLastPos := parser:p_len
            EXIT
         ENDIF
      ENDIF

      nLastPos := parser:p_pos
      IF nLastPos == 0
         EXIT
      ENDIF
   ENDDO

   IF nLastPos > 0 .AND. nLastPos < parser:p_len
      oThisTag:addNode( THtmlNode():new( Self, "_text_", , SubStr( parser:p_str, nLastPos + 1 ) ) )
   ENDIF

   RETURN Self

// parses a HTML string without any changes to indentation and line breaks
METHOD parseHtmlFixed( parser ) CLASS THtmlNode

   LOCAL nStart, nEnd

   // keep entire Html text within tag
   nStart := parser:p_pos + 1
   P_SEEK( parser, "<" )

   IF P_NEXT( parser ) == "!" .AND. P_NEXT( parser ) == "["
      // <![CDATA[   ]]>
      P_SEEK( parser, "]]>" )
   ENDIF

   IF ! P_PEEK( parser, "/" + ::htmlTagName )
      // seek  <  /endtag>
      P_SEEKI( parser, "/" + ::htmlTagName )
   ENDIF

   // back to "<"
   DO WHILE ! P_PREV( parser ) == "<"  /* NOTE: != changed to ! == */
   ENDDO

   nEnd  := parser:p_pos
   ::addNode( THtmlNode():new( Self, "_text_", , SubStr( parser:p_Str, nStart, nEnd - nStart ) ) )

   ::htmlEndTagName := "/" + ::htmlTagName

   P_SEEK( parser, ">" )

   RETURN Self

// adds a new CHILD node to the current one
METHOD addNode( oTHtmlNode ) CLASS THtmlNode

   IF oTHtmlNode:parent != NIL .AND. ! oTHtmlNode:parent == Self
      oTHtmlNode:delete()
   ENDIF

   oTHtmlNode:parent := Self
   oTHtmlNode:root   := ::root

   AAdd( ::htmlContent, oTHtmlNode )

   IF ::root != NIL .AND. ::root:_document != NIL
      ::root:_document:changed := .T.
   ENDIF

   RETURN oTHtmlNode

// inserts a SIBLING node before the current one
METHOD insertBefore( oTHtmlNode ) CLASS THtmlNode

   IF ::parent == NIL
      RETURN ::error( "Cannot insert before root node", ::className(), ":insertBefore()", EG_ARG, hb_AParams() )
   ENDIF

   IF oTHtmlNode:parent != NIL .AND. ! oTHtmlNode:parent == Self
      oTHtmlNode:delete()
   ENDIF

   oTHtmlNode:parent := ::parent
   oTHtmlNode:root   := ::root

   IF ::root != NIL .AND. ::root:_document != NIL
      ::root:_document:changed := .T.
   ENDIF

   IF HB_ISARRAY( ::parent:htmlContent )
      hb_AIns( ::parent:htmlContent, 1, oTHtmlNode, .T. )
   ENDIF

   RETURN oTHtmlNode

// inserts a SIBLING node after the current one
METHOD insertAfter( oTHtmlNode ) CLASS THtmlNode

   LOCAL nPos

   IF oTHtmlNode:parent != NIL .AND. ! oTHtmlNode:parent == Self
      oTHtmlNode:delete()
   ENDIF

   oTHtmlNode:parent := ::parent
   oTHtmlNode:root   := ::root

   IF ::root != NIL .AND. ::root:_document != NIL
      ::root:_document:changed := .T.
   ENDIF

   IF ( nPos := hb_AScan( ::parent:htmlContent, Self,,, .T. ) + 1 ) > Len( ::parent:htmlContent )
      ::parent:addNode( oTHtmlNode )
   ELSE
      hb_AIns( ::parent:htmlContent, nPos, oTHtmlNode, .T. )
   ENDIF

   RETURN oTHtmlNode

// deletes this node from the object tree
METHOD Delete()  CLASS THtmlNode

   IF ::parent == NIL
      RETURN Self
   ENDIF

   IF ::root != NIL .AND. ::root:_document != NIL
      ::root:_document:changed := .T.
   ENDIF

   IF HB_ISARRAY( ::parent:htmlContent )
      hb_ADel( ::parent:htmlContent, hb_AScan( ::parent:htmlContent, Self,,, .T. ), .T. )
   ENDIF

   ::parent := NIL
   ::root   := NIL

   RETURN Self

// returns first node in subtree (.F.) or first node of entire tree (.T.)
METHOD firstNode( lRoot ) CLASS THtmlNode

   IF hb_defaultValue( lRoot, .F. )
      RETURN ::root:htmlContent[ 1 ]
   ELSEIF ::htmlTagName == "_text_"
      RETURN ::parent:htmlContent[ 1 ]
   ENDIF

   RETURN iif( Empty( ::htmlContent ), NIL, ::htmlContent[ 1 ] )

// returns last node in subtree (.F.) or last node of entire tree (.T.)
METHOD lastNode( lRoot ) CLASS THtmlNode

   hb_default( @lRoot, .F. )

   IF ::htmlTagName == "_text_"
      RETURN ::parent:lastNode( lRoot )
   ENDIF

   RETURN ATail( iif( lRoot, ::root:collect(), ::collect() ) )

// returns next node
METHOD nextNode() CLASS THtmlNode

   LOCAL nPos, aNodes

   IF ::htmlTagName == "_root_"
      RETURN ::htmlContent[ 1 ]
   ENDIF

   /* NOTE: != changed to ! == */
   IF ! ::htmlTagName == "_text_" .AND. ! Empty( ::htmlContent )
      RETURN ::htmlContent[ 1 ]
   ENDIF

   IF ( nPos := hb_AScan( ::parent:htmlContent, Self,,, .T. ) ) < Len( ::parent:htmlContent )
      RETURN ::parent:htmlContent[ nPos + 1 ]
   ENDIF

   aNodes := ::parent:parent:collect()
   nPos   := hb_AScan( aNodes, Self,,, .T. )

   RETURN iif( nPos == Len( aNodes ), NIL, aNodes[ nPos + 1 ] )

// returns previous node
METHOD prevNode() CLASS THtmlNode

   LOCAL nPos, aNodes

   IF ::htmlTagName == "_root_"
      RETURN NIL
   ENDIF

   aNodes := ::parent:collect( Self )
   nPos   := hb_AScan( aNodes, Self,,, .T. )

   RETURN iif( nPos == 1, ::parent, aNodes[ nPos - 1 ] )

// creates HTML code for this node
METHOD toString( nIndent , nIncIndent ) CLASS THtmlNode
   
   LOCAL cHtml

   hb_default( @nIndent, -4 )
   hb_default( @nIncIndent, 4 )
   
   cHtml := ::__toString( nIndent , nIncIndent )

   //TODO: Workaround until reviewing the logic of the <!-- and --> tags
   cHtml:=strTran(cHtml,"<!-->","<!--")
   cHtml:=strTran(cHtml,"<-->","-->")
   cHtml:=strTran(cHtml,"</-->","-->")
   cHtml:=strTran(cHtml,"-->-->","-->")
   cHtml:=strTran(cHtml,"</!-->","-->")
   
   //TODO: Workaround until reviewing the logic of the EOL
   while " "+THTML_HB_EOL$cHtml
      cHtml:=strTran(cHtml," "+THTML_HB_EOL,THTML_HB_EOL)
   end while   
   while THTML_HB_EOL+THTML_HB_EOL$cHtml
      cHtml:=strTran(cHtml,THTML_HB_EOL+THTML_HB_EOL,THTML_HB_EOL)
   end while
   
   cHtml:=strTran(cHtml,THTML_HB_EOL,hb_eol())

   RETURN(cHtml)

METHOD __toString( nIndent , nIncIndent ) CLASS THtmlNode

   LOCAL cIndent, cHtml := "", oNode

   IF ::htmlTagName == "_text_"
      // a leaf has no child nodes
      cHtml += ::htmlContent
      RETURN cHtml
   ENDIF

   hb_default( @nIndent, -4 )
   hb_default( @nIncIndent, 4 )

   cIndent := iif( ::keepFormatting(), "", Space( Max( 0, nIndent ) ) )

   IF ! ::htmlTagName == "_root_"
      // all nodes but the root node have a HTML tag
      IF ! ::isInline() .OR. ::htmlTagName == "!--"
         cHtml += cIndent
      ELSEIF ::keepFormatting()
         cHtml += THTML_HB_EOL
      ENDIF
      cHtml += "<" + ::htmlTagName + ::attrToString()

      IF ! ::htmlEndTagName == "/"
         cHtml += ">"
      ENDIF
   ENDIF

   IF HB_ISARRAY( ::htmlContent )

      FOR EACH oNode IN ::htmlContent
         IF ! oNode:isInline() .OR. oNode:htmlTagName == "!--"
            cHtml += THTML_HB_EOL
         ENDIF
         cHtml += oNode:__toString( nIndent + nIncIndent )
      NEXT

   ELSEIF HB_ISSTRING( ::htmlContent )
      cHtml += ::htmlContent
   ENDIF

   IF ::htmlEndTagName != NIL
      IF ::isInline() .OR. ::keepFormatting() .OR. ::isType( CM_HEADING ) .OR. ::isType( CM_HEAD )
         RETURN cHtml += iif( ::htmlEndTagName == "/", " />", "<" + ::htmlEndTagName + ">" )
      ENDIF
      IF ! Right( cHtml, Len( THTML_HB_EOL ) ) == THTML_HB_EOL
         cHtml += THTML_HB_EOL
      ENDIF
      RETURN cHtml += cIndent + iif( ::htmlEndTagName == "/", " />", "<" + ::htmlEndTagName + ">" )
   ELSEIF ::htmlTagName $ "!--,br"
      RETURN cHtml += THTML_HB_EOL + cIndent
   ENDIF

   RETURN cHtml

// Builds the attribute string
METHOD attrToString() CLASS THtmlNode

   LOCAL aAttr, cAttr

   IF ::htmlAttributes == NIL
      cAttr := ""
   ELSEIF HB_ISSTRING( ::htmlAttributes )
      cAttr := " " + ::htmlAttributes
   ELSE
      // attributes are parsed into a Hash
      aAttr := __getAttributes( ::htmlTagType[ 1 ] )
      cAttr := ""
      hb_HEval( ::htmlAttributes, {| cKey, cValue | cAttr += __AttrToStr( cKey, cValue, aAttr, Self ) } )
   ENDIF

   RETURN cAttr

STATIC FUNCTION __getAttributes( sAttrFunc )

   LOCAL aAttr
   LOCAL nAttr

   BEGIN SEQUENCE WITH __BreakBlock()
      aAttr := sAttrFunc:exec()
   RECOVER
      aAttr := {}  // Tag has no attributes
   END SEQUENCE

   //Add Global Attributes
   //The global attributes below can be used on any HTML element
   if t_aGlbAttr==NIL
      t_aGlbAttr:=THtmlAttr_GlobalAttributes()
   endif
   for nAttr:=1 to len(t_aGlbAttr)
      if (len(t_aGlbAttr[nAttr])>=1)
        if (aScan(aAttr,{|a|if(len(a)>=1,a[1]==t_aGlbAttr[nAttr][1],.F.)})==0)
            aAdd(aAttr,t_aGlbAttr[nAttr])
        endif
      endif
   next nAttr

   RETURN(aAttr)

STATIC FUNCTION __AttrToStr( cName, cValue, aAttr, oTHtmlNode )

   LOCAL cNameToLower
   LOCAL nPos

   IF ("data-"==Left(cName,5).or."aria-"==Left(cName,5))
        /*
        The data-* attribute is used to store custom data private to the page or application.
        The data-* attribute gives us the ability to embed custom data attributes on all HTML elements.
        The stored (custom) data can then be used in the page's JavaScript to create a more engaging user experience (without any Ajax calls or server-side database queries).
        The data-* attribute consist of two parts:
        The attribute name should not contain any uppercase letters, and must be at least one character long after the prefix "data-"
        The attribute value can be any string
        Note: Custom attributes prefixed with "data-" will be completely ignored by the user agent.*/
      RETURN " " + cName + "=" + '"' + cValue + '"'
   ENDIF

   cNameToLower:=Lower( cName )
   IF ( nPos := AScan( aAttr, {| a | a[ 1 ] == cNameToLower } ) ) == 0
      // Tag doesn't have this attribute
      RETURN oTHtmlNode:error( "Invalid HTML attribute for: <" + oTHtmlNode:htmlTagName + ">", oTHtmlNode:className(), cName, EG_ARG, { cName, cValue } )
   ENDIF

   IF aAttr[ nPos ][ 2 ] == HTML_ATTR_TYPE_BOOL
      RETURN " " + cName
   ENDIF

   RETURN " " + cName + "=" + '"' + cValue + '"'

// collects all (sub)nodes of the tree in a one dimensional array
METHOD collect( oEndNode ) CLASS THtmlNode

   LOCAL stack, oSubNode

   stack := S_STACK()

   IF ::htmlTagName == "_root_"
      FOR EACH oSubNode IN ::htmlContent
         __CollectTags( oSubNode, stack, oEndNode )
      NEXT
   ELSE
      __CollectTags( Self, stack, oEndNode )
   ENDIF

   S_COMPRESS( stack )

   RETURN stack[ S_DATA ]

STATIC FUNCTION __CollectTags( oTHtmlNode, stack, oEndNode )

   LOCAL oSubNode

   S_PUSH( stack, oTHtmlNode )

   IF oTHtmlNode:isNode() .AND. ! oTHtmlNode == oEndNode
      FOR EACH oSubNode IN oTHtmlNode:htmlContent
         __CollectTags( oSubNode, stack, oEndNode )
      NEXT
   ENDIF

   RETURN stack

// Retrieves the textual content of a node
METHOD getText( cEOL ) CLASS THtmlNode

   LOCAL cCHR9 := Chr( 9 )
   LOCAL cText := ""
   LOCAL oNode

   hb_default( @cEOL, hb_eol() )

   IF ::htmlTagName == "_text_"
      RETURN RTrim( ::htmlContent ) + cEOL
   ENDIF

   FOR EACH oNode IN ::htmlContent
      cText += oNode:getText( cEOL )
      IF Lower( ::htmlTagName ) $ "td,th" .AND. hb_AScan( ::parent:htmlContent, Self,,, .T. ) < Len( ::parent:htmlContent )
         // leave table rows in one line, cells separated by Tab
         cText := hb_StrShrink( cText, Len( cEOL ) ) + cCHR9
      ENDIF
   NEXT

   RETURN cText

// Returns the value of an HTML attribute
METHOD getAttribute( cName ) CLASS THtmlNode

   LOCAL hHash := ::getAttributes()
   LOCAL cValue

   IF ! HB_ISHASH( hHash )
      RETURN hHash
   ENDIF

   BEGIN SEQUENCE WITH __BreakBlock()
      cValue := hHash[ cName ]
   RECOVER
      cValue := NIL
   END SEQUENCE

   RETURN cValue

// Returns all HTML attributes as a Hash
METHOD getAttributes() CLASS THtmlNode

   IF ::htmlTagType[ 1 ] == NIL
      // Tag has no valid attributes
      RETURN NIL

   ELSEIF hb_LeftEq( ::htmlTagName, "!" )
      // <!DOCTYPE > and <!-- comments --> have no HTML attributes
      RETURN ::htmlAttributes

   ELSEIF ::htmlAttributes == NIL
      ::htmlAttributes := { => }
      hb_HCaseMatch( ::htmlAttributes, .F. )

   ELSEIF HB_ISSTRING( ::htmlAttributes )
      IF ::htmlAttributes == "/"
         ::htmlAttributes := { => }
         hb_HCaseMatch( ::htmlAttributes, .F. )
      ELSE
         ::htmlAttributes := __ParseAttr( P_PARSER( AllTrim( ::htmlAttributes ) ) )
      ENDIF
   ENDIF

   RETURN ::htmlAttributes

// HTML attribute parser
STATIC FUNCTION __ParseAttr( parser )

   LOCAL cChr, nMode := 1  // 1=name, 2=value
   LOCAL aAttr := { "", "" }
   LOCAL hHash := { => }
   LOCAL nStart, nEnd
   LOCAL lIsQuoted := .F.

   hb_HCaseMatch( hHash, .F. )

   DO WHILE ! ( cChr := P_NEXT( parser ) ) == ""

      SWITCH cChr
      CASE "="
         lIsQuoted := .F.

         IF nMode == 2
            aAttr[ 2 ] += "="
         ELSE
            nMode := 2
         ENDIF
         EXIT

      CASE " "
         IF nMode == 1
            IF ! aAttr[ 1 ] == ""
               hHash[ aAttr[ 1 ] ] := aAttr[ 2 ]
               aAttr[ 1 ] := ""
               aAttr[ 2 ] := ""
            ENDIF
            LOOP
         ENDIF

         nMode := iif( lIsQuoted, 2, 1 )
         hHash[ aAttr[ 1 ] ] := aAttr[ 2 ]
         aAttr[ 1 ] := ""
         aAttr[ 2 ] := ""

         DO WHILE P_NEXT( parser ) == " "
         ENDDO

         IF parser:p_pos > Len( parser:p_str )
            RETURN hHash
         ENDIF

         parser:p_end := parser:p_pos
         parser:p_pos --
         EXIT

      CASE '"'
      CASE "'"
         lIsQuoted := .T.
         parser:p_end := parser:p_pos
         parser:p_pos ++

         nStart := parser:p_pos

         IF SubStr( parser:p_str, nStart, 1 ) == cChr
            // empty value ""
            hHash[ aAttr[ 1 ] ] := ""
            parser:p_end := parser:p_pos
            parser:p_pos --
         ELSE
            P_SEEKI( parser, cChr )
            nEnd := parser:p_pos

            IF nEnd > 0
               aAttr[ 2 ] := SubStr( parser:p_str, nStart, nEnd - nStart )
            ELSE
               aAttr[ 2 ] := SubStr( parser:p_str, nStart )
            ENDIF

            hHash[ aAttr[ 1 ] ] := aAttr[ 2 ]
         ENDIF

         aAttr[ 1 ] := ""
         aAttr[ 2 ] := ""
         nMode := 1

         IF nEnd == 0
            RETURN hHash
         ENDIF

         EXIT

      OTHERWISE
         aAttr[ nMode ] += cChr
      ENDSWITCH
   ENDDO

   IF ! aAttr[ 1 ] == ""
      hHash[ aAttr[ 1 ] ] := aAttr[ 2 ]
   ENDIF

   RETURN hHash

// Sets one attribute and value
METHOD setAttribute( cName, cValue ) CLASS THtmlNode

   LOCAL aAttr
   lOCAL cNameToLower
   LOCAL nPos
   LOCAL hHash

   IF ("data-"==Left(cName,5).or."aria-"==Left(cName,5))

      hHash := {=>}

   ELSE

       hHash := ::getAttributes()

       IF !HB_ISHASH( hHash )
          // Tag doesn't have any attribute
          RETURN ::error( "Invalid HTML attribute for: <" + ::htmlTagName + ">", ::className(), cName, EG_ARG, { cName, cValue } )
       ENDIF

       aAttr := __getAttributes( ::htmlTagType[ 1 ] )

       cNameToLower:=Lower( cName )
       IF ( nPos := AScan( aAttr, {| a | a[ 1 ] == cNameToLower } ) ) == 0
          // Tag doesn't have this attribute
          RETURN ::error( "Invalid HTML attribute for: <" + ::htmlTagName + ">", ::className(), cName, EG_ARG, { cName, cValue } )
       ENDIF

   ENDIF

   IF !Empty(nPos) .and. aAttr[ nPos ][ 2 ] == HTML_ATTR_TYPE_BOOL
      hHash[ cName ] := ""
   ELSE
      hHash[ cName ] := cValue
   ENDIF

   RETURN hHash[ cName ]

// Sets all attribute and values ( [ Character: 'cAttr="xAttrValue"' ] ou [ Hash {"cAttr"=>"xAttrValue"} ] )
METHOD setAttributes( xHtml ) CLASS THtmlNode

   ::htmlAttributes := xHtml

   RETURN ::getAttributes()

// Removes one attribute
METHOD delAttribute( cName ) CLASS THtmlNode

   LOCAL xVal := ::getAttribute( cName )
   LOCAL lRet := .F.

   IF xVal != NIL
      BEGIN SEQUENCE WITH __BreakBlock()
         hb_HDel( ::htmlAttributes, cName )
         lRet := .T.
      RECOVER
         lRet := .F.
      END SEQUENCE
   ENDIF

   RETURN lRet

// Removes all attributes
METHOD delAttributes() CLASS THtmlNode

   ::htmlAttributes := NIL

   RETURN .T.

// Checks for the existence of an attribute
METHOD isAttribute( cName ) CLASS THtmlNode

   LOCAL lRet

   BEGIN SEQUENCE WITH __BreakBlock()
      lRet := cName $ ::getAttributes()
   RECOVER
      lRet := .F.
   END SEQUENCE

   RETURN lRet

// Error handling
METHOD noMessage( ... ) CLASS THtmlNode
   RETURN ::noAttribute( __GetMessage(), hb_AParams() )

// Non existent message -> returns and/or creates Tag or Attribute
METHOD noAttribute( cName, aValue ) CLASS THtmlNode

   LOCAL oNode

   cName := Lower( cName )

   IF hb_LeftEq( cName, "_" )
      cName := SubStr( cName, 1 + 1 )
   ENDIF

   IF cName $ t_hHT
      // message identifies a html tag
      oNode := ::findNodeByTagName( cName )

      IF oNode == NIL
         oNode := THtmlNode():new( Self, cName )
         IF ! oNode:isOptional() .AND. ! oNode:isEmpty()
            oNode:htmlEndTagName := "/" + cName
         ENDIF
         ::addNode( oNode )
      ENDIF

      RETURN oNode

   ELSEIF Right( cName, 1 ) == "s" .AND. hb_StrShrink( cName ) $ t_hHT
      // message is the plural of a html tag -> oNode:forms -> Array of <FORM> tags
      RETURN ::findNodesByTagName( hb_StrShrink( cName ), ATail( aValue ) )
   ENDIF

   IF ! Empty( aValue )
      RETURN ::setAttribute( cName, aValue[ 1 ] )
   ENDIF

   RETURN ::getAttribute( cName )

// finds the first node in tree with this name
METHOD findNodeByTagName( cName ) CLASS THtmlNode

   LOCAL aNodes := ::collect()
   LOCAL cNameToLower
   LOCAL oNode

   cNameToLower:=Lower( cName )
   FOR EACH oNode IN aNodes
      IF Lower( oNode:htmlTagName ) == cNameToLower
         RETURN oNode
      ENDIF
   NEXT

   RETURN NIL

// collects all nodes in tree with this name
METHOD findNodesByTagName( cName, nOrdinal ) CLASS THtmlNode

   LOCAL aNodes := ::collect()
   LOCAL cNameToLower
   LOCAL oNode
   LOCAL aRet := {}

   cNameToLower:=Lower( cName )
   FOR EACH oNode IN aNodes
      IF Lower( oNode:htmlTagName ) == cNameToLower
         AAdd( aRet, oNode )
      ENDIF
   NEXT

   IF HB_ISNUMERIC( nOrdinal )
      IF nOrdinal < 1 .OR. nOrdinal > Len( aRet )
         RETURN NIL
      ENDIF
      RETURN aRet[ nOrdinal ]
   ENDIF

   RETURN aRet

// returns the text node of this node
METHOD _getTextNode() CLASS THtmlNode

   IF ::htmlTagName == "_text_"
      RETURN Self
   ENDIF

   ::addNode( THtmlNode():new( Self, "_text_", , "" ) )

   RETURN ATail( ::htmlContent )

// assigns text to a text node of this node
METHOD _setTextNode( cText ) CLASS THtmlNode

   cText := LTrim( hb_ValToStr( cText ) )

   DO WHILE "<" $ cText
      cText := StrTran( cText, "<", "&lt;" )
   ENDDO

   DO WHILE ">" $ cText
      cText := StrTran( cText, ">", "&gt;" )
   ENDDO

   ::_getTextNode():htmlContent := iif( cText == "", "&nbsp;", cText )

   RETURN Self

// called by "+" operator
// Creates a new node of the specified tag name and raises error if cTagName is invalid
METHOD pushNode( cTagName ) CLASS THtmlNode

   LOCAL oNode
   LOCAL cAttr := AllTrim( cTagName )
   LOCAL cName := CutStr( " ", @cAttr )

   IF ::isEmpty()
      RETURN ::error( "Cannot add HTML tag to: <" + ::htmlTagName + ">", ::className(), "+", EG_ARG, { cName } )
   ENDIF

   IF ! cName $ t_hHT
      IF hb_LeftEq( cName, "/" ) .AND. SubStr( cName, 2 ) $ t_hHT
         IF ! Lower( SubStr( cName, 2 ) ) == Lower( ::htmlTagName )
            RETURN ::error( "Not a valid closing HTML tag for: <" + ::htmlTagName + ">", ::className(), "-", EG_ARG, { cName } )
         ENDIF
         RETURN ::parent
      ENDIF
      RETURN ::error( "Invalid HTML tag", ::className(), "+", EG_ARG, { cName } )
   ENDIF

   IF LTrim( cAttr ) == ""
      cAttr := NIL
   ENDIF

   oNode := THtmlNode():new( Self, cName, cAttr )
   IF ! oNode:isOptional() .AND. ! oNode:isEmpty()
      oNode:htmlEndTagName := "/" + cName
   ELSEIF cName=="!--"
      oNode:htmlEndTagName := "--"
   ENDIF
   ::addNode( oNode )

   RETURN oNode

// called by "-" operator
// returns the parent of this node and raises error if cName is an invalid closing tag
METHOD popNode( cName ) CLASS THtmlNode

   LOCAL endTag

   cName := Lower( LTrim( cName ) )

   IF hb_LeftEq( cName, "/" )
      cName := SubStr( cName, 1 + 1 )
   ENDIF

   IF ! cName == Lower( ::htmlTagName )
      RETURN ::error( "Invalid closing HTML tag for: <" + ::htmlTagName + ">", ::className(), "-", EG_ARG, { cName } )
   ENDIF

   /* tfonrouge: 2010-05-25
      this allows to properly close the tags "tr,th,td" by simply using:
      node - ["tr","th","td"]
    */
   IF hb_AScan( { "tr", "th", "td" }, cName,,, .T. ) > 0
      endTag := "</" + cName + ">"
      IF ! Right( ::toString(), 3 + Len( cName ) ) == endTag
         ::addNode( THtmlNode():new( Self, "/" + cName, ,  ) )
      ENDIF
   ENDIF

   RETURN ::parent

// Generic parsing function

STATIC FUNCTION CutStr( cCut, cString )

   LOCAL cLeftPart, i

   IF ( i := At( cCut, cString ) ) > 0
      cLeftPart := Left( cString, i - 1 )
      cString   := SubStr( cString, i + Len( cCut ) )
   ELSE
      cLeftPart := cString
      cString   := ""
   ENDIF

   RETURN cLeftPart

FUNCTION THtmlInit( lInit )

   IF ! hb_defaultValue( lInit, .T. )
      t_aHA := NIL
      t_hHT := NIL
      t_aHAAria := NIL
#ifdef HB_LEGACY_LEVEL4
      t_aHtmlAnsiEntities := NIL
#endif
      t_lInit := .F.
   ELSEIF ! t_lInit
      t_aHA := Array( HTML_ATTR_COUNT )
      t_aHAAria := Array( HTML_ATTR_ARIA_COUNT )
#ifdef HB_LEGACY_LEVEL4
      _Init_Html_AnsiCharacterEntities()
#endif
      _Init_Html_Attributes()
      _Init_Html_TagTypes()
      t_lInit := .T.
   ENDIF

   RETURN .T.

FUNCTION THtmlCleanup()
   RETURN THtmlInit( .F. )

FUNCTION THtmlTagType( cTagName )

   LOCAL aType

   IF t_hHT == NIL
      THtmlInit()
   ENDIF

   BEGIN SEQUENCE WITH __BreakBlock()
      aType := t_hHT[ cTagName ]
   RECOVER
      aType := t_hHT[ "_text_" ]
   END SEQUENCE

   RETURN aType

FUNCTION THtmlIsValid( cTagName, cAttrName )

   LOCAL lRet := .T., aValue
   LOCAL cAttrNameToLower

   IF t_hHT == NIL
      THtmlInit()
   ENDIF

   BEGIN SEQUENCE WITH __BreakBlock()
      aValue := t_hHT[ cTagName ]
      IF cAttrName != NIL
         cAttrNameToLower:=Lower( cAttrName )
         aValue := __getAttributes( aValue[ 1 ] )
         lRet   := AScan( aValue, {| a | Lower( a[ 1 ] ) == cAttrNameToLower } ) > 0
      ENDIF
   RECOVER
      lRet := .F.
   END SEQUENCE

   RETURN lRet

/* HTML Tag data are adopted for Harbour from Tidy
   https://github.com/htacg/tidy-html5 */

STATIC PROCEDURE _Init_Html_TagTypes

   t_hHT := { => }

   hb_HCaseMatch( t_hHT, .F. )

   t_hHT[ "_root_"     ] := {                             ,         ( CM_INLINE )                                               }
   t_hHT[ "_text_"     ] := {                             ,         ( CM_INLINE )                                               }
   t_hHT[ "!--"        ] := {                             ,         ( CM_INLINE )                                               }
   t_hHT[ "!DOCTYPE"   ] := {                             ,         ( CM_INLINE )                                               }
   t_hHT[ "a"          ] := { @THtmlAttr_A()              ,         ( CM_INLINE )                                               }
   t_hHT[ "abbr"       ] := { @THtmlAttr_ABBR()           ,         ( CM_INLINE )                                               }
   t_hHT[ "acronym"    ] := { @THtmlAttr_ACRONYM()        , hb_bitOr( CM_INLINE , CM_OBSOLETE )                                 }
   t_hHT[ "address"    ] := { @THtmlAttr_ADDRESS()        ,         ( CM_BLOCK )                                                }
   t_hHT[ "align"      ] := {                             ,         ( CM_BLOCK )                                                }
   t_hHT[ "applet"     ] := { @THtmlAttr_APPLET()         , hb_bitOr( CM_OBJECT, CM_IMG, CM_INLINE, CM_PARAM , CM_OBSOLETE )    }
   t_hHT[ "area"       ] := { @THtmlAttr_AREA()           , hb_bitOr( CM_BLOCK, CM_EMPTY )                                      }
   t_hHT[ "article"    ] := { @THtmlAttr_ARTICLE()        ,         ( CM_BLOCK)                                                 }
   t_hHT[ "aside"      ] := { @THtmlAttr_ASIDE()          ,         ( CM_BLOCK )                                                }
   t_hHT[ "audio"      ] := { @THtmlAttr_AUDIO()          ,         ( CM_BLOCK )                                                }
   t_hHT[ "b"          ] := { @THtmlAttr_B()              ,         ( CM_INLINE )                                               }
   t_hHT[ "base"       ] := { @THtmlAttr_BASE()           , hb_bitOr( CM_HEAD, CM_EMPTY )                                       }
   t_hHT[ "basefont"   ] := { @THtmlAttr_BASEFONT()       , hb_bitOr( CM_INLINE, CM_EMPTY , CM_OBSOLETE )                       }
   t_hHT[ "bdi"        ] := { @THtmlAttr_BDI()            ,         ( CM_INLINE )                                               }
   t_hHT[ "bdo"        ] := { @THtmlAttr_BDO()            ,         ( CM_INLINE )                                               }
   t_hHT[ "bgsound"    ] := { @THtmlAttr_BGSOUND()        , hb_bitOr( CM_HEAD, CM_EMPTY , CM_OBSOLETE )                         }
   t_hHT[ "big"        ] := { @THtmlAttr_BIG()            , hb_bitOr( CM_INLINE , CM_OBSOLETE )                                 }
   t_hHT[ "blink"      ] := {                             , hb_bitOr( CM_INLINE , CM_OBSOLETE )                                 }
   t_hHT[ "blockquote" ] := { @THtmlAttr_BLOCKQUOTE()     ,         ( CM_BLOCK )                                                }
   t_hHT[ "body"       ] := { @THtmlAttr_BODY()           , hb_bitOr( CM_HTML, CM_OPT, CM_OMITST )                              }
   t_hHT[ "br"         ] := { @THtmlAttr_BR()             , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "button"     ] := { @THtmlAttr_BUTTON()         ,         ( CM_INLINE )                                               }
   t_hHT[ "canvas"     ] := { @THtmlAttr_CANVAS()         , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "caption"    ] := { @THtmlAttr_CAPTION()        ,         ( CM_TABLE )                                                }
   t_hHT[ "center"     ] := { @THtmlAttr_CENTER()         , hb_bitOr( CM_BLOCK , CM_OBSOLETE )                                  }
   t_hHT[ "cite"       ] := { @THtmlAttr_CITE()           ,         ( CM_INLINE )                                               }
   t_hHT[ "code"       ] := { @THtmlAttr_CODE()           ,         ( CM_INLINE )                                               }
   t_hHT[ "col"        ] := { @THtmlAttr_COL()            , hb_bitOr( CM_TABLE, CM_EMPTY )                                      }
   t_hHT[ "colgroup"   ] := { @THtmlAttr_COLGROUP()       , hb_bitOr( CM_TABLE, CM_OPT )                                        }
   t_hHT[ "command"    ] := { @THtmlAttr_COMMAND()        ,         ( CM_INLINE )                                               }
   t_hHT[ "comment"    ] := {                             ,         ( CM_INLINE )                                               }
   t_hHT[ "data"       ] := { @THtmlAttr_DATA()           ,         ( CM_INLINE )                                               }
   t_hHT[ "datalist"   ] := {                             , hb_bitOr( CM_BLOCK, CM_EMPTY )                                      }
   t_hHT[ "dd"         ] := { @THtmlAttr_DD()             , hb_bitOr( CM_DEFLIST, CM_OPT, CM_NO_INDENT )                        }
   t_hHT[ "del"        ] := { @THtmlAttr_DEL()            , hb_bitOr( CM_INLINE, CM_BLOCK, CM_MIXED )                           }
   t_hHT[ "details"    ] := { @THtmlAttr_DETAILS()        ,         ( CM_BLOCK )                                                }
   t_hHT[ "dfn"        ] := { @THtmlAttr_DFN()            ,         ( CM_INLINE )                                               }
   t_hHT[ "dialog"     ] := { @THtmlAttr_DIALOG()         ,         ( CM_INLINE )                                               }
   t_hHT[ "dir"        ] := { @THtmlAttr_DIR()            , hb_bitOr( CM_BLOCK, CM_OBSOLETE )                                   }
   t_hHT[ "div"        ] := { @THtmlAttr_DIV()            ,         ( CM_BLOCK )                                                }
   t_hHT[ "dl"         ] := { @THtmlAttr_DL()             ,         ( CM_BLOCK )                                                }
   t_hHT[ "dt"         ] := { @THtmlAttr_DT()             , hb_bitOr( CM_DEFLIST, CM_OPT, CM_NO_INDENT )                        }
   t_hHT[ "em"         ] := { @THtmlAttr_EM()             ,         ( CM_INLINE )                                               }
   t_hHT[ "embed"      ] := { @THtmlAttr_EMBED()          , hb_bitOr( CM_INLINE, CM_IMG, CM_EMPTY )                             }
   t_hHT[ "fieldset"   ] := { @THtmlAttr_FIELDSET()       ,         ( CM_INLINE )                                               }
   t_hHT[ "figcaption" ] := { @THtmlAttr_FIGCAPTION()     ,         ( CM_BLOCK )                                                }
   t_hHT[ "figure"     ] := { @THtmlAttr_FIGURE()         ,         ( CM_BLOCK )                                                }
   t_hHT[ "font"       ] := { @THtmlAttr_FONT()           , hb_bitOr( CM_INLINE , CM_OBSOLETE )                                 }
   t_hHT[ "footer"     ] := {                             , hb_bitOr( CM_HTML, CM_OPT, CM_OMITST )                              }
   t_hHT[ "form"       ] := { @THtmlAttr_FORM()           ,         ( CM_BLOCK )                                                }
   t_hHT[ "frame"      ] := { @THtmlAttr_FRAME()          , hb_bitOr( CM_FRAMES, CM_EMPTY , CM_OBSOLETE  )                      }
   t_hHT[ "frameset"   ] := { @THtmlAttr_FRAMESET()       , hb_bitOr( CM_HTML, CM_FRAMES , CM_OBSOLETE )                        }
   t_hHT[ "h1"         ] := { @THtmlAttr_H1()             , hb_bitOr( CM_BLOCK, CM_HEADING )                                    }
   t_hHT[ "h2"         ] := { @THtmlAttr_H2()             , hb_bitOr( CM_BLOCK, CM_HEADING )                                    }
   t_hHT[ "h3"         ] := { @THtmlAttr_H3()             , hb_bitOr( CM_BLOCK, CM_HEADING )                                    }
   t_hHT[ "h4"         ] := { @THtmlAttr_H4()             , hb_bitOr( CM_BLOCK, CM_HEADING )                                    }
   t_hHT[ "h5"         ] := { @THtmlAttr_H5()             , hb_bitOr( CM_BLOCK, CM_HEADING )                                    }
   t_hHT[ "h6"         ] := { @THtmlAttr_H6()             , hb_bitOr( CM_BLOCK, CM_HEADING )                                    }
   t_hHT[ "head"       ] := { @THtmlAttr_HEAD()           , hb_bitOr( CM_HTML, CM_OPT, CM_OMITST )                              }
   t_hHT[ "header"     ] := { @THtmlAttr_HEADER()         , hb_bitOr( CM_BLOCK, CM_EMPTY )                                      }
   t_hHT[ "hgroup"     ] := { @THtmlAttr_HGROUP()         , hb_bitOr( CM_BLOCK, CM_EMPTY, CM_OBSOLETE )                                      }
   t_hHT[ "hr"         ] := { @THtmlAttr_HR()             , hb_bitOr( CM_BLOCK, CM_EMPTY )                                      }
   t_hHT[ "html"       ] := { @THtmlAttr_HTML()           , hb_bitOr( CM_HTML, CM_OPT, CM_OMITST )                              }
   t_hHT[ "i"          ] := { @THtmlAttr_I()              ,         ( CM_INLINE )                                               }
   t_hHT[ "iframe"     ] := { @THtmlAttr_IFRAME()         ,         ( CM_INLINE )                                               }
   t_hHT[ "ilayer"     ] := {                             ,         ( CM_INLINE )                                               }
   t_hHT[ "img"        ] := { @THtmlAttr_IMG()            , hb_bitOr( CM_INLINE, CM_IMG, CM_EMPTY )                             }
   t_hHT[ "input"      ] := { @THtmlAttr_INPUT()          , hb_bitOr( CM_INLINE, CM_IMG, CM_EMPTY )                             }
   t_hHT[ "ins"        ] := { @THtmlAttr_INS()            , hb_bitOr( CM_INLINE, CM_BLOCK, CM_MIXED )                           }
   t_hHT[ "isindex"    ] := { @THtmlAttr_ISINDEX()        , hb_bitOr( CM_BLOCK, CM_EMPTY, CM_OBSOLETE )                         }
   t_hHT[ "kbd"        ] := { @THtmlAttr_KBD()            ,         ( CM_INLINE )                                               }
   t_hHT[ "keygen"     ] := { @THtmlAttr_KEYGEN()         , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "label"      ] := { @THtmlAttr_LABEL()          ,         ( CM_INLINE )                                               }
   t_hHT[ "layer"      ] := {                             ,         ( CM_BLOCK )                                                }
   t_hHT[ "legend"     ] := { @THtmlAttr_LEGEND()         ,         ( CM_INLINE )                                               }
   t_hHT[ "li"         ] := { @THtmlAttr_LI()             , hb_bitOr( CM_LIST, CM_OPT, CM_NO_INDENT )                           }
   t_hHT[ "link"       ] := { @THtmlAttr_LINK()           , hb_bitOr( CM_HEAD, CM_EMPTY )                                       }
   t_hHT[ "listing"    ] := { @THtmlAttr_LISTING()        , hb_bitOr( CM_BLOCK, CM_OBSOLETE )                                   }
   t_hHT[ "main"       ] := {                             ,         ( CM_BLOCK )                                                }
   t_hHT[ "map"        ] := { @THtmlAttr_MAP()            ,         ( CM_INLINE )                                               }
   t_hHT[ "marquee"    ] := { @THtmlAttr_MARQUEE()        , hb_bitOr( CM_INLINE, CM_OPT, CM_OBSOLETE )                          }
   t_hHT[ "mark"       ] := {                             , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "menu"       ] := { @THtmlAttr_MENU()           , hb_bitOr( CM_BLOCK, CM_OBSOLETE )                                   }
   t_hHT[ "menuitem"   ] := { @THtmlAttr_MENUITEM()       , hb_bitOr( CM_BLOCK, CM_OBSOLETE )                                   }
   t_hHT[ "meta"       ] := { @THtmlAttr_META()           , hb_bitOr( CM_HEAD, CM_EMPTY )                                       }
   t_hHT[ "meter"      ] := { @THtmlAttr_METER()          , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "multicol"   ] := {                             , hb_bitOr( CM_BLOCK, CM_OBSOLETE )                                   }
   t_hHT[ "nav"        ] := {                             ,         ( CM_BLOCK )                                                }
   t_hHT[ "nextid"     ] := { @THtmlAttr_NEXTID()         , hb_bitOr( CM_HEAD, CM_EMPTY , CM_OBSOLETE )                         }
   t_hHT[ "nobr"       ] := {                             , hb_bitOr( CM_INLINE , CM_OBSOLETE )                                 }
   t_hHT[ "noembed"    ] := {                             , hb_bitOr( CM_INLINE , CM_OBSOLETE )                                 }
   t_hHT[ "noframes"   ] := { @THtmlAttr_NOFRAMES()       , hb_bitOr( CM_BLOCK, CM_FRAMES , CM_OBSOLETE )                       }
   t_hHT[ "nolayer"    ] := {                             , hb_bitOr( CM_BLOCK, CM_INLINE, CM_MIXED )                           }
   t_hHT[ "nosave"     ] := {                             ,         ( CM_BLOCK )                                                }
   t_hHT[ "noscript"   ] := { @THtmlAttr_NOSCRIPT()       , hb_bitOr( CM_BLOCK, CM_INLINE, CM_MIXED )                           }
   t_hHT[ "object"     ] := { @THtmlAttr_OBJECT()         , hb_bitOr( CM_OBJECT, CM_HEAD, CM_IMG, CM_INLINE, CM_PARAM )         }
   t_hHT[ "ol"         ] := { @THtmlAttr_OL()             ,         ( CM_BLOCK )                                                }
   t_hHT[ "optgroup"   ] := { @THtmlAttr_OPTGROUP()       , hb_bitOr( CM_FIELD, CM_OPT )                                        }
   t_hHT[ "option"     ] := { @THtmlAttr_OPTION()         , hb_bitOr( CM_FIELD, CM_OPT )                                        }
   t_hHT[ "output"     ] := { @THtmlAttr_OUTPUT()         , hb_bitOr( CM_BLOCK, CM_OPT )                                        }
   t_hHT[ "p"          ] := { @THtmlAttr_P()              , hb_bitOr( CM_BLOCK, CM_OPT )                                        }
   t_hHT[ "param"      ] := { @THtmlAttr_PARAM()          , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "picture"    ] := { @THtmlAttr_PARAM()          , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "plaintext"  ] := { @THtmlAttr_PLAINTEXT()      , hb_bitOr( CM_BLOCK, CM_OBSOLETE )                                   }
   t_hHT[ "pre"        ] := { @THtmlAttr_PRE()            ,         ( CM_BLOCK )                                                }
   t_hHT[ "progress"   ] := { @THtmlAttr_PROGRESS()       ,         ( CM_BLOCK )                                                }
   t_hHT[ "q"          ] := { @THtmlAttr_Q()              ,         ( CM_INLINE )                                               }
   t_hHT[ "rb"         ] := { @THtmlAttr_RB()             ,         ( CM_INLINE )                                               }
   t_hHT[ "rbc"        ] := { @THtmlAttr_RBC()            ,         ( CM_INLINE )                                               }
   t_hHT[ "rp"         ] := { @THtmlAttr_RP()             ,         ( CM_INLINE )                                               }
   t_hHT[ "rt"         ] := { @THtmlAttr_RT()             ,         ( CM_INLINE )                                               }
   t_hHT[ "rtc"        ] := { @THtmlAttr_RTC()            ,         ( CM_INLINE )                                               }
   t_hHT[ "ruby"       ] := { @THtmlAttr_RUBY()           ,         ( CM_INLINE )                                               }
   t_hHT[ "s"          ] := { @THtmlAttr_S()              ,         ( CM_INLINE )                                               }
   t_hHT[ "samp"       ] := { @THtmlAttr_SAMP()           ,         ( CM_INLINE )                                               }
   t_hHT[ "script"     ] := { @THtmlAttr_SCRIPT()         , hb_bitOr( CM_HEAD, CM_MIXED, CM_BLOCK, CM_INLINE )                  }
   t_hHT[ "section"    ] := {                             ,         ( CM_BLOCK )                                                }
   t_hHT[ "select"     ] := { @THtmlAttr_SELECT()         , hb_bitOr( CM_INLINE, CM_FIELD )                                     }
   t_hHT[ "server"     ] := {                             , hb_bitOr( CM_HEAD, CM_MIXED, CM_BLOCK, CM_INLINE )                  }
   t_hHT[ "servlet"    ] := {                             , hb_bitOr( CM_OBJECT, CM_IMG, CM_INLINE, CM_PARAM )                  }
   t_hHT[ "small"      ] := { @THtmlAttr_SMALL()          ,         ( CM_INLINE )                                               }
   t_hHT[ "spacer"     ] := {                             , hb_bitOr( CM_INLINE, CM_EMPTY , CM_OBSOLETE )                       }
   t_hHT[ "source"     ] := { @THtmlAttr_SOURCE()         , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "span"       ] := { @THtmlAttr_SPAN()           ,         ( CM_INLINE )                                               }
   t_hHT[ "strike"     ] := { @THtmlAttr_STRIKE()         , hb_bitOr( CM_INLINE, CM_OBSOLETE )                                  }
   t_hHT[ "strong"     ] := { @THtmlAttr_STRONG()         ,         ( CM_INLINE )                                               }
   t_hHT[ "style"      ] := { @THtmlAttr_STYLE()          ,         ( CM_HEAD )                                                 }
   t_hHT[ "sub"        ] := { @THtmlAttr_SUB()            ,         ( CM_INLINE )                                               }
   t_hHT[ "summary"    ] := {                             ,         ( CM_INLINE )                                               }
   t_hHT[ "sup"        ] := { @THtmlAttr_SUP()            ,         ( CM_INLINE )                                               }
   t_hHT[ "svg"        ] := { @THtmlAttr_SVG()            ,         ( CM_BLOCK )                                                }
   t_hHT[ "table"      ] := { @THtmlAttr_TABLE()          ,         ( CM_BLOCK )                                                }
   t_hHT[ "tbody"      ] := { @THtmlAttr_TBODY()          , hb_bitOr( CM_TABLE, CM_ROWGRP, CM_OPT )                             }
   t_hHT[ "td"         ] := { @THtmlAttr_TD()             , hb_bitOr( CM_ROW, CM_OPT, CM_NO_INDENT )                            }
   t_hHT[ "template"   ] := {                             ,         ( CM_BLOCK )                                                }
   t_hHT[ "textarea"   ] := { @THtmlAttr_TEXTAREA()       , hb_bitOr( CM_INLINE, CM_FIELD )                                     }
   t_hHT[ "tfoot"      ] := { @THtmlAttr_TFOOT()          , hb_bitOr( CM_TABLE, CM_ROWGRP, CM_OPT )                             }
   t_hHT[ "th"         ] := { @THtmlAttr_TH()             , hb_bitOr( CM_ROW, CM_OPT, CM_NO_INDENT )                            }
   t_hHT[ "thead"      ] := { @THtmlAttr_THEAD()          , hb_bitOr( CM_TABLE, CM_ROWGRP, CM_OPT )                             }
   t_hHT[ "time"       ] := { @THtmlAttr_TIME()           , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "title"      ] := { @THtmlAttr_TITLE()          ,         ( CM_HEAD )                                                 }
   t_hHT[ "tr"         ] := { @THtmlAttr_TR()             , hb_bitOr( CM_TABLE, CM_OPT )                                        }
   t_hHT[ "track"      ] := { @THtmlAttr_TRACK()          , hb_bitOr( CM_MIXED, CM_BLOCK, CM_INLINE )                           }
   t_hHT[ "tt"         ] := { @THtmlAttr_TT()             , hb_bitOr(  CM_INLINE, CM_OBSOLETE )                                 }
   t_hHT[ "u"          ] := { @THtmlAttr_U()              ,         ( CM_INLINE )                                               }
   t_hHT[ "ul"         ] := { @THtmlAttr_UL()             ,         ( CM_BLOCK )                                                }
   t_hHT[ "var"        ] := { @THtmlAttr_VAR()            ,         ( CM_INLINE )                                               }
   t_hHT[ "video"      ] := { @THtmlAttr_VIDEO()          , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "wbr"        ] := {                             , hb_bitOr( CM_INLINE, CM_EMPTY )                                     }
   t_hHT[ "xmp"        ] := { @THtmlAttr_XMP()            , hb_bitOr( CM_BLOCK, CM_OBSOLETE )                                   }

   RETURN

STATIC PROCEDURE _Init_Html_Attributes

   /* HTML Tag attribute data are adopted for Harbour from Tidy */

   /*     attribute                             NAME                      TYPE                   */
   t_aHA[ HTML_ATTR_ABBR                 ] := { "abbr"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ACCEPT               ] := { "accept"                , HTML_ATTR_TYPE_XTYPE          }
   t_aHA[ HTML_ATTR_ACCEPT_CHARSET       ] := { "accept-charset"        , HTML_ATTR_TYPE_CHARSET        }
   t_aHA[ HTML_ATTR_ACCESSKEY            ] := { "accesskey"             , HTML_ATTR_TYPE_CHARACTER      }
   t_aHA[ HTML_ATTR_ACTION               ] := { "action"                , HTML_ATTR_TYPE_ACTION         }
   t_aHA[ HTML_ATTR_ADD_DATE             ] := { "add_date"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ALIGN                ] := { "align"                 , HTML_ATTR_TYPE_ALIGN          }
   t_aHA[ HTML_ATTR_ALINK                ] := { "alink"                 , HTML_ATTR_TYPE_COLOR          }
   t_aHA[ HTML_ATTR_ALLOWFULLSCREEN      ] := { "allowfullscreen"       , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_ALT                  ] := { "alt"                   , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ARCHIVE              ] := { "archive"               , HTML_ATTR_TYPE_URLS           }
   t_aHA[ HTML_ATTR_ASYNC                ] := { "async"                 , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_AUTOCAPITALIZE       ] := { "autocapitalize"        , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_AUTOCOMPLETE         ] := { "autocomplete"          , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_AUTOFOCUS            ] := { "autofocus"             , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_AUTOPLAY             ] := { "autoplay"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_AXIS                 ] := { "axis"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_BACKGROUND           ] := { "background"            , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_BGCOLOR              ] := { "bgcolor"               , HTML_ATTR_TYPE_COLOR          }
   t_aHA[ HTML_ATTR_BGPROPERTIES         ] := { "bgproperties"          , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_BORDER               ] := { "border"                , HTML_ATTR_TYPE_BORDER         }
   t_aHA[ HTML_ATTR_BORDERCOLOR          ] := { "bordercolor"           , HTML_ATTR_TYPE_COLOR          }
   t_aHA[ HTML_ATTR_BOTTOMMARGIN         ] := { "bottommargin"          , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_CELLPADDING          ] := { "cellpadding"           , HTML_ATTR_TYPE_LENGTH         }
   t_aHA[ HTML_ATTR_CELLSPACING          ] := { "cellspacing"           , HTML_ATTR_TYPE_LENGTH         }
   t_aHA[ HTML_ATTR_CHAR                 ] := { "char"                  , HTML_ATTR_TYPE_CHARACTER      }
   t_aHA[ HTML_ATTR_CHAROFF              ] := { "charoff"               , HTML_ATTR_TYPE_LENGTH         }
   t_aHA[ HTML_ATTR_CHARSET              ] := { "charset"               , HTML_ATTR_TYPE_CHARSET        }
   t_aHA[ HTML_ATTR_CHECKED              ] := { "checked"               , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_CITE                 ] := { "cite"                  , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_CLASS                ] := { "class"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_CLASSID              ] := { "classid"               , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_CLEAR                ] := { "clear"                 , HTML_ATTR_TYPE_CLEAR          }
   t_aHA[ HTML_ATTR_CODE                 ] := { "code"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_CODEBASE             ] := { "codebase"              , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_CODETYPE             ] := { "codetype"              , HTML_ATTR_TYPE_XTYPE          }
   t_aHA[ HTML_ATTR_COLOR                ] := { "color"                 , HTML_ATTR_TYPE_COLOR          }
   t_aHA[ HTML_ATTR_COLS                 ] := { "cols"                  , HTML_ATTR_TYPE_COLS           }
   t_aHA[ HTML_ATTR_COLSPAN              ] := { "colspan"               , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_COMPACT              ] := { "compact"               , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_CONTENT              ] := { "content"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_CONTENTEDITABLE      ] := { "contenteditable"       , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_CONTEXTMENU          ] := { "contextmenu"           , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_CONTROLS             ] := { "controls"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_COORDS               ] := { "coords"                , HTML_ATTR_TYPE_COORDS         }
   t_aHA[ HTML_ATTR_CROSSORIGIN          ] := { "crossorigin"           , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_DATA                 ] := { "data"                  , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_DATAFLD              ] := { "datafld"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_DATAFORMATAS         ] := { "dataformatas"          , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_DATAPAGESIZE         ] := { "datapagesize"          , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_DATASRC              ] := { "datasrc"               , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_DATATYPE             ] := { "datatype"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_DATETIME             ] := { "datetime"              , HTML_ATTR_TYPE_DATE           }
   t_aHA[ HTML_ATTR_DECLARE              ] := { "declare"               , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_DEFER                ] := { "defer"                 , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_DIR                  ] := { "dir"                   , HTML_ATTR_TYPE_TEXTDIR        }
   t_aHA[ HTML_ATTR_DIRNAME              ] := { "dirname"               , HTML_ATTR_TYPE_TEXTDIR        }
   t_aHA[ HTML_ATTR_DISABLED             ] := { "disabled"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_DISPLAY              ] := { "display"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_DOWNLOAD             ] := { "download"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_DRAGGABLE            ] := { "draggable"             , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_DROPZONE             ] := { "dropzone"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ENCODING             ] := { "encoding"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ENCTYPE              ] := { "enctype"               , HTML_ATTR_TYPE_XTYPE          }
   t_aHA[ HTML_ATTR_ENTERKEYHINT         ] := { "enterkeyhint"          , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_EVENT                ] := { "event"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_EXPORTPARTS          ] := { "exportparts"           , HTML_ATTR_TYPE_UNKNOWN        }
   t_aHA[ HTML_ATTR_FACE                 ] := { "face"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_FOR                  ] := { "for"                   , HTML_ATTR_TYPE_IDREF          }
   t_aHA[ HTML_ATTR_FORM                 ] := { "form"                  , HTML_ATTR_TYPE_IDREF          }
   t_aHA[ HTML_ATTR_FORMACTION           ] := { "formaction"            , HTML_ATTR_TYPE_IDREF          }
   t_aHA[ HTML_ATTR_FORMENCTYPE          ] := { "formenctype"           , HTML_ATTR_TYPE_TYPE           }
   t_aHA[ HTML_ATTR_FORMMETHOD           ] := { "formmethod"            , HTML_ATTR_TYPE_FSUBMIT        }
   t_aHA[ HTML_ATTR_FORMNOVALIDATE       ] := { "formnovalidate"        , HTML_ATTR_TYPE_FVALIDATE      }
   t_aHA[ HTML_ATTR_FORMTARGET           ] := { "formtarget"            , HTML_ATTR_TYPE_TARGET         }
   t_aHA[ HTML_ATTR_FRAME                ] := { "frame"                 , HTML_ATTR_TYPE_TFRAME         }
   t_aHA[ HTML_ATTR_FRAMEBORDER          ] := { "frameborder"           , HTML_ATTR_TYPE_FBORDER        }
   t_aHA[ HTML_ATTR_FRAMESPACING         ] := { "framespacing"          , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_GRIDX                ] := { "gridx"                 , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_GRIDY                ] := { "gridy"                 , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_HEADERS              ] := { "headers"               , HTML_ATTR_TYPE_IDREFS         }
   t_aHA[ HTML_ATTR_HEIGHT               ] := { "height"                , HTML_ATTR_TYPE_LENGTH         }
   t_aHA[ HTML_ATTR_HIDDEN               ] := { "hidden"                , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_HIGH                 ] := { "high"                  , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_HREF                 ] := { "href"                  , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_HREFLANG             ] := { "hreflang"              , HTML_ATTR_TYPE_LANG           }
   t_aHA[ HTML_ATTR_HSPACE               ] := { "hspace"                , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_HTTP_EQUIV           ] := { "http-equiv"            , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ICON                 ] := { "icon"                  , HTML_ATTR_TYPE_ICON           }
   t_aHA[ HTML_ATTR_ID                   ] := { "id"                    , HTML_ATTR_TYPE_IDDEF          }
   t_aHA[ HTML_ATTR_INTEGRITY            ] := { "integrity"             , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_IS                   ] := { "is"                    , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ISMAP                ] := { "ismap"                 , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_ITEMID               ] := { "itemid"                , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ITEMPROP             ] := { "itemprop"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ITEMREF              ] := { "itemref"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ITEMSCOPE            ] := { "itemscope"             , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_ITEMTYPE             ] := { "itemtype"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_INPUTMODE            ] := { "inputmode"             , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_KEYTYPE              ] := { "keytype"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_KIND                 ] := { "kind"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_LABEL                ] := { "label"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_LANG                 ] := { "lang"                  , HTML_ATTR_TYPE_LANG           }
   t_aHA[ HTML_ATTR_LANGUAGE             ] := { "language"              , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_LAST_MODIFIED        ] := { "last_modified"         , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_LAST_VISIT           ] := { "last_visit"            , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_LEFTMARGIN           ] := { "leftmargin"            , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_LINK                 ] := { "link"                  , HTML_ATTR_TYPE_COLOR          }
   t_aHA[ HTML_ATTR_LIST                 ] := { "list"                  , HTML_ATTR_TYPE_LIST           }
   t_aHA[ HTML_ATTR_LONGDESC             ] := { "longdesc"              , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_LOOP                 ] := { "loop"                  , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_LOW                  ] := { "low"                   , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_LOWSRC               ] := { "lowsrc"                , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_MANIFEST             ] := { "manifest"              , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_MARGINHEIGHT         ] := { "marginheight"          , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_MARGINWIDTH          ] := { "marginwidth"           , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_MAX                  ] := { "max"                   , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_MAXLENGTH            ] := { "maxlength"             , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_MEDIA                ] := { "media"                 , HTML_ATTR_TYPE_MEDIA          }
   t_aHA[ HTML_ATTR_MEDIAGROUP           ] := { "mediagroup"            , HTML_ATTR_TYPE_MEDIA          }
   t_aHA[ HTML_ATTR_METHOD               ] := { "method"                , HTML_ATTR_TYPE_FSUBMIT        }
   t_aHA[ HTML_ATTR_METHODS              ] := { "methods"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_MIN                  ] := { "min"                   , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_MULTIPLE             ] := { "multiple"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_MUTED                ] := { "muted"                 , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_N                    ] := { "n"                     , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_NAME                 ] := { "name"                  , HTML_ATTR_TYPE_NAME           }
   t_aHA[ HTML_ATTR_NOHREF               ] := { "nohref"                , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_NONCE                ] := { "nonce"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_NORESIZE             ] := { "noresize"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_NOSHADE              ] := { "noshade"               , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_NOVALIDATE           ] := { "novalidate"            , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_NOWRAP               ] := { "nowrap"                , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_OBJECT               ] := { "object"                , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ONABORT              ] := { "onabort"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONAFTERUPDATE        ] := { "onafterupdate"         , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONAFTERPRINT         ] := { "onafterprint"          , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONBEFOREPRINT        ] := { "onbeforeprint"         , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONBEFOREUNLOAD       ] := { "onbeforeunload"        , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONBEFOREUPDATE       ] := { "onbeforeupdate"        , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONBLUR               ] := { "onblur"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONCANPLAY            ] := { "oncanplay"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONCANPLAYTHROUGH     ] := { "oncanplaythrough"      , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONCHANGE             ] := { "onchange"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONCLICK              ] := { "onclick"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONCONTEXTMENU        ] := { "oncontextmenu"         , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONCUECHANGE          ] := { "oncuechange"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDATAAVAILABLE      ] := { "ondataavailable"       , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDATASETCHANGED     ] := { "ondatasetchanged"      , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDATASETCOMPLETE    ] := { "ondatasetcomplete"     , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDRAG               ] := { "ondrag"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDRAGEND            ] := { "ondragend"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDRAGENTER          ] := { "ondragenter"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDRAGLEAVE          ] := { "ondragleave"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDRAGOVER           ] := { "ondragover"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDRAGSTART          ] := { "ondragstart"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDBLCLICK           ] := { "ondblclick"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDROP               ] := { "ondrop"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONDURATIONCHANGE     ] := { "ondurationchange"      , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONEMPTIED            ] := { "onemptied"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONENDED              ] := { "onended"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONERROR              ] := { "onerror"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONERRORUPDATE        ] := { "onerrorupdate"         , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONFOCUS              ] := { "onfocus"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONHASHCHANGE         ] := { "onhashchange"          , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONINPUT              ] := { "oninput"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONINVALID            ] := { "oninvalid"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONKEYDOWN            ] := { "onkeydown"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONKEYPRESS           ] := { "onkeypress"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONKEYUP              ] := { "onkeyup"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONLOAD               ] := { "onload"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONLOADEDDATA         ] := { "onloadeddata"          , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONLOADEDMETADATA     ] := { "onloadedmetadata"      , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONLOADSTART          ] := { "onloadstart"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONMESSAGE            ] := { "onmessage"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONMOUSEDOWN          ] := { "onmousedown"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONMOUSEMOVE          ] := { "onmousemove"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONMOUSEOUT           ] := { "onmouseout"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONMOUSEOVER          ] := { "onmouseover"           , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONMOUSEUP            ] := { "onmouseup"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONMOUSEWHEEL         ] := { "onmousewheel"          , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONOFFLINE            ] := { "onoffline"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONONLINE             ] := { "ononline"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONPAGEHIDE           ] := { "onpagehide"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONPAGESHOW           ] := { "onpageshow"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONPAUSE              ] := { "onpause"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONPLAY               ] := { "onplay"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONPLAYING            ] := { "onplaying"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONPOPSTATE           ] := { "onpopstate"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONPROGRESS           ] := { "onprogress"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONRATECHANGE         ] := { "onratechange"          , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONREADYSTATECHANGE   ] := { "onreadystatechange"    , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONREDO               ] := { "onredo"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONRESET              ] := { "onreset"               , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONRESIZE             ] := { "onresize"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONROWENTER           ] := { "onrowenter"            , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONROWEXIT            ] := { "onrowexit"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSCROLL             ] := { "onscroll"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSEEKED             ] := { "onseeked"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSEEKING            ] := { "onseeking"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSELECT             ] := { "onselect"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSHOW               ] := { "onshow"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSUBMIT             ] := { "onsubmit"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSTALLED            ] := { "onstalled"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSTORAGE            ] := { "onstorage"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONSUSPEND            ] := { "onsuspend"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONTIMEUPDATE         ] := { "ontimeupdate"          , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONUNDO               ] := { "onundo"                , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONUNLOAD             ] := { "onunload"              , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONVOLUMECHANGE       ] := { "onvolumechange"        , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_ONWAITING            ] := { "onwaiting"             , HTML_ATTR_TYPE_SCRIPT         }
   t_aHA[ HTML_ATTR_OPEN                 ] := { "open"                  , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_OPTIMUM              ] := { "optimum"               , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_PART                 ] := { "part"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_PATTERN              ] := { "pattern"               , HTML_ATTR_TYPE_PATTERN        }
   t_aHA[ HTML_ATTR_PLACEHOLDER          ] := { "placeholder"           , HTML_ATTR_TYPE_PLACEHOLDER    }
   t_aHA[ HTML_ATTR_PLAYSINLINE          ] := { "playsinline"           , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_POSTER               ] := { "poster"                , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_PRELOAD              ] := { "preload"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_PREFIX               ] := { "prefix"                , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_PROFILE              ] := { "profile"               , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_PROMPT               ] := { "prompt"                , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_PUBDATE              ] := { "pubdate"               , HTML_ATTR_TYPE_PUBDATE        }
   t_aHA[ HTML_ATTR_RADIOGROUP           ] := { "radiogroup"            , HTML_ATTR_TYPE_RADIOGROUP     }
   t_aHA[ HTML_ATTR_REFERRERPOLICY       ] := { "referrerpolicy"        , HTML_ATTR_TYPE_REFERRERPOLICY }
   t_aHA[ HTML_ATTR_REQUIRED             ] := { "required"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_RBSPAN               ] := { "rbspan"                , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_READONLY             ] := { "readonly"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_REL                  ] := { "rel"                   , HTML_ATTR_TYPE_LINKTYPES      }
   t_aHA[ HTML_ATTR_REV                  ] := { "rev"                   , HTML_ATTR_TYPE_LINKTYPES      }
   t_aHA[ HTML_ATTR_REVERSED             ] := { "reversed"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_RIGHTMARGIN          ] := { "rightmargin"           , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_ROLE                 ] := { "role"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_ROWS                 ] := { "rows"                  , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_ROWSPAN              ] := { "rowspan"               , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_RULES                ] := { "rules"                 , HTML_ATTR_TYPE_TRULES         }
   t_aHA[ HTML_ATTR_SANDBOX              ] := { "sandbox"               , HTML_ATTR_TYPE_SANDBOX        }
   t_aHA[ HTML_ATTR_SCHEME               ] := { "scheme"                , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_SCOPE                ] := { "scope"                 , HTML_ATTR_TYPE_SCOPE          }
   t_aHA[ HTML_ATTR_SCOPED               ] := { "scoped"                , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_SCROLLING            ] := { "scrolling"             , HTML_ATTR_TYPE_SCROLL         }
   t_aHA[ HTML_ATTR_SDAFORM              ] := { "sdaform"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_SDAPREF              ] := { "sdapref"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_SDASUFF              ] := { "sdasuff"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_SEAMLESS             ] := { "seamless"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_SELECTED             ] := { "selected"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_SHAPE                ] := { "shape"                 , HTML_ATTR_TYPE_SHAPE          }
   t_aHA[ HTML_ATTR_SHOWGRID             ] := { "showgrid"              , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_SHOWGRIDX            ] := { "showgridx"             , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_SHOWGRIDY            ] := { "showgridy"             , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_SIZE                 ] := { "size"                  , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_SIZES                ] := { "sizes"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_SPAN                 ] := { "span"                  , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_SPELLCHECK           ] := { "spellcheck"            , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_SRC                  ] := { "src"                   , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_SRCDOC               ] := { "srcdoc"                , HTML_ATTR_TYPE_SRCDOC         }
   t_aHA[ HTML_ATTR_SRCLANG              ] := { "srclang"               , HTML_ATTR_TYPE_LANG           }
   t_aHA[ HTML_ATTR_SRCSET               ] := { "srcset"                , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_STANDBY              ] := { "standby"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_START                ] := { "start"                 , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_STEP                 ] := { "step"                  , HTML_ATTR_TYPE_STEP           }
   t_aHA[ HTML_ATTR_STYLE                ] := { "style"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_SUMMARY              ] := { "summary"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_TABINDEX             ] := { "tabindex"              , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_TARGET               ] := { "target"                , HTML_ATTR_TYPE_TARGET         }
   t_aHA[ HTML_ATTR_TEXT                 ] := { "text"                  , HTML_ATTR_TYPE_COLOR          }
   t_aHA[ HTML_ATTR_TITLE                ] := { "title"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_TOPMARGIN            ] := { "topmargin"             , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_TRANSLATE            ] := { "translate"             , HTML_ATTR_TYPE_BOOL           }
   t_aHA[ HTML_ATTR_TYPE                 ] := { "type"                  , HTML_ATTR_TYPE_TYPE           }
   t_aHA[ HTML_ATTR_UNKNOWN              ] := { "unknown!"              , HTML_ATTR_TYPE_UNKNOWN        }
   t_aHA[ HTML_ATTR_URN                  ] := { "urn"                   , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_USEMAP               ] := { "usemap"                , HTML_ATTR_TYPE_URL            }
   t_aHA[ HTML_ATTR_VALIGN               ] := { "valign"                , HTML_ATTR_TYPE_VALIGN         }
   t_aHA[ HTML_ATTR_VALUE                ] := { "value"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_VALUETYPE            ] := { "valuetype"             , HTML_ATTR_TYPE_VTYPE          }
   t_aHA[ HTML_ATTR_VERSION              ] := { "version"               , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_VLINK                ] := { "vlink"                 , HTML_ATTR_TYPE_COLOR          }
   t_aHA[ HTML_ATTR_VSPACE               ] := { "vspace"                , HTML_ATTR_TYPE_NUMBER         }
   t_aHA[ HTML_ATTR_WIDTH                ] := { "width"                 , HTML_ATTR_TYPE_LENGTH         }
   t_aHA[ HTML_ATTR_WRAP                 ] := { "wrap"                  , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_XML_BASE             ] := { "xml:base"              , HTML_ATTR_TYPE_BASE           }
   t_aHA[ HTML_ATTR_XML_LANG             ] := { "xml:lang"              , HTML_ATTR_TYPE_LANG           }
   t_aHA[ HTML_ATTR_XML_SPACE            ] := { "xml:space"             , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_XMLNS                ] := { "xmlns"                 , HTML_ATTR_TYPE_PCDATA         }
   t_aHA[ HTML_ATTR_XMLNS_XLINK          ] := { "xmlns:xlink"           , HTML_ATTR_TYPE_URL            }

/*Begin TODO:*/

   t_aHA[ HTML_ATTR_ABOUT                ] := { "about"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_INLIST               ] := { "inlist"                , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_PROPERTY             ] := { "property"              , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_RESOURCE             ] := { "resource"              , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_TYPEOF               ] := { "typeof"                , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_VOCAB                ] := { "vocab"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_AS                   ] := { "as"                    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SLOT                 ] := { "slot"                  , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_LOADING              ] := { "loading"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_FILL                 ] := { "fill"                  , HTML_ATTR_TYPE_UNKNOWN   }

/*End TODO:*/

/*Begin TODO:*/

   /* HTML SVG Tag attribute data are adopted for Harbour from Tidy */

   t_aHA[ HTML_ATTR_SVG_BASEPROFILE           ] := { "baseprofile"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_BY                    ] := { "by"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_COLORINTERPOLATION    ] := { "colorinterpolation"   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_COLORRENDERING        ] := { "colorrendering"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_CONTENTSCRIPTTYPE     ] := { "contentscripttype"    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_CONTENTSTYLETYPE      ] := { "contentstyletype"     , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_CX                    ] := { "cx"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_CY                    ] := { "cy"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_ENTERKEYHINT          ] := { "enterkeyhint"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_EXPORTPARTS           ] := { "exportparts"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FILL                  ] := { "fill"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FILL_RULE             ] := { "fill-rule"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FILL_OPACITY          ] := { "fill-opacity"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FILTER                ] := { "filter"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FLOOD_COLOR           ] := { "flood-color"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FLOOD_OPACITY         ] := { "flood-opacity"        , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FONT_SIZE             ] := { "font-size"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FONT_SIZE_ADJUST      ] := { "font-size-adjust"     , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FONT_STYLE            ] := { "font-style"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_VISIBILITY            ] := { "visibility"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FROM                  ] := { "from"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_FR                    ] := { "fr"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_HEIGHT                ] := { "height"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_KEYPOINTS             ] := { "keypoints"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_KEYTIMES              ] := { "keytimes"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_LENGTHADJUST          ] := { "lengthadjust"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_LETTER_SPACING        ] := { "letter-spacing"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_LIGHTING_COLOR        ] := { "lighting-color"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_MARKERHEIGHT          ] := { "markerheight"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_MARKERWIDTH           ] := { "markerwidth"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_MASK                  ] := { "mask"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_MEDIA                 ] := { "media"                , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_NUMOCTAVES            ] := { "numoctaves"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_NONCE                 ] := { "nonce"                , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_OPACITY               ] := { "opacity"              , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_OPERATOR              ] := { "operator"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_ORIENT                ] := { "orient"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_PART                  ] := { "part"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_PATH                  ] := { "path"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_PATHLENGTH            ] := { "pathlength"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_PATTERNCONTENTUNITS   ] := { "patterncontentunits"  , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_PATTERNTRANSFORM      ] := { "patterntransform"     , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_PATTERNUNITS          ] := { "patternunits"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_POINTER_EVENTS        ] := { "pointer-events"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_POINTS                ] := { "points"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_POINTSATX             ] := { "pointsatx"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_POINTSATY             ] := { "pointsaty"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_POINTSATZ             ] := { "pointsatz"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_PRESERVEASPECTRATIO   ] := { "preserveaspectratio"  , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_R                     ] := { "r"                    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_RADIUS                ] := { "radius"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_REPEATCOUNT           ] := { "repeatcount"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_REPEATDUR             ] := { "repeatdur"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_RESTART               ] := { "restart"              , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_ROTATE                ] := { "rotate"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_RX                    ] := { "rx"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_RY                    ] := { "ry"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_SCALE                 ] := { "scale"                , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_SEED                  ] := { "seed"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_SHAPE_RENDERING       ] := { "shape-rendering"      , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STARTOFFSET           ] := { "startoffset"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STDDEVIATION          ] := { "stddeviation"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STITCHTILES           ] := { "stitchtiles"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STOP_COLOR            ] := { "stop-color"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STOP_OPACITY          ] := { "stop-opacity"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE                ] := { "stroke"               , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE_DASHARRAY      ] := { "stroke-dasharray"     , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE_DASHOFFSET     ] := { "stroke-dashoffset"    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE_LINECAP        ] := { "stroke-linecap"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE_OPACITY        ] := { "stroke-opacity"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE_WIDTH          ] := { "stroke-width"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE_LINEJOIN       ] := { "stroke-linejoin"      , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STROKE_MITERLIMIT     ] := { "stroke-miterlimit"    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_STYLE                 ] := { "style"                , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_SURFACESCALE          ] := { "surfacescale"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_SYSTEMLANGUAGE        ] := { "systemlanguage"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TABINDEX              ] := { "tabindex"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TABLEVALUES           ] := { "tablevalues"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TEXT_ANCHOR           ] := { "text-anchor"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TEXT_DECORATION       ] := { "text-decoration"      , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TEXT_RENDERING        ] := { "text-rendering"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TEXTLENGTH            ] := { "textlength"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TO                    ] := { "to"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TRANSFORM             ] := { "transform"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_TYPE                  ] := { "type"                 , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_VECTOR_EFFECT         ] := { "vector-effect"        , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_VIEWBOX               ] := { "viewbox"              , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_VISIBILITY            ] := { "visibility"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_WIDTH                 ] := { "width"                , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_WORD_SPACING          ] := { "word-spacing"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_X                     ] := { "x"                    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_X1                    ] := { "x1"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_X2                    ] := { "x2"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_XCHANNELSELECTOR      ] := { "xchannelselector"     , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_XML_LANG              ] := { "xml:lang"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_XML_BASE              ] := { "xml:base"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_Y                     ] := { "y"                    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_Y1                    ] := { "y1"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_Y2                    ] := { "y2"                   , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_YCHANNELSELECTOR      ] := { "ychannelselector"     , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_Z                     ] := { "z"                    , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHA[ HTML_ATTR_SVG_ZOOMANDPAN            ] := { "zoomandpan"           , HTML_ATTR_TYPE_UNKNOWN   }

/*End TODO:*/

   /* HTML Tag Aria attribute data are adopted for Harbour from Tidy */

   t_aHAAria[ HTML_ATTR_ARIA_ACTIVEDESCENDANT] := { "aria_activedescendant" , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_ATOMIC          ] := { "aria_atomic"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_AUTOCOMPLETE    ] := { "aria_autocomplete"     , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_BUSY            ] := { "aria_busy"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_CHECKED         ] := { "aria_checked"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_CONTROLS        ] := { "aria_controls"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_DESCRIBEDBY     ] := { "aria_describedby"      , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_DISABLED        ] := { "aria_disabled"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_DROPEFFECT      ] := { "aria_dropeffect"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_EXPANDED        ] := { "aria_expanded"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_FLOWTO          ] := { "aria_flowto"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_GRABBED         ] := { "aria_grabbed"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_HASPOPUP        ] := { "aria_haspopup"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_HIDDEN          ] := { "aria_hidden"           , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_INVALID         ] := { "aria_invalid"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_LABEL           ] := { "aria_label"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_LABELLEDBY      ] := { "aria_labelledby"       , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_LEVEL           ] := { "aria_level"            , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_LIVE            ] := { "aria_live"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_MULTILINE       ] := { "aria_multiline"        , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_MULTISELECTABLE ] := { "aria_multiselectable"  , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_ORIENTATION     ] := { "aria_orientation"      , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_OWNS            ] := { "aria_owns"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_POSINSET        ] := { "aria_posinset"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_PRESSED         ] := { "aria_pressed"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_READONLY        ] := { "aria_readonly"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_RELEVANT        ] := { "aria_relevant"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_REQUIRED        ] := { "aria_required"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_SELECTED        ] := { "aria_selected"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_SETSIZE         ] := { "aria_setsize"          , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_SORT            ] := { "aria_sort"             , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_VALUEMAX        ] := { "aria_valuemax"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_VALUEMIN        ] := { "aria_valuemin"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_VALUENOW        ] := { "aria_valuenow"         , HTML_ATTR_TYPE_UNKNOWN   }
   t_aHAAria[ HTML_ATTR_ARIA_VALUETEXT       ] := { "aria_valuetext"        , HTML_ATTR_TYPE_UNKNOWN   }

   RETURN

/*
    The global attributes below can be used on any HTML element
    https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes
    http://www-db.deis.unibo.it/courses/TW/DOCS/w3schools/tags/ref_standardattributes.asp.html

*/
STATIC FUNCTION THtmlAttr_GlobalAttributes()
   RETURN { ;
      t_aHA[ HTML_ATTR_ACCESSKEY        ], ;
      t_aHA[ HTML_ATTR_AUTOCAPITALIZE   ], ;
      t_aHA[ HTML_ATTR_AUTOFOCUS        ], ;
      t_aHA[ HTML_ATTR_CLASS            ], ;
      t_aHA[ HTML_ATTR_CONTENTEDITABLE  ], ;
      t_aHA[ HTML_ATTR_CONTEXTMENU      ], ;
      t_aHA[ HTML_ATTR_DATA             ], ;
      t_aHA[ HTML_ATTR_DATAFLD          ], ;
      t_aHA[ HTML_ATTR_DATAFORMATAS     ], ;
      t_aHA[ HTML_ATTR_DATAPAGESIZE     ], ;
      t_aHA[ HTML_ATTR_DATASRC          ], ;
      t_aHA[ HTML_ATTR_DATATYPE         ], ;
      t_aHA[ HTML_ATTR_DIR              ], ;
      t_aHA[ HTML_ATTR_DISPLAY          ], ;
      t_aHA[ HTML_ATTR_DRAGGABLE        ], ;
      t_aHA[ HTML_ATTR_DROPZONE         ], ;
      t_aHA[ HTML_ATTR_ENTERKEYHINT     ], ;
      t_aHA[ HTML_ATTR_EXPORTPARTS      ], ;
      t_aHA[ HTML_ATTR_HIDDEN           ], ;
      t_aHA[ HTML_ATTR_ID               ], ;
      t_aHA[ HTML_ATTR_INPUTMODE        ], ;
      t_aHA[ HTML_ATTR_IS               ], ;
      t_aHA[ HTML_ATTR_ITEMID           ], ;
      t_aHA[ HTML_ATTR_ITEMPROP         ], ;
      t_aHA[ HTML_ATTR_ITEMREF          ], ;
      t_aHA[ HTML_ATTR_ITEMSCOPE        ], ;
      t_aHA[ HTML_ATTR_ITEMTYPE         ], ;
      t_aHA[ HTML_ATTR_LANG             ], ;
      t_aHA[ HTML_ATTR_NONCE            ], ;
      t_aHA[ HTML_ATTR_ONCONTEXTMENU    ], ;
      t_aHA[ HTML_ATTR_ONDRAG           ], ;
      t_aHA[ HTML_ATTR_ONDRAGEND        ], ;
      t_aHA[ HTML_ATTR_ONDRAGENTER      ], ;
      t_aHA[ HTML_ATTR_ONDRAGLEAVE      ], ;
      t_aHA[ HTML_ATTR_ONDRAGOVER       ], ;
      t_aHA[ HTML_ATTR_ONDRAGSTART      ], ;
      t_aHA[ HTML_ATTR_ONDROP           ], ;
      t_aHA[ HTML_ATTR_ONMESSAGE        ], ;
      t_aHA[ HTML_ATTR_ONMOUSEWHEEL     ], ;
      t_aHA[ HTML_ATTR_PART             ], ;
      t_aHA[ HTML_ATTR_PREFIX           ], ;
      t_aHA[ HTML_ATTR_ROLE             ], ;
      t_aHA[ HTML_ATTR_SLOT             ], ;
      t_aHA[ HTML_ATTR_SPELLCHECK       ], ;
      t_aHA[ HTML_ATTR_STYLE            ], ;
      t_aHA[ HTML_ATTR_TABINDEX         ], ;
      t_aHA[ HTML_ATTR_TITLE            ], ;
      t_aHA[ HTML_ATTR_TRANSLATE        ], ;
      t_aHA[ HTML_ATTR_XML_BASE         ], ;
      t_aHA[ HTML_ATTR_XML_LANG         ], ;
      t_aHA[ HTML_ATTR_UNKNOWN          ] }

STATIC FUNCTION THtmlAttr_A()
   RETURN { ;
      t_aHA[ HTML_ATTR_CHARSET          ], ;
      t_aHA[ HTML_ATTR_COORDS           ], ;
      t_aHA[ HTML_ATTR_HREF             ], ;
      t_aHA[ HTML_ATTR_HREFLANG         ], ;
      t_aHA[ HTML_ATTR_METHODS          ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_ONBLUR           ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONFOCUS          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_REL              ], ;
      t_aHA[ HTML_ATTR_REV              ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SHAPE            ], ;
      t_aHA[ HTML_ATTR_TARGET           ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_URN              ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_DOWNLOAD         ] }

STATIC FUNCTION THtmlAttr_ABBR()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_ACRONYM()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_ADDRESS()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_APPLET()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ALT              ], ;
      t_aHA[ HTML_ATTR_ARCHIVE          ], ;
      t_aHA[ HTML_ATTR_CODE             ], ;
      t_aHA[ HTML_ATTR_CODEBASE         ], ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_HSPACE           ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_OBJECT           ], ;
      t_aHA[ HTML_ATTR_VSPACE           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ] }

STATIC FUNCTION THtmlAttr_AREA()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALT              ], ;
      t_aHA[ HTML_ATTR_COORDS           ], ;
      t_aHA[ HTML_ATTR_HREF             ], ;
      t_aHA[ HTML_ATTR_HREFLANG         ], ;
      t_aHA[ HTML_ATTR_NOHREF           ], ;
      t_aHA[ HTML_ATTR_MEDIA            ], ;
      t_aHA[ HTML_ATTR_ONBLUR           ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONFOCUS          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_REFERRERPOLICY   ], ;
      t_aHA[ HTML_ATTR_REL              ], ;
      t_aHA[ HTML_ATTR_SHAPE            ], ;
      t_aHA[ HTML_ATTR_TARGET           ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_ARTICLE()
   RETURN { ;
      t_aHA[ HTML_ATTR_NAME             ] }

STATIC FUNCTION THtmlAttr_ASIDE()
   RETURN { ;
      t_aHA[ HTML_ATTR_NAME             ] }

STATIC FUNCTION THtmlAttr_AUDIO()
   RETURN { ;
      t_aHA[ HTML_ATTR_AUTOPLAY         ], ;
      t_aHA[ HTML_ATTR_ONCANPLAY        ], ;
      t_aHA[ HTML_ATTR_ONCANPLAYTHROUGH ], ;
      t_aHA[ HTML_ATTR_CONTROLS         ], ;
      t_aHA[ HTML_ATTR_CROSSORIGIN      ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_LOOP             ], ;
      t_aHA[ HTML_ATTR_MEDIAGROUP       ], ;
      t_aHA[ HTML_ATTR_ONABORT          ], ;
      t_aHA[ HTML_ATTR_ONDURATIONCHANGE ], ;
      t_aHA[ HTML_ATTR_ONEMPTIED        ], ;
      t_aHA[ HTML_ATTR_ONENDED          ], ;
      t_aHA[ HTML_ATTR_ONERROR          ], ;
      t_aHA[ HTML_ATTR_ONLOADEDDATA     ], ;
      t_aHA[ HTML_ATTR_ONLOADEDMETADATA ], ;
      t_aHA[ HTML_ATTR_ONLOADSTART      ], ;
      t_aHA[ HTML_ATTR_ONPAUSE          ], ;
      t_aHA[ HTML_ATTR_ONPLAY           ], ;
      t_aHA[ HTML_ATTR_ONPLAYING        ], ;
      t_aHA[ HTML_ATTR_ONPROGRESS       ], ;
      t_aHA[ HTML_ATTR_ONRATECHANGE     ], ;
      t_aHA[ HTML_ATTR_ONSEEKED         ], ;
      t_aHA[ HTML_ATTR_ONSEEKING        ], ;
      t_aHA[ HTML_ATTR_ONSTALLED        ], ;
      t_aHA[ HTML_ATTR_ONSUSPEND        ], ;
      t_aHA[ HTML_ATTR_ONTIMEUPDATE     ], ;
      t_aHA[ HTML_ATTR_ONVOLUMECHANGE   ], ;
      t_aHA[ HTML_ATTR_ONWAITING        ], ;
      t_aHA[ HTML_ATTR_SRC              ] }

STATIC FUNCTION THtmlAttr_B()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BASE()
   RETURN { ;
      t_aHA[ HTML_ATTR_HREF             ], ;
      t_aHA[ HTML_ATTR_TARGET           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BASEFONT()
   RETURN { ;
      t_aHA[ HTML_ATTR_COLOR            ], ;
      t_aHA[ HTML_ATTR_FACE             ], ;
      t_aHA[ HTML_ATTR_SIZE             ] }

STATIC FUNCTION THtmlAttr_BDI()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BDO()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BGSOUND()
/* TODO:
balance
volume
*/
   RETURN { ;
      t_aHA[ HTML_ATTR_LOOP             ], ;
      t_aHA[ HTML_ATTR_SRC              ] }

STATIC FUNCTION THtmlAttr_BIG()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BLOCKQUOTE()
   RETURN { ;
      t_aHA[ HTML_ATTR_CITE             ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BODY()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALINK            ], ;
      t_aHA[ HTML_ATTR_BACKGROUND       ], ;
      t_aHA[ HTML_ATTR_BGCOLOR          ], ;
      t_aHA[ HTML_ATTR_LINK             ], ;
      t_aHA[ HTML_ATTR_ONAFTERPRINT     ], ;
      t_aHA[ HTML_ATTR_ONBEFOREPRINT    ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONERROR          ], ;
      t_aHA[ HTML_ATTR_ONHASHCHANGE     ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONLOAD           ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ONOFFLINE        ], ;
      t_aHA[ HTML_ATTR_ONONLINE         ], ;
      t_aHA[ HTML_ATTR_ONPAGEHIDE       ], ;
      t_aHA[ HTML_ATTR_ONPAGESHOW       ], ;
      t_aHA[ HTML_ATTR_ONPOPSTATE       ], ;
      t_aHA[ HTML_ATTR_ONUNLOAD         ], ;
      t_aHA[ HTML_ATTR_ONREDO           ], ;
      t_aHA[ HTML_ATTR_ONRESIZE         ], ;
      t_aHA[ HTML_ATTR_ONSTORAGE        ], ;
      t_aHA[ HTML_ATTR_ONUNDO           ], ;
      t_aHA[ HTML_ATTR_TEXT             ], ;
      t_aHA[ HTML_ATTR_VLINK            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BR()
   RETURN { ;
      t_aHA[ HTML_ATTR_CLEAR            ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_BUTTON()
   RETURN { ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_FORMACTION       ], ;
      t_aHA[ HTML_ATTR_FORMENCTYPE      ], ;
      t_aHA[ HTML_ATTR_FORMMETHOD       ], ;
      t_aHA[ HTML_ATTR_FORMNOVALIDATE   ], ;
      t_aHA[ HTML_ATTR_FORMTARGET       ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_ONBLUR           ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONFOCUS          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_VALUE            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_CANVAS()
   RETURN { ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ] }

STATIC FUNCTION THtmlAttr_CAPTION()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_CENTER()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ] }

STATIC FUNCTION THtmlAttr_CITE()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_CODE()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_COL()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SPAN             ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_COLGROUP()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SPAN             ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_COMMAND()
   RETURN { ;
      t_aHA[ HTML_ATTR_CHECKED          ], ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_ICON             ], ;
      t_aHA[ HTML_ATTR_LABEL            ], ;
      t_aHA[ HTML_ATTR_RADIOGROUP       ], ;
      t_aHA[ HTML_ATTR_TYPE             ] }

STATIC FUNCTION THtmlAttr_DATA()
   RETURN { ;
      t_aHA[ HTML_ATTR_VALUE            ] }

STATIC FUNCTION THtmlAttr_DD()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_DEL()
   RETURN { ;
      t_aHA[ HTML_ATTR_CITE             ], ;
      t_aHA[ HTML_ATTR_DATETIME         ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_DETAILS()
   RETURN { ;
      t_aHA[ HTML_ATTR_OPEN             ] }

STATIC FUNCTION THtmlAttr_DFN()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_DIALOG()
   RETURN { ;
      t_aHA[ HTML_ATTR_OPEN             ] }

STATIC FUNCTION THtmlAttr_DIR()
   RETURN { ;
      t_aHA[ HTML_ATTR_COMPACT          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ] }

STATIC FUNCTION THtmlAttr_DIV()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ONSCROLL         ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_DL()
   RETURN { ;
      t_aHA[ HTML_ATTR_COMPACT          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_DT()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_EM()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_EMBED()
   RETURN { ;
      t_aHA[ HTML_ATTR_HEIGHT          ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_ONCANPLAY        ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_ONABORT          ], ;
      t_aHA[ HTML_ATTR_ONERROR          ] }

STATIC FUNCTION THtmlAttr_FIELDSET()
   RETURN { ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_FONT()
   RETURN { ;
      t_aHA[ HTML_ATTR_COLOR            ], ;
      t_aHA[ HTML_ATTR_FACE             ], ;
      t_aHA[ HTML_ATTR_SIZE             ] }

STATIC FUNCTION THtmlAttr_FORM()
   RETURN { ;
      t_aHA[ HTML_ATTR_ACCEPT           ], ;
      t_aHA[ HTML_ATTR_ACCEPT_CHARSET   ], ;
      t_aHA[ HTML_ATTR_ACTION           ], ;
      t_aHA[ HTML_ATTR_ENCTYPE          ], ;
      t_aHA[ HTML_ATTR_METHOD           ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_NOVALIDATE       ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ONRESET          ], ;
      t_aHA[ HTML_ATTR_ONSUBMIT         ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SDASUFF          ], ;
      t_aHA[ HTML_ATTR_TARGET           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_AUTOCOMPLETE     ] }

STATIC FUNCTION THtmlAttr_FRAME()
   RETURN { ;
      t_aHA[ HTML_ATTR_FRAMEBORDER      ], ;
      t_aHA[ HTML_ATTR_LONGDESC         ], ;
      t_aHA[ HTML_ATTR_MARGINHEIGHT     ], ;
      t_aHA[ HTML_ATTR_MARGINWIDTH      ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_NORESIZE         ], ;
      t_aHA[ HTML_ATTR_SCROLLING        ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_ASYNC            ] }

STATIC FUNCTION THtmlAttr_FIGURE()
   RETURN { ;
      t_aHA[ HTML_ATTR_NAME             ] }

STATIC FUNCTION THtmlAttr_FIGCAPTION()
   RETURN { ;
      t_aHA[ HTML_ATTR_NAME             ] }

STATIC FUNCTION THtmlAttr_FRAMESET()
   RETURN { ;
      t_aHA[ HTML_ATTR_COLS             ], ;
      t_aHA[ HTML_ATTR_ONLOAD           ], ;
      t_aHA[ HTML_ATTR_ONUNLOAD         ], ;
      t_aHA[ HTML_ATTR_ROWS             ] }

STATIC FUNCTION THtmlAttr_H1()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_H2()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_H3()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_H4()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_H5()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_H6()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_HEAD()
   RETURN { ;
      t_aHA[ HTML_ATTR_PROFILE          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_HEADER()
   RETURN {}

STATIC FUNCTION THtmlAttr_HGROUP()
RETURN {}

STATIC FUNCTION THtmlAttr_HR()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_NOSHADE          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SIZE             ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_HTML()
   RETURN { ;
      t_aHA[ HTML_ATTR_MANIFEST         ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_VERSION          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_I()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_IFRAME()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_FRAMEBORDER      ], ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_LONGDESC         ], ;
      t_aHA[ HTML_ATTR_MARGINHEIGHT     ], ;
      t_aHA[ HTML_ATTR_MARGINWIDTH      ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_SANDBOX          ], ;
      t_aHA[ HTML_ATTR_SCROLLING        ], ;
      t_aHA[ HTML_ATTR_SEAMLESS         ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_SRCDOC           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_ALLOWFULLSCREEN  ], ;
      t_aHA[ HTML_ATTR_ASYNC            ] }

STATIC FUNCTION THtmlAttr_IMG()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ALT              ], ;
      t_aHA[ HTML_ATTR_BORDER           ], ;
      t_aHA[ HTML_ATTR_CROSSORIGIN      ], ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_HSPACE           ], ;
      t_aHA[ HTML_ATTR_ISMAP            ], ;
      t_aHA[ HTML_ATTR_LONGDESC         ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_ONABORT          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONERROR          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_USEMAP           ], ;
      t_aHA[ HTML_ATTR_VSPACE           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_ASYNC            ] }

STATIC FUNCTION THtmlAttr_INPUT()
   RETURN { ;
      t_aHA[ HTML_ATTR_ACCEPT           ], ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ALT              ], ;
      t_aHA[ HTML_ATTR_AUTOCAPITALIZE   ], ;
      t_aHA[ HTML_ATTR_CHECKED          ], ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_DIRNAME          ], ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_FORMACTION       ], ;
      t_aHA[ HTML_ATTR_FORMENCTYPE      ], ;
      t_aHA[ HTML_ATTR_FORMMETHOD       ], ;
      t_aHA[ HTML_ATTR_FORMNOVALIDATE   ], ;
      t_aHA[ HTML_ATTR_FORMTARGET       ], ;
      t_aHA[ HTML_ATTR_ISMAP            ], ;
      t_aHA[ HTML_ATTR_LIST             ], ;
      t_aHA[ HTML_ATTR_MAX              ], ;
      t_aHA[ HTML_ATTR_MAXLENGTH        ], ;
      t_aHA[ HTML_ATTR_MIN              ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_ONBLUR           ], ;
      t_aHA[ HTML_ATTR_ONCHANGE         ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONFOCUS          ], ;
      t_aHA[ HTML_ATTR_ONINPUT          ], ;
      t_aHA[ HTML_ATTR_ONINVALID        ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ONSELECT         ], ;
      t_aHA[ HTML_ATTR_PATTERN          ], ;
      t_aHA[ HTML_ATTR_PLACEHOLDER      ], ;
      t_aHA[ HTML_ATTR_READONLY         ], ;
      t_aHA[ HTML_ATTR_REQUIRED         ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SIZE             ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_STEP             ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_USEMAP           ], ;
      t_aHA[ HTML_ATTR_VALUE            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_ASYNC            ], ;
      t_aHA[ HTML_ATTR_AUTOCOMPLETE     ] }

STATIC FUNCTION THtmlAttr_INS()
   RETURN { ;
      t_aHA[ HTML_ATTR_CITE             ], ;
      t_aHA[ HTML_ATTR_DATETIME         ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_ISINDEX()
   RETURN { ;
      t_aHA[ HTML_ATTR_PROMPT           ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ] }

STATIC FUNCTION THtmlAttr_KBD()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_KEYGEN()
   RETURN { ;
      t_aHA[ HTML_ATTR_CHALLENGE        ], ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_KEYTYPE          ], ;
      t_aHA[ HTML_ATTR_NAME             ] }

STATIC FUNCTION THtmlAttr_LABEL()
   RETURN { ;
      t_aHA[ HTML_ATTR_FOR              ], ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_ONBLUR           ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONFOCUS          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_LEGEND()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_LI()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_VALUE            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_LINK()
   RETURN { ;
      t_aHA[ HTML_ATTR_AS               ], ;
      t_aHA[ HTML_ATTR_CHARSET          ], ;
      t_aHA[ HTML_ATTR_CROSSORIGIN      ], ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_HREF             ], ;
      t_aHA[ HTML_ATTR_HREFLANG         ], ;
      t_aHA[ HTML_ATTR_INTEGRITY        ], ;
      t_aHA[ HTML_ATTR_MEDIA            ], ;
      t_aHA[ HTML_ATTR_METHODS          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONERROR          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_REL              ], ;
      t_aHA[ HTML_ATTR_REV              ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SIZES            ], ;
      t_aHA[ HTML_ATTR_TARGET           ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_URN              ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_SRCSET           ] }

STATIC FUNCTION THtmlAttr_LISTING()
   RETURN { ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ] }

STATIC FUNCTION THtmlAttr_MAP()
   RETURN { ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_MARQUEE()
/* TODO:
behavior
direction
scrollamount
scrolldelay
onbounce
onfinish
onstart
*/
   RETURN { ;
      t_aHA[ HTML_ATTR_BGCOLOR          ], ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_HSPACE           ], ;
      t_aHA[ HTML_ATTR_LOOP             ], ;
      t_aHA[ HTML_ATTR_VSPACE           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ] }

STATIC FUNCTION THtmlAttr_MENU()
   RETURN { ;
      t_aHA[ HTML_ATTR_COMPACT          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ONSHOW           ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ] }

STATIC FUNCTION THtmlAttr_MENUITEM()
   RETURN { ;
      t_aHA[ HTML_ATTR_CHECKED          ], ;
      t_aHA[ HTML_ATTR_DEFAULT          ], ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_ICON             ], ;
      t_aHA[ HTML_ATTR_LABEL            ], ;
      t_aHA[ HTML_ATTR_RADIOGROUP       ], ;
      t_aHA[ HTML_ATTR_TYPE             ] }

STATIC FUNCTION THtmlAttr_META()
   RETURN { ;
      t_aHA[ HTML_ATTR_CHARSET          ], ;
      t_aHA[ HTML_ATTR_CONTENT          ], ;
      t_aHA[ HTML_ATTR_HTTP_EQUIV       ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_SCHEME           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_METER()
   RETURN { ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_HIGH             ], ;
      t_aHA[ HTML_ATTR_LOW              ], ;
      t_aHA[ HTML_ATTR_MAX              ], ;
      t_aHA[ HTML_ATTR_MIN              ], ;
      t_aHA[ HTML_ATTR_OPTIMUM          ], ;
      t_aHA[ HTML_ATTR_VALUE            ] }

STATIC FUNCTION THtmlAttr_NEXTID()
   RETURN { ;
      t_aHA[ HTML_ATTR_N                ] }

STATIC FUNCTION THtmlAttr_NOFRAMES()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ] }

STATIC FUNCTION THtmlAttr_NOSCRIPT()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_OBJECT()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN                ], ;
      t_aHA[ HTML_ATTR_ARCHIVE              ], ;
      t_aHA[ HTML_ATTR_BORDER               ], ;
      t_aHA[ HTML_ATTR_CLASSID              ], ;
      t_aHA[ HTML_ATTR_CODEBASE             ], ;
      t_aHA[ HTML_ATTR_CODETYPE             ], ;
      t_aHA[ HTML_ATTR_DECLARE              ], ;
      t_aHA[ HTML_ATTR_FORM                 ], ;
      t_aHA[ HTML_ATTR_HEIGHT               ], ;
      t_aHA[ HTML_ATTR_HSPACE               ], ;
      t_aHA[ HTML_ATTR_NAME                 ], ;
      t_aHA[ HTML_ATTR_ONABORT              ], ;
      t_aHA[ HTML_ATTR_ONCANPLAY            ], ;
      t_aHA[ HTML_ATTR_ONCLICK              ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK           ], ;
      t_aHA[ HTML_ATTR_ONERROR              ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN            ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS           ], ;
      t_aHA[ HTML_ATTR_ONKEYUP              ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT           ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP            ], ;
      t_aHA[ HTML_ATTR_ONREADYSTATECHANGE   ], ;
      t_aHA[ HTML_ATTR_STANDBY              ], ;
      t_aHA[ HTML_ATTR_TYPE                 ], ;
      t_aHA[ HTML_ATTR_USEMAP               ], ;
      t_aHA[ HTML_ATTR_VSPACE               ], ;
      t_aHA[ HTML_ATTR_WIDTH                ], ;
      t_aHA[ HTML_ATTR_XMLNS                ] }

STATIC FUNCTION THtmlAttr_OL()
   RETURN { ;
      t_aHA[ HTML_ATTR_COMPACT          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_START            ], ;
      t_aHA[ HTML_ATTR_REVERSED         ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_OPTGROUP()
   RETURN { ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_LABEL            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_OPTION()
   RETURN { ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_LABEL            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SELECTED         ], ;
      t_aHA[ HTML_ATTR_VALUE            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_OUTPUT()
   RETURN { ;
      t_aHA[ HTML_ATTR_FOR              ], ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_NAME             ] }

STATIC FUNCTION THtmlAttr_P()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_PARAM()
   RETURN { ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_VALUE            ], ;
      t_aHA[ HTML_ATTR_VALUETYPE        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_PLAINTEXT()
   RETURN { ;
      t_aHA[ HTML_ATTR_SDAFORM          ] }

STATIC FUNCTION THtmlAttr_PRE()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XML_SPACE        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_PROGRESS()
   RETURN { ;
      t_aHA[ HTML_ATTR_MAX              ], ;
      t_aHA[ HTML_ATTR_VALUE            ] }

STATIC FUNCTION THtmlAttr_Q()
   RETURN { ;
      t_aHA[ HTML_ATTR_CITE             ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_RB()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_RBC()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_RP()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_RT()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_RBSPAN           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_RTC()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_RUBY()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_S()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ] }

STATIC FUNCTION THtmlAttr_SAMP()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_SCRIPT()
   RETURN { ;
      t_aHA[ HTML_ATTR_CHARSET          ], ;
      t_aHA[ HTML_ATTR_CROSSORIGIN      ], ;
      t_aHA[ HTML_ATTR_DEFER            ], ;
      t_aHA[ HTML_ATTR_EVENT            ], ;
      t_aHA[ HTML_ATTR_FOR              ], ;
      t_aHA[ HTML_ATTR_INTEGRITY        ], ;
      t_aHA[ HTML_ATTR_LANGUAGE         ], ;
      t_aHA[ HTML_ATTR_ONERROR          ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_XML_SPACE        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_ASYNC            ] }

STATIC FUNCTION THtmlAttr_SELECT()
   RETURN { ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_MULTIPLE         ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_ONBLUR           ], ;
      t_aHA[ HTML_ATTR_ONCHANGE         ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONFOCUS          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_REQUIRED         ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_SIZE             ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_AUTOCOMPLETE     ] }

STATIC FUNCTION THtmlAttr_SMALL()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_SOURCE()
   RETURN { ;
      t_aHA[ HTML_ATTR_MEDIA            ], ;
      t_aHA[ HTML_ATTR_SIZES            ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_SRCSET           ], ;
      t_aHA[ HTML_ATTR_TYPE             ] }

STATIC FUNCTION THtmlAttr_SPAN()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_STRIKE()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ] }

STATIC FUNCTION THtmlAttr_STRONG()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_STYLE()
   RETURN { ;
      t_aHA[ HTML_ATTR_MEDIA            ], ;
      t_aHA[ HTML_ATTR_SCOPED           ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_XML_SPACE        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_SRCSET           ] }

STATIC FUNCTION THtmlAttr_SUB()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_SUP()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_SVG()
   RETURN { ;
   t_aHA[ HTML_ATTR_SVG_BASEPROFILE           ], ;
   t_aHA[ HTML_ATTR_SVG_BY                    ], ;
   t_aHA[ HTML_ATTR_SVG_COLORINTERPOLATION    ], ;
   t_aHA[ HTML_ATTR_SVG_COLORRENDERING        ], ;
   t_aHA[ HTML_ATTR_SVG_CONTENTSCRIPTTYPE     ], ;
   t_aHA[ HTML_ATTR_SVG_CONTENTSTYLETYPE      ], ;
   t_aHA[ HTML_ATTR_SVG_CX                    ], ;
   t_aHA[ HTML_ATTR_SVG_CY                    ], ;
   t_aHA[ HTML_ATTR_SVG_ENTERKEYHINT          ], ;
   t_aHA[ HTML_ATTR_SVG_EXPORTPARTS           ], ;
   t_aHA[ HTML_ATTR_SVG_FILL                  ], ;
   t_aHA[ HTML_ATTR_SVG_FILL_RULE             ], ;
   t_aHA[ HTML_ATTR_SVG_FILL_OPACITY          ], ;
   t_aHA[ HTML_ATTR_SVG_FILTER                ], ;
   t_aHA[ HTML_ATTR_SVG_FLOOD_COLOR           ], ;
   t_aHA[ HTML_ATTR_SVG_FLOOD_OPACITY         ], ;
   t_aHA[ HTML_ATTR_SVG_FONT_SIZE             ], ;
   t_aHA[ HTML_ATTR_SVG_FONT_SIZE_ADJUST      ], ;
   t_aHA[ HTML_ATTR_SVG_FONT_STYLE            ], ;
   t_aHA[ HTML_ATTR_SVG_VISIBILITY            ], ;
   t_aHA[ HTML_ATTR_SVG_FROM                  ], ;
   t_aHA[ HTML_ATTR_SVG_FR                    ], ;
   t_aHA[ HTML_ATTR_SVG_HEIGHT                ], ;
   t_aHA[ HTML_ATTR_SVG_KEYPOINTS             ], ;
   t_aHA[ HTML_ATTR_SVG_KEYTIMES              ], ;
   t_aHA[ HTML_ATTR_SVG_LENGTHADJUST          ], ;
   t_aHA[ HTML_ATTR_SVG_LETTER_SPACING        ], ;
   t_aHA[ HTML_ATTR_SVG_LIGHTING_COLOR        ], ;
   t_aHA[ HTML_ATTR_SVG_MARKERHEIGHT          ], ;
   t_aHA[ HTML_ATTR_SVG_MARKERWIDTH           ], ;
   t_aHA[ HTML_ATTR_SVG_MASK                  ], ;
   t_aHA[ HTML_ATTR_SVG_MEDIA                 ], ;
   t_aHA[ HTML_ATTR_SVG_NUMOCTAVES            ], ;
   t_aHA[ HTML_ATTR_SVG_NONCE                 ], ;
   t_aHA[ HTML_ATTR_SVG_OPACITY               ], ;
   t_aHA[ HTML_ATTR_SVG_OPERATOR              ], ;
   t_aHA[ HTML_ATTR_SVG_ORIENT                ], ;
   t_aHA[ HTML_ATTR_SVG_PART                  ], ;
   t_aHA[ HTML_ATTR_SVG_PATH                  ], ;
   t_aHA[ HTML_ATTR_SVG_PATHLENGTH            ], ;
   t_aHA[ HTML_ATTR_SVG_PATTERNCONTENTUNITS   ], ;
   t_aHA[ HTML_ATTR_SVG_PATTERNTRANSFORM      ], ;
   t_aHA[ HTML_ATTR_SVG_PATTERNUNITS          ], ;
   t_aHA[ HTML_ATTR_SVG_POINTER_EVENTS        ], ;
   t_aHA[ HTML_ATTR_SVG_POINTS                ], ;
   t_aHA[ HTML_ATTR_SVG_POINTSATX             ], ;
   t_aHA[ HTML_ATTR_SVG_POINTSATY             ], ;
   t_aHA[ HTML_ATTR_SVG_POINTSATZ             ], ;
   t_aHA[ HTML_ATTR_SVG_PRESERVEASPECTRATIO   ], ;
   t_aHA[ HTML_ATTR_SVG_R                     ], ;
   t_aHA[ HTML_ATTR_SVG_RADIUS                ], ;
   t_aHA[ HTML_ATTR_SVG_REPEATCOUNT           ], ;
   t_aHA[ HTML_ATTR_SVG_REPEATDUR             ], ;
   t_aHA[ HTML_ATTR_SVG_RESTART               ], ;
   t_aHA[ HTML_ATTR_SVG_ROTATE                ], ;
   t_aHA[ HTML_ATTR_SVG_RX                    ], ;
   t_aHA[ HTML_ATTR_SVG_RY                    ], ;
   t_aHA[ HTML_ATTR_SVG_SCALE                 ], ;
   t_aHA[ HTML_ATTR_SVG_SEED                  ], ;
   t_aHA[ HTML_ATTR_SVG_SHAPE_RENDERING       ], ;
   t_aHA[ HTML_ATTR_SVG_STARTOFFSET           ], ;
   t_aHA[ HTML_ATTR_SVG_STDDEVIATION          ], ;
   t_aHA[ HTML_ATTR_SVG_STITCHTILES           ], ;
   t_aHA[ HTML_ATTR_SVG_STOP_COLOR            ], ;
   t_aHA[ HTML_ATTR_SVG_STOP_OPACITY          ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE                ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE_DASHARRAY      ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE_DASHOFFSET     ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE_LINECAP        ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE_OPACITY        ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE_WIDTH          ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE_LINEJOIN       ], ;
   t_aHA[ HTML_ATTR_SVG_STROKE_MITERLIMIT     ], ;
   t_aHA[ HTML_ATTR_SVG_STYLE                 ], ;
   t_aHA[ HTML_ATTR_SVG_SURFACESCALE          ], ;
   t_aHA[ HTML_ATTR_SVG_SYSTEMLANGUAGE        ], ;
   t_aHA[ HTML_ATTR_SVG_TABINDEX              ], ;
   t_aHA[ HTML_ATTR_SVG_TABLEVALUES           ], ;
   t_aHA[ HTML_ATTR_SVG_TEXT_ANCHOR           ], ;
   t_aHA[ HTML_ATTR_SVG_TEXT_DECORATION       ], ;
   t_aHA[ HTML_ATTR_SVG_TEXT_RENDERING        ], ;
   t_aHA[ HTML_ATTR_SVG_TEXTLENGTH            ], ;
   t_aHA[ HTML_ATTR_SVG_TO                    ], ;
   t_aHA[ HTML_ATTR_SVG_TRANSFORM             ], ;
   t_aHA[ HTML_ATTR_SVG_TYPE                  ], ;
   t_aHA[ HTML_ATTR_SVG_VECTOR_EFFECT         ], ;
   t_aHA[ HTML_ATTR_SVG_VIEWBOX               ], ;
   t_aHA[ HTML_ATTR_SVG_VISIBILITY            ], ;
   t_aHA[ HTML_ATTR_SVG_WIDTH                 ], ;
   t_aHA[ HTML_ATTR_SVG_WORD_SPACING          ], ;
   t_aHA[ HTML_ATTR_SVG_X                     ], ;
   t_aHA[ HTML_ATTR_SVG_X1                    ], ;
   t_aHA[ HTML_ATTR_SVG_X2                    ], ;
   t_aHA[ HTML_ATTR_SVG_XCHANNELSELECTOR      ], ;
   t_aHA[ HTML_ATTR_SVG_XML_LANG              ], ;
   t_aHA[ HTML_ATTR_SVG_XML_BASE              ], ;
   t_aHA[ HTML_ATTR_SVG_Y                     ], ;
   t_aHA[ HTML_ATTR_SVG_Y1                    ], ;
   t_aHA[ HTML_ATTR_SVG_Y2                    ], ;
   t_aHA[ HTML_ATTR_SVG_YCHANNELSELECTOR      ], ;
   t_aHA[ HTML_ATTR_SVG_Z                     ], ;
   t_aHA[ HTML_ATTR_SVG_ZOOMANDPAN            ] }

STATIC FUNCTION THtmlAttr_TABLE()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_BGCOLOR          ], ;
      t_aHA[ HTML_ATTR_BORDER           ], ;
      t_aHA[ HTML_ATTR_CELLPADDING      ], ;
      t_aHA[ HTML_ATTR_CELLSPACING      ], ;
      t_aHA[ HTML_ATTR_FRAME            ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_RULES            ], ;
      t_aHA[ HTML_ATTR_SUMMARY          ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_TBODY()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_TD()
   RETURN { ;
      t_aHA[ HTML_ATTR_ABBR             ], ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_AXIS             ], ;
      t_aHA[ HTML_ATTR_BGCOLOR          ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_COLSPAN          ], ;
      t_aHA[ HTML_ATTR_HEADERS          ], ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_NOWRAP           ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ROWSPAN          ], ;
      t_aHA[ HTML_ATTR_SCOPE            ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_TEXTAREA()
   RETURN { ;
      t_aHA[ HTML_ATTR_COLS             ], ;
      t_aHA[ HTML_ATTR_DIRNAME          ], ;
      t_aHA[ HTML_ATTR_DISABLED         ], ;
      t_aHA[ HTML_ATTR_FORM             ], ;
      t_aHA[ HTML_ATTR_NAME             ], ;
      t_aHA[ HTML_ATTR_ONBLUR           ], ;
      t_aHA[ HTML_ATTR_ONCHANGE         ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONFOCUS          ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ONSELECT         ], ;
      t_aHA[ HTML_ATTR_PATTERN          ], ;
      t_aHA[ HTML_ATTR_PLACEHOLDER      ], ;
      t_aHA[ HTML_ATTR_READONLY         ], ;
      t_aHA[ HTML_ATTR_REQUIRED         ], ;
      t_aHA[ HTML_ATTR_ROWS             ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ], ;
      t_aHA[ HTML_ATTR_AUTOCOMPLETE     ] }

STATIC FUNCTION THtmlAttr_TFOOT()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_TH()
   RETURN { ;
      t_aHA[ HTML_ATTR_ABBR             ], ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_AXIS             ], ;
      t_aHA[ HTML_ATTR_BGCOLOR          ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_COLSPAN          ], ;
      t_aHA[ HTML_ATTR_HEADERS          ], ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_NOWRAP           ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_ROWSPAN          ], ;
      t_aHA[ HTML_ATTR_SCOPE            ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_THEAD()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_TIME()
   RETURN { ;
      t_aHA[ HTML_ATTR_PUBDATE          ] }

STATIC FUNCTION THtmlAttr_TITLE()
   RETURN { ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_TR()
   RETURN { ;
      t_aHA[ HTML_ATTR_ALIGN            ], ;
      t_aHA[ HTML_ATTR_BGCOLOR          ], ;
      t_aHA[ HTML_ATTR_CHAR             ], ;
      t_aHA[ HTML_ATTR_CHAROFF          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_VALIGN           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_TRACK()
   RETURN { ;
      t_aHA[ HTML_ATTR_DEFAULT          ], ;
      t_aHA[ HTML_ATTR_KIND             ], ;
      t_aHA[ HTML_ATTR_LABEL            ], ;
      t_aHA[ HTML_ATTR_ONCUECHANGE      ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_SRCLANG          ] }

STATIC FUNCTION THtmlAttr_TT()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_U()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ] }

STATIC FUNCTION THtmlAttr_UL()
   RETURN { ;
      t_aHA[ HTML_ATTR_COMPACT          ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_TYPE             ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_VAR()
   RETURN { ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDBLCLICK       ], ;
      t_aHA[ HTML_ATTR_ONKEYDOWN        ], ;
      t_aHA[ HTML_ATTR_ONKEYPRESS       ], ;
      t_aHA[ HTML_ATTR_ONKEYUP          ], ;
      t_aHA[ HTML_ATTR_ONMOUSEDOWN      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEMOVE      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOUT       ], ;
      t_aHA[ HTML_ATTR_ONMOUSEOVER      ], ;
      t_aHA[ HTML_ATTR_ONMOUSEUP        ], ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_VIDEO()
   RETURN { ;
      t_aHA[ HTML_ATTR_AUTOPLAY         ], ;
      t_aHA[ HTML_ATTR_CONTROLS         ], ;
      t_aHA[ HTML_ATTR_HEIGHT           ], ;
      t_aHA[ HTML_ATTR_LOOP             ], ;
      t_aHA[ HTML_ATTR_MEDIAGROUP       ], ;
      t_aHA[ HTML_ATTR_MUTED            ], ;
      t_aHA[ HTML_ATTR_POSTER           ], ;
      t_aHA[ HTML_ATTR_PRELOAD          ], ;
      t_aHA[ HTML_ATTR_SRC              ], ;
      t_aHA[ HTML_ATTR_WIDTH            ], ;
      t_aHA[ HTML_ATTR_ONABORT          ], ;
      t_aHA[ HTML_ATTR_ONCANPLAY        ], ;
      t_aHA[ HTML_ATTR_ONCANPLAYTHROUGH ], ;
      t_aHA[ HTML_ATTR_ONCLICK          ], ;
      t_aHA[ HTML_ATTR_ONDURATIONCHANGE ], ;
      t_aHA[ HTML_ATTR_ONEMPTIED        ], ;
      t_aHA[ HTML_ATTR_ONENDED          ], ;
      t_aHA[ HTML_ATTR_ONERROR          ], ;
      t_aHA[ HTML_ATTR_ONLOADEDDATA     ], ;
      t_aHA[ HTML_ATTR_ONLOADEDMETADATA ], ;
      t_aHA[ HTML_ATTR_ONLOADSTART      ], ;
      t_aHA[ HTML_ATTR_ONPAUSE          ], ;
      t_aHA[ HTML_ATTR_ONPLAY           ], ;
      t_aHA[ HTML_ATTR_ONPLAYING        ], ;
      t_aHA[ HTML_ATTR_ONPROGRESS       ], ;
      t_aHA[ HTML_ATTR_ONRATECHANGE     ], ;
      t_aHA[ HTML_ATTR_ONSEEKED         ], ;
      t_aHA[ HTML_ATTR_ONSEEKING        ], ;
      t_aHA[ HTML_ATTR_ONSTALLED        ], ;
      t_aHA[ HTML_ATTR_ONSUSPEND        ], ;
      t_aHA[ HTML_ATTR_ONTIMEUPDATE     ], ;
      t_aHA[ HTML_ATTR_ONVOLUMECHANGE   ], ;
      t_aHA[ HTML_ATTR_ONWAITING        ], ;
      t_aHA[ HTML_ATTR_PLAYSINLINE      ], ;
      t_aHA[ HTML_ATTR_POSTER           ], ;
      t_aHA[ HTML_ATTR_XMLNS            ] }

STATIC FUNCTION THtmlAttr_XMP()
   RETURN { ;
      t_aHA[ HTML_ATTR_SDAFORM          ], ;
      t_aHA[ HTML_ATTR_SDAPREF          ] }

#ifdef HB_LEGACY_LEVEL4

// Converts an HTML formatted text string to the ANSI character set
FUNCTION HtmlToANSI( cHtmlText )

   IF t_aHtmlAnsiEntities == NIL
      _Init_Html_AnsiCharacterEntities()
   ENDIF

   RETURN StrTran( hb_StrReplace( cHtmlText, t_aHtmlAnsiEntities, t_cHtmlAnsiChars ), "&nbsp;", " " )

// Converts an HTML formatted text string to the OEM character set
FUNCTION HtmlToOEM( cHtmlText )
   RETURN hb_ANSIToOEM( HtmlToANSI( cHtmlText ) )

// Inserts HTML character entities into an ANSI text string
FUNCTION ANSIToHtml( cAnsiText )

   LOCAL cHtmlText := ""
   LOCAL parser    := P_PARSER( cAnsiText )
   LOCAL nStart    := 1
   LOCAL cEntity, cText, cChr, nEnd

   IF t_aHtmlAnsiEntities == NIL
      _Init_Html_AnsiCharacterEntities()
   ENDIF

   // Convert to Html but ignore all html character entities
   DO WHILE P_SEEK( parser, "&" ) > 0
      nEnd  := parser:p_pos
      cText := SubStr( parser:p_str, nStart, nEnd - nStart )

      DO WHILE ! ( cChr := P_NEXT( parser ) ) $ "; " .AND. ! Empty( cChr ) .AND. parser:p_pos != 0
      ENDDO

      SWITCH cChr
      CASE ";"  // HTML character entity found
         nStart  := nEnd
         nEnd    := parser:p_pos + 1
         cEntity := SubStr( parser:p_str, nStart, nEnd - nStart )
         parser:p_end := parser:p_pos
         parser:p_pos++
         EXIT
      CASE " "  // "&" character found
         cHtmlText += cText
         nStart    := nEnd
         nEnd      := parser:p_pos + 1
         cText     := SubStr( parser:p_str, nStart, nEnd - nStart )
         nStart    := nEnd
         cHtmlText += "&amp;" + SubStr( cText, 2 )
         LOOP
      OTHERWISE
         cEntity := NIL
      ENDSWITCH

      IF cEntity != NIL
         nStart := parser:p_pos
         cHtmlText += hb_StrReplace( cText, t_cHtmlAnsiChars, t_aHtmlAnsiEntities ) + cEntity
      ENDIF
   ENDDO

   RETURN cHtmlText + hb_StrReplace( SubStr( parser:p_str, nStart ), t_cHtmlAnsiChars, t_aHtmlAnsiEntities )

// Inserts HTML character entities into an OEM text string
FUNCTION OEMToHtml( cOemText )
   RETURN ANSIToHtml( hb_OEMToANSI( cOemText ) )

// This function returns the HTML character entities that are exchangeable between ANSI and OEM character sets
STATIC PROCEDURE _Init_Html_AnsiCharacterEntities()

   t_cHtmlAnsiChars := ;
      hb_BChar(  38 ) + ;
      hb_BChar(  60 ) + ;
      hb_BChar(  62 ) + ;
      hb_BChar( 192 ) + ;
      hb_BChar( 193 ) + ;
      hb_BChar( 194 ) + ;
      hb_BChar( 195 ) + ;
      hb_BChar( 196 ) + ;
      hb_BChar( 197 ) + ;
      hb_BChar( 198 ) + ;
      hb_BChar( 199 ) + ;
      hb_BChar( 200 ) + ;
      hb_BChar( 201 ) + ;
      hb_BChar( 202 ) + ;
      hb_BChar( 203 ) + ;
      hb_BChar( 204 ) + ;
      hb_BChar( 205 ) + ;
      hb_BChar( 206 ) + ;
      hb_BChar( 207 ) + ;
      hb_BChar( 208 ) + ;
      hb_BChar( 209 ) + ;
      hb_BChar( 210 ) + ;
      hb_BChar( 211 ) + ;
      hb_BChar( 212 ) + ;
      hb_BChar( 213 ) + ;
      hb_BChar( 214 ) + ;
      hb_BChar( 216 ) + ;
      hb_BChar( 217 ) + ;
      hb_BChar( 218 ) + ;
      hb_BChar( 219 ) + ;
      hb_BChar( 220 ) + ;
      hb_BChar( 221 ) + ;
      hb_BChar( 222 ) + ;
      hb_BChar( 223 ) + ;
      hb_BChar( 224 ) + ;
      hb_BChar( 225 ) + ;
      hb_BChar( 226 ) + ;
      hb_BChar( 227 ) + ;
      hb_BChar( 228 ) + ;
      hb_BChar( 229 ) + ;
      hb_BChar( 230 ) + ;
      hb_BChar( 231 ) + ;
      hb_BChar( 232 ) + ;
      hb_BChar( 233 ) + ;
      hb_BChar( 234 ) + ;
      hb_BChar( 235 ) + ;
      hb_BChar( 236 ) + ;
      hb_BChar( 237 ) + ;
      hb_BChar( 238 ) + ;
      hb_BChar( 239 ) + ;
      hb_BChar( 240 ) + ;
      hb_BChar( 241 ) + ;
      hb_BChar( 242 ) + ;
      hb_BChar( 243 ) + ;
      hb_BChar( 244 ) + ;
      hb_BChar( 245 ) + ;
      hb_BChar( 246 ) + ;
      hb_BChar( 248 ) + ;
      hb_BChar( 249 ) + ;
      hb_BChar( 250 ) + ;
      hb_BChar( 251 ) + ;
      hb_BChar( 252 ) + ;
      hb_BChar( 253 ) + ;
      hb_BChar( 254 ) + ;
      hb_BChar( 255 ) + ;
      hb_BChar(  94 ) + ;
      hb_BChar( 162 ) + ;
      hb_BChar( 163 ) + ;
      hb_BChar( 165 ) + ;
      hb_BChar( 166 ) + ;
      hb_BChar( 167 ) + ;
      hb_BChar( 169 ) + ;
      hb_BChar( 174 ) + ;
      hb_BChar( 176 ) + ;
      hb_BChar( 191 ) + ;
      hb_BChar( 126 )

   t_aHtmlAnsiEntities := __HtmlEntities()

   RETURN

#endif

// Converts an HTML formatted text string to the current character set
FUNCTION tip_HtmlToStr( cHtmlText )

   _Init_Html_CharacterEntities()

   RETURN StrTran( hb_StrReplace( cHtmlText, t_aHtmlUnicEntities, t_cHtmlUnicChars ), "&nbsp;", " " )

// Inserts HTML character entities into a text string
FUNCTION tip_StrToHtml( cAnsiText )

   LOCAL cHtmlText := ""
   LOCAL parser    := P_PARSER( cAnsiText )
   LOCAL nStart    := 1
   LOCAL cEntity, cText, cChr, nEnd

   _Init_Html_CharacterEntities()

   // Convert to Html but ignore all html character entities
   DO WHILE P_SEEK( parser, "&" ) > 0
      nEnd  := parser:p_pos
      cText := SubStr( parser:p_str, nStart, nEnd - nStart )

      DO WHILE ! ( cChr := P_NEXT( parser ) ) $ "; " .AND. ! Empty( cChr ) .AND. parser:p_pos != 0
      ENDDO

      SWITCH cChr
      CASE ";"  // HTML character entity found
         nStart  := nEnd
         nEnd    := parser:p_pos + 1
         cEntity := SubStr( parser:p_str, nStart, nEnd - nStart )
         parser:p_end := parser:p_pos
         parser:p_pos++
         EXIT
      CASE " "  // "&" character found
         cHtmlText += cText
         nStart    := nEnd
         nEnd      := parser:p_pos + 1
         cText     := SubStr( parser:p_str, nStart, nEnd - nStart )
         nStart    := nEnd
         cHtmlText += "&amp;" + SubStr( cText, 2 )
         LOOP
      OTHERWISE
         cEntity := NIL
      ENDSWITCH

      IF cEntity != NIL
         nStart := parser:p_pos
         cHtmlText += hb_StrReplace( cText, t_cHtmlUnicChars, t_aHtmlUnicEntities ) + cEntity
      ENDIF
   ENDDO

   RETURN cHtmlText + hb_StrReplace( SubStr( parser:p_str, nStart ), t_cHtmlUnicChars, t_aHtmlUnicEntities )

STATIC PROCEDURE _Init_Html_CharacterEntities()

   IF t_aHtmlUnicEntities == NIL .OR. ! t_cHtmlCP == hb_cdpSelect()
      t_cHtmlCP := hb_cdpSelect()
      t_cHtmlUnicChars := hb_UTF8ToStr( "&<>¢£¥¦§©®°¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ^¢£¥¦§©®°¿~" )
      t_aHtmlUnicEntities := __HtmlEntities()
   ENDIF

   RETURN

STATIC FUNCTION __HtmlEntities()
   RETURN { ;
      "&amp;"    , ;  // ampersand
      "&lt;"     , ;  // less-than sign
      "&gt;"     , ;  // greater-than sign
      "&Agrave;" , ;  // Latin capital letter a with grave
      "&Aacute;" , ;  // Latin capital letter a with acute
      "&Acirc;"  , ;  // Latin capital letter a with circumflex
      "&Atilde;" , ;  // Latin capital letter a with tilde
      "&Auml;"   , ;  // Latin capital letter a with diaeresis
      "&Aring;"  , ;  // Latin capital letter a with ring above
      "&AElig;"  , ;  // Latin capital letter ae
      "&Ccedil;" , ;  // Latin capital letter c with cedilla
      "&Egrave;" , ;  // Latin capital letter e with grave
      "&Eacute;" , ;  // Latin capital letter e with acute
      "&Ecirc;"  , ;  // Latin capital letter e with circumflex
      "&Euml;"   , ;  // Latin capital letter e with diaeresis
      "&Igrave;" , ;  // Latin capital letter i with grave
      "&Iacute;" , ;  // Latin capital letter i with acute
      "&Icirc;"  , ;  // Latin capital letter i with circumflex
      "&Iuml;"   , ;  // Latin capital letter i with diaeresis
      "&ETH;"    , ;  // Latin capital letter eth
      "&Ntilde;" , ;  // Latin capital letter n with tilde
      "&Ograve;" , ;  // Latin capital letter o with grave
      "&Oacute;" , ;  // Latin capital letter o with acute
      "&Ocirc;"  , ;  // Latin capital letter o with circumflex
      "&Otilde;" , ;  // Latin capital letter o with tilde
      "&Ouml;"   , ;  // Latin capital letter o with diaeresis
      "&Oslash;" , ;  // Latin capital letter o with stroke
      "&Ugrave;" , ;  // Latin capital letter u with grave
      "&Uacute;" , ;  // Latin capital letter u with acute
      "&Ucirc;"  , ;  // Latin capital letter u with circumflex
      "&Uuml;"   , ;  // Latin capital letter u with diaeresis
      "&Yacute;" , ;  // Latin capital letter y with acute
      "&THORN;"  , ;  // Latin capital letter thorn
      "&szlig;"  , ;  // Latin small letter sharp s (German Eszett)
      "&agrave;" , ;  // Latin small letter a with grave
      "&aacute;" , ;  // Latin small letter a with acute
      "&acirc;"  , ;  // Latin small letter a with circumflex
      "&atilde;" , ;  // Latin small letter a with tilde
      "&auml;"   , ;  // Latin small letter a with diaeresis
      "&aring;"  , ;  // Latin small letter a with ring above
      "&aelig;"  , ;  // Latin lowercase ligature ae
      "&ccedil;" , ;  // Latin small letter c with cedilla
      "&egrave;" , ;  // Latin small letter e with grave
      "&eacute;" , ;  // Latin small letter e with acute
      "&ecirc;"  , ;  // Latin small letter e with circumflex
      "&euml;"   , ;  // Latin small letter e with diaeresis
      "&igrave;" , ;  // Latin small letter i with grave
      "&iacute;" , ;  // Latin small letter i with acute
      "&icirc;"  , ;  // Latin small letter i with circumflex
      "&iuml;"   , ;  // Latin small letter i with diaeresis
      "&eth;"    , ;  // Latin small letter eth
      "&ntilde;" , ;  // Latin small letter n with tilde
      "&ograve;" , ;  // Latin small letter o with grave
      "&oacute;" , ;  // Latin small letter o with acute
      "&ocirc;"  , ;  // Latin small letter o with circumflex
      "&otilde;" , ;  // Latin small letter o with tilde
      "&ouml;"   , ;  // Latin small letter o with diaeresis
      "&oslash;" , ;  // Latin small letter o with stroke
      "&ugrave;" , ;  // Latin small letter u with grave
      "&uacute;" , ;  // Latin small letter u with acute
      "&ucirc;"  , ;  // Latin small letter u with circumflex
      "&uuml;"   , ;  // Latin small letter u with diaeresis
      "&yacute;" , ;  // Latin small letter y with acute
      "&thorn;"  , ;  // Latin small letter thorn
      "&yuml;"   , ;  // Latin small letter y with diaeresis
      "&circ;"   , ;  // modifier letter circumflex accent
      "&cent;"   , ;  // cent sign
      "&pound;"  , ;  // pound sign
      "&yen;"    , ;  // yen sign
      "&brvbar;" , ;  // broken bar
      "&sect;"   , ;  // section sign
      "&copy;"   , ;  // copyright sign
      "&reg;"    , ;  // registered sign
      "&deg;"    , ;  // degree sign
      "&iquest;" , ;  // inverted question mark
      "&tilde;"   }   // small tilde
