/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *     Generates Javascript & DHTML list menus
 *     (see the website/jList dir for an example)
 *
 *     Uses list.js and resize.js (heavily modified) found at
 *     developer.Netscape.com
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "cgi.ch"

CREATE CLASS TJsList

   VAR nH INIT STD_OUT
   VAR aScript INIT {}
   VAR aItems INIT {}
   VAR cScript INIT ""
   VAR nTimes INIT 0
   VAR nItems INIT 0
   VAR cMainNode INIT ""
   VAR cCurrentNode INIT ""
   VAR Style INIT _WHITE_BLACK_STYLE
   VAR FONT INIT "Verdana"
   VAR Size INIT 2
   VAR BGCOLOR INIT "white"
   VAR FontColor INIT "black"

   METHOD New( name, lOpen, width, height, bgColor, ;
      FONT, fntColor, fntSize, cMinusImg, cPlusImg )

   METHOD NewNode( name, lOpen, width, height, bgColor )

   METHOD SetFont( name, font, fntColor, fntSize )

   METHOD AddItem( name, url, bgColor )

   METHOD AddLink( name, url, img, bgColor )

   METHOD EndNode( name, caption )

   METHOD Build( xPos, yPos )

   METHOD Put( cFile )

ENDCLASS

/****
*
*     Create main node
*
*/

METHOD New( name, lOpen, width, height, bgColor, ;
      FONT, fntColor, fntSize, cMinusImg, cPlusImg ) CLASS TJsList

   LOCAL cStr

   __defaultNIL( @name, "l" )
   __defaultNIL( @lOpen, .F. )
   __defaultNIL( @WIDTH, 200 )
   __defaultNIL( @HEIGHT, 22 )
   __defaultNIL( @BGCOLOR, "white" )
   __defaultNIL( @FONT, "Verdana" )
   __defaultNIL( @fntColor, "black" )
   __defaultNIL( @fntSize, 2 )
   __defaultNIL( @cMinusImg, "minus.gif" )
   __defaultNIL( @cPlusImg, "plus.gif" )

   ::font      := FONT
   ::size      := fntSize
   ::fontColor := fntColor
   ::bgColor   := BGCOLOR

   ::nItems  := 0
   ::aSCript := {}

   cStr := "<HTML>" + CRLF() + "<HEAD>" + CRLF() + ;
      "<STYLE>" + ::Style + "</STYLE>" + CRLF() + ;
      '<SCRIPT LANGUAGE="JavaScript1.2" SRC="resize.js"></SCRIPT>' + CRLF() + ;
      CRLF() + ;
      '<SCRIPT LANGUAGE="JavaScript1.2" SRC="list.js"></SCRIPT>' + CRLF() + ;
      CRLF() + ;
      '<SCRIPT LANGUAGE="JavaScript">' + CRLF() + ;
      "<!--" + crlf() + ;
      "var " + name + ";" + CRLF() + CRLF() + ;
      "function listInit() {" + CRLF() + ;
      "var width =" + hb_ntos( width ) + ";" + ;
      "var height=" + hb_ntos( height ) + ";" + CRLF() + ;
      'listSetImages( "' + cMinusImg + '", "' + cPlusImg + '" );' + CRLF() + CRLF()

   ::cMainNode := name

   cStr += ""       // SPACE(10)
   cStr += name + " = new List("
   cStr += iif( lOpen, "true,", "false," )
   cStr += hb_ntos( width ) + ","
   cStr += hb_ntos( height ) + ","
   cStr += '"' + BGCOLOR + '"' + ");" + CRLF()
   cStr += ""       // SPACE(10)
   cStr += name + [.setFont("<FONT FACE='] + FONT + [' SIZE=] + hb_ntos( fntSize ) + [' COLOR='] + fntColor + ['>","</FONT>");] + CRLF()

   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN Self

/****
*
*     Add a new sub-node
*
*/

METHOD NewNode( name, lOpen, width, height, bgColor ) CLASS TJsList

   LOCAL cStr := ""

   __defaultNIL( @lOpen, .F. )
   __defaultNIL( @WIDTH, 200 )
   __defaultNIL( @HEIGHT, 22 )
   __defaultNIL( @BGCOLOR, "white" )
   cStr += ""       // SPACE(10)
   cStr += name + "= new List("
   cStr += iif( lOpen, "true,", "false," )
   cStr += hb_ntos( width ) + ","
   cStr += hb_ntos( height ) + ","
   cStr += '"' + BGCOLOR + '"' + ");" + CRLF()

   ::cCurrentNode := name
   ::nItems++
   AAdd( ::aScript, cStr )

   ::setFont()

   RETURN Self

/****
*
*     Set the font for an item or node
*
*/

METHOD SetFont( name, font, fntColor, fntSize ) CLASS TJsList

   LOCAL cStr := ""

   __defaultNIL( @name, ::cCurrentNode )
   __defaultNIL( @FONT, ::font )
   __defaultNIL( @fntColor, ::fontColor )
   __defaultNIL( @fntSize, ::Size )

   cStr += name + [.setFont("<FONT ] + ;
      [ FACE = '] + font + [' ] + ;
      [ SIZE = ] + hb_ntos( fntSize ) + ['] + ;
      [ COLOR = '] + fntColor + [' ] + ;
      [ > ","</FONT>");] + CRLF()

   AAdd( ::aScript, cStr )

   RETURN self

/****
*
*     Add a menu item
*
*/

METHOD AddItem( name, url, bgColor ) CLASS TJsList

   LOCAL cStr := ""
   LOCAL cUrl

   __defaultNIL( @name, "o" )
   __defaultNIL( @url, "" )
   cUrl := [<A HREF='] + url + ['>] + htmlSpace( 2 ) + name + htmlSpace( 2 )
   cStr += ::cCurrentNode + '.addItem( "' + cUrl + '"' + iif( bgColor != NIL, ',"' + bgColor + '"', "" ) + ');' + CRLF()
   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN self

/****
*
*     Add a menu item
*
*/

METHOD AddLink( name, url, img, bgColor ) CLASS TJsList

   LOCAL cStr := ""
   LOCAL cUrl

   __defaultNIL( @name, "o" )
   __defaultNIL( @url, "" )
   __defaultNIL( @img, "webpage.jpg" )
   cUrl := "<A HREF='" + url + "'><IMG SRC='" + img + "' border=0 align=absmiddle>" + htmlSpace( 2 ) + name + htmlSpace( 2 )
   cStr += ::cCurrentNode + '.addItem( "' + curl + '"' + iif( bgColor != NIL, ',"' + bgColor + '"', "" ) + ');' + CRLF()
   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN self

METHOD EndNode( name, caption ) CLASS TJsList

   LOCAL cStr := ""

   ::cCurrentNode := ::cMainNode
   cStr           += ::cMainNode + ".addList( " + name + ", '<B>" + caption + "</B>' );" + CRLF()

   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN self

METHOD Build( xPos, yPos ) CLASS TJsList

   LOCAL i
   LOCAL cStr := ""

   __defaultNIL( @xPos, 5 )
   __defaultNIL( @yPos, 5 )

   cStr += ::cMainNode + ".build(" + hb_ntos( xPos ) + "," + hb_ntos( yPos ) + ");" + CRLF()
   cStr += "}" + CRLF()
   CsTR += "// -->" + crlf()
   cStr += "</SCRIPT>" + CRLF()
   cStr += '<STYLE TYPE="text/css">' + CRLF()
   cStr += "#spacer { position: absolute; height: 1120; }" + CRLF()
   cStr += "</STYLE>" + CRLF()
   cStr += '<STYLE TYPE="text/css">' + CRLF()
   FOR i := 0 TO ::nItems + 6
      cStr += "#" + ::cMainNode + "Item" + hb_ntos( i ) + " { position:absolute; }" + CRLF()
   NEXT
   cStr += "</STYLE>" + CRLF()

   AAdd( ::aScript, cStr )
   cStr := ""

   cStr += "<TITLE>Collapsable Lists: Basic Example</TITLE>" + CRLF()
   cStr += "</HEAD>" + CRLF()
   cStr += '<BODY ONLOAD="listInit();" BGCOLOR="#FFFFFF">' + CRLF()
   cStr += '<DIV ID="spacer"></DIV>' + CRLF()
#if 0
   cStr += '<DIV ID="' + ::cMainNode + 'Item0" NAME="' + ::cMainNode + 'Item0"></DIV>' + CRLF()
#endif

   FOR i := 0 TO ::nItems
      cStr += '<DIV ID="' + ::cMainNode + 'Item' + hb_ntos( i ) + '" NAME="' + ::cMainNode + 'Item' + hb_ntos( i ) + '"></DIV>' + CRLF()
   NEXT
   cStr += "</BODY></HTML>" + CRLF()

   AAdd( ::aScript, cStr )

   RETURN Self

METHOD Put( cFile ) CLASS TJsList

   IF cFile == NIL
      ::nH := STD_OUT
   ELSE
      ::nH := FCreate( cFile )
   ENDIF

   AEval( ::aScript, {| e | FWrite( ::nH, e ) } )

   FClose( ::nH )

   RETURN Self
