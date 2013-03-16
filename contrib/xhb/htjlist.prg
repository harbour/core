/*
 * Harbour Project source code:
 *     Generates Javascript and DHTML list menus
 *     (see the website/jList dir for an example)
 *
 *     Uses list.js and resize.js (heavily modified) found at
 *     developer.netscape.com
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "cgi.ch"

CREATE CLASS TJSList

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
      FONT, fntColor, fntSize, cMinusImg, cPlusImg ) CLASS TJSList

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

   cStr := "<html>" + CRLF() + "<head>" + CRLF() + ;
      "<style>" + ::Style + "</style>" + CRLF() + ;
      '<script language="JavaScript1.2" src="resize.js"></script>' + CRLF() + ;
      CRLF() + ;
      '<script language="JavaScript1.2" src="list.js"></script>' + CRLF() + ;
      CRLF() + ;
      '<script language="JavaScript">' + CRLF() + ;
      "<!--" + crlf() + ;
      "var " + name + ";" + CRLF() + CRLF() + ;
      "function listInit() {" + CRLF() + ;
      "var width =" + hb_ntos( width ) + ";" + ;
      "var height=" + hb_ntos( height ) + ";" + CRLF() + ;
      'listSetImages( "' + cMinusImg + '", "' + cPlusImg + '" );' + CRLF() + CRLF()

   ::cMainNode := name

   cStr += ""       // Space( 10 )
   cStr += name + " = new List("
   cStr += iif( lOpen, "true,", "false," )
   cStr += hb_ntos( width ) + ","
   cStr += hb_ntos( height ) + ","
   cStr += '"' + BGCOLOR + '"' + ");" + CRLF()
   cStr += ""       // Space( 10 )
   cStr += name + '.SetFont("<font face=' + "'" + FONT + "' size=" + hb_ntos( fntSize ) + "' color='" + fntColor + "'" + '>","</font>");' + CRLF()

   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN Self

/****
*
*     Add a new sub-node
*
*/

METHOD NewNode( name, lOpen, width, height, bgColor ) CLASS TJSList

   LOCAL cStr := ""

   __defaultNIL( @lOpen, .F. )
   __defaultNIL( @WIDTH, 200 )
   __defaultNIL( @HEIGHT, 22 )
   __defaultNIL( @BGCOLOR, "white" )
   cStr += ""       // Space(10)
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

METHOD SetFont( name, font, fntColor, fntSize ) CLASS TJSList

   LOCAL cStr

   __defaultNIL( @name, ::cCurrentNode )
   __defaultNIL( @FONT, ::font )
   __defaultNIL( @fntColor, ::fontColor )
   __defaultNIL( @fntSize, ::Size )

   cStr := name + '.SetFont("<font ' + ;
      " face= '" + font + "' " + ;
      " size= " + hb_ntos( fntSize ) + "'" + ;
      " color= '" + fntColor + "' " + ;
      ' > ","</font>");' + CRLF()

   AAdd( ::aScript, cStr )

   RETURN self

/****
*
*     Add a menu item
*
*/

METHOD AddItem( name, url, bgColor ) CLASS TJSList

   LOCAL cStr
   LOCAL cUrl

   __defaultNIL( @name, "o" )
   __defaultNIL( @url, "" )
   cUrl := "<a href='" + url + "'>" + htmlSpace( 2 ) + name + htmlSpace( 2 )
   cStr := ::cCurrentNode + '.addItem( "' + cUrl + '"' + iif( bgColor != NIL, ',"' + bgColor + '"', "" ) + ');' + CRLF()
   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN self

/****
*
*     Add a menu item
*
*/

METHOD AddLink( name, url, img, bgColor ) CLASS TJSList

   LOCAL cStr
   LOCAL cUrl

   __defaultNIL( @name, "o" )
   __defaultNIL( @url, "" )
   __defaultNIL( @img, "webpage.jpg" )
   cUrl := "<a href='" + url + "'><img src='" + img + "' border=0 align=absmiddle>" + htmlSpace( 2 ) + name + htmlSpace( 2 )
   cStr := ::cCurrentNode + '.addItem( "' + curl + '"' + iif( bgColor != NIL, ',"' + bgColor + '"', "" ) + ');' + CRLF()
   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN self

METHOD EndNode( name, caption ) CLASS TJSList

   LOCAL cStr

   ::cCurrentNode := ::cMainNode
   cStr           := ::cMainNode + ".addList( " + name + ", '<b>" + caption + "</b>' );" + CRLF()

   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN self

METHOD Build( xPos, yPos ) CLASS TJSList

   LOCAL i
   LOCAL cStr := ""

   __defaultNIL( @xPos, 5 )
   __defaultNIL( @yPos, 5 )

   cStr += ::cMainNode + ".build(" + hb_ntos( xPos ) + "," + hb_ntos( yPos ) + ");" + CRLF()
   cStr += "}" + CRLF()
   CsTR += "// -->" + crlf()
   cStr += "</script>" + CRLF()
   cStr += '<style type="text/css">' + CRLF()
   cStr += "#spacer { position: absolute; height: 1120; }" + CRLF()
   cStr += "</style>" + CRLF()
   cStr += '<style type="text/css">' + CRLF()
   FOR i := 0 TO ::nItems + 6
      cStr += "#" + ::cMainNode + "Item" + hb_ntos( i ) + " { position:absolute; }" + CRLF()
   NEXT
   cStr += "</style>" + CRLF()

   AAdd( ::aScript, cStr )
   cStr := ""

   cStr += "<title>Collapsable Lists: Basic Example</title>" + CRLF()
   cStr += "</head>" + CRLF()
   cStr += '<body onload="listInit();" bgcolor="#FFFFFF">' + CRLF()
   cStr += '<div id="spacer"></div>' + CRLF()
#if 0
   cStr += '<div id="' + ::cMainNode + 'Item0" name="' + ::cMainNode + 'Item0"></div>' + CRLF()
#endif

   FOR i := 0 TO ::nItems
      cStr += '<div id="' + ::cMainNode + "Item" + hb_ntos( i ) + '" name="' + ::cMainNode + "Item" + hb_ntos( i ) + '"></div>' + CRLF()
   NEXT
   cStr += "</body></html>" + CRLF()

   AAdd( ::aScript, cStr )

   RETURN Self

METHOD Put( cFile ) CLASS TJSList

   IF cFile == NIL
      ::nH := STD_OUT
   ELSE
      ::nH := FCreate( cFile )
   ENDIF

   AEval( ::aScript, {| e | FWrite( ::nH, e ) } )

   FClose( ::nH )

   RETURN Self
