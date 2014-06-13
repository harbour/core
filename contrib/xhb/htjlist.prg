/*
 * Harbour Project source code:
 *     Generates Javascript and DHTML list menus
 *     (see the website/jList dir for an example)
 *
 *     Uses list.js and resize.js (heavily modified) found at
 *     developer.netscape.com
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (Porting this library to Harbour)
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
#include "cgi.ch"

#include "fileio.ch"

CREATE CLASS TJSList

   VAR nH INIT hb_GetStdOut()
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

/* Create main node */
METHOD New( name, lOpen, width, height, bgColor, ;
      FONT, fntColor, fntSize, cMinusImg, cPlusImg ) CLASS TJSList

   LOCAL cStr

   hb_default( @name, "l" )
   hb_default( @WIDTH, 200 )
   hb_default( @HEIGHT, 22 )
   hb_default( @BGCOLOR, "white" )
   hb_default( @FONT, "Verdana" )
   hb_default( @fntColor, "black" )
   hb_default( @fntSize, 2 )

   ::font      := FONT
   ::size      := fntSize
   ::fontColor := fntColor
   ::bgColor   := BGCOLOR
   ::cMainNode := name

   cStr := "<html>" + hb_eol() + "<head>" + hb_eol() + ;
      "<style>" + ::Style + "</style>" + hb_eol() + ;
      '<script language="JavaScript1.2" src="resize.js"></script>' + hb_eol() + ;
      hb_eol() + ;
      '<script language="JavaScript1.2" src="list.js"></script>' + hb_eol() + ;
      hb_eol() + ;
      '<script language="JavaScript">' + hb_eol() + ;
      "<!--" + crlf() + ;
      "var " + name + ";" + hb_eol() + hb_eol() + ;
      "function listInit() {" + hb_eol() + ;
      "var width =" + hb_ntos( width ) + ";" + ;
      "var height=" + hb_ntos( height ) + ";" + hb_eol() + ;
      'listSetImages( "' + hb_defaultValue( cMinusImg, "minus.gif" ) + '", "' + ;
      hb_defaultValue( cPlusImg, "plus.gif" ) + '" );' + hb_eol() + hb_eol()

   cStr += ""       // Space( 10 )
   cStr += name + " = new List("
   cStr += iif( hb_defaultValue( lOpen, .F. ), "true,", "false," )
   cStr += hb_ntos( width ) + ","
   cStr += hb_ntos( height ) + ","
   cStr += '"' + BGCOLOR + '"' + ");" + hb_eol()
   cStr += ""       // Space( 10 )
   cStr += name + '.SetFont("<font face=' + ;
      "'" + FONT + "' size=" + hb_ntos( fntSize ) + "' color='" + fntColor + "'" + '>","</font>");' + hb_eol()

   ::nItems++
   AAdd( ::aScript, cStr )

   RETURN Self

/* Add a new sub-node */
METHOD NewNode( name, lOpen, width, height, bgColor ) CLASS TJSList

   IF HB_ISSTRING( name )
      ::cCurrentNode := name
   ELSE
      name := "x"
   ENDIF

   ::nItems++

   AAdd( ::aScript, ;
      name + "= new List(" + ;
      iif( hb_defaultValue( lOpen, .F. ), "true,", "false," ) + ;
      hb_ntos( hb_defaultValue( WIDTH, 200 ) ) + "," + ;
      hb_ntos( hb_defaultValue( HEIGHT, 22 ) ) + "," + ;
      '"' + hb_defaultValue( BGCOLOR, "white" ) + '"' + ");" + hb_eol() )

   ::setFont()

   RETURN Self

/* Set the font for an item or node */
METHOD SetFont( name, font, fntColor, fntSize ) CLASS TJSList

   AAdd( ::aScript, ;
      hb_defaultValue( name, ::cCurrentNode ) + '.SetFont("<font ' + ;
      " face='" + hb_defaultValue( FONT, ::font ) + "' " + ;
      " size=" + hb_ntos( hb_defaultValue( fntSize, ::Size ) ) + "'" + ;
      " color='" + hb_defaultValue( fntColor, ::fontColor ) + "' " + ;
      ' > ","</font>");' + hb_eol() )

   RETURN self

/* Add a menu item */
METHOD AddItem( name, url, bgColor ) CLASS TJSList

   LOCAL cUrl := ;
      "<a href='" + hb_defaultValue( url, "" ) + "'>" + ;
      htmlSpace( 2 ) + hb_defaultValue( name, "o" ) + htmlSpace( 2 )

   ::nItems++
   AAdd( ::aScript, ::cCurrentNode + '.addItem( "' + cUrl + '"' + iif( HB_ISSTRING( bgColor ), ',"' + bgColor + '"', "" ) + ');' + hb_eol() )

   RETURN self

/* Add a menu item */
METHOD AddLink( name, url, img, bgColor ) CLASS TJSList

   LOCAL cUrl := ;
      "<a href='" + hb_defaultValue( url, "" ) + "'>" + ;
      "<img src='" + hb_defaultValue( img, "webpage.jpg" ) + "' border=0 align=absmiddle>" + ;
      htmlSpace( 2 ) + hb_defaultValue( name, "o" ) + htmlSpace( 2 )

   ::nItems++
   AAdd( ::aScript, ::cCurrentNode + '.addItem( "' + cUrl + '"' + iif( HB_ISSTRING( bgColor ), ',"' + bgColor + '"', "" ) + ');' + hb_eol() )

   RETURN self

METHOD EndNode( name, caption ) CLASS TJSList

   ::cCurrentNode := ::cMainNode

   AAdd( ::aScript, ::cMainNode + ".addList( " + name + ", '<b>" + caption + "</b>' );" + hb_eol() )

   RETURN self

METHOD Build( xPos, yPos ) CLASS TJSList

   LOCAL i

   LOCAL cStr := ;
      ::cMainNode + ".build(" + hb_ntos( hb_defaultValue( xPos, 5 ) ) + "," + hb_ntos( hb_defaultValue( yPos, 5 ) ) + ");" + hb_eol() + ;
      "}" + hb_eol() + ;
      "// -->" + hb_eol() + ;
      "</script>" + hb_eol() + ;
      '<style type="text/css">' + hb_eol() + ;
      "#spacer { position: absolute; height: 1120; }" + hb_eol() + ;
      "</style>" + hb_eol() + ;
      '<style type="text/css">' + hb_eol()

   FOR i := 0 TO ::nItems + 6
      cStr += "#" + ::cMainNode + "Item" + hb_ntos( i ) + " { position:absolute; }" + hb_eol()
   NEXT
   cStr += "</style>" + hb_eol()

   AAdd( ::aScript, cStr )

   cStr := ;
      "<title>Collapsable Lists: Basic Example</title>" + hb_eol() + ;
      "</head>" + hb_eol() + ;
      '<body onload="listInit();" bgcolor="#FFFFFF">' + hb_eol() + ;
      '<div id="spacer"></div>' + hb_eol()
#if 0
   cStr += '<div id="' + ::cMainNode + 'Item0" name="' + ::cMainNode + 'Item0"></div>' + hb_eol()
#endif

   FOR i := 0 TO ::nItems
      cStr += '<div id="' + ::cMainNode + "Item" + hb_ntos( i ) + '" name="' + ::cMainNode + "Item" + hb_ntos( i ) + '"></div>' + hb_eol()
   NEXT
   cStr += "</body></html>" + hb_eol()

   AAdd( ::aScript, cStr )

   RETURN Self

METHOD Put( cFile ) CLASS TJSList

   IF ( ::nH := iif( HB_ISSTRING( cFile ), FCreate( cFile ), hb_GetStdOut() ) ) != F_ERROR
      AEval( ::aScript, {| e | FWrite( ::nH, e ) } )
      FClose( ::nH )
   ENDIF

   RETURN Self
