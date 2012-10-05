/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * JavaScript Window Class
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
#include "common.ch"
#include "cgi.ch"

CLASS TJsWindow

   DATA nH
   DATA Name INIT ""
   DATA oHtm
   DATA VarName INIT ""
   DATA URL INIT ""
   DATA Features INIT ""

   DATA ScreenX, ScreenY INIT 100
   DATA HEIGHT, WIDTH INIT 300
   DATA innerHeight, innerWidth, outerHeight INIT 0
   DATA alwaysRaised, alwaysLowered INIT .F.
   DATA Menubar, personalBar INIT .F.
   DATA location, directories, copyHistory INIT .F.
   DATA Toolbar INIT .F.
   DATA Status, TitleBar INIT .T.
   DATA Scrollbars, Resizable, dependent INIT .T.

   DATA TITLE
   DATA aScriptSRC
   DATA aServerSRC
   DATA BGIMAGE, BGCOLOR, fontColor
   DATA Style

   DATA onLoad
   DATA onUnLoad

   METHOD New( cVarName, cUrl, cName, x, y, w, h )

   METHOD setOnLoad( c ) INLINE ::onLoad := c

   METHOD setOnUnLoad( c ) INLINE ::onUnLoad := c

   METHOD Alert( c ) INLINE ::QOut( "alert('" + c + "')" )

   METHOD confirm( c ) INLINE ::QOut( "confirm('" + c + "')" )

   METHOD SetSize( x, y, h, w )

   METHOD Write( c )

   METHOD lineBreak() INLINE ::QOut( "<BR>" )

   METHOD Paragraph() INLINE ::QOut( "<P></P>" )

   METHOD Center( l ) INLINE ::QOut( iif( l, "<CENTER>", "</CENTER>" ) )

   METHOD bold( l ) INLINE ::QOut( iif( l, "<B>", "</B>" ) )

   METHOD Italic( l ) INLINE ::QOut( iif( l, "<I>", "</I>" ) )

   METHOD ULine( l ) INLINE ::QOut( iif( l, "<U>", "</U>" ) )

   METHOD Put()

   METHOD Begin()

   METHOD End()

   METHOD QOut( c )

   METHOD WriteLN( c ) INLINE ::QOut( c )

   METHOD SetFeatures( alwaysRaised, alwaysLowered, ;
      Resizable, Menubar, personalBar, ;
      dependent, location, directories, ;
      Scrollbars, Status, TitleBar, Toolbar, copyHistory )

   METHOD ImageURL( cImage, cUrl, nHeight, nBorder, ;
      cOnClick, cOnMsover, cOnMsout, ;
      cName, cAlt )

ENDCLASS

/****
*
*     Start a new window definition
*
*/

METHOD New( cVarName, cUrl, cName, x, y, w, h ) CLASS TJsWindow

   DEFAULT cVarName TO "newWin"
   DEFAULT cURL TO " "
   DEFAULT cName TO cVarName
   DEFAULT x TO 100
   DEFAULT y TO 100
   DEFAULT h TO 300
   DEFAULT w TO 300

   ::nH      := HtmlPageHandle()
   ::oHtm    := HtmlPageObject()
   ::varName := cVarName
   ::URL     := cUrl
   ::Name    := cName

   ::ScreenX := x
   ::ScreenY := y
   ::height  := h
   ::width   := w

   RETURN Self

/****
*
*     Set the properties of the window
*
*/

METHOD SetFeatures( alwaysRaised, alwaysLowered, ;
      Resizable, Menubar, personalBar, ;
      dependent, location, directories, ;
      Scrollbars, Status, TitleBar, Toolbar, copyHistory ) CLASS TJsWindow

   LOCAL cStr := ""

   DEFAULT alwaysRaised TO ::alwaysRaised
   DEFAULT alwaysLowered TO ::alwaysLowered
   DEFAULT Resizable TO ::Resizable
   DEFAULT Menubar TO ::Menubar
   DEFAULT personalBar TO ::personalBar
   DEFAULT dependent TO ::dependent
   DEFAULT location TO ::location
   DEFAULT directories TO ::directories
   DEFAULT Scrollbars TO ::Scrollbars
   DEFAULT Status TO ::Status
   DEFAULT TitleBar TO ::TitleBar
   DEFAULT Toolbar TO ::Toolbar
   DEFAULT copyHistory TO ::copyHistory

   IF alwaysRaised
      cStr += "alwaysraised=yes,"
   ELSE
      cStr += "alwaysraised=no,"
   ENDIF
   IF alwaysLowered
      cStr += "alwayslowered=yes,"
   ELSE
      cStr += "alwayslowered=no,"
   ENDIF
   IF Resizable
      cStr += "resizable=yes,"
   ELSE
      cStr += "resizable=no,"
   ENDIF
   IF Menubar
      cStr += "menubar=yes,"
   ELSE
      cStr += "menubar=no,"
   ENDIF
   IF personalBar
      cStr += "personalbar=yes,"
   ELSE
      cStr += "personalbar=no,"
   ENDIF
   IF dependent
      cStr += "dependent=yes,"
   ELSE
      cStr += "dependent=no,"
   ENDIF
   IF location
      cStr += "location=yes,"
   ELSE
      cStr += "location=no,"
   ENDIF
   IF directories
      cStr += "directories=yes,"
   ELSE
      cStr += "directories=no,"
   ENDIF
   IF Scrollbars
      cStr += "scrollbars=yes,"
   ELSE
      cStr += "scrollbars=no,"
   ENDIF
   IF Status
      cStr += "status=yes,"
   ELSE
      cStr += "status=no,"
   ENDIF
   IF TitleBar
      cStr += "titlebar=yes,"
   ELSE
      cStr += "titlebar=no,"
   ENDIF
   IF Toolbar
      cStr += "toolbar=yes,"
   ELSE
      cStr += "toolbar=no,"
   ENDIF
   IF copyHistory
      cStr += "copyHistory=yes,"
   ELSE
      cStr += "copyHistory=no,"
   ENDIF

   ::features += iif( Empty( ::Features ), cStr + ",", cStr )

   RETURN Self

/****
*
*     set the size for the window
*
*/

METHOD SetSize( x, y, h, w ) CLASS TJsWindow

   LOCAL cStr := ""

   DEFAULT x TO ::ScreenX, ;
      y TO ::ScreenY, ;
      h TO ::height, ;
      w TO ::width

   ::ScreenX := x
   ::ScreenY := y
   ::height  := h
   ::width   := w

   cStr := "screenX=" + hb_ntos( ::screenX ) + ","

   cStr += "screenY=" + hb_ntos( ::screenY ) + ","
   cStr += "height=" + hb_ntos( ::height ) + ","
   cStr += "width=" + hb_ntos( ::width )

   ::features += iif( Empty( ::Features ), cStr + ",", cStr )

   RETURN Self

/****
*
*     Open the window from within the current document
*
*/

METHOD Put() CLASS TJsWindow

   LOCAL cStr := ""

   IF ::nH == NIL
      ::nH := HtmlPageHandle()
      IF ::nH == NIL
         RETURN Self
      ENDIF
   ENDIF

   IF Empty( ::features )
      ::setSize()
      ::setFeatures()
   ENDIF

   IF ! HB_ISSTRING( ::name )
      ::name := "newWin"
   ENDIF

   cStr += ::varName + " = window.open('" + ;
      ::URL + "', '" + ;
      ::varName + "', '" + ;
      ::features + "')"

   HtmlJsCmd( ::nH, cStr )

   RETURN Self

/****
*
*     Output stand alone Javascript code in the current document
*
*/

METHOD Write( c ) CLASS TJsWindow

   HtmlJsCmd( ::nH, ::varName + ".document.write('" + c + "')" + CRLF() )

   RETURN Self

/****
*
*     Output Javascript (or HTML) code in the current document and
*     in the current script
*
*/

METHOD QOut( c ) CLASS TJsWindow

   FWrite( ::nH, ::varName + ".document.write('" + c + "')" + CRLF() )

   RETURN Self

/****
*
*     Begin HTML output to the window from within the current document
*     and the current script
*
*
*/

METHOD Begin() CLASS TJsWindow

   LOCAL i

   FWrite( ::nH, "<SCRIPT LANGUAGE=JavaScript 1.2>" + CRLF() )
   FWrite( ::nH, "<!--" + CRLF() )
   ::QOut( "<HTML><HEAD>" )

   IF ::Title != NIL
      ::QOut( "<TITLE>" + ::Title + "</TITLE>" )
   ENDIF

   IF ::aScriptSrc != NIL
      FOR i := 1 TO Len( ::aScriptSrc )
         ::QOut( ;
            '<SCRIPT LANGUAGE=JavaScript SRC="' + ::aScriptSrc[ i ] + '"></SCRIPT>' )
      NEXT
   ENDIF

   IF ::aServerSrc != NIL
      FOR i := 1 TO Len( ::aServerSrc )
         ::QOut( ;
            '<SCRIPT LANGUAGE=JavaScript SRC="' + ::aServerSrc[ i ] + '" RUNAT=SERVER></SCRIPT>' )
      NEXT
   ENDIF

   IF ::Style != NIL
      ::QOut( "<STYLE> " + ::Style + " </STYLE>" )
   ENDIF

   ::QOut( "</HEAD>" + "<BODY" )

   IF ::onLoad != NIL
      ::QOut( '   onLoad="' + ::onLoad + '"' )
   ENDIF

   IF ::onUnLoad != NIL
      ::QOut( ' onUnload="' + ::onUnLoad + '"' )
   ENDIF

   ::QOut( '>' )

   IF ::bgColor != NIL
      ::QOut( '<BODY BGCOLOR="' + ::bgColor + '">' )
   ENDIF

   IF ::fontColor != NIL
      ::QOut( '<BODY TEXT="' + ::fontColor + '">' )
   ENDIF

   IF ::bgImage != NIL
      ::QOut( '<BODY BACKGROUND="' + ::bgImage + '">' )
   ENDIF

   FWrite( ::nH, "//-->" )
   FWrite( ::nH, "</SCRIPT>" + CRLF() )

   RETURN Self

/****
*
*     End HTML output to the window
*
*/

METHOD End() CLASS TJsWindow

   HtmlJsCmd( ::nH, ::varName + ".document.write('</BODY></HTML>')" + CRLF() )

   RETURN Self

/****
*
*     Place an image link to the window
*
*/

METHOD ImageURL( cImage, cUrl, nHeight, nBorder, ;
      cOnClick, cOnMsover, cOnMsout, ;
      cName, cAlt ) CLASS TJsWindow

   LOCAL cStr := ""

   DEFAULT cUrl TO ""

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"' + CRLF()
   ENDIF
   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"' + CRLF()
   ENDIF

   IF nBorder != NIL
      cStr += " BORDER = " + hb_ntos( nBorder ) + CRLF()
   ENDIF

   IF nHeight != NIL
      cStr += " HEIGHT = " + hb_ntos( nHeight ) + "% " + CRLF()
   ENDIF

   IF cOnClick != NIL
      cStr += ' onClick="' + cOnClick + '"' + CRLF()
   ENDIF
   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsOver + '"' + CRLF()
   ENDIF
   IF cOnMsOut != NIL
      cStr += ' onMouseOut="' + cOnMsOut + '"' + CRLF()
   ENDIF

   IF cURL != NIL
      ::QOut( '<A HREF=' + cUrl + '><IMG SRC="' + cImage + '"' + ;
         cStr + '></A>' )
   ELSE
      ::QOut( '<IMG SRC="' + cImage + '"' + ;
         cStr + '></A>' )
   ENDIF

   RETURN Self
