/*
 * Harbour Project source code:
 * JavaScript Window Class
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

CREATE CLASS TJSWindow

   VAR nH
   VAR Name INIT ""
   VAR oHtm
   VAR VarName INIT ""
   VAR URL INIT ""
   VAR Features INIT ""

   VAR ScreenX, ScreenY INIT 100
   VAR HEIGHT, WIDTH INIT 300
   VAR innerHeight, innerWidth, outerHeight INIT 0
   VAR alwaysRaised, alwaysLowered INIT .F.
   VAR Menubar, personalBar INIT .F.
   VAR location, directories, copyHistory INIT .F.
   VAR Toolbar INIT .F.
   VAR Status, TitleBar INIT .T.
   VAR Scrollbars, Resizable, dependent INIT .T.

   VAR TITLE
   VAR aScriptSRC
   VAR aServerSRC
   VAR BGIMAGE, BGCOLOR, fontColor
   VAR Style

   VAR onLoad
   VAR onUnLoad

   METHOD New( cVarName, cUrl, cName, x, y, w, h )
   METHOD setOnLoad( c ) INLINE ::onLoad := c
   METHOD setOnUnLoad( c ) INLINE ::onUnLoad := c
   METHOD Alert( c ) INLINE ::QOut( "Alert('" + c + "')" )
   METHOD confirm( c ) INLINE ::QOut( "confirm('" + c + "')" )
   METHOD SetSize( x, y, h, w )
   METHOD Write( c )
   METHOD lineBreak() INLINE ::QOut( "<br />" )
   METHOD Paragraph() INLINE ::QOut( "<p></p>" )
   METHOD Center( l ) INLINE ::QOut( iif( l, "<center>", "</center>" ) )
   METHOD bold( l ) INLINE ::QOut( iif( l, "<b>", "</b>" ) )
   METHOD Italic( l ) INLINE ::QOut( iif( l, "<i>", "</i>" ) )
   METHOD ULine( l ) INLINE ::QOut( iif( l, "<u>", "</u>" ) )
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

/* Start a new window definition
*/
METHOD New( cVarName, cUrl, cName, x, y, w, h ) CLASS TJSWindow

   ::nH      := HtmlPageHandle()
   ::oHtm    := HtmlPageObject()
   ::varName := hb_defaultValue( cVarName, "newWin" )
   ::URL     := hb_defaultValue( cURL, " " )
   ::Name    := hb_defaultValue( cName, ::varName )

   ::ScreenX := hb_defaultValue( x, 100 )
   ::ScreenY := hb_defaultValue( y, 100 )
   ::height  := hb_defaultValue( h, 300 )
   ::width   := hb_defaultValue( w, 300 )

   RETURN Self

/* Set the properties of the window
*/
METHOD SetFeatures( alwaysRaised, alwaysLowered, ;
      Resizable, Menubar, personalBar, ;
      dependent, location, directories, ;
      Scrollbars, Status, TitleBar, Toolbar, copyHistory ) CLASS TJSWindow

   LOCAL cStr := ""

   IF hb_defaultValue( alwaysRaised, ::alwaysRaised )
      cStr += "alwaysraised=yes,"
   ELSE
      cStr += "alwaysraised=no,"
   ENDIF
   IF hb_defaultValue( alwaysLowered, ::alwaysLowered )
      cStr += "alwayslowered=yes,"
   ELSE
      cStr += "alwayslowered=no,"
   ENDIF
   IF hb_defaultValue( Resizable, ::Resizable )
      cStr += "resizable=yes,"
   ELSE
      cStr += "resizable=no,"
   ENDIF
   IF hb_defaultValue( Menubar, ::Menubar )
      cStr += "menubar=yes,"
   ELSE
      cStr += "menubar=no,"
   ENDIF
   IF hb_defaultValue( personalBar, ::personalBar )
      cStr += "personalbar=yes,"
   ELSE
      cStr += "personalbar=no,"
   ENDIF
   IF hb_defaultValue( dependent, ::dependent )
      cStr += "dependent=yes,"
   ELSE
      cStr += "dependent=no,"
   ENDIF
   IF hb_defaultValue( location, ::location )
      cStr += "location=yes,"
   ELSE
      cStr += "location=no,"
   ENDIF
   IF hb_defaultValue( directories, ::directories )
      cStr += "directories=yes,"
   ELSE
      cStr += "directories=no,"
   ENDIF
   IF hb_defaultValue( Scrollbars, ::Scrollbars )
      cStr += "scrollbars=yes,"
   ELSE
      cStr += "scrollbars=no,"
   ENDIF
   IF hb_defaultValue( Status, ::Status )
      cStr += "status=yes,"
   ELSE
      cStr += "status=no,"
   ENDIF
   IF hb_defaultValue( TitleBar, ::TitleBar )
      cStr += "titlebar=yes,"
   ELSE
      cStr += "titlebar=no,"
   ENDIF
   IF hb_defaultValue( Toolbar, ::Toolbar )
      cStr += "toolbar=yes,"
   ELSE
      cStr += "toolbar=no,"
   ENDIF
   IF hb_defaultValue( copyHistory, ::copyHistory )
      cStr += "copyHistory=yes,"
   ELSE
      cStr += "copyHistory=no,"
   ENDIF

   ::features += iif( Empty( ::Features ), cStr + ",", cStr )

   RETURN Self

/* set the size for the window
*/
METHOD SetSize( x, y, h, w ) CLASS TJSWindow

   LOCAL cStr

   ::ScreenX := hb_defaultValue( x, ::ScreenX )
   ::ScreenY := hb_defaultValue( y, ::ScreenY )
   ::height  := hb_defaultValue( h, ::height )
   ::width   := hb_defaultValue( w, ::width )

   cStr := ;
      "screenX=" + hb_ntos( ::screenX ) + "," + ;
      "screenY=" + hb_ntos( ::screenY ) + "," + ;
      "height=" + hb_ntos( ::height ) + "," + ;
      "width=" + hb_ntos( ::width )

   ::features += iif( Empty( ::Features ), cStr + ",", cStr )

   RETURN Self

/* Open the window from within the current document
*/
METHOD Put() CLASS TJSWindow

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

   hb_default( @::name, "newWin" )

   cStr += ::varName + " = window.open('" + ;
      ::URL + "', '" + ;
      ::varName + "', '" + ;
      ::features + "')"

   HtmlJSCmd( ::nH, cStr )

   RETURN Self

/* Output stand alone Javascript code in the current document
*/
METHOD Write( c ) CLASS TJSWindow

   HtmlJSCmd( ::nH, ::varName + ".document.write('" + c + "')" + CRLF() )

   RETURN Self

/* Output Javascript (or HTML) code in the current document and
*  in the current script
*/
METHOD QOut( c ) CLASS TJSWindow

   FWrite( ::nH, ::varName + ".document.write('" + c + "')" + CRLF() )

   RETURN Self

/* Begin HTML output to the window from within the current document
*  and the current script
*/
METHOD Begin() CLASS TJSWindow

   LOCAL i

   FWrite( ::nH, "<script language=JavaScript 1.2>" + CRLF() )
   FWrite( ::nH, "<!--" + CRLF() )
   ::QOut( "<html><head>" )

   IF HB_ISSTRING( ::Title )
      ::QOut( "<title>" + ::Title + "</title>" )
   ENDIF

   IF HB_ISARRAY( ::aScriptSrc ) .OR. HB_ISHASH( ::aScriptSrc )
      FOR EACH i IN ::aScriptSrc
         ::QOut( '<script language=JavaScript src="' + i + '"></script>' )
      NEXT
   ENDIF

   IF HB_ISARRAY( ::aServerSrc ) .OR. HB_ISHASH( ::aServerSrc )
      FOR EACH i IN ::aServerSrc
         ::QOut( '<script language=JavaScript src="' + i + '" runat=SERVER></script>' )
      NEXT
   ENDIF

   IF HB_ISSTRING( ::Style )
      ::QOut( "<style> " + ::Style + " </style>" )
   ENDIF

   ::QOut( "</head>" + "<body" )

   IF HB_ISSTRING( ::onLoad )
      ::QOut( '   onLoad="' + ::onLoad + '"' )
   ENDIF

   IF HB_ISSTRING( ::onUnLoad )
      ::QOut( ' onUnload="' + ::onUnLoad + '"' )
   ENDIF

   ::QOut( '>' )

   IF HB_ISSTRING( ::bgColor )
      ::QOut( '<body bgcolor="' + ::bgColor + '">' )
   ENDIF

   IF HB_ISSTRING( ::fontColor )
      ::QOut( '<body text="' + ::fontColor + '">' )
   ENDIF

   IF HB_ISSTRING( ::bgImage )
      ::QOut( '<body background="' + ::bgImage + '">' )
   ENDIF

   FWrite( ::nH, "//-->" )
   FWrite( ::nH, "</script>" + crlf() )

   RETURN Self

/* End HTML output to the window
*/
METHOD End() CLASS TJSWindow

   HtmlJSCmd( ::nH, ::varName + ".document.write('</body></html>')" + CRLF() )

   RETURN Self

/* Place an image link to the window
*/
METHOD ImageURL( cImage, cUrl, nHeight, nBorder, ;
      cOnClick, cOnMsover, cOnMsout, ;
      cName, cAlt ) CLASS TJSWindow

   LOCAL cStr := ""

   IF HB_ISSTRING( cName )
      cStr += ' name= "' + cName + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cAlt )
      cStr += ' alt= "' + cAlt + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nBorder )
      cStr += " border= " + hb_ntos( nBorder ) + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nHeight )
      cStr += " height= " + hb_ntos( nHeight ) + "% " + CRLF()
   ENDIF

   IF HB_ISSTRING( cOnClick )
      cStr += ' onClick="' + cOnClick + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += ' onMouseOver="' + cOnMsOver + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += ' onMouseOut="' + cOnMsOut + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( cURL )
      ::QOut( '<a href=' + cUrl + '><img src="' + cImage + '"' + cStr + '></a>' )
   ELSE
      ::QOut( '<img src="' + cImage + '"' + cStr + '></a>' )
   ENDIF

   RETURN Self
