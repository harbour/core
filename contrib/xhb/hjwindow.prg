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

   __defaultNIL( @cVarName, "newWin" )
   __defaultNIL( @cURL, " " )
   __defaultNIL( @cName, cVarName )
   __defaultNIL( @x, 100 )
   __defaultNIL( @y, 100 )
   __defaultNIL( @h, 300 )
   __defaultNIL( @w, 300 )

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

/* Set the properties of the window
*/
METHOD SetFeatures( alwaysRaised, alwaysLowered, ;
      Resizable, Menubar, personalBar, ;
      dependent, location, directories, ;
      Scrollbars, Status, TitleBar, Toolbar, copyHistory ) CLASS TJSWindow

   LOCAL cStr := ""

   __defaultNIL( @alwaysRaised, ::alwaysRaised )
   __defaultNIL( @alwaysLowered, ::alwaysLowered )
   __defaultNIL( @Resizable, ::Resizable )
   __defaultNIL( @Menubar, ::Menubar )
   __defaultNIL( @personalBar, ::personalBar )
   __defaultNIL( @dependent, ::dependent )
   __defaultNIL( @location, ::location )
   __defaultNIL( @directories, ::directories )
   __defaultNIL( @Scrollbars, ::Scrollbars )
   __defaultNIL( @Status, ::Status )
   __defaultNIL( @TitleBar, ::TitleBar )
   __defaultNIL( @Toolbar, ::Toolbar )
   __defaultNIL( @copyHistory, ::copyHistory )

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

/* set the size for the window
*/
METHOD SetSize( x, y, h, w ) CLASS TJSWindow

   LOCAL cStr := ""

   __defaultNIL( @x, ::ScreenX )
   __defaultNIL( @y, ::ScreenY )
   __defaultNIL( @h, ::height )
   __defaultNIL( @w, ::width )

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

   IF ::Title != NIL
      ::QOut( "<title>" + ::Title + "</title>" )
   ENDIF

   IF ::aScriptSrc != NIL
      FOR EACH i IN ::aScriptSrc
         ::QOut( '<script language=JavaScript src="' + i + '"></script>' )
      NEXT
   ENDIF

   IF ::aServerSrc != NIL
      FOR EACH i IN ::aServerSrc
         ::QOut( '<script language=JavaScript src="' + i + '" runat=SERVER></script>' )
      NEXT
   ENDIF

   IF ::Style != NIL
      ::QOut( "<style> " + ::Style + " </style>" )
   ENDIF

   ::QOut( "</head>" + "<body" )

   IF ::onLoad != NIL
      ::QOut( '   onLoad="' + ::onLoad + '"' )
   ENDIF

   IF ::onUnLoad != NIL
      ::QOut( ' onUnload="' + ::onUnLoad + '"' )
   ENDIF

   ::QOut( '>' )

   IF ::bgColor != NIL
      ::QOut( '<body bgcolor="' + ::bgColor + '">' )
   ENDIF

   IF ::fontColor != nil
      ::QOut( '<body text="' + ::fontColor + '">' )
   ENDIF

   IF ::bgImage != nil
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

   __defaultNIL( @cUrl, "" )

   IF cName != NIL
      cStr += ' name= "' + cName + '"' + CRLF()
   ENDIF
   IF cAlt != NIL
      cStr += ' alt= "' + cAlt + '"' + CRLF()
   ENDIF

   IF nBorder != NIL
      cStr += " border= " + hb_ntos( nBorder ) + CRLF()
   ENDIF

   IF nHeight != NIL
      cStr += " height= " + hb_ntos( nHeight ) + "% " + CRLF()
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
      ::QOut( '<a href=' + cUrl + '><img src="' + cImage + '"' + ;
         cStr + '></a>' )
   ELSE
      ::QOut( '<img src="' + cImage + '"' + ;
         cStr + '></a>' )
   ENDIF

   RETURN Self
