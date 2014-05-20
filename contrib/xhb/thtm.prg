/*
 * Harbour Project source code:
 * Main HTML CLASS for HTMLLIB
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

THREAD STATIC t_nHtm  := NIL
THREAD STATIC t_cForm := 0
THREAD STATIC t_oPage := 0

/****
*
*     Constructors :
*
*     THtml():New()          Creates a new HTML document
*     THtml():CGINew()       Creates a new CGI-HTML document
*
*/

CREATE CLASS THtml

   VAR nH
   VAR FName, TITLE
   VAR FontFace INIT "Verdana"
   VAR FontSize INIT 1
   VAR FontColor INIT "black"
   VAR aImages
   VAR lCgi  INIT .F.
   VAR cStr    INIT ""
   VAR BaseURL, BaseTarget
   VAR lFont INIT .F.
   VAR cEncoding

#if 0
   METHOD New( cFile, cTitle, cLinkTitle, cCharSet, cScriptSRC, ;
           BGIMAGE, BGCOLOR, txtColor, cJavaCode, ;
           onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr, ;
           cStyle, aimages, baseURL, baseTarget, ;
           nRefresh, cRefreshURL, cStyleScr, lnocache )
#endif

   METHOD Newalt( cType )
   METHOD cgiNew( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
      BGIMAGE, BGCOLOR, txtColor, aJsCode, ;
      onLoad, onUnload, ;
      cLinkClr, cVLinkClr, cALinkClr, ;
      cStyle, aImages, aServerSrc, ;
      cBaseURL, cBaseTarget, ;
      nRefresh, cRefreshURL, cStyleScr, ;
      lNocache, NOF, nMarginTop, nMarginHeight, ;
      nMarginWidth, nMarginLeft, lCgi, cFile )
   METHOD New( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
      BGIMAGE, BGCOLOR, txtColor, aJsCode, ;
      onLoad, onUnload, ;
      cLinkClr, cVLinkClr, cALinkClr, ;
      cStyle, aImages, aServerSrc, ;
      cBaseURL, cBaseTarget, ;
      nRefresh, cRefreshURL, cStyleScr, ;
      lNocache, NOF, nMarginTop, nMarginHeight, ;
      nMarginWidth, nMarginLeft, lCgi, cFile )

   METHOD CGIClose()
   METHOD SetPageColor( cColor, lBody ) INLINE hb_default( @lBody, .T. ), ::cStr += iif( lBody, "<body bgcolor=" + '"' + cColor + '"' + ">", " bgcolor=" + '"' + cColor + '"' + " " )
   METHOD SetTextColor( cColor, lBody ) INLINE hb_default( @lBody, .T. ), ::cStr += iif( lBody, "<body text=" + '"' + cColor + '"' + ">", " text=" + '"' + cColor + '"' + " " )
   METHOD SetBgImage( cImage, lBody ) INLINE hb_default( @lBody, .T. ), ::cStr += iif( lBody, "<body background=" + '"' + cImage + '"' + ">", " background=" + '"' + cImage + '"' + " " )
   METHOD Close()
   METHOD SetCenter( lOn ) INLINE ::cStr += iif( lOn, "<center>", "</center>" )
   METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )
   METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut )
   METHOD DefineFont( cFont, cType, nSize, cColor, lSet )
   METHOD EndFont()
   METHOD Say( str, cFont, nSize, type, cColor, cStyle )
   METHOD QQOut( c ) INLINE hb_default( @c, "" ), ::cStr += c
   METHOD QOut( c ) INLINE hb_default( @c, "" ), ::cStr += CRLF() + c + "<br />" + CRLF()
   METHOD Write( c ) INLINE hb_default( @c, "" ), ::cStr += c
   METHOD WriteData( c ) INLINE hb_default( @c, "" ), iif( ::cEncoding == NIL, ::cStr += c, ::cStr += hb_Translate( c,, ::cEncoding ) )
   METHOD WriteLN( c ) INLINE hb_default( @c, "" ), ::cStr += CRLF() + c + "<br />" + CRLF()
   METHOD SayColor( t, c ) INLINE hb_default( @t, "" ), hb_default( @c, "black" ), ;
      ::cStr += "<font color=" + '"' + c + '"' + ">" + t + "</font>"
   METHOD Space( n ) INLINE hb_default( @n, 1 ), ::cStr += Replicate( "&nbsp;", n  )
   METHOD PutImage( cImage, nBorder, nHeight, cOnclick, cOnMsOver, cOnMsOut, ;
      cName, cAlt, cTarget, nWidth, lBreak, ID, MAP, ALING, HSPACE )
   METHOD Text( cText, nCols, lWrap ) INLINE hb_default( @lWrap, .T. ), hb_default( @nCols, 80 ), ;
      ::cStr += "<pre" + iif( HB_ISNUMERIC( nCols ), " cols=" + '"' + hb_ntos( nCols ) + '"', "" ) + iif( lWrap, " WRAP>", ">" ) + CRLF() + cText + CRLF() + "</pre>" + CRLF()
   METHOD MultiCol( txt, cols, gutter, width ) INLINE hb_default( @txt, "" ), ;
      hb_default( @cols, 2 ), ;
      hb_default( @gutter, 5 ), ;
      hb_default( @width, 100 ), ;
      ::cStr += "<multicol cols=" + '"' + hb_ntos( cols ) + '"' + " gutter=" + '"' + hb_ntos( gutter ) + '"' + " width=" + '"' + hb_ntos( width ) + '"' + ">", ;
      ::cStr += txt, ;
      ::cStr += "</multicol>"
   METHOD PutHeading( cText, nWeight, lCentered )
   METHOD HLine( nSize, nWidth, lShade, cColor )
   METHOD PutParagraph() INLINE ::cStr += "<p> </p>" + CRLF()
   METHOD Paragraph( lStart, cAlign, cStyle )
   METHOD PutBreak() INLINE ::cStr += "<br />" + CRLF()
   METHOD Marquee( cText, cFont, cFntColor, nFntSize, cAlign, nWidth, nHeight, cbgColor, ;
      cBehavior, cDirection, nScrollAmt, nScrollDelay, LOOP, ;
      onMsOver, onMsOut, onClick, onStart, onFinish )
   METHOD StartMarquee( cFont, cFntColor, nFntSize, cAlign, nWidth, nHeight, cbgColor, ;
      cBehavior, cDirection, nScrollAmt, nScrollDelay, LOOP, ;
      onMsOver, onMsOut, onClick, onStart, onFinish )
   METHOD EndMarquee()
   METHOD PutTextUrl( cText, cUrl, cOnClick, cOnMsOver, cOnMsout, cTarget, font, clr, size, style, bld, lbreak, cClass )
   METHOD PutImageUrl( cImage, nBorder, nHeight, cUrl, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ;
      Id, hSpace, Aling )
   METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
      l3d, lRuleCols, lRuleRows, cClrDark, cClrLight, cClrBorder, ;
      nCellPadding, nCellSpacing, cAling, lRules, ;
      bgImage, cStyle, Id, NOF )
   METHOD TableHead( cHead, cColor, cAlign, cFont, nSize, cFntColor, nHeight, cBgPic )
   METHOD NewTableRow( cColor, vAling, aLing )
   METHOD EndTableRow()
   METHOD NewTableCell( cAlign, cColor, cFont, nSize, cFntColor, nHeight, ;
      cBgPic, nWidth, lWrap, nColspan, nRowspan, cValign, ;
      clrdrk, clrlt, cBdrClr, cClass, lNoFont )
   METHOD EndTableCell()
   METHOD EndTable()
   METHOD NewList() INLINE ::cStr += "<ul>" + CRLF()
   METHOD ListItem() INLINE ::cStr += "<li> "
   METHOD EndList() INLINE ::cStr += "</ul> "
   METHOD NewForm( cMethod, cAction, cName )
   METHOD FormImage( cText, name, File )
   METHOD FormEdit( cType, cName, xValue, nSize )
   METHOD FormReset( cText )
   METHOD FormSubmit( cText )
   METHOD FormQOut( c ) INLINE ::cStr += c + "<br />" + CRLF()
   METHOD FormQQOut( c ) INLINE ::cStr += c + CRLF()
   METHOD EndForm() INLINE ::cStr += CRLF() + "</form>" + CRLF()
   METHOD PushButton( cName, cCaption, cCgiApp, cOnClick, cOnFocus, cOnBlur, cOnMsOver, cOnMsOut, style, ID )
   METHOD endButton()
   METHOD Button( cName, cCaption, cOnClick, cCgiApp, cOnMsOver, cOnMsOut, style, ID )
   METHOD iFrame( name, src, border, marginwidth, marginheight, scrolling, align, WIDTH, HEIGHT )
   METHOD StartJava() INLINE ::cStr += '<script language="JavaScript">' + CRLF() + "<!--" + CRLF()
   METHOD PutJavaSource( c ) INLINE ::cStr += Space( 5 ) + "src=" + '"' + c + '"' + CRLF()
   METHOD PutJava( c ) INLINE ::cStr += Space( 5 ) + c + CRLF()
   METHOD EndJava() INLINE ::cStr += "                  //-->" + CRLF() + "</script>" + CRLF()
   METHOD serverCode( c ) INLINE ::cStr += "<server>" + Space( 9 ) + c + CRLF() + "</server>" + CRLF()
   METHOD FWrite( c ) INLINE FWrite( ::nH, c )
   METHOD FWriteLN( c ) INLINE FWrite( ::nH, c + CRLF() )
   METHOD Span( c, Style )
   METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, cOnclick, ;
      cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText )
   METHOD Comment( cText )
   METHOD AddObject( cType, cClassid, cAling, cCode, lDisable, cCodeBase, ;
      cName, nWidth, nHeight )
   METHOD AddParam( cType, cValue )
   METHOD EndOBJect()
   METHOD PutLinkName( cName )
   METHOD NewMap( cName ) INLINE ::cStr += "<map name=" + cName + ">"
   METHOD MapArea( Shape, Alt, Coord, Url ) INLINE ;
      ::cStr += "<area shape=" + Shape + " alt=" + alt + " coords=" + Coord + " href=" + Url + ">" + CRLF()
   METHOD EndMap() INLINE ::cStr += "</map>"

ENDCLASS

/* Starts a new CGI-HTML stream file.
*/
METHOD cgiNew( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
      BGIMAGE, BGCOLOR, txtColor, aJsCode, ;
      onLoad, onUnload, ;
      cLinkClr, cVLinkClr, cALinkClr, ;
      cStyle, aImages, aServerSrc, ;
      cBaseURL, cBaseTarget, ;
      nRefresh, cRefreshURL, cStyleScr, ;
      lNocache, NOF, nMarginTop, nMarginHeight, ;
      nMarginWidth, nMarginLeft, lCgi, cFile ) CLASS THtml

   HB_SYMBOL_UNUSED( lCgi )

   RETURN ::new( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
      BGIMAGE, BGCOLOR, txtColor, aJsCode, ;
      onLoad, onUnload, ;
      cLinkClr, cVLinkClr, cALinkClr, ;
      cStyle, aImages, aServerSrc, ;
      cBaseURL, cBaseTarget, ;
      nRefresh, cRefreshURL, cStyleScr, ;
      lNocache, NOF, nMarginTop, nMarginHeight, ;
      nMarginWidth, nMarginLeft, .T., cFile )

METHOD New( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
      BGIMAGE, BGCOLOR, txtColor, aJsCode, ;
      onLoad, onUnload, ;
      cLinkClr, cVLinkClr, cALinkClr, ;
      cStyle, aImages, aServerSrc, ;
      cBaseURL, cBaseTarget, ;
      nRefresh, cRefreshURL, cStyleScr, ;
      lNocache, NOF, nMarginTop, nMarginHeight, ;
      nMarginWidth, nMarginLeft, lCgi, cFile ) CLASS THtml

   LOCAL i

   hb_default( @lCgi, .F. )

   ::lCgi := lCgi
   IF lCgi
      hb_default( @cTitle, "New CGI-HTML page" )
   ELSE
      hb_default( @cFile, "file1.htm" )
      hb_default( @cTitle, "New HTML page" )
   ENDIF

   hb_default( @cLinkTitle, cTitle )
   hb_default( @cRefreshURL, "" )
   hb_default( @cCharset, "UTF-8" )
   hb_default( @lNocache, .F. )

   ::nH    := hb_GetStdOut()
   ::Title := cTitle
   IF lCgi
      ::FName := "cgiout.htm"
   ELSE
      ::FName := cFile
   ENDIF
   IF lCgi
      ::cStr += "Content-Type: text/html" + CRLF() + CRLF()
      FWrite( ::nh, ::cStr )
   ENDIF
   ::cStr := ""

   ::cStr += "<html>" + CRLF() + ;
      "<head>" + CRLF() + ;
      "   <title>" + cTitle + "</title>" + CRLF()

   IF HB_ISSTRING( cBaseURL )
      ::cStr += "<base href=" + '"' + cBaseURL + '"'

      IF HB_ISSTRING( cBaseTarget )
         ::cStr += " target=" + '"' + cBaseTarget + '"'
      ENDIF

      ::cStr += ">" + CRLF()
   ENDIF
#if 0
/* TOFIX: Luiz please review it */
   ::cStr += ;
      "   <link title=" + '"' + cLinkTitle + '"' + CRLF() + ;
      '                href="mailto:mail@example.net">' + CRLF() + ;
      '   <meta http-equiv="Content-Type" content="text/html; charset=' + cCharset + '"' + " />" + CRLF() )
#endif
   IF HB_ISSTRING( cStyleScr )
      ::cStr += "   <link href=" + '"' + cStyleScr + '"' + " rel='STYLESHEET' type='text/css' />" + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nRefresh )
      ::cStr += '   <meta http-equiv="Refresh" content=' + '"' + hb_ntos( nRefresh ) + "; url=" + cRefreshURL + '"' + " />"
   ENDIF

   IF lnocache
      ::cStr += '   <meta http-equiv="pragma" content="no-cache" />'
   ENDIF

   IF HB_ISARRAY( aJsCode )
      AEval( aJsCode, {| e | HtmlJSCmd( ::nH, e ) } )
   ENDIF

   IF HB_ISARRAY( aScriptSrc ) .OR. HB_ISHASH( aScriptSrc )
      FOR EACH i IN aScriptSrc
         ::cStr += "<script language=JavaScript src=" + '"' + i + '"' + " />" + CRLF()
      NEXT
   ENDIF

   IF HB_ISARRAY( aServerSrc ) .OR. HB_ISHASH( aServerSrc )
      FOR EACH i IN aServerSrc
         ::cStr += "<script language=JavaScript src=" + '"' + i + '"' + " runat=SERVER />" + CRLF()
      NEXT
   ENDIF

   // preload images...
   IF HB_ISARRAY( aImages )

      ::aImages := aImages

      ::cStr += ;
         '<script language="JavaScript">' + CRLF() + ;
         "<!--" + CRLF() + ;
         "if(document.images)" + CRLF() + ;
         "{" + CRLF()

      FOR EACH i IN aImages
         ::cStr += Space( 5 ) + i[ 1 ] + "=new Image(100,50);" + CRLF()
         ::cStr += Space( 5 ) + i[ 1 ] + ".src=" + '"' + i[ 2 ] + '"' + ";" + CRLF()
      NEXT

      ::cStr += "}" + CRLF()

      ::cStr += "" + CRLF() + ;
         Space( 5 ) + "// Function to 'activate' images." + CRLF() + ;
         Space( 5 ) + "function imageOn(imgName) {" + CRLF() + ;
         Space( 5 ) + "        if (document.images) {" + CRLF() + ;
         Space( 5 ) + "            imgOn=eval" + '(imgName + "on.src");' + CRLF() + ;
         Space( 5 ) + "            document[imgName].src = imgOn;" + CRLF() + ;
         Space( 5 ) + "        }" + CRLF() + ;
         Space( 5 ) + "}" + CRLF() + ;
         CRLF() + ;
         Space( 5 ) + "// Function to 'deactivate' images." + CRLF() + ;
         Space( 5 ) + "function imageOff(imgName) {" + CRLF() + ;
         Space( 5 ) + "        if (document.images) {" + CRLF() + ;
         Space( 5 ) + "            imgOff=eval" + '(imgName + "off.src");' + CRLF() + ;
         Space( 5 ) + "            document[imgName].src = imgOff;" + CRLF() + ;
         Space( 5 ) + "        }" + CRLF() + ;
         Space( 5 ) + "}" + CRLF() + ;
         CRLF() + ;
         Space( 5 ) + "// Function for 'pressed' images." + CRLF() + ;
         Space( 5 ) + "function imagePress(imgName) {" + CRLF() + ;
         Space( 5 ) + "        if (document.images) {" + CRLF() + ;
         Space( 5 ) + "            imgPress=eval" + '(imgName + "press.src");' + CRLF() + ;
         Space( 5 ) + "            document[imgName].src = imgPress;" + CRLF() + ;
         Space( 5 ) + "        }" + CRLF() + ;
         Space( 5 ) + "}" + CRLF() + ;
         CRLF() + ;
         "//-->" + CRLF() + ;
         "</script>" + CRLF()

   ENDIF

   IF HB_ISSTRING( cStyle )
      ::cStr += "<style> " + cStyle + " </style>" + CRLF()
   ENDIF

   ::cStr += ;
      "</head>" + CRLF() + ;
      "<body"

   IF HB_ISSTRING( onLoad )
      ::cStr += "   onLoad=" + '"' + onLoad + '"'
   ENDIF
   IF HB_ISSTRING( NOF )
      ::cStr += "   nof=" + '"' + nof + '"'
   ENDIF
   IF HB_ISSTRING( onUnLoad )
      ::cStr += " onUnload=" + '"' + onUnLoad + '"'
   ENDIF
   IF HB_ISSTRING( cLinkClr )
      ::cStr += " link=" + '"' + cLinkClr + '"'
   ENDIF
   IF HB_ISSTRING( cVLinkClr )
      ::cStr += " vlnk=" + '"' + cVLinkClr + '"'
   ENDIF
   IF HB_ISSTRING( cALinkClr )
      ::cStr += " alink=" + '"' + cALinkClr + '"'
   ENDIF

   IF HB_ISSTRING( BGIMAGE )
      ::SetBgImage( bgImage, .F. )
   ENDIF
   IF HB_ISSTRING( BGCOLOR )
      ::SetPageColor( bgColor, .F. )
   ENDIF
   IF HB_ISSTRING( txtColor )
      ::SetTextColor( txtColor, .F. )
   ENDIF

   IF HB_ISNUMERIC( nMarginTop )
      ::cStr += " topMargin=" + hb_ntos( nMarginTop )
   ENDIF
   IF HB_ISNUMERIC( nMarginLeft )
      ::cStr += " LeftMargin=" + hb_ntos( nMarginLeft )
   ENDIF
   IF HB_ISNUMERIC( nMarginHeight )
      ::cStr += " marginheight=" + hb_ntos( nMarginHeight )
   ENDIF
   IF HB_ISNUMERIC( nMarginWidth )
      ::cStr += " marginwidth=" + hb_ntos( nMarginWidth )
   ENDIF

   ::cStr += ">" + CRLF()

   t_nHtm := ::nH

   t_oPage := Self

   RETURN self

METHOD NewAlt( cType ) CLASS THtml

   ::nH    := hb_GetStdOut()
   ::cStr += "Content-Type: " + cType + CRLF() + CRLF()

   t_nHtm := ::nH

   t_oPage := Self

   RETURN self


METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := CRLF() + "<font"

   hb_default( @lset, HB_ISSTRING( cFont ) )  /* keep it on top */
   hb_default( @cFont, ::fontFace )
   hb_default( @nSize, ::fontSize )
   hb_default( @cColor, ::fontColor )

   IF HB_ISSTRING( cFont )
      cStr += " face=" + '"' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF
   ENDIF

   IF HB_ISNUMERIC( nSize )
      cStr += " size=" + '"' + hb_ntos( nSize ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF
   ENDIF

   IF HB_ISSTRING( cColor )
      cStr += " color=" + '"' + cColor + '"' + ">"

      IF lset
         ::fontColor := cColor
      ENDIF
   ELSE
      cStr += ">"
   ENDIF

   IF HB_ISLOGICAL( lBold )
      iif( lBold, cStr += "<b>", cStr += "</b>" )
   ENDIF

   IF HB_ISLOGICAL( lItalic )
      iif( lItalic, cStr += "<i>", cStr += "</i>" )
   ENDIF

   IF HB_ISLOGICAL( lULine )
      iif( lULine, cStr += "<u>", cStr += "</u>" )
   ENDIF

   cStr += "</font>"
   ::cStr += cStr + CRLF()

   RETURN Self

/****
*     Begin a font definition. They may be nested but make sure you
*     end the definition appropriately later
*/

METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut ) CLASS THtml

   LOCAL cStr := "<font "

   hb_default( @lSet, .T. )
   hb_default( @lPut, .F. )
   hb_default( @cFont, ::fontFace )
   hb_default( @nSize, ::fontSize )
   hb_default( @cColor, ::fontColor )

   IF HB_ISSTRING( cFont )
      cStr += " face=" + '"' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF
   ENDIF

   IF lPut

      IF HB_ISNUMERIC( nSize )
         cStr += " size=" + '"' + hb_ntos( nSize ) + '"'

         IF lSet
            ::fontSize := nSize
         ENDIF
      ENDIF

      IF HB_ISSTRING( cColor )
         cStr += " color=" + '"' + cColor + '"' + ">"

         IF lSet
            ::fontColor := cColor
         ENDIF
      ELSE
         cStr += ">"
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF HB_ISLOGICAL( lBold )
      iif( lBold, cStr += "<b>", cStr += "</b>" )
   ENDIF

   IF HB_ISLOGICAL( lItalic )
      iif( lItalic, cStr += "<i>", cStr += "</i>" )
   ENDIF

   IF HB_ISLOGICAL( lULine )
      iif( lULine, cStr += "<u>", cStr += "</u>" )
   ENDIF

   ::cStr += cStr + CRLF()

   RETURN Self

/****
*     Begin a font definition by font type "name".
*     Use ::endFont() to cancel this font
*/

METHOD DefineFont( cFont, cType, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := "<font "

   hb_default( @lset, HB_ISSTRING( cFont ) )  /* keep it on top */
   hb_default( @cFont, ::fontFace )
   hb_default( @nSize, ::fontSize )
   hb_default( @cColor, ::fontColor )

   IF HB_ISSTRING( cFont )
      cStr += " face=" + '"' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF
   ENDIF

   IF HB_ISNUMERIC( nSize )
      cStr += " size=" + '"' + hb_ntos( nSize ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF
   ENDIF

   IF HB_ISSTRING( cColor )
      cStr += " color=" + '"' + cColor + '"' + ">"

      IF lset
         ::fontColor := cColor
      ENDIF
   ELSE
      cStr += ">"
   ENDIF

   IF HB_ISSTRING( cType )
      cStr += cType
   ENDIF

   ::cStr += cStr + CRLF()

   RETURN Self

/* End a font definition
*/
METHOD EndFont() CLASS THtml

   ::cStr += "</font>" + CRLF()

   RETURN Self

METHOD Say( str, cFont, nSize, type, cColor, cStyle ) CLASS THtml

   LOCAL cOut    := ""
   LOCAL lBold   := .F.
   LOCAL lItalic := .F.
   LOCAL lULine  := .F.
   LOCAL lEm     := .F.
   LOCAL lStrong := .F.

   hb_default( @str, "" )
   hb_default( @cFONT, ::FontFace )
   hb_default( @nSize, ::FontSize )
   hb_default( @cCOLOR, ::FontColor )

   IF HB_ISSTRING( cFONT ) .OR. HB_ISNUMERIC( nSize ) .OR. HB_ISSTRING( cCOLOR )
      cOut := "<font " + ;
         iif( HB_ISSTRING( cFont ), "face=" + '"' + cFont + '"', "" ) + ;
         iif( HB_ISSTRING( cColor ), " color=" + cColor, "" ) + ;
         iif( HB_ISNUMERIC( nSize ), " size=" + hb_ntos( nSize ), "" )

      IF HB_ISSTRING( cStyle )
         cOut += " style=" + '"' + cStyle + '"' + ">"
      ELSE
         cOut += ">"
      ENDIF
   ENDIF

   IF HB_ISSTRING( type )

      IF "<" $ type

         IF "<b>" $ type
            lBold := .T.
            cOut  += "<b>"
         ENDIF

         IF "<i>" $ type
            lItalic := .T.
            cOut    += "<i>"
         ENDIF

         IF "<u>" $ type
            lULine := .T.
            cOut   += "<u>"
         ENDIF

         IF "<em>" $ type
            lEm  := .T.
            cOut += "<em>"
         ENDIF

         IF "<strong>" $ type
            lStrong := .T.
            cOut    += "<strong>"
         ENDIF
      ENDIF
   ENDIF

   cOut += str

   IF lBold
      cOut += "</b>"
   ENDIF

   IF lItalic
      cOut += "</i>"
   ENDIF

   IF lULine
      cOut += "</u>"
   ENDIF

   IF lStrong
      cOut += "</strong>"
   ENDIF

   IF lEm
      cOut += "</em>"
   ENDIF

   IF HB_ISSTRING( cFONT ) .OR. HB_ISNUMERIC( nSize ) .OR. HB_ISSTRING( cCOLOR )
      cOut += "</font>"
   ENDIF

   ::cStr += cOut + CRLF()

   RETURN Self

METHOD Paragraph( lStart, cAlign, cStyle ) CLASS THtml

   LOCAL cStr := "<p"

   hb_default( @lStart, .T. )
   hb_default( @cAlign, "LEFT" )

   IF lStart
      cStr := "<p align=" + '"' + cAlign + '"'

      IF HB_ISSTRING( cStyle )
         cStr += " style=" + '"' + cStyle + '"'
      ENDIF

      cStr += ">"
   ELSE
      cStr := "</p>"
   ENDIF

   cStr += CRLF()
   ::cStr += cStr

   RETURN Self

/* Put a Horizontal line
*/
METHOD HLine( nSize, nWidth, lShade, cColor ) CLASS THtml

   hb_default( @nSize, 3 )
   hb_default( @nWidth, 100 )
   hb_default( @lShade, .T. )

   IF lShade
      ::cStr += CRLF() + ;
         "<hr size=" + hb_ntos( nSize ) + iif( HB_ISSTRING( cColor ), " COLOR  " + cColor, "" ) + " width=" + hb_ntos( nWidth ) + "%>" + ;
         CRLF()
   ELSE
      ::cStr += CRLF() + ;
         "<hr noshade size=" + hb_ntos( nSize ) + iif( HB_ISSTRING( cColor ), " COLOR  " + cColor, "" ) + " width=" + hb_ntos( nWidth ) + "%>" + ;
         CRLF()
   ENDIF

   RETURN Self

/* Put an HTML heading ( large text )
*/
METHOD PutHeading( cText, nWeight, lCentered ) CLASS THtml

   hb_default( @nWeight, 3 )
   hb_default( @lCentered, .F. )

   IF lCentered
      ::cStr += "<center>"
   ENDIF

   ::cStr += "<h" + hb_ntos( nWeight ) + ">" + cText + "</h" + hb_ntos( nWeight ) + ">" + CRLF()

   IF lCentered
      ::cStr += "</center>"
   ENDIF

   RETURN Self

/* Put a text link.
*/
METHOD PutTextUrl( cText, cUrl, cOnClick, cOnMsOver, cOnMsout, cTarget, font, clr, size, style, bld, lbreak, cClass ) CLASS THtml

   LOCAL cStr := ""

   hb_default( @cUrl, "" )
   hb_default( @bld, .F. )
   hb_default( @lBreak, .F. )

   ::cStr += "<a href=" + '"' + cUrl + '"' + CRLF()

   IF HB_ISSTRING( cOnClick )
      ::cStr += Space( 5 ) + "onClick=" + '"' + cOnClick + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      ::cStr += Space( 5 ) + "onMouseOver=" + '"' + cOnMsOver + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      ::cStr += Space( 5 ) + "onMouseOut=" + '"' + cOnMsOut + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( cTarget )
      ::cStr += Space( 5 ) + "target=" + cTarget + CRLF()
   ENDIF

   IF HB_ISSTRING( cClass )
      ::cStr += Space( 5 ) + "class=" + cClass + CRLF()
   ENDIF

   IF bld
      cStr += "<b>" + CRLF()
   ENDIF

   IF HB_ISSTRING( FONT ) .OR. HB_ISSTRING( clr ) .OR. HB_ISNUMERIC( size ) .OR. HB_ISSTRING( style )
      cStr += " <font " + CRLF()

      IF HB_ISSTRING( FONT )
         cStr += " face=" + '"' + FONT + '"'
      ENDIF
      IF HB_ISSTRING( clr )
         cStr += " color=" + clr
      ENDIF
      IF HB_ISNUMERIC( size )
         cStr += " size=" + hb_ntos( size )
      ENDIF
      IF HB_ISSTRING( style )
         cStr += " style=" + '"' + style + '"'
      ENDIF

      cStr += ">"
   ENDIF

   cStr += cText

   ::cStr += ">" + cStr
   IF HB_ISSTRING( FONT ) .OR. HB_ISSTRING( clr ) .OR. HB_ISNUMERIC( size ) .OR. HB_ISSTRING( style )
      ::cStr += "</font>"
   ENDIF

   IF bld
      ::cStr += "</b>"
   ENDIF

   ::cStr += "</a>" + iif( lBreak, "<br />" + CRLF(), CRLF() )

   RETURN Self

/* Put an Image link.
*/
METHOD PutImageUrl( cImage, nBorder, nHeight, cUrl, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ;
      Id, hSpace, Aling ) CLASS THtml

   LOCAL cStr := ""

   hb_default( @lbreak, .F. )

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cAlt )
      cStr += " alt=" + '"' + cAlt + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nBorder )
      cStr += " border=" + hb_ntos( nBorder ) + CRLF()
   ELSEIF HB_ISSTRING( nBorder )
      cStr += " border=" + nBorder + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " " + CRLF()
   ELSEIF HB_ISSTRING( nHeight )
      cStr += " height=" + nHeight + " " + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " " + CRLF()
   ELSEIF HB_ISSTRING( nWidth )
      cStr += " width=" + nWidth + " " + CRLF()
   ENDIF

   IF HB_ISSTRING( cOnClick )
      cStr += " onClick=" + '"' + cOnClick + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += " onMouseOver=" + '"' + cOnMsOver + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += " onMouseOut=" + '"' + cOnMsOut + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( cTarget )
      cStr += " target=" + cTarget + CRLF()
   ENDIF
   IF HB_ISSTRING( Id )
      cstr += " id=" + Id
   ENDIF
   IF HB_ISSTRING( Aling )
      cStr += " align=" + '"' + Aling + '"'
   ENDIF
   IF HB_ISNUMERIC( hSpace )
      cStr += " hSpace= " + hb_ntos( hSpace ) + " "
   ENDIF

   ::cStr += ;
      "<a href=" + cUrl + iif( HB_ISSTRING( cClass ), " class=" + '"' + cClass + '"', "" ) + "><img src=" + '"' + cImage + '"' + ;
      cStr + "></a>" + iif( lBreak, "<br />" + CRLF(), "" )

   RETURN Self

METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText ) CLASS THtml

   LOCAL cStr := ""

   hb_default( @lbreak, .F. )

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"'
   ENDIF
   IF HB_ISSTRING( cAlt )
      cStr += " alt=" + '"' + cAlt + '"'
   ENDIF
   IF HB_ISNUMERIC( nBorder )
      cStr += " border=" + hb_ntos( nBorder )
   ENDIF

   IF HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " "
   ELSEIF HB_ISSTRING( nHeight )
      cStr += " height=" + nHeight + " "
   ENDIF

   IF HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " "
   ELSEIF HB_ISSTRING( nWidth )
      cStr += " width=" + nWidth + " "
   ENDIF

   IF HB_ISSTRING( cOnClick )
      cStr += " onClick=" + '"' + cOnClick + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += " onMouseOver=" + '"' + cOnMsOver + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += " onMouseOut=" + '"' + cOnMsOut + '"'
   ENDIF
   IF HB_ISSTRING( cTarget )
      cStr += " target=" + cTarget
   ENDIF

   ::cStr += ;
      "<a href=" + cUrl + iif( HB_ISSTRING( cClass ), " class=" + '"' + cClass + '"', "" ) + ">" + cText + "<img src=" + '"' + cImage + '"' + ;
      cStr + "></a>" + iif( lBreak, "<br />" + CRLF(), "" )

   RETURN Self

/* Put an Image.
*/
METHOD PutImage( cImage, nBorder, nHeight, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, ;
      nWidth, lbreak, Id, Map, Aling, hSpace ) CLASS THtml

   LOCAL cStr := ""

   hb_default( @lbreak, .F. )

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"'
   ENDIF
   IF HB_ISSTRING( cAlt )
      cStr += " alt=" + '"' + cAlt + '"'
   ENDIF

   IF HB_ISNUMERIC( nBorder )
      cStr += " border=" + hb_ntos( nBorder )
   ELSEIF HB_ISSTRING( nBorder )
      cStr += " border=" + '"' + nBorder + '"'
   ENDIF

   IF HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " "
   ELSEIF HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDIF

   IF HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " "
   ELSEIF HB_ISSTRING( nWidth )
      cStr += " width=" + nWidth + " "
   ENDIF

   IF HB_ISSTRING( cOnClick )
      cStr += " onClick=" + '"' + cOnClick + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += " onMouseOver=" + '"' + cOnMsOver + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += " onMouseOut=" + '"' + cOnMsOut + '"'
   ENDIF
   IF HB_ISSTRING( Map )
      cStr += " usemap=" + Map
   ENDIF
   IF HB_ISSTRING( cTarget )
      cStr += " target=" + '"' + cTarget + '"'
   ENDIF
   IF HB_ISSTRING( Id )
      cstr += " id=" + Id
   ENDIF
   IF HB_ISSTRING( Aling )
      cStr += " align=" + '"' + Aling + '"'
   ENDIF
   IF HB_ISNUMERIC( hSpace )
      cStr += " hSpace= " + hb_ntos( hSpace ) + " "
   ENDIF

   ::cStr += ;
      "<img src=" + '"' + cImage + '"' + ;
      cStr + ">" + iif( lBreak, "<br />" + CRLF(), "" )

   RETURN Self

/* Close an HTML disk file
*/
METHOD Close() CLASS THtml

#if 0
   ::cStr += ::cStr
#endif
   ::cStr += "</body>" + CRLF()
   ::cStr += "</html>" + CRLF()

   FWrite( ::nh, ::cStr )

   IF ! ::lCgi
      FClose( ::nH )
   ENDIF

   ::cStr := ""

   RETURN Self

/* Close a CGI-HTML stream file
*/
METHOD cgiClose() CLASS THtml

   ::cStr += "</body>" + CRLF()
   ::cStr += "</html>" + CRLF()
   FWrite( ::nh, ::cStr )
   FWrite( ::nH, CRLF() )

   RETURN Self

/* Start an HTML table definition.
*/
METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
      l3d, lRuleCols, lRuleRows, cClrDark, cClrLight, cClrBorder, ;
      nCellPadding, nCellSpacing, cAling, lRules, ;
      bgImage, cStyle, Id, NOF ) CLASS THtml

   LOCAL cStr  := CRLF() + CRLF() + "<table "
   LOCAL xCols := nCols

   hb_default( @l3d, .T. )
   hb_default( @lRuleCols, .F. )
   hb_default( @lRuleRows, .F. )

   IF HB_ISSTRING( colorFore )
      cStr += " bordercolor=" + ColorFore + " "
   ENDIF

   IF HB_ISSTRING( colorbg )
      cStr += " bgcolor=" + ColorBG + " "
   ENDIF

   cStr += iif( HB_ISNUMERIC( nBorder ), "border=" + hb_ntos( nBorder ) + " ", "border " )

   IF HB_ISNUMERIC( ncellpadding )
      cStr += " CellPadding=" + hb_ntos( nCellPadding )
   ENDIF

   IF HB_ISNUMERIC( nCellSpacing )
      cStr += " CellSpacing=" + hb_ntos( nCellSpacing )
   ENDIF

   IF HB_ISSTRING( cAling )
      cStr += " aling=" + '"' + cAling + '"'
   ENDIF

   cStr += iif( HB_ISNUMERIC( xCols ), " cols=" + hb_ntos( nCols ), "" )

   IF HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth )
   ELSEIF HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"'
   ENDIF

   IF HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight )
   ELSEIF HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDIF

   IF l3d
      cStr += " bordercolorlight=#000000 " + ;
         " bordercolordark=#FFFFFF "
   ENDIF

   IF HB_ISSTRING( cClrDark )
      cStr += " bordercolordark=" + cClrDark
   ENDIF

   IF HB_ISSTRING( cClrLight )
      cStr += " bordercolorlight=" + cClrLight
   ENDIF

   IF HB_ISSTRING( cClrBorder )
      cStr += " bordercolor=" + cClrBorder
   ENDIF

   IF lRuleCols
      cStr += " rules=COLS"
   ELSEIF lRuleRows
      cStr += " rules=ROWS"
   ELSEIF HB_ISLOGICAL( lRules ) .AND. lRules
      cStr += " rules=ALL"
   ENDIF

   IF HB_ISSTRING( bgImage )
      cStr += " background=" + '"' + bgImage + '"' + " "
   ENDIF
   IF HB_ISSTRING( cStyle )
      cStr += " style=" + '"' + cStyle + '"' + " "
   ENDIF
   IF HB_ISSTRING( Id )
      cStr += " id=" + Id
   ENDIF
   IF HB_ISSTRING( NOF )
      cStr += " nof=" + '"' + NOF + '"'
   ENDIF

   cStr += ">" + CRLF()

   ::cStr += cStr + CRLF()

   RETURN Self

/* Define a table column Header.
*/
METHOD TableHead( cHead, cColor, cAlign, ;
      cFont, nSize, cFntColor, nHeight, cBgPic ) CLASS THtml

   LOCAL cStr := Space( 3 ) + "<th"

   hb_default( @cFont, ::fontFace )
   hb_default( @nSize, ::fontSize )
   hb_default( @cFntColor, ::fontColor )

   IF HB_ISSTRING( cColor )
      cStr += " bgcolor=" + '"' + cColor + '"'
   ENDIF
   IF HB_ISSTRING( cAlign )
      cStr += " align=" + '"' + cAlign + '"'
   ENDIF
   IF HB_ISNUMERIC( nHeight )
      cStr += " height=" + '"' + hb_ntos( nHeight ) + '"'
   ENDIF
   IF HB_ISSTRING( cBgPic )
      cStr += " background=" + '"' + cBgPic + '"'
   ENDIF

   cStr += ">"

   IF HB_ISSTRING( cFont )
      cStr += "<font face=" + '"' + cFont + '"'

      IF HB_ISNUMERIC( nSize )
         cStr += " size=" + '"' + hb_ntos( nSize ) + '"'
      ENDIF
      IF HB_ISSTRING( cFntColor )
         cStr += " color=" + '"' + cFntColor + '"'
      ENDIF

      cStr += ">"
   ENDIF

   cStr += cHead + iif( HB_ISSTRING( cFont ), "</font>", "" ) + "</th>" + CRLF()

   ::cStr += cStr

   RETURN Self

/* Start a table row definition.
*/
METHOD NewTableRow( cColor, vAling, aLing ) CLASS THtml

   LOCAL cStr := Space( 5 ) + "<tr"

   IF HB_ISSTRING( cColor )
      cStr += " bgcolor=" + cColor
   ENDIF
   IF HB_ISSTRING( vAling )
      cStr += " vAling=" + vAling
   ENDIF
   IF HB_ISSTRING( ALING )
      cStr += " Aling=" + ALING
   ENDIF

   cStr += ">" + CRLF()
   ::cStr += cStr

   RETURN Self

/* End a table row definition.
*/
METHOD EndTableRow() CLASS THtml

   ::cStr += Space( 5 ) + "</tr>" + CRLF()

   RETURN Self

/* Start a table cell definition.
*/
METHOD NewTableCell( cAlign, cColor, ;
      cFont, nSize, cFntColor, nHeight, ;
      cBgPic, nWidth, lWrap, ;
      nColspan, nRowspan, cValign, clrdrk, clrlt, cBdrClr, cClass, lNoFont ) CLASS THtml

   LOCAL cStr := Space( 10 ) + "<td"
   LOCAL cAli := cAlign

   hb_default( @lNoFont, .T. )
   hb_default( @cFont, ::fontFace )
   hb_default( @nSize, ::fontSize )
   hb_default( @cFntColor, ::fontColor )
   hb_default( @cAlign, "LEFT" )
   hb_default( @lWrap, .T. )

   IF HB_ISSTRING( cBdrClr )
      cStr += " bordercolor=" + cBdrClr
   ENDIF
   IF HB_ISSTRING( cColor )
      cStr += " bgcolor=" + cColor
   ENDIF
   IF HB_ISSTRING( cAlign ) .AND. caLi != NIL
      cStr += " align=" + cAlign
   ENDIF
   IF HB_ISSTRING( cValign )
      cStr += " valign=" + cValign
   ENDIF
   IF HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight )
   ELSEIF HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDIF
   IF HB_ISSTRING( cBgPic )
      cStr += " background=" + '"' + cBgPic + '"'
   ENDIF
   IF HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth )
   ELSEIF HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"'
   ENDIF
   IF HB_ISNUMERIC( nColspan )
      cStr += " colspan=" + hb_ntos( nColspan )
   ELSEIF HB_ISSTRING( nColspan )
      cStr += " colspan=" + '"' + nColspan + '"'
   ENDIF
   IF HB_ISSTRING( clrdrk )
      cStr += " borderColorDark=" + clrdrk
   ENDIF
   IF HB_ISSTRING( clrlt )
      cStr += " bordercolorlight=" + clrlt
   ENDIF
   IF HB_ISSTRING( cClass )
      cStr += " Class=" + '"' + cClass + '"' + " "
   ENDIF

   IF HB_ISNUMERIC( nRowspan )
      cStr += " rowspan=" + hb_ntos( nRowspan )
   ELSEIF HB_ISSTRING( nRowspan )
      cStr += " rowspan=" + '"' + nRowspan + '"'
   ENDIF

   IF ! lWrap
      cStr += " nowrap"
   ENDIF

   cStr += ">"

   IF ! lNoFont
      cStr += "<font "

      IF HB_ISNUMERIC( nSize )
         cStr += "size=" + hb_ntos( nSize )
      ENDIF

      IF HB_ISSTRING( cFntColor )
         cStr += " color=" + cFntColor
      ENDIF

      IF ! Empty( cFont )
         cStr += " face=" + '"' + cFont + '"' + ">"
      ELSE
         cStr += ">"
      ENDIF

      ::lFont := .T.
   ENDIF

   ::cStr += cStr

   RETURN Self

/* End a table cell definition.
*/
METHOD EndTableCell() CLASS THtml

   IF ::lFont
      ::cStr += "</font></td>" + CRLF()
   ELSE
      ::cStr += "</td>" + CRLF()
   ENDIF

   ::lFont := .F.

   RETURN Self

/* End a table definition.
*/
METHOD EndTable() CLASS THtml

   ::cStr += ;
      "</table>" + CRLF() + ;
      CRLF() + CRLF() + CRLF()

   RETURN Self

/* Creates a new form
*/
METHOD NewForm( cMethod, cAction, cName ) CLASS THtml

   hb_default( @cMethod, "POST" )
   hb_default( @cName, "newForm" )

   ::cStr += CRLF() + "<form"

   IF HB_ISSTRING( cMethod )
      ::cStr += " method=" + '"' + cMethod + '"'
   ENDIF
   IF HB_ISSTRING( cName )
      ::cStr += " name=" + '"' + cName + '"'
   ENDIF
   IF HB_ISSTRING( cAction )
      ::cStr += " action=" + '"' + cAction + '"'
   ENDIF

   ::cStr += ">" + CRLF()

   t_cForm := cName

   RETURN Self

/* Adds a form edit field
*/
METHOD FormEdit( cType, cName, xValue, nSize ) CLASS THtml

   hb_default( @cType, "edit" )

   ::cStr += "<input type=" + '"' + cType + '"'

   IF HB_ISSTRING( cName )
      ::cStr += " Name=" + '"' + cName + '"'
   ENDIF

   IF xValue != NIL
      ::cStr += " Value=" + '"' + HtmlAny2Str( xValue ) + '"'
   ENDIF

   IF HB_ISNUMERIC( nSize )
      ::cStr += " Size=" + '"' + HtmlAny2Str( nSize ) + '"'
   ENDIF

   ::cStr += ">"

   RETURN Self

/* Adds a form submit button
*/
METHOD FormSubmit( cText ) CLASS THtml

   ::cStr += '<input type="submit" Value=' + '"' + cText + '"' + ">" + CRLF()

   RETURN Self

/* Adds a form image button
*/
METHOD FormImage( cText, name, file ) CLASS THtml

   HB_SYMBOL_UNUSED( cText )

   ::cStr += '<input type="IMAGE" name=' + '"' + name + '"' + ' src=' + '"' + file + '"' + ">" + CRLF()

   RETURN Self

/* Adds a reset button
*/
METHOD FormReset( cText ) CLASS THtml

   ::cStr += '<input type="Reset" Value=' + '"' + cText + '"' + ">" + CRLF()

   RETURN Self

/* Insert a standalone push button and assign an action to it
*  Either pass onClick or cCgiApp - not both
*/
METHOD PushButton( cName, cCaption, ;
      cCgiApp, ;
      cOnClick, ;
      cOnFocus, cOnBlur, ;
      cOnMsOver, cOnMsOut, ;
      style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<input type=BUTTON " + CRLF()

   hb_default( @cOnMsOver, "window.status=this.name;" )
   hb_default( @cOnMsOut, "window.status='';" )

   IF HB_ISSTRING( cName )
      cStr += "        name=" + cName
   ENDIF
   IF HB_ISSTRING( cCaption )
      cStr += "       value=" + cCaption
   ENDIF
   IF HB_ISSTRING( style )
      cStr += "       style=" + '"' + style + '"'
   ENDIF
   IF HB_ISSTRING( ID )
      cStr += "          id=" + '"' + ID + '"'
   ENDIF
   IF HB_ISSTRING( cOnClick )
      cStr += "     onClick=" + '"' + cOnClick + '"'
   ENDIF
   IF HB_ISSTRING( cOnFocus )
      cStr += "     onFocus=" + '"' + cOnFocus + '"'
   ENDIF
   IF HB_ISSTRING( cOnBlur )
      cStr += "      onBlur=" + '"' + cOnBlur + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += " onMouseOver=" + '"' + cOnMsover + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += "  onMouseOut=" + '"' + cOnMsout + '"'
   ENDIF
   IF HB_ISSTRING( cCgiApp )
      cStr += '     onClick="location.href=' + cCgiApp + ';"'
   ENDIF

   ::cStr += cStr + ">"

   RETURN Self

/* Insert a standalone <button> push button and assign an action to it
*/
METHOD Button( cName, cCaption, ;
      cOnClick, ;
      cCGIApp, ;
      cOnMsOver, cOnMsOut, ;
      Style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<button " + CRLF()

   hb_default( @cOnMsOver, "window.status=this.name;" )
   hb_default( @cOnMsOut, "window.status='';" )

   IF HB_ISSTRING( cName )
      cStr += "        name=" + cName
   ENDIF
   IF HB_ISSTRING( cCaption )
      cStr += "       title=" + cCaption
   ENDIF
   IF HB_ISSTRING( style )
      cStr += "       style=" + '"' + style + '"'
   ENDIF
   IF HB_ISSTRING( ID )
      cStr += "          id=" + '"' + ID + '"'
   ENDIF
   IF HB_ISSTRING( cOnClick )
      cStr += "     onClick=" + '"' + cOnClick + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += " onMouseOver=" + '"' + cOnMsover + '"'
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += "  onMouseOut=" + '"' + cOnMsout + '"'
   ENDIF
   IF HB_ISSTRING( cCgiApp )
      cStr += '     onClick="location.href=' + cCgiApp + ';"'
   ENDIF

   ::cStr += cStr + ">" + CRLF()

   RETURN Self

/* End a <button> definition
*/
METHOD EndButton() CLASS THtml

   ::cStr += CRLF() + CRLF() + "</button>" + CRLF()

   RETURN Self

/* Display a scrolling marquee effect
*/
METHOD Marquee( cText, cFont, cFntColor, nFntSize, ;
      cAlign, nWidth, nHeight, cbgColor, ;
      cBehavior, cDirection, ;
      nScrollAmt, nScrollDelay, LOOP, ;
      onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS THtml

   hb_default( @cFont, "Verdana" )
   hb_default( @cFntColor, "white" )
   hb_default( @nFntSize, 3 )
   hb_default( @cAlign, "middle" )
   hb_default( @nWidth, 100 )
   hb_default( @cText, "" )
   hb_default( @cBgColor, "black" )
   hb_default( @cBehavior, "scroll" )
   hb_default( @cDirection, "left" )
   hb_default( @nScrollAmt, 5 )
   hb_default( @nScrolldelay, 2 )
   hb_default( @LOOP, 0 )

   ::StartFont( cFont, , , , nFntSize, cFntColor )

   ::cStr += ;
      "<marquee align=" + '"' + cAlign + '"' + " " + ;
      "behavior=" + '"' + cBehavior + '"' + " " + ;
      "width=" + '"' + hb_ntos( nWidth ) + "%" + '"' + " " + ;
      iif( HB_ISNUMERIC( nHeight ), "height=" + hb_ntos( nHeight ) + " ", "" ) + ;
      "bgColor=" + '"' + cBgColor + '"' + " " + ;
      "scrollamount=" + '"' + hb_ntos( nScrollAmt ) + '"' + " " + ;
      "scrolldelay=" + '"' + hb_ntos( nScrollDelay ) + '"' + " " + ;
      "loop=" + iif( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + " " + ;
      "direction=" + '"' + cDirection + '"' + " " + ;
      iif( HB_ISSTRING( onMsOver ), "onMouseOver=" + '"' + onMsOver + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onMsOut ), "onMouseOut=" + '"' + onMsOut + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onClick ), "onClick=" + '"' + onClick + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onStart ), "onStart=" + '"' + onStart + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onFinish ), "onFinish=" + '"' + onFinish + '"' + " ", "" ) + ;
      ">" + ;
      cText + ;
      "</marquee>" + CRLF()

   ::EndFont()

   RETURN Self

/* Start a scrolling marquee effect definition
*/
METHOD StartMarquee( cFont, cFntColor, nFntSize, ;
      cAlign, nWidth, nHeight, cbgColor, ;
      cBehavior, cDirection, ;
      nScrollAmt, nScrollDelay, LOOP, ;
      onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS THtml

   LOCAL cStr := ""

   hb_default( @cFont, "Verdana" )
   hb_default( @cFntColor, "white" )
   hb_default( @nFntSize, 3 )
   hb_default( @cAlign, "middle" )
   hb_default( @nWidth, 100 )
   hb_default( @cBgColor, "black" )
   hb_default( @cBehavior, "scroll" )
   hb_default( @cDirection, "left" )
   hb_default( @nScrollAmt, 5 )
   hb_default( @nScrolldelay, 2 )

   ::StartFont( cFont, , , , nFntSize, cFntColor )

   cStr += "<marquee align=" + '"' + cAlign + '"' + " " + ;
      "behavior=" + '"' + cBehavior + '"' + " " + ;
      "width=" + '"' + hb_ntos( nWidth ) + "%" + '"' + " " + ;
      iif( HB_ISNUMERIC( nHeight ), "height=" + hb_ntos( nHeight ) + " ", "" ) + ;
      "bgColor=" + '"' + cBgColor + '"' + " " + ;
      "scrollamount=" + '"' + hb_ntos( nScrollAmt ) + '"' + " " + ;
      "scrolldelay=" + '"' + hb_ntos( nScrollDelay ) + '"' + " " + ;
      "loop=" + iif( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + " " + ;
      "direction=" + '"' + cDirection + '"' + " " + ;
      iif( HB_ISSTRING( onMsOver ), "onMouseOver=" + '"' + onMsOver + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onMsOut ), "onMouseOut=" + '"' + onMsOut + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onClick ), "onClick=" + '"' + onClick + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onStart ), "onStart=" + '"' + onStart + '"' + " ", "" ) + ;
      iif( HB_ISSTRING( onFinish ), "onFinish=" + '"' + onFinish + '"' + " ", "" ) + ;
      ">" + ;
      CRLF()

   ::cStr += cStr
   ::EndFont()

   RETURN Self

METHOD EndMarquee() CLASS THtml

   ::cStr += "</marquee>" + CRLF()

   RETURN Self

/* Define an inline frame.
*/
METHOD iFrame( name, src, border, marginwidth, marginheight, ;
      scrolling, align, WIDTH, HEIGHT ) CLASS THtml

   LOCAL cStr := "<iframe " + CRLF()

   hb_default( @BORDER, .T. )
   hb_default( @name, "Frame01" )
#if 0
   hb_default( @align, "vertical" )
#endif

   IF HB_ISSTRING( name )
      cStr += Space( 5 ) + "        name=" + '"' + name + '"' + CRLF()
   ENDIF
   IF HB_ISSTRING( src )
      cStr += Space( 5 ) + "         src=" + '"' + src + '"' + CRLF()
   ENDIF

   IF BORDER
      cStr += Space( 5 ) + " frameborder=1" + CRLF()
   ELSE
      cStr += Space( 5 ) + " frameborder=0" + CRLF()
   ENDIF

   cStr += Space( 5 ) + "   scrolling=" + '"' + iif( scrolling, "yes", "no" ) + '"' + CRLF()

   IF HB_ISNUMERIC( marginwidth )
      cStr += Space( 5 ) + " marginwidth=" + '"' + hb_ntos( marginWidth ) + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( marginheight )
      cStr += Space( 5 ) + "marginheight=" + '"' + hb_ntos( marginheight ) + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( WIDTH )
      cStr += Space( 5 ) + "       width=" + '"' + hb_ntos( Width ) + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( HEIGHT )
      cStr += Space( 5 ) + "      height=" + '"' + hb_ntos( height ) + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( align )
      cStr += Space( 5 ) + "       align=" + '"' + align + '"' + CRLF()
   ENDIF

   cStr += ">" + CRLF()
   cStr += "</iframe>" + CRLF()

   ::cStr += cStr

   RETURN Self

METHOD Span( c, Style ) CLASS THtml

   LOCAL cStr := "<span "

   IF HB_ISSTRING( style )
      cStr += " style=" + '"' + Style + '"'
   ENDIF
   cStr += ">" + c + "</span>"
   ::cStr += cStr

   RETURN Self

METHOD Comment( cText ) CLASS THtml

   LOCAL cStr := CRLF() + "<!-- "

   cStr += cText + " -->"
   ::cStr += cStr

   RETURN Self

METHOD AddObject( cType, cClassid, cAling, cCode, lDisable, cCodeBase, cName, nWidth, nHeight ) CLASS THtml

   LOCAL cStr := "<object "

   IF HB_ISSTRING( cType )
      cStr += " type=" + '"' + cType + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( cClassId )
      cStr += " classid=" + '"' + cClassId + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( cAling )
      cStr += " aling=" + '"' + cAling + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( cCode )
      cStr += " code=" + '"' + cCode + '"' + CRLF()
   ENDIF

   IF lDisable
      cStr += " DISABLED " + CRLF()
   ENDIF

   IF HB_ISSTRING( cCodebase )
      cStr += " codebase=" + '"' + cCodebase + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " " + CRLF()
   ELSEIF HB_ISSTRING( nHeight )
      cStr += " height=" + nHeight + " " + CRLF()
   ENDIF

   IF HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " " + CRLF()
   ELSEIF HB_ISSTRING( nWidth )
      cStr += " width=" + nWidth + " " + CRLF()
   ENDIF

   cStr += " >"
   ::cStr += cStr + CRLF()

   RETURN Self

METHOD EndObject() CLASS THtml

   ::cStr += "</object>" + CRLF()

   RETURN Self

METHOD ADDPARAM( cType, cValue ) CLASS THtml

   ::cStr += "<param name=" + '"' + cType + '"' + " value=" + '"' + cValue + '"' + ">" + CRLF()

   RETURN Self

METHOD PutLinkName( cName ) CLASS THtml

   ::cStr += "<a name=" + '"' + cName + '"' + "></a>"

   RETURN Self

/* Returns the current HTML page handle
*/
FUNCTION HtmlPageHandle()
   RETURN t_nHtm

/* Returns the current ( or last ) form name
*/
FUNCTION HtmlFormName()
   RETURN t_cForm

/* Return the current THtml() object.
*/
FUNCTION HtmlPageObject()
   RETURN t_oPage

/* Decodes a URL encoded string. Also handles international charsets.
*/
FUNCTION HtmlDecodeUrl( cString )
   RETURN hb_StrReplace( cString, { ;
      "%26" => "&", ;
      "%2B" => "+", ;
      "%20" => " ", ;
      "%27" => "'", ;
      "+"   => " ", ;
      "%2C" => ",", ;
      "%21" => "!", ;
      "%7E" => "~", ;
      "%23" => "#", ;
      "%24" => "!", ;
      "%25" => "%", ;
      "%5E" => "^", ;
      "%28" => "(", ;
      "%29" => ")", ;
      "%60" => "`", ;
      "%2F" => "/" } )

/* Inserts inline Javascript source
*/
PROCEDURE HtmlJSCmd( nH, cCmd )

   FWrite( hb_defaultValue( nH, HtmlPageHandle() ), ;
      "<script language=JavaScript 1.2>" + CRLF() + "<!--" + CRLF() + ;
      hb_defaultValue( cCmd, "" ) + CRLF() + ;
      "//-->" + CRLF() + "</script>" + CRLF() )

   RETURN

/* HtmlLinkStyle()
*/
FUNCTION HtmlLinkStyle( cHoverStyle, cHoverClr, cHoverBG, cLinkStyle, cLinkClr, cLinkBG )
   RETURN ;
      "<!-- A:hover {text-decoration:" + hb_defaultValue( cHoverStyle, "normal" ) + ;
      ";color:" + hb_defaultValue( cHoverClr, "white" ) + ;
      ";background:" + hb_defaultValue( cHoverBg, "black" ) + ;
      ";} A:link {text-decoration:" + hb_defaultValue( cLinkStyle, "normal" ) + ;
      ";color:" + hb_defaultValue( cLinkClr, "black" ) + ;
      ";background:" + hb_defaultValue( cLinkBg, "white" ) + ";}-->"

FUNCTION HtmlPadL( cStr, n )

   LOCAL cRet
   LOCAL nSpaces

   IF ! HB_ISNUMERIC( n )
      RETURN cStr
   ENDIF

   nSpaces := n - Len( cStr )

   IF n <= 0
      cRet := Right( cStr, n )
   ELSE
      cRet := Replicate( _HTML_SPACE, nSpaces ) + cStr
   ENDIF

   RETURN cRet

FUNCTION HtmlPadR( cStr, n )

   LOCAL cRet
   LOCAL nSpaces

   IF ! HB_ISNUMERIC( n )
      RETURN cStr
   ENDIF

   nSpaces := n - Len( cStr )

   IF n <= 0
      cRet := Left( cStr, n )
   ELSE
      cRet := cStr + Replicate( _HTML_SPACE, nSpaces )
   ENDIF

   RETURN cRet

FUNCTION Any2Str( xVal )
   RETURN HtmlAny2Str( xVal )

FUNCTION HtmlAny2Str( xVal )

   SWITCH ValType( xVal )
   CASE "M"
   CASE "C" ; RETURN iif( Empty( xVal ), ".", xVal )
   CASE "N" ; RETURN hb_ntos( xVal )
   CASE "O" ; RETURN "<" + xVal:CLASSNAME() + ">"
   CASE "D" ; RETURN DToC( xVal )
   CASE "L" ; RETURN iif( xVal, "T", "F" )
   CASE "B" ; RETURN "{||...}"
   CASE "U" ; RETURN "NIL"
   ENDSWITCH

   RETURN "<Unknown Value>"
