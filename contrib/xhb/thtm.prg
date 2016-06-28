/*
 * Main HTML CLASS for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (Porting this library to Harbour)
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

THREAD STATIC t_nHtm
THREAD STATIC t_cForm := 0
THREAD STATIC t_oPage := 0

#translate Q_( <x>, <y>, <z> ) => ( ( <x> + '"' ) + <y> + ( '"' + <z> ) )

/* Constructors:

   THtml():New()          Creates a new HTML document
   THtml():CGINew()       Creates a new CGI-HTML document
 */

CREATE CLASS THtml

   VAR nH
   VAR FName, TITLE
   VAR FontFace  INIT "Verdana"
   VAR FontSize  INIT 1
   VAR FontColor INIT "black"
   VAR aImages
   VAR lCgi      INIT .F.
   VAR cStr      INIT ""
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
   METHOD SetPageColor( cColor, lBody ) INLINE ::cStr += iif( hb_defaultValue( lBody, .T. ), Q_( "<body bgcolor=", cColor, ">" ), Q_( " bgcolor=", cColor, " " ) )
   METHOD SetTextColor( cColor, lBody ) INLINE ::cStr += iif( hb_defaultValue( lBody, .T. ), Q_( "<body text=", cColor, ">" ), Q_( " text=", cColor, " " ) )
   METHOD SetBgImage( cImage, lBody ) INLINE ::cStr += iif( hb_defaultValue( lBody, .T. ), Q_( "<body background=", cImage, ">" ), Q_( " background=", cImage, " " ) )
   METHOD Close()
   METHOD SetCenter( lOn ) INLINE ::cStr += iif( lOn, "<center>", "</center>" )
   METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )
   METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut )
   METHOD DefineFont( cFont, cType, nSize, cColor, lSet )
   METHOD EndFont()
   METHOD Say( str, cFont, nSize, type, cColor, cStyle )
   METHOD QQOut( c ) INLINE ::cStr += hb_defaultValue( c, "" )
   METHOD QOut( c ) INLINE ::cStr += hb_eol() + hb_defaultValue( c, "" ) + "<br />" + hb_eol()
   METHOD Write( c ) INLINE ::cStr += hb_defaultValue( c, "" )
   METHOD WriteData( c ) INLINE iif( ::cEncoding == NIL, ::cStr += hb_defaultValue( c, "" ), ::cStr += hb_Translate( hb_defaultValue( c, "" ),, ::cEncoding ) )
   METHOD WriteLN( c ) INLINE ::cStr += hb_eol() + hb_defaultValue( c, "" ) + "<br />" + hb_eol()
   METHOD SayColor( t, c ) INLINE ::cStr += Q_( "<font color=", hb_defaultValue( c, "black" ), ">" ) + hb_defaultValue( t, "" ) + "</font>"
   METHOD Space( n ) INLINE ::cStr += Replicate( "&nbsp;", hb_defaultValue( n, 1 ) )
   METHOD PutImage( cImage, nBorder, nHeight, cOnclick, cOnMsOver, cOnMsOut, ;
      cName, cAlt, cTarget, nWidth, lBreak, ID, MAP, ALIGN, HSPACE )
   METHOD Text( cText, nCols, lWrap ) INLINE ::cStr += "<pre" + " cols=" + '"' + hb_ntos( hb_defaultValue( nCols, 80 ) ) + '"' + iif( hb_defaultValue( lWrap, .T. ), " WRAP>", ">" ) + hb_eol() + cText + hb_eol() + "</pre>" + hb_eol()
   METHOD MultiCol( txt, cols, gutter, width ) INLINE ;
      ::cStr += "<multicol cols=" + '"' + hb_ntos( hb_defaultValue( cols, 2 ) ) + '"' + " gutter=" + '"' + hb_ntos( hb_defaultValue( gutter, 5 ) ) + '"' + " width=" + '"' + hb_ntos( hb_defaultValue( width, 100 ) ) + '"' + ">", ;
      ::cStr += hb_defaultValue( txt, "" ), ;
      ::cStr += "</multicol>"
   METHOD PutHeading( cText, nWeight, lCentered )
   METHOD HLine( nSize, nWidth, lShade, cColor )
   METHOD PutParagraph() INLINE ::cStr += "<p> </p>" + hb_eol()
   METHOD Paragraph( lStart, cAlign, cStyle )
   METHOD PutBreak() INLINE ::cStr += "<br />" + hb_eol()
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
      Id, hSpace, Align )
   METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
      l3d, lRuleCols, lRuleRows, cClrDark, cClrLight, cClrBorder, ;
      nCellPadding, nCellSpacing, cAlign, lRules, ;
      bgImage, cStyle, Id, NOF )
   METHOD TableHead( cHead, cColor, cAlign, cFont, nSize, cFntColor, nHeight, cBgPic )
   METHOD NewTableRow( cColor, valign, align )
   METHOD EndTableRow()
   METHOD NewTableCell( cAlign, cColor, cFont, nSize, cFntColor, nHeight, ;
      cBgPic, nWidth, lWrap, nColspan, nRowspan, cValign, ;
      clrdrk, clrlt, cBdrClr, cClass, lNoFont )
   METHOD EndTableCell()
   METHOD EndTable()
   METHOD NewList() INLINE ::cStr += "<ul>" + hb_eol()
   METHOD ListItem() INLINE ::cStr += "<li> "
   METHOD EndList() INLINE ::cStr += "</ul> "
   METHOD NewForm( cMethod, cAction, cName )
   METHOD FormImage( cText, name, File )
   METHOD FormEdit( cType, cName, xValue, nSize )
   METHOD FormReset( cText )
   METHOD FormSubmit( cText )
   METHOD FormQOut( c ) INLINE ::cStr += c + "<br />" + hb_eol()
   METHOD FormQQOut( c ) INLINE ::cStr += c + hb_eol()
   METHOD EndForm() INLINE ::cStr += hb_eol() + "</form>" + hb_eol()
   METHOD PushButton( cName, cCaption, cCgiApp, cOnClick, cOnFocus, cOnBlur, cOnMsOver, cOnMsOut, style, ID )
   METHOD endButton()
   METHOD Button( cName, cCaption, cOnClick, cCgiApp, cOnMsOver, cOnMsOut, style, ID )
   METHOD iFrame( name, src, border, marginwidth, marginheight, scrolling, align, WIDTH, HEIGHT )
   METHOD StartJava() INLINE ::cStr += '<script language="JavaScript">' + hb_eol() + "<!--" + hb_eol()
   METHOD PutJavaSource( c ) INLINE ::cStr += Space( 5 ) + "src=" + '"' + c + '"' + hb_eol()
   METHOD PutJava( c ) INLINE ::cStr += Space( 5 ) + c + hb_eol()
   METHOD EndJava() INLINE ::cStr += "                  //-->" + hb_eol() + "</script>" + hb_eol()
   METHOD serverCode( c ) INLINE ::cStr += "<server>" + Space( 9 ) + c + hb_eol() + "</server>" + hb_eol()
   METHOD FWrite( c ) INLINE FWrite( ::nH, c )
   METHOD FWriteLN( c ) INLINE FWrite( ::nH, c + hb_eol() )
   METHOD Span( c, Style )
   METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, cOnclick, ;
      cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText )
   METHOD Comment( cText )
   METHOD AddObject( cType, cClassid, cAlign, cCode, lDisable, cCodeBase, ;
      cName, nWidth, nHeight )
   METHOD AddParam( cType, cValue )
   METHOD EndOBJect()
   METHOD PutLinkName( cName )
   METHOD NewMap( cName ) INLINE ::cStr += "<map name=" + cName + ">"
   METHOD MapArea( Shape, Alt, Coord, Url ) INLINE ;
      ::cStr += "<area shape=" + Shape + " alt=" + alt + " coords=" + Coord + " href=" + Url + ">" + hb_eol()
   METHOD EndMap() INLINE ::cStr += "</map>"

ENDCLASS

/* Starts a new CGI-HTML stream file. */
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

   ::nH    := hb_GetStdOut()
   ::Title := cTitle
   IF lCgi
      ::FName := "cgiout.htm"
   ELSE
      ::FName := cFile
      ::nH := FCreate( cFile )
   ENDIF
   IF lCgi
      ::cStr += "Content-Type: text/html" + hb_eol() + hb_eol()
      FWrite( ::nh, ::cStr )
   ENDIF
   ::cStr := ""

   ::cStr += "<html>" + hb_eol() + ;
      "<head>" + hb_eol() + ;
      "   <title>" + cTitle + "</title>" + hb_eol()

   IF HB_ISSTRING( cBaseURL )
      ::cStr += "<base href=" + '"' + cBaseURL + '"'

      IF HB_ISSTRING( cBaseTarget )
         ::cStr += " target=" + '"' + cBaseTarget + '"'
      ENDIF

      ::cStr += ">" + hb_eol()
   ENDIF
#if 0
/* TOFIX: Luiz please review it */
   ::cStr += ;
      "   <link title=" + '"' + hb_defaultValue( cLinkTitle, cTitle ) + '"' + hb_eol() + ;
      '                href="mailto:mail@example.org">' + hb_eol() + ;
      '   <meta http-equiv="Content-Type" content="text/html; charset=' + hb_defaultValue( cCharset, "UTF-8" ) + '"' + " />" + hb_eol() )
#else
   HB_SYMBOL_UNUSED( cLinkTitle )
   HB_SYMBOL_UNUSED( cCharset )
#endif
   IF HB_ISSTRING( cStyleScr )
      ::cStr += "   <link href=" + '"' + cStyleScr + '"' + " rel='STYLESHEET' type='text/css' />" + hb_eol()
   ENDIF

   IF HB_ISNUMERIC( nRefresh )
      ::cStr += '   <meta http-equiv="Refresh" content=' + '"' + hb_ntos( nRefresh ) + "; url=" + hb_defaultValue( cRefreshURL, "" ) + '"' + " />"
   ENDIF

   IF hb_defaultValue( lNocache, .F. )
      ::cStr += '   <meta http-equiv="pragma" content="no-cache" />'
   ENDIF

   IF HB_ISARRAY( aJsCode )
      AEval( aJsCode, {| e | HtmlJSCmd( ::nH, e ) } )
   ENDIF

   IF HB_ISARRAY( aScriptSrc ) .OR. HB_ISHASH( aScriptSrc )
      FOR EACH i IN aScriptSrc
         ::cStr += Q_( "<script language=JavaScript src=", i, " />" ) + hb_eol()
      NEXT
   ENDIF

   IF HB_ISARRAY( aServerSrc ) .OR. HB_ISHASH( aServerSrc )
      FOR EACH i IN aServerSrc
         ::cStr += Q_( "<script language=JavaScript src=", i, " runat=SERVER />" ) + hb_eol()
      NEXT
   ENDIF

   // preload images...
   IF HB_ISARRAY( aImages )

      ::aImages := aImages

      ::cStr += ;
         '<script language="JavaScript">' + hb_eol() + ;
         "<!--" + hb_eol() + ;
         "if(document.images)" + hb_eol() + ;
         "{" + hb_eol()

      FOR EACH i IN aImages
         ::cStr += ;
            Space( 5 ) + i[ 1 ] + "=new Image(100,50);" + hb_eol() + ;
            Space( 5 ) + i[ 1 ] + Q_( ".src=", i[ 2 ], ";" ) + hb_eol()
      NEXT

      ::cStr += "}" + hb_eol()

      ::cStr += "" + hb_eol() + ;
         Space( 5 ) + "// Function to 'activate' images." + hb_eol() + ;
         Space( 5 ) + "function imageOn(imgName) {" + hb_eol() + ;
         Space( 5 ) + "        if (document.images) {" + hb_eol() + ;
         Space( 5 ) + "            imgOn=eval" + '(imgName + "on.src");' + hb_eol() + ;
         Space( 5 ) + "            document[imgName].src = imgOn;" + hb_eol() + ;
         Space( 5 ) + "        }" + hb_eol() + ;
         Space( 5 ) + "}" + hb_eol() + ;
         hb_eol() + ;
         Space( 5 ) + "// Function to 'deactivate' images." + hb_eol() + ;
         Space( 5 ) + "function imageOff(imgName) {" + hb_eol() + ;
         Space( 5 ) + "        if (document.images) {" + hb_eol() + ;
         Space( 5 ) + "            imgOff=eval" + '(imgName + "off.src");' + hb_eol() + ;
         Space( 5 ) + "            document[imgName].src = imgOff;" + hb_eol() + ;
         Space( 5 ) + "        }" + hb_eol() + ;
         Space( 5 ) + "}" + hb_eol() + ;
         hb_eol() + ;
         Space( 5 ) + "// Function for 'pressed' images." + hb_eol() + ;
         Space( 5 ) + "function imagePress(imgName) {" + hb_eol() + ;
         Space( 5 ) + "        if (document.images) {" + hb_eol() + ;
         Space( 5 ) + "            imgPress=eval" + '(imgName + "press.src");' + hb_eol() + ;
         Space( 5 ) + "            document[imgName].src = imgPress;" + hb_eol() + ;
         Space( 5 ) + "        }" + hb_eol() + ;
         Space( 5 ) + "}" + hb_eol() + ;
         hb_eol() + ;
         "//-->" + hb_eol() + ;
         "</script>" + hb_eol()
   ENDIF

   IF HB_ISSTRING( cStyle )
      ::cStr += "<style> " + cStyle + " </style>" + hb_eol()
   ENDIF

   ::cStr += ;
      "</head>" + hb_eol() + ;
      "<body"

   IF HB_ISSTRING( onLoad )
      ::cStr += " onLoad=" + '"' + onLoad + '"'
   ENDIF
   IF HB_ISSTRING( NOF )
      ::cStr += " nof=" + '"' + nof + '"'
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

   ::cStr += ">" + hb_eol()

   t_nHtm := ::nH

   t_oPage := Self

   RETURN self

METHOD NewAlt( cType ) CLASS THtml

   ::nH := hb_GetStdOut()
   ::cStr += "Content-Type: " + cType + hb_eol() + hb_eol()

   t_nHtm := ::nH

   t_oPage := Self

   RETURN self


METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := hb_eol() + "<font"

   hb_default( @lSet, HB_ISSTRING( cFont ) )  /* keep it on top */
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
      cStr += " color=" + '"' + cColor + '"'

      IF lSet
         ::fontColor := cColor
      ENDIF
   ENDIF

   cStr += ">"

   IF HB_ISLOGICAL( lBold )
      cStr += iif( lBold, "<b>", "</b>" )
   ENDIF
   IF HB_ISLOGICAL( lItalic )
      cStr += iif( lItalic, "<i>", "</i>" )
   ENDIF
   IF HB_ISLOGICAL( lULine )
      cStr += iif( lULine, "<u>", "</u>" )
   ENDIF

   ::cStr += cStr + "</font>" + hb_eol()

   RETURN Self

/* Begin a font definition. They may be nested but make sure you
   end the definition appropriately later */
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
         cStr += " color=" + '"' + cColor + '"'

         IF lSet
            ::fontColor := cColor
         ENDIF
      ENDIF
   ENDIF

   cStr += ">"

   IF HB_ISLOGICAL( lBold )
      cStr += iif( lBold, "<b>", "</b>" )
   ENDIF
   IF HB_ISLOGICAL( lItalic )
      cStr += iif( lItalic, "<i>", "</i>" )
   ENDIF
   IF HB_ISLOGICAL( lULine )
      cStr += iif( lULine, "<u>", "</u>" )
   ENDIF

   ::cStr += cStr + hb_eol()

   RETURN Self

/* Begin a font definition by font type "name".
   Use ::endFont() to cancel this font */
METHOD DefineFont( cFont, cType, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := "<font "

   hb_default( @lSet, HB_ISSTRING( cFont ) )  /* keep it on top */
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
      cStr += Q_( " color=", cColor, ">" )

      IF lset
         ::fontColor := cColor
      ENDIF
   ELSE
      cStr += ">"
   ENDIF

   IF HB_ISSTRING( cType )
      cStr += cType
   ENDIF

   ::cStr += cStr + hb_eol()

   RETURN Self

/* End a font definition */
METHOD EndFont() CLASS THtml

   ::cStr += "</font>" + hb_eol()

   RETURN Self

METHOD Say( str, cFont, nSize, type, cColor, cStyle ) CLASS THtml

   LOCAL cStd    := ""
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
      cStd := "<font " + ;
         iif( HB_ISSTRING( cFont ), "face=" + '"' + cFont + '"', "" ) + ;
         iif( HB_ISSTRING( cColor ), " color=" + cColor, "" ) + ;
         iif( HB_ISNUMERIC( nSize ), " size=" + hb_ntos( nSize ), "" )

      IF HB_ISSTRING( cStyle )
         cStd += Q_( " style=", cStyle, ">" )
      ELSE
         cStd += ">"
      ENDIF
   ENDIF

   IF HB_ISSTRING( type )

      IF "<" $ type

         IF "<b>" $ type
            lBold := .T.
            cStd += "<b>"
         ENDIF

         IF "<i>" $ type
            lItalic := .T.
            cStd += "<i>"
         ENDIF

         IF "<u>" $ type
            lULine := .T.
            cStd += "<u>"
         ENDIF

         IF "<em>" $ type
            lEm  := .T.
            cStd += "<em>"
         ENDIF

         IF "<strong>" $ type
            lStrong := .T.
            cStd += "<strong>"
         ENDIF
      ENDIF
   ENDIF

   cStd += str

   IF lBold
      cStd += "</b>"
   ENDIF

   IF lItalic
      cStd += "</i>"
   ENDIF

   IF lULine
      cStd += "</u>"
   ENDIF

   IF lStrong
      cStd += "</strong>"
   ENDIF

   IF lEm
      cStd += "</em>"
   ENDIF

   IF HB_ISSTRING( cFONT ) .OR. HB_ISNUMERIC( nSize ) .OR. HB_ISSTRING( cCOLOR )
      cStd += "</font>"
   ENDIF

   ::cStr += cStd + hb_eol()

   RETURN Self

METHOD Paragraph( lStart, cAlign, cStyle ) CLASS THtml

   LOCAL cStr

   IF hb_defaultValue( lStart, .T. )
      cStr := "<p align=" + '"' + hb_defaultValue( cAlign, "LEFT" ) + '"'

      IF HB_ISSTRING( cStyle )
         cStr += " style=" + '"' + cStyle + '"'
      ENDIF

      cStr += ">"
   ELSE
      cStr := "</p>"
   ENDIF

   ::cStr += cStr + hb_eol()

   RETURN Self

/* Put a Horizontal line */
METHOD HLine( nSize, nWidth, lShade, cColor ) CLASS THtml

   ::cStr += hb_eol() + ;
      "<hr" + iif( hb_defaultValue( lShade, .T. ), "", " noshade" ) + ;
      " size=" + hb_ntos( hb_defaultValue( nSize, 3 ) ) + iif( HB_ISSTRING( cColor ), " COLOR  " + cColor, "" ) + ;
      " width=" + hb_ntos( hb_defaultValue( nWidth, 100 ) ) + "%>" + ;
      hb_eol()

   RETURN Self

/* Put an HTML heading ( large text ) */
METHOD PutHeading( cText, nWeight, lCentered ) CLASS THtml

   hb_default( @nWeight, 3 )
   hb_default( @lCentered, .F. )

   IF lCentered
      ::cStr += "<center>"
   ENDIF

   ::cStr += "<h" + hb_ntos( nWeight ) + ">" + cText + "</h" + hb_ntos( nWeight ) + ">" + hb_eol()

   IF lCentered
      ::cStr += "</center>"
   ENDIF

   RETURN Self

/* Put a text link. */
METHOD PutTextUrl( cText, cUrl, cOnClick, cOnMsOver, cOnMsout, cTarget, font, clr, size, style, bld, lbreak, cClass ) CLASS THtml

   LOCAL cStr := ""

   IF HB_ISSTRING( cOnClick )
      cStr += Space( 5 ) + "onClick=" + '"' + cOnClick + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += Space( 5 ) + "onMouseOver=" + '"' + cOnMsOver + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += Space( 5 ) + "onMouseOut=" + '"' + cOnMsOut + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cTarget )
      cStr += Space( 5 ) + "target=" + cTarget + hb_eol()
   ENDIF
   IF HB_ISSTRING( cClass )
      cStr += Space( 5 ) + "class=" + cClass + hb_eol()
   ENDIF

   cStr += ">"

   hb_default( @bld, .F. )

   IF bld
      cStr += "<b>" + hb_eol()
   ENDIF

   IF HB_ISSTRING( FONT ) .OR. HB_ISSTRING( clr ) .OR. HB_ISNUMERIC( size ) .OR. HB_ISSTRING( style )
      cStr += " <font " + hb_eol()

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

   IF HB_ISSTRING( FONT ) .OR. HB_ISSTRING( clr ) .OR. HB_ISNUMERIC( size ) .OR. HB_ISSTRING( style )
      cStr += "</font>"
   ENDIF

   IF bld
      cStr += "</b>"
   ENDIF

   ::cStr += ;
      "<a href=" + '"' + hb_defaultValue( cUrl, "" ) + '"' + hb_eol() + ;
      cStr + "</a>" + iif( hb_defaultValue( lBreak, .F. ), "<br />", "" ) + hb_eol()

   RETURN Self

/* Put an Image link. */
METHOD PutImageUrl( cImage, nBorder, nHeight, cUrl, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ;
      Id, hSpace, Align ) CLASS THtml

   LOCAL cStr := ""

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cAlt )
      cStr += " alt=" + '"' + cAlt + '"' + hb_eol()
   ENDIF

   DO CASE
   CASE HB_ISNUMERIC( nBorder )
      cStr += " border=" + hb_ntos( nBorder ) + hb_eol()
   CASE HB_ISSTRING( nBorder )
      cStr += " border=" + nBorder + hb_eol()
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " " + hb_eol()
   CASE HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"' + " " + hb_eol()
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " " + hb_eol()
   CASE HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"' + " " + hb_eol()
   ENDCASE

   IF HB_ISSTRING( cOnClick )
      cStr += " onClick=" + '"' + cOnClick + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cOnMsOver )
      cStr += " onMouseOver=" + '"' + cOnMsOver + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cOnMsOut )
      cStr += " onMouseOut=" + '"' + cOnMsOut + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cTarget )
      cStr += " target=" + cTarget + hb_eol()
   ENDIF
   IF HB_ISSTRING( Id )
      cstr += " id=" + Id
   ENDIF
   IF HB_ISSTRING( Align )
      cStr += " align=" + '"' + Align + '"'
   ENDIF
   IF HB_ISNUMERIC( hSpace )
      cStr += " hSpace= " + hb_ntos( hSpace ) + " "
   ENDIF

   ::cStr += ;
      "<a href=" + cUrl + iif( HB_ISSTRING( cClass ), " class=" + '"' + cClass + '"', "" ) + "><img src=" + '"' + cImage + '"' + ;
      cStr + "></a>" + iif( hb_defaultValue( lBreak, .F. ), "<br />" + hb_eol(), "" )

   RETURN Self

METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText ) CLASS THtml

   LOCAL cStr := ""

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"'
   ENDIF
   IF HB_ISSTRING( cAlt )
      cStr += " alt=" + '"' + cAlt + '"'
   ENDIF
   IF HB_ISNUMERIC( nBorder )
      cStr += " border=" + hb_ntos( nBorder )
   ENDIF

   DO CASE
   CASE HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " "
   CASE HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"' + " "
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " "
   CASE HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"' + " "
   ENDCASE

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
      cStr + "></a>" + iif( hb_defaultValue( lBreak, .F. ), "<br />" + hb_eol(), "" )

   RETURN Self

/* Put an Image. */
METHOD PutImage( cImage, nBorder, nHeight, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, ;
      nWidth, lbreak, Id, Map, Align, hSpace ) CLASS THtml

   LOCAL cStr := ""

   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"'
   ENDIF
   IF HB_ISSTRING( cAlt )
      cStr += " alt=" + '"' + cAlt + '"'
   ENDIF

   DO CASE
   CASE HB_ISNUMERIC( nBorder )
      cStr += " border=" + hb_ntos( nBorder )
   CASE HB_ISSTRING( nBorder )
      cStr += " border=" + '"' + nBorder + '"'
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " "
   CASE HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " "
   CASE HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"' + " "
   ENDCASE

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
   IF HB_ISSTRING( Align )
      cStr += " align=" + '"' + Align + '"'
   ENDIF
   IF HB_ISNUMERIC( hSpace )
      cStr += " hSpace= " + hb_ntos( hSpace ) + " "
   ENDIF

   ::cStr += ;
      "<img src=" + '"' + cImage + '"' + ;
      cStr + ">" + iif( hb_defaultValue( lBreak, .F. ), "<br />" + hb_eol(), "" )

   RETURN Self

/* Close an HTML disk file */
METHOD Close() CLASS THtml

   ::cStr += ;
      "</body>" + hb_eol() + ;
      "</html>" + hb_eol()

   FWrite( ::nh, ::cStr )

   IF ! ::lCgi
      FClose( ::nH )
   ENDIF

   ::cStr := ""

   RETURN Self

/* Close a CGI-HTML stream file */
METHOD cgiClose() CLASS THtml

   ::cStr += ;
      "</body>" + hb_eol() + ;
      "</html>" + hb_eol()

   FWrite( ::nh, ::cStr )
   FWrite( ::nH, hb_eol() )

   RETURN Self

/* Start an HTML table definition. */
METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
      l3d, lRuleCols, lRuleRows, cClrDark, cClrLight, cClrBorder, ;
      nCellPadding, nCellSpacing, cAlign, lRules, ;
      bgImage, cStyle, Id, NOF ) CLASS THtml

   LOCAL cStr  := hb_eol() + hb_eol() + "<table "
   LOCAL xCols := nCols

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
   IF HB_ISSTRING( cAlign )
      cStr += " align=" + '"' + cAlign + '"'
   ENDIF

   cStr += iif( HB_ISNUMERIC( xCols ), " cols=" + hb_ntos( nCols ), "" )

   DO CASE
   CASE HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth )
   CASE HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"'
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight )
   CASE HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDCASE

   IF hb_defaultValue( l3d, .T. )
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

   DO CASE
   CASE hb_defaultValue( lRuleCols, .F. )
      cStr += " rules=COLS"
   CASE hb_defaultValue( lRuleRows, .F. )
      cStr += " rules=ROWS"
   CASE hb_defaultValue( lRules, .F. )
      cStr += " rules=ALL"
   ENDCASE

   IF HB_ISSTRING( bgImage )
      cStr += Q_( " background=", bgImage, " " )
   ENDIF
   IF HB_ISSTRING( cStyle )
      cStr += Q_( " style=", cStyle, " " )
   ENDIF
   IF HB_ISSTRING( Id )
      cStr += " id=" + Id
   ENDIF
   IF HB_ISSTRING( NOF )
      cStr += " nof=" + '"' + NOF + '"'
   ENDIF

   ::cStr += cStr + ">" + hb_eol() + hb_eol()

   RETURN Self

/* Define a table column Header. */
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

   ::cStr += cStr + cHead + iif( HB_ISSTRING( cFont ), "</font>", "" ) + "</th>" + hb_eol()

   RETURN Self

/* Start a table row definition. */
METHOD NewTableRow( cColor, valign, align ) CLASS THtml

   LOCAL cStr := Space( 5 ) + "<tr"

   IF HB_ISSTRING( cColor )
      cStr += " bgcolor=" + cColor
   ENDIF
   IF HB_ISSTRING( valign )
      cStr += " valign=" + valign
   ENDIF
   IF HB_ISSTRING( align )
      cStr += " align=" + align
   ENDIF

   ::cStr += cStr + ">" + hb_eol()

   RETURN Self

/* End a table row definition. */
METHOD EndTableRow() CLASS THtml

   ::cStr += Space( 5 ) + "</tr>" + hb_eol()

   RETURN Self

/* Start a table cell definition. */
METHOD NewTableCell( cAlign, cColor, ;
      cFont, nSize, cFntColor, nHeight, ;
      cBgPic, nWidth, lWrap, ;
      nColspan, nRowspan, cValign, clrdrk, clrlt, cBdrClr, cClass, lNoFont ) CLASS THtml

   LOCAL cStr := Space( 10 ) + "<td"
   LOCAL cAli := cAlign

   hb_default( @cFont, ::fontFace )
   hb_default( @nSize, ::fontSize )
   hb_default( @cFntColor, ::fontColor )
   hb_default( @cAlign, "LEFT" )

   IF HB_ISSTRING( cBdrClr )
      cStr += " bordercolor=" + cBdrClr
   ENDIF
   IF HB_ISSTRING( cColor )
      cStr += " bgcolor=" + cColor
   ENDIF
   IF HB_ISSTRING( cAlign ) .AND. cAli != NIL
      cStr += " align=" + cAlign
   ENDIF
   IF HB_ISSTRING( cValign )
      cStr += " valign=" + cValign
   ENDIF
   IF HB_ISSTRING( cBgPic )
      cStr += " background=" + '"' + cBgPic + '"'
   ENDIF

   DO CASE
   CASE HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight )
   CASE HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth )
   CASE HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"'
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nColspan )
      cStr += " colspan=" + hb_ntos( nColspan )
   CASE HB_ISSTRING( nColspan )
      cStr += " colspan=" + '"' + nColspan + '"'
   ENDCASE
   IF HB_ISSTRING( clrdrk )
      cStr += " borderColorDark=" + clrdrk
   ENDIF
   IF HB_ISSTRING( clrlt )
      cStr += " bordercolorlight=" + clrlt
   ENDIF
   IF HB_ISSTRING( cClass )
      cStr += Q_( " Class=", cClass, " " )
   ENDIF

   DO CASE
   CASE HB_ISNUMERIC( nRowspan )
      cStr += " rowspan=" + hb_ntos( nRowspan )
   CASE HB_ISSTRING( nRowspan )
      cStr += " rowspan=" + '"' + nRowspan + '"'
   ENDCASE

   IF ! hb_defaultValue( lWrap, .T. )
      cStr += " nowrap"
   ENDIF

   cStr += ">"

   IF ! hb_defaultValue( lNoFont, .T. )
      cStr += "<font "

      IF HB_ISNUMERIC( nSize )
         cStr += "size=" + hb_ntos( nSize )
      ENDIF
      IF HB_ISSTRING( cFntColor )
         cStr += " color=" + cFntColor
      ENDIF

      IF ! Empty( cFont )
         cStr += Q_( " face=", cFont, ">" )
      ELSE
         cStr += ">"
      ENDIF

      ::lFont := .T.
   ENDIF

   ::cStr += cStr

   RETURN Self

/* End a table cell definition. */
METHOD EndTableCell() CLASS THtml

   IF ::lFont
      ::cStr += "</font></td>" + hb_eol()
   ELSE
      ::cStr += "</td>" + hb_eol()
   ENDIF

   ::lFont := .F.

   RETURN Self

/* End a table definition. */
METHOD EndTable() CLASS THtml

   ::cStr += ;
      "</table>" + hb_eol() + ;
      hb_eol() + hb_eol() + hb_eol()

   RETURN Self

/* Creates a new form */
METHOD NewForm( cMethod, cAction, cName ) CLASS THtml

   __defaultNIL( @cMethod, "POST" )
   __defaultNIL( @cName, "newForm" )

   ::cStr += hb_eol() + "<form"

   IF HB_ISSTRING( cMethod )
      ::cStr += " method=" + '"' + cMethod + '"'
   ENDIF
   IF HB_ISSTRING( cName )
      ::cStr += " name=" + '"' + cName + '"'
   ENDIF
   IF HB_ISSTRING( cAction )
      ::cStr += " action=" + '"' + cAction + '"'
   ENDIF

   ::cStr += ">" + hb_eol()

   t_cForm := cName

   RETURN Self

/* Adds a form edit field */
METHOD FormEdit( cType, cName, xValue, nSize ) CLASS THtml

   ::cStr += "<input type=" + '"' + hb_defaultValue( cType, "edit" ) + '"'

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

/* Adds a form submit button */
METHOD FormSubmit( cText ) CLASS THtml

   ::cStr += Q_( '<input type="submit" Value=', cText, ">" ) + hb_eol()

   RETURN Self

/* Adds a form image button */
METHOD FormImage( cText, name, file ) CLASS THtml

   HB_SYMBOL_UNUSED( cText )

   ::cStr += '<input type="IMAGE" name=' + '"' + name + '"' + ' src=' + '"' + file + '"' + ">" + hb_eol()

   RETURN Self

/* Adds a reset button */
METHOD FormReset( cText ) CLASS THtml

   ::cStr += Q_( '<input type="Reset" Value=', cText, ">" ) + hb_eol()

   RETURN Self

/* Insert a standalone push button and assign an action to it
   Either pass onClick or cCgiApp - not both */
METHOD PushButton( cName, cCaption, ;
      cCgiApp, ;
      cOnClick, ;
      cOnFocus, cOnBlur, ;
      cOnMsOver, cOnMsOut, ;
      style, ID ) CLASS THtml

   LOCAL cStr := hb_eol() + "<input type=BUTTON " + hb_eol()

   __defaultNIL( @cOnMsOver, "window.status=this.name;" )
   __defaultNIL( @cOnMsOut, "window.status='';" )

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

/* Insert a standalone <button> push button and assign an action to it */
METHOD Button( cName, cCaption, ;
      cOnClick, ;
      cCGIApp, ;
      cOnMsOver, cOnMsOut, ;
      Style, ID ) CLASS THtml

   LOCAL cStr := hb_eol() + "<button " + hb_eol()

   __defaultNIL( @cOnMsOver, "window.status=this.name;" )
   __defaultNIL( @cOnMsOut, "window.status='';" )

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

   ::cStr += cStr + ">" + hb_eol()

   RETURN Self

/* End a <button> definition */
METHOD EndButton() CLASS THtml

   ::cStr += hb_eol() + hb_eol() + "</button>" + hb_eol()

   RETURN Self

/* Display a scrolling marquee effect */
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
      Q_( "<marquee align=", cAlign, " " ) + ;
      Q_( "behavior=", cBehavior, " " ) + ;
      Q_( "width=", hb_ntos( nWidth ) + "%", " " ) + ;
      iif( HB_ISNUMERIC( nHeight ), "height=" + hb_ntos( nHeight ) + " ", "" ) + ;
      Q_( "bgColor=", cBgColor, " " ) + ;
      Q_( "scrollamount=", hb_ntos( nScrollAmt ), " " ) + ;
      Q_( "scrolldelay=", hb_ntos( nScrollDelay ), " " ) + ;
      "loop=" + iif( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + " " + ;
      Q_( "direction=", cDirection, " " ) + ;
      iif( HB_ISSTRING( onMsOver ), Q_( "onMouseOver=", onMsOver, " " ), "" ) + ;
      iif( HB_ISSTRING( onMsOut ), Q_( "onMouseOut=", onMsOut, " " ), "" ) + ;
      iif( HB_ISSTRING( onClick ), Q_( "onClick=", onClick, " " ), "" ) + ;
      iif( HB_ISSTRING( onStart ), Q_( "onStart=", onStart, " " ), "" ) + ;
      iif( HB_ISSTRING( onFinish ), Q_( "onFinish=", onFinish, " " ), "" ) + ;
      ">" + ;
      cText + ;
      "</marquee>" + hb_eol()

   ::EndFont()

   RETURN Self

/* Start a scrolling marquee effect definition */
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

   cStr += ;
      Q_( "<marquee align=", cAlign, " " ) + ;
      Q_( "behavior=", cBehavior, " " ) + ;
      Q_( "width=", hb_ntos( nWidth ) + "%", " " ) + ;
      iif( HB_ISNUMERIC( nHeight ), "height=" + hb_ntos( nHeight ) + " ", "" ) + ;
      Q_( "bgColor=", cBgColor, " " ) + ;
      Q_( "scrollamount=", hb_ntos( nScrollAmt ), " " ) + ;
      Q_( "scrolldelay=", hb_ntos( nScrollDelay ), " " ) + ;
      "loop=" + iif( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + " " + ;
      Q_( "direction=", cDirection, " " ) + ;
      iif( HB_ISSTRING( onMsOver ), Q_( "onMouseOver=", onMsOver, " " ), "" ) + ;
      iif( HB_ISSTRING( onMsOut ), Q_( "onMouseOut=", onMsOut, " " ), "" ) + ;
      iif( HB_ISSTRING( onClick ), Q_( "onClick=", onClick, " " ), "" ) + ;
      iif( HB_ISSTRING( onStart ), Q_( "onStart=", onStart, " " ), "" ) + ;
      iif( HB_ISSTRING( onFinish ), Q_( "onFinish=", onFinish, " " ), "" ) + ;
      ">" + ;
      hb_eol()

   ::cStr += cStr
   ::EndFont()

   RETURN Self

METHOD EndMarquee() CLASS THtml

   ::cStr += "</marquee>" + hb_eol()

   RETURN Self

/* Define an inline frame. */
METHOD iFrame( name, src, border, marginwidth, marginheight, ;
      scrolling, align, WIDTH, HEIGHT ) CLASS THtml

   LOCAL cStr := "<iframe " + hb_eol()

   __defaultNIL( @name, "Frame01" )
#if 0
   __defaultNIL( @align, "vertical" )
#endif

   IF HB_ISSTRING( name )
      cStr += Space( 5 ) + "        name=" + '"' + name + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( src )
      cStr += Space( 5 ) + "         src=" + '"' + src + '"' + hb_eol()
   ENDIF
   IF hb_defaultValue( BORDER, .T. )
      cStr += Space( 5 ) + " frameborder=1" + hb_eol()
   ELSE
      cStr += Space( 5 ) + " frameborder=0" + hb_eol()
   ENDIF

   cStr += Space( 5 ) + "   scrolling=" + '"' + iif( scrolling, "yes", "no" ) + '"' + hb_eol()

   IF HB_ISNUMERIC( marginwidth )
      cStr += Space( 5 ) + " marginwidth=" + '"' + hb_ntos( marginWidth ) + '"' + hb_eol()
   ENDIF
   IF HB_ISNUMERIC( marginheight )
      cStr += Space( 5 ) + "marginheight=" + '"' + hb_ntos( marginheight ) + '"' + hb_eol()
   ENDIF
   IF HB_ISNUMERIC( WIDTH )
      cStr += Space( 5 ) + "       width=" + '"' + hb_ntos( Width ) + '"' + hb_eol()
   ENDIF
   IF HB_ISNUMERIC( HEIGHT )
      cStr += Space( 5 ) + "      height=" + '"' + hb_ntos( height ) + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( align )
      cStr += Space( 5 ) + "       align=" + '"' + align + '"' + hb_eol()
   ENDIF

   ::cStr += cStr + ;
      ">" + hb_eol() + ;
      "</iframe>" + hb_eol()

   RETURN Self

METHOD Span( c, Style ) CLASS THtml

   LOCAL cStr := "<span"

   IF HB_ISSTRING( style )
      cStr += " style=" + '"' + Style + '"'
   ENDIF

   ::cStr += cStr + ">" + c + "</span>"

   RETURN Self

METHOD Comment( cText ) CLASS THtml

   ::cStr += hb_eol() + "<!-- " + cText + " -->"

   RETURN Self

METHOD AddObject( cType, cClassid, cAlign, cCode, lDisable, cCodeBase, cName, nWidth, nHeight ) CLASS THtml

   LOCAL cStr := "<object "

   IF HB_ISSTRING( cType )
      cStr += " type=" + '"' + cType + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cClassId )
      cStr += " classid=" + '"' + cClassId + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cAlign )
      cStr += " align=" + '"' + cAlign + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cCode )
      cStr += " code=" + '"' + cCode + '"' + hb_eol()
   ENDIF
   IF lDisable
      cStr += " DISABLED " + hb_eol()
   ENDIF
   IF HB_ISSTRING( cCodebase )
      cStr += " codebase=" + '"' + cCodebase + '"' + hb_eol()
   ENDIF
   IF HB_ISSTRING( cName )
      cStr += " name=" + '"' + cName + '"' + hb_eol()
   ENDIF

   DO CASE
   CASE HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight ) + " " + hb_eol()
   CASE HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"' + " " + hb_eol()
   ENDCASE

   DO CASE
   CASE HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth ) + " " + hb_eol()
   CASE HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"' + " " + hb_eol()
   ENDCASE

   ::cStr += cStr + " >" + hb_eol()

   RETURN Self

METHOD EndObject() CLASS THtml

   ::cStr += "</object>" + hb_eol()

   RETURN Self

METHOD ADDPARAM( cType, cValue ) CLASS THtml

   ::cStr += "<param name=" + '"' + cType + '"' + " value=" + '"' + cValue + '"' + ">" + hb_eol()

   RETURN Self

METHOD PutLinkName( cName ) CLASS THtml

   ::cStr += Q_( "<a name=", cName, "></a>" )

   RETURN Self

/* Returns the current HTML page handle */
FUNCTION HtmlPageHandle()
   RETURN t_nHtm

/* Returns the current ( or last ) form name */
FUNCTION HtmlFormName()
   RETURN t_cForm

/* Return the current THtml() object. */
FUNCTION HtmlPageObject()
   RETURN t_oPage

/* Decodes a URL encoded string. Also handles international charsets. */
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

/* Inserts inline Javascript source */
PROCEDURE HtmlJSCmd( nH, cCmd )

   FWrite( hb_defaultValue( nH, HtmlPageHandle() ), ;
      "<script language=JavaScript 1.2>" + hb_eol() + "<!--" + hb_eol() + ;
      hb_defaultValue( cCmd, "" ) + hb_eol() + ;
      "//-->" + hb_eol() + "</script>" + hb_eol() )

   RETURN

/* HtmlLinkStyle() */
FUNCTION HtmlLinkStyle( cHoverStyle, cHoverClr, cHoverBG, cLinkStyle, cLinkClr, cLinkBG )
   RETURN ;
      "<!-- A:hover {text-decoration:" + hb_defaultValue( cHoverStyle, "normal" ) + ;
      ";color:" + hb_defaultValue( cHoverClr, "white" ) + ;
      ";background:" + hb_defaultValue( cHoverBg, "black" ) + ;
      ";} A:link {text-decoration:" + hb_defaultValue( cLinkStyle, "normal" ) + ;
      ";color:" + hb_defaultValue( cLinkClr, "black" ) + ;
      ";background:" + hb_defaultValue( cLinkBg, "white" ) + ";}-->"

FUNCTION HtmlPadL( cStr, n )

   IF ! HB_ISNUMERIC( n )
      RETURN cStr
   ENDIF

   IF n <= 0
      RETURN Right( cStr, n )
   ENDIF

   RETURN Replicate( _HTML_SPACE, n - Len( cStr ) ) + cStr

FUNCTION HtmlPadR( cStr, n )

   IF ! HB_ISNUMERIC( n )
      RETURN cStr
   ENDIF

   IF n <= 0
      RETURN Left( cStr, n )
   ENDIF

   RETURN cStr + Replicate( _HTML_SPACE, n - Len( cStr ) )

FUNCTION Any2Str( xVal )
   RETURN HtmlAny2Str( xVal )

FUNCTION HtmlAny2Str( xVal )

   SWITCH ValType( xVal )
   CASE "M"
   CASE "C" ; RETURN iif( Empty( xVal ), ".", xVal )
   CASE "N" ; RETURN hb_ntos( xVal )
   CASE "O" ; RETURN "<" + xVal:CLASSNAME() + ">"
   CASE "D" ; RETURN DToC( xVal )
   CASE "T" ; RETURN hb_TToC( xVal )
   CASE "L" ; RETURN iif( xVal, "T", "F" )
   CASE "B" ; RETURN "{||...}"
   CASE "U" ; RETURN "NIL"
   ENDSWITCH

   RETURN "<Unrecognized Value>"
