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

THREAD STATIC t_nHtm  := NIL
THREAD STATIC t_cForm := 0
THREAD STATIC t_oPage := 0

/****
*
*     Class THtml()
*
*     Constructors :
*
*     THtml():New()          Creates a new HTML document
*
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
   VAR cStr    Init ""
   VAR BaseURL, BaseTarget
   VAR lFont INIT .F.

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

   METHOD SetPageColor( cColor, lBody ) INLINE hb_default( @lBody, .T. ), ::cStr += iif( lBody, '<body bgcolor="' + cColor + '">', ' bgcolor="' + cColor + '" ' )

   METHOD SetTextColor( cColor, lBody ) INLINE hb_default( @lBody, .T. ), ::cStr += iif( lBody, '<body text="' + cColor + '">', ' text="' + cColor + '" ' )

   METHOD SetBgImage( cImage, lBody ) INLINE hb_default( @lBody, .T. ), ::cStr += iif( lBody, '<body background="' + cImage + '">', ' background="' + cImage + '" ' )

   METHOD Close()

   METHOD SetCenter( lOn ) INLINE ::cStr += iif( lOn, "<center>", "</center>" )

   METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )

   METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut )

   METHOD DefineFont( cFont, cType, nSize, cColor, lSet )

   METHOD EndFont()

   METHOD Say( str, font, size, type, color, style )

   METHOD QQOut( c ) INLINE hb_default( @c, "" ), ::cStr += c

   METHOD QOut( c ) INLINE hb_default( @c, "" ), ::cStr += CRLF() + c + "<br />" + CRLF()

   METHOD Write( c ) INLINE hb_default( @c, "" ), ::cStr += c

   METHOD WriteLN( c ) INLINE hb_default( @c, "" ), ::cStr += CRLF() + c + "<br />" + CRLF()

   METHOD SayColor( t, c ) INLINE hb_default( @t, "" ), hb_default( @c, "black" ), ;
      ::cStr += '<font color="' + c + '">' + t + "</font>"

   METHOD Space( n ) INLINE hb_default( @n, 1 ), ::cStr += Replicate( "&nbsp;", n  )

   METHOD PutImage( cImage, nBorder, nHeight, cOnclick, cOnMsOver, cOnMsOut, ;
      cName, cAlt, cTarget, nWidth, lBreak, ID, MAP, ALING, HSPACE )

   METHOD Text( cText, nCols, lWrap ) INLINE hb_default( @lWrap, .T. ), hb_default( @nCols, 80 ), ;
      ::cStr += "<pre" + iif( nCols != NIL, ' cols="' + hb_ntos( nCols ) + "'", "" ) + iif( lWrap, " WRAP>", ">" ) + CRLF() + cText + CRLF() + "</pre>" + CRLF()

   METHOD MultiCol( txt, cols, gutter, width ) INLINE hb_default( @txt, "" ), ;
      hb_default( @cols, 2 ), ;
      hb_default( @gutter, 5 ), ;
      hb_default( @width, 100 ), ;
      ::cStr += '<multicol cols="' + hb_ntos( cols ) + '" gutter="' + hb_ntos( gutter ) + '" width="' + hb_ntos( width ) + '">', ;
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

   METHOD PutJavaSource( c ) INLINE ::cStr += Space( 5 ) + 'src="' + c + '"' + CRLF()

   METHOD PutJava( c ) INLINE ::cStr += Space( 5 ) + c + CRLF()

   METHOD EndJava() INLINE ::cStr += "                  //-->" + CRLF() + "</script>" + CRLF()

   METHOD serverCode( c ) INLINE ::cStr += "<server>" + Space( 9 ) + c + CRLF() + "</server>" + CRLF()

   METHOD FWrite( c ) INLINE FWrite( ::nH, c )

   METHOD FWriteLN( c ) INLINE FWrite( ::nH, c + CRLF() )

   METHOD Span( c, Style )

   METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, cOnclick, ;
      cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText )

   METHOD Comment( cText )

   METHOD ADDoBJECT( cType, cClassid, cAling, cCode, lDisable, cCodeBase, ;
      cName, nWidth, nHeight )

   METHOD ADDPARAM( cType, cValue )

   METHOD EndOBJect()

   METHOD PutLinkName( cName )

   METHOD NewMap( cName ) INLINE ::cStr += "<map name=" + cName + ">"


   METHOD MapArea( Shape, Alt, Coord, Url ) INLINE ;
      ::cStr += "<area shape=" + Shape + " alt=" + alt + " coords=" + Coord + " href=" + Url + ">" + CRLF()

   METHOD EndMap() INLINE ::cStr += "</map>"

ENDCLASS

/****
*
*     THtml():CGINew()
*
*     Starts a new CGI-HTML stream file.
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

   return ::new( cTitle, cLinkTitle, cCharSet, aScriptSRC, ;
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

   __defaultNIL( @lCgi, .F. )
   ::lCgi := lCgi
   IF lCgi
      __defaultNIL( @cTitle, "New CGI-HTML page" )
      __defaultNIL( @cLinkTitle, cTitle )
      __defaultNIL( @cRefreshURL, "" )
      __defaultNIL( @cCharset, "windows-1251" )
      __defaultNIL( @lNocache, .F. )
   ELSE
      __defaultNIL( @cFile, "file1.htm" )
      __defaultNIL( @cTitle, "New HTML page" )
      __defaultNIL( @cLinkTitle, cTitle )
      __defaultNIL( @cRefreshURL, "" )
      __defaultNIL( @cCharset, "windows-1251" )
      __defaultNIL( @lNocache, .F. )
   ENDIF


   ::nH    := STD_OUT
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
   ::cStr := ''

   ::cStr += '<html>' + CRLF() + ;
      "<head>" + CRLF() + ;
      "   <title>" + cTitle + "</title>" + CRLF()

   IF cBaseURL != NIL
      ::cStr += "<base href='" + cBaseURL + "'"

      IF cBaseTarget != NIL
         ::cStr += " target='" + cBaseTarget + "'"
      ENDIF

      ::cStr += ">" + CRLF()
   ENDIF
#if 0
/* TOFIX: Luiz please review it */
   ::cStr += ;
      '   <link title="' + cLinkTitle + '"' + CRLF() + ;
      '                href="mailto:culik@sl.conex.net" >' + CRLF() + ;
      '   <meta http-equiv="Content-Type" content="text/html; charset=' + cCharset + '" />' + CRLF() )
#endif
   IF cStyleScr != NIL
      ::cStr += '   <link href="' + cStyleScr + '"' + " rel='STYLESHEET' type='text/css' />" + CRLF()
   ENDIF

   IF nRefresh != NIL
      ::cStr += '   <meta http-equiv="Refresh" content="' + hb_ntos( nRefresh ) + "; url=" + cRefreshURL + '" />'
   ENDIF

   IF lnocache
      ::cStr += '   <meta http-equiv="pragma" content="no-cache" />'
   ENDIF

   IF aJsCode != NIL
      AEval( aJsCode, {| e | HtmlJSCmd( ::nH, e ) } )
   ENDIF

   IF aScriptSrc != NIL

      FOR i := 1 TO Len( aScriptSrc )
         ::cStr += ;
            '<script language=JavaScript src="' + aScriptSrc[ i ] + '" />' + CRLF()
      NEXT

   ENDIF

   IF aServerSrc != NIL

      FOR i := 1 TO Len( aServerSrc )
         ::cStr += ;
            '<script language=JavaScript src="' + aServerSrc[ i ] + '" runat=SERVER />' + CRLF()
      NEXT

   ENDIF

   // preload images...
   IF aImages != NIL
      ::aImages := aImages
      ::cStr += ;
         '<script language="JavaScript">' + CRLF()
      ::cStr += "<!--" + CRLF()
      ::cStr += "if(document.images)" + CRLF()
      ::cStr += "{" + CRLF()
      FOR i := 1 TO Len( aImages )
         ::cStr += Space( 5 ) + aImages[ i, 1 ] + "=new Image(100,50);" + CRLF()
         ::cStr += Space( 5 ) + aImages[ i, 1 ] + '.src="' + aImages[ i, 2 ] + '";' + CRLF()
      NEXT
      ::cStr += "}" + CRLF()

      ::cStr += "" + CRLF()
      ::cStr += Space( 5 ) + "// Function to 'activate' images." + CRLF()
      ::cStr += Space( 5 ) + "function imageOn(imgName) {" + CRLF()
      ::cStr += Space( 5 ) + "        if (document.images) {" + CRLF()
      ::cStr += Space( 5 ) + '            imgOn=eval' + '(imgName + "on.src");' + CRLF()
      ::cStr += Space( 5 ) + '            document[imgName].src = imgOn;' + CRLF()
      ::cStr += Space( 5 ) + "        }" + CRLF()
      ::cStr += Space( 5 ) + "}" + CRLF()
      ::cStr += CRLF()
      ::cStr += Space( 5 ) + "// Function to 'deactivate' images." + CRLF()
      ::cStr += Space( 5 ) + "function imageOff(imgName) {" + CRLF()
      ::cStr += Space( 5 ) + "        if (document.images) {" + CRLF()
      ::cStr += Space( 5 ) + '            imgOff=eval' + '(imgName + "off.src");' + CRLF()
      ::cStr += Space( 5 ) + '            document[imgName].src = imgOff;' + CRLF()
      ::cStr += Space( 5 ) + "        }" + CRLF()
      ::cStr += Space( 5 ) + "}" + CRLF()
      ::cStr += CRLF()
      ::cStr += Space( 5 ) + "// Function for 'pressed' images." + CRLF()
      ::cStr += Space( 5 ) + "function imagePress(imgName) {" + CRLF()
      ::cStr += Space( 5 ) + "        if (document.images) {" + CRLF()
      ::cStr += Space( 5 ) + '            imgPress=eval' + '(imgName + "press.src");' + CRLF()
      ::cStr += Space( 5 ) + '            document[imgName].src = imgPress;' + CRLF()
      ::cStr += Space( 5 ) + "        }" + CRLF()
      ::cStr += Space( 5 ) + "}" + CRLF()
      ::cStr += CRLF()
      ::cStr += '//-->' + CRLF()
      ::cStr += '</script>' + CRLF()

   ENDIF

   IF cStyle != NIL
      ::cStr += "<style> " + cStyle + " </style>" + CRLF()
   ENDIF

   ::cStr += ;
      '</head>' + CRLF() + ;
      '<body'

   IF onLoad != NIL
      ::cStr += '   onLoad="' + onLoad + '"'
   ENDIF

   IF NOF != NIL
      ::cStr += '   nof="' + nof + '"'
   ENDIF

   IF onUnLoad != NIL
      ::cStr += ' onUnload="' + onUnLoad + '"'
   ENDIF

   IF cLinkClr != NIL
      ::cStr += ' link="' + cLinkClr + '"'
   ENDIF

   IF cVLinkClr != NIL
      ::cStr += ' vlnk="' + cVLinkClr + '"'
   ENDIF

   IF cALinkClr != NIL
      ::cStr += ' alink="' + cALinkClr + '"'
   ENDIF

   IF BGIMAGE != NIL
      ::SetBgImage( bgImage, .F. )
   ENDIF

   IF BGCOLOR != NIL
      ::SetPageColor( bgColor, .F. )
   ENDIF

   IF txtColor != NIL
      ::SetTextColor( txtColor, .F. )
   ENDIF

   IF nMarginTop != NIL
      ::cStr += ' topMargin=' + hb_ntos( nMarginTop )
   ENDIF

   IF nMarginLeft != NIL
      ::cStr += ' LeftMargin=' + hb_ntos( nMarginLeft )
   ENDIF

   IF nMarginHeight != NIL
      ::cStr += ' marginheight=' + hb_ntos( nMarginHeight )
   ENDIF

   IF nMarginWidth != NIL
      ::cStr += ' marginwidth=' + hb_ntos( nMarginWidth )
   ENDIF

   ::cStr += ">"

   ::cStr += CRLF()

   t_nHtm := ::nH

   t_oPage := Self

   RETURN self

METHOD NewAlt( cType ) CLASS THtml

   ::nH    := STD_OUT
   ::cStr += 'Content-Type: ' + cType + CRLF() + CRLF()

   t_nHtm := ::nH

   t_oPage := Self

   RETURN self


/****
*
*     THtml():SetFont()
*
*     obvious...
*/

METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := CRLF() + '<font'

   __defaultNIL( @cFont, ::fontFace )
   __defaultNIL( @nSize, ::fontSize )
   __defaultNIL( @cColor, ::fontColor )
   __defaultNIL( @lset, iif( cFont != NIL, .T., .F. ) )

   IF cFont != NIL
      cStr += ' face="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF nSize != NIL
      cStr += ' size="' + hb_ntos( nSize ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF

   ENDIF

   IF cColor != NIL
      cStr += ' color= "' + cColor + '">'

      IF lset
         ::fontColor := cColor
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF lBold != NIL
      iif( lBold, cStr += '<b>', cStr += '</b>' )
   ENDIF

   IF lItalic != NIL
      iif( lItalic, cStr += '<i>', cStr += '</i>' )
   ENDIF

   IF lULine != NIL
      iif( lULine, cStr += '<u>', cStr += '</u>' )
   ENDIF

   cStr += '</font>'
   ::cStr += cStr + CRLF()

   RETURN Self

/****
*
*     THtml():StartFont()
*
*     Begin a font definition. They may be nested but make sure you
*     end the definition appropriately later
*/

METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut ) CLASS THtml

   LOCAL cStr := "<font "

   __defaultNIL( @lSet, .T. )
   __defaultNIL( @lPut, .F. )
   __defaultNIL( @cFont, ::fontFace )
   __defaultNIL( @nSize, ::fontSize )
   __defaultNIL( @cColor, ::fontColor )

   IF cFont != NIL
      cStr += ' face="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF lPut

      IF nSize != NIL
         cStr += ' size="' + hb_ntos( nSize ) + '"'

         IF lSet
            ::fontSize := nSize
         ENDIF

      ENDIF

      IF cColor != NIL
         cStr += ' color= "' + cColor + '">'

         IF lSet
            ::fontColor := cColor
         ENDIF

      ELSE
         cStr += ">"
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF lBold != NIL
      iif( lBold, cStr += '<b>', cStr += '</b>' )
   ENDIF

   IF lItalic != NIL
      iif( lItalic, cStr += '<i>', cStr += '</i>' )
   ENDIF

   IF lULine != NIL
      iif( lULine, cStr += '<u>', cStr += '</u>' )
   ENDIF

   ::cStr += cStr + CRLF()

   RETURN Self

/****
*
*     THtml():DefineFont()
*
*     Begin a font definition by font type "name".
*     Use ::endFont() to cancel this font
*/

METHOD DefineFont( cFont, cType, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := "<font "

   __defaultNIL( @cFont, ::fontFace )
   __defaultNIL( @nSize, ::fontSize )
   __defaultNIL( @cColor, ::fontColor )
   __defaultNIL( @lset, iif( cFont != NIL, .T., .F. ) )

   IF cFont != NIL
      cStr += ' face="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF nSize != NIL
      cStr += ' size="' + hb_ntos( nSize ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF

   ENDIF

   IF cColor != NIL
      cStr += ' color= "' + cColor + '">'

      IF lset
         ::fontColor := cColor
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF cType != NIL
      cStr += cType
   ENDIF

   ::cStr += cStr + CRLF()

   RETURN Self

/****
*
*     THtml():EndFont()
*
*     End a font definition
*/

METHOD EndFont() CLASS THtml

   ::cStr += '</font>' + CRLF()

   RETURN Self

/****
*
*     THtml():say()
*
*
*
*/

METHOD Say( str, font, size, type, color, style ) CLASS THtml

   LOCAL cOut    := ""
   LOCAL lBold   := .F.
   LOCAL lItalic := .F.
   LOCAL lULine  := .F.
   LOCAL lEm     := .F.
   LOCAL lStrong := .F.
   LOCAL nSize   := Size

   __defaultNIL( @str, "" )
   __defaultNIL( @FONT, ::FontFace )
   __defaultNIL( @size, ::FontSize )
   __defaultNIL( @COLOR, ::FontColor )

   IF FONT != NIL .OR. Size != NIL .OR. COLOR != NIL
      cOut := '<font ' + iif( font != NIL, 'face="' + font + '"', '' ) + iif( color != NIL, ' color=' + color, '' ) + iif( nSize != NIL, ' size=' + hb_ntos( size ), "" )

      IF Style != NIL
         cOut += '" Style="' + style + '">'
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

   IF FONT != NIL .OR. Size != NIL .OR. COLOR != NIL
      cOut += "</font>"
   ENDIF

   ::cStr += cOut + CRLF()

   RETURN Self

/****
*
*     THtml():paragraph()
*
*
*
*/

METHOD Paragraph( lStart, cAlign, cStyle ) CLASS THtml

   LOCAL cStr := "<p"

   __defaultNIL( @lStart, .T. )
   __defaultNIL( @cAlign, "LEFT" )

   IF lStart
      cStr := "<p align='" + cAlign + "'"

      IF cStyle != NIL
         cStr += ' style="' + cStyle + '"'
      ENDIF

      cStr += ">"
   ELSE
      cStr := "</p>"
   ENDIF

   cStr += CRLF()
   ::cStr += cStr

   RETURN Self

/****
*
*     THtml():HLine()
*
*     Put a Horizontal line
*/

METHOD HLine( nSize, nWidth, lShade, cColor ) CLASS THtml

   __defaultNIL( @nSize, 3 )
   __defaultNIL( @nWidth, 100 )
   __defaultNIL( @lShade, .T. )

   IF lShade
      ::cStr += CRLF() + ;
         "<hr size = " + hb_ntos( nSize ) + iif( cColor != NIL, " COLOR  " + cColor, "" ) + " width= " + hb_ntos( nWidth ) + '%>' + ;
         CRLF()
   ELSE
      ::cStr += CRLF() + ;
         "<hr noshade size = " + hb_ntos( nSize ) + iif( cColor != NIL, " COLOR  " + cColor, "" ) + " width= " + hb_ntos( nWidth ) + '%>' + ;
         CRLF()
   ENDIF

   RETURN Self

/****
*
*     THtml():PutHeading()
*
*     Put an HTML heading ( large text )
*/

METHOD PutHeading( cText, nWeight, lCentered ) CLASS THtml

   __defaultNIL( @nWeight, 3 )
   __defaultNIL( @lCentered, .F. )

   IF lCentered
      ::cStr += "<center>"
   ENDIF

   ::cStr += "<h" + hb_ntos( nWeight ) + ">" + cText + "</h" + hb_ntos( nWeight ) + ">" + CRLF()

   IF lCentered
      ::cStr += "</center>"
   ENDIF

   RETURN Self

/****
*
*     THtml():putTextURL()
*
*     Put a text link.
*/

METHOD PutTextUrl( cText, cUrl, cOnClick, cOnMsOver, cOnMsout, cTarget, font, clr, size, style, bld, lbreak, cClass ) CLASS THtml

   LOCAL cStr := ""

   __defaultNIL( @cUrl, "" )
   __defaultNIL( @bld, .F. )
   __defaultNIL( @lBreak, .F. )

   ::cStr += ;
      '<a href="' + cUrl + '"' + CRLF()

   IF cOnClick != NIL
      ::cStr += ;
         Space( 5 ) + 'onClick="' + cOnClick + '"' + CRLF()
   ENDIF
   IF cOnMsOver != NIL
      ::cStr += ;
         Space( 5 ) + 'onMouseOver="' + cOnMsOver + '"' + CRLF()
   ENDIF
   IF cOnMsOut != NIL
      ::cStr += ;
         Space( 5 ) + 'onMouseOut="' + cOnMsOut + '"' + CRLF()
   ENDIF

   IF cTarget != NIL
      ::cStr += ;
         Space( 5 ) + 'target=' + cTarget + CRLF()
   ENDIF

   IF cClass != NIL
      ::cStr += ;
         Space( 5 ) + 'class=' + cClass + CRLF()
   ENDIF

   IF bld
      cStr += "<b>" + CRLF()
   ENDIF

   IF FONT != NIL .OR. clr != NIL .OR. size != NIL .OR. style != NIL
#if 0
      cStr += " Font" + ValType( font ) + "color" + ValType( clr ) + "size" + ValType( size ) + "style" + ValType( style )
#endif
      cStr += " <font " + CRLF()

      IF FONT != NIL
         cStr += ' face="' + FONT + '"'
      ENDIF

      IF clr != NIL
         cStr += ' color=' + clr
      ENDIF

      IF size != NIL
         cStr += ' size=' + hb_ntos( size )
      ENDIF

      IF style != NIL
         cStr += ' style="' + style + '"'
      ENDIF

   ENDIF

   IF FONT != NIL .OR. clr != NIL .OR. size != NIL .OR. style != NIL
      cStr += ">" + cText
   ELSE
      cStr += cText
   ENDIF

   ::cStr += ;
      ">" + cStr
   IF FONT != NIL .OR. clr != NIL .OR. size != NIL .OR. style != NIL
      ::cStr += ;
         '</font>'
   ENDIF

   IF bld
      ::cStr += ;
         '</b>'
   ENDIF

   ::cStr += ;
      '</a>' + iif( lBreak, "<br />" + CRLF(), CRLF() )

   RETURN Self

/****
*
*     THtml():putImageURL()
*
*     Put an Image link.
*/

METHOD PutImageUrl( cImage, nBorder, nHeight, cUrl, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, ;
      Id, hSpace, Aling ) CLASS THtml

   LOCAL cStr := ""

   __defaultNIL( @lbreak, .F. )

   IF cName != NIL
      cStr += ' name= "' + cName + '"' + CRLF()
   ENDIF

   IF cAlt != NIL
      cStr += ' alt= "' + cAlt + '"' + CRLF()
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + iif( HB_ISNUMERIC( nBorder ), hb_ntos( nBorder ), nBorder ) + CRLF()
   ENDIF

   IF nHeight != NIL .AND. HB_ISNUMERIC( nHeight )
      cStr += " height = " + hb_ntos( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .AND. HB_ISSTRING( nHeight )
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .AND. HB_ISNUMERIC( nWidth )
      cStr += " width = " + hb_ntos( nWidth ) + " " + CRLF()
   ELSEIF nWidth != NIL .AND. HB_ISSTRING( nWidth )
      cStr += " width = " + nWidth + " " + CRLF()
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

   IF cTarget != NIL
      cStr += ' target=' + cTarget + CRLF()
   ENDIF

   IF Id != NIL
      cstr += " id=" + Id
   ENDIF

   IF Aling != NIL
      cStr += ' align="' + Aling + '"'
   ENDIF

   IF hSpace != NIL
      cStr += " hSpace= " + hb_ntos( hSpace ) + " "
   ENDIF

   ::cStr += ;
      '<a href=' + cUrl + iif( cClass != NIL, ' class="' + cClass + '"', "" ) + '><img src="' + cImage + '"' + ;
      cStr + '></a>' + iif( lBreak, "<br />" + CRLF(), "" )

   RETURN Self

METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText ) CLASS THtml

   LOCAL cStr := ""

   __defaultNIL( @lbreak, .F. )
   IF cName != NIL
      cStr += ' name= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' alt= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + hb_ntos( nBorder )
   ENDIF

   IF nHeight != NIL .AND. HB_ISNUMERIC( nHeight )
      cStr += " height = " + hb_ntos( nHeight ) + " "
   ELSEIF nHeight != NIL .AND. HB_ISSTRING( nHeight )
      cStr += " height = " + nHeight + " "
   ENDIF

   IF nWidth != NIL .AND. HB_ISNUMERIC( nWidth )
      cStr += " width = " + hb_ntos( nWidth ) + " "
   ELSEIF nWidth != NIL .AND. HB_ISSTRING( nWidth )
      cStr += " width = " + nWidth + " "
   ENDIF

   IF cOnClick != NIL
      cStr += ' onClick="' + cOnClick + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsOver + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += ' onMouseOut="' + cOnMsOut + '"'
   ENDIF

   IF cTarget != NIL
      cStr += ' target=' + cTarget
   ENDIF

   ::cStr += ;
      '<a href=' + cUrl + iif( cClass != NIL, ' class="' + cClass + '"', "" ) + ">" + cText + '<img src="' + cImage + '"' + ;
      cStr + '></a>' + iif( lBreak, "<br />" + CRLF(), "" )

   RETURN Self

/****
*
*     THtml():putImage()
*
*     Put an Image.
*/

METHOD PutImage( cImage, nBorder, nHeight, ;
      cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, ;
      nWidth, lbreak, Id, Map, Aling, hSpace ) CLASS THtml

   LOCAL cStr := ""

   __defaultNIL( @lbreak, .F. )

   IF cName != NIL
      cStr += ' name= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' alt= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL .AND. HB_ISNUMERIC( nBorder )
      cStr += " border= " + hb_ntos( nBorder )
   ELSEIF nBorder != NIL .AND. HB_ISSTRING( nBorder )
      cStr += " border= " + '"' + nBorder + '"'
   ENDIF

   IF nHeight != NIL .AND. HB_ISNUMERIC( nHeight )
      cStr += " height= " + hb_ntos( nHeight ) + " "
   ELSEIF nHeight != NIL .AND. HB_ISSTRING( nHeight )
      cStr += " height= " + '"' + nHeight + '"'
   ENDIF

   IF nWidth != NIL .AND. HB_ISNUMERIC( nWidth )
      cStr += " width= " + hb_ntos( nWidth ) + " "
   ELSEIF nWidth != NIL .AND. HB_ISSTRING( nWidth )
      cStr += " width= " + nWidth + " "
   ENDIF

   IF cOnClick != NIL
      cStr += ' onClick="' + cOnClick + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsOver + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += ' onMouseOut="' + cOnMsOut + '"'
   ENDIF

   IF Map != NIL
      cStr += " usemap=" + Map
   ENDIF

   IF cTarget != NIL
      cStr += ' target="' + cTarget + '"'
   ENDIF

   IF Id != NIL
      cstr += " id=" + Id
   ENDIF

   IF Aling != NIL
      cStr += ' align="' + Aling + '"'
   ENDIF

   IF hSpace != NIL
      cStr += " hSpace= " + hb_ntos( hSpace ) + " "
   ENDIF

   ::cStr += ;
      '<img src="' + cImage + '"' + ;
      cStr + ">" + iif( lBreak, "<br />" + CRLF(), "" )

   RETURN Self

/****
*
*     THtml():Close()
*
*     Close an HTML disk file
*
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

/****
*
*     THtml():CGIClose()
*
*     Close a CGI-HTML stream file
*/

METHOD cgiClose() CLASS THtml

   ::cStr += "</body>" + CRLF()
   ::cStr += "</html>" + CRLF()
   FWrite( ::nh, ::cStr )
   FWrite( ::nH, CRLF() )

   RETURN Self

/****
*
*     THtml():defineTable()
*
*     Start an HTML table definition.
*
*
*/

METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
      l3d, lRuleCols, lRuleRows, cClrDark, cClrLight, cClrBorder, ;
      nCellPadding, nCellSpacing, cAling, lRules, ;
      bgImage, cStyle, Id, NOF ) CLASS THtml

   LOCAL cStr  := CRLF() + CRLF() + "<table "
   LOCAL xCols := nCols

   __defaultNIL( @l3d, .T. )
   __defaultNIL( @lRuleCols, .F. )
   __defaultNIL( @lRuleRows, .F. )

   IF ColorFore != NIL
      cStr += " bordercolor=" + ColorFore + " "
   ENDIF

   IF Colorbg != NIL
      cStr += " bgcolor=" + ColorBG + " "
   ENDIF

   cStr += iif( nBorder = NIL, "border ", "border=" + hb_ntos( nBorder ) + " " )

   IF ncellpadding != NIL
      cStr += ' CellPadding=' + hb_ntos( nCellPadding )
   ENDIF

   IF nCellSpacing != NIL
      cStr += ' CellSpacing=' + hb_ntos( nCellSpacing )
   ENDIF

   IF cAling != NIL
      cStr += ' aling=' + '"' + cAling + '"'
   ENDIF

   cStr += iif( xCols != NIL, " cols=" + hb_ntos( nCols ), "" )

   IF nWidth != NIL .AND. HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth )
   ELSEIF nWidth != NIL .AND. HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"'
   ENDIF

   IF nHeight != NIL .AND. HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight )
   ELSEIF nHeight != NIL .AND. HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDIF

   IF l3d
      cStr += ' bordercolorlight=#000000 ' + ;
         ' bordercolordark=#FFFFFF '
   ENDIF

   IF cClrDark != NIL
      cStr += ' bordercolordark=' + cClrDark
   ENDIF

   IF cClrLight != NIL
      cStr += ' bordercolorlight=' + cClrLight
   ENDIF

   IF cClrBorder != NIL
      cStr += ' bordercolor=' + cClrBorder
   ENDIF

   IF lRuleCols
      cStr += " rules=COLS"
   ELSEIF lRuleRows
      cStr += " rules=ROWS"
   ELSEIF lRules != NIL .AND. lRules
      cStr += " rules=ALL"
   ENDIF

   IF bgImage != NIL
      cStr += ' background="' + bgImage + '" '
   ENDIF
   IF cStyle != NIL
      cStr += ' style ="' + cStyle + '" '
   ENDIF

   IF Id != NIL
      cStr += ' id=' + Id
   ENDIF

   IF NOF != NIL
      cStr += ' nof="' + NOF + '"'
   ENDIF

   cStr += ">" + CRLF()

   ::cStr += cStr + CRLF()

   RETURN Self

/****
*
*     THtml():TableHead()
*
*     Define a table column Header.
*
*/

METHOD TableHead( cHead, cColor, cAlign, ;
      cFont, nSize, cFntColor, nHeight, cBgPic ) CLASS THtml

   LOCAL cStr := Space( 3 ) + "<th"

   __defaultNIL( @cFont, ::fontFace )
   __defaultNIL( @nSize, ::fontSize )
   __defaultNIL( @cFntColor, ::fontColor )

   IF cColor != NIL
      cStr += " bgcolor=" + '"' + cColor + '"'
   ENDIF

   IF cAlign != NIL
      cStr += " align=" + '"' + cAlign + '"'
   ENDIF

   IF nHeight != NIL
      cStr += " height=" + '"' + hb_ntos( nHeight ) + '"'
   ENDIF

   IF cBgPic != NIL
      cStr += " background=" + '"' + cBgPic + '"'
   ENDIF

   cStr += ">"

   IF cFont != NIL
      cStr += '<font face="' + cFont + '"'

      IF nSize != NIL
         cStr += ' size="' + hb_ntos( nSize ) + '"'
      ENDIF

      IF cFntColor != NIL
         cStr += ' color="' + cFntColor + '"'
      ENDIF

      cStr += ">"
   ENDIF

   cStr += cHead + iif( cFont != NIL, '</font>', "" ) + "</th>" + CRLF()

   ::cStr += cStr

   RETURN Self

/****
*
*     THtml():NewTableRow()
*
*     Start a table row definition.
*
*/

METHOD NewTableRow( cColor, vAling, aLing ) CLASS THtml

   LOCAL cStr := Space( 5 ) + "<tr"

   IF cColor != NIL
      cStr += " bgcolor=" + cColor
   ENDIF

   IF vAling != NIL
      cStr += " vAling=" + vAling
   ENDIF

   IF ALING != NIL
      cStr += " Aling=" + ALING
   ENDIF

   cStr += ">" + CRLF()
   ::cStr += cStr

   RETURN Self

/****
*
*     THtml():EndTableRow()
*
*     End a table row definition.
*
*/

METHOD EndTableRow() CLASS THtml

   ::cStr += Space( 5 ) + "</tr>" + CRLF()

   RETURN Self

/****
*
*     THtml():NewTableCell()
*
*     Start a table cell definition.
*
*/

METHOD NewTableCell( cAlign, cColor, ;
      cFont, nSize, cFntColor, nHeight, ;
      cBgPic, nWidth, lWrap, ;
      nColspan, nRowspan, cValign, clrdrk, clrlt, cBdrClr, cClass, lNoFont ) CLASS THtml

   LOCAL cStr := Space( 10 ) + "<td"
   LOCAL cAli := cAlign

   __defaultNIL( @lNoFont, .T. )
   __defaultNIL( @cFont, ::fontFace )
   __defaultNIL( @nSize, ::fontSize )
   __defaultNIL( @cFntColor, ::fontColor )
   __defaultNIL( @cAlign, "LEFT" )
   __defaultNIL( @lWrap, .T. )

   IF cBdrClr != NIL
      cStr += " bordercolor=" + cBdrClr
   ENDIF

   IF cColor != NIL
      cStr += " bgcolor=" + cColor
   ENDIF

   IF cAlign != NIL .AND. caLi != NIL
      cStr += " align=" + cAlign
   ENDIF

   IF cValign != NIL
      cStr += " valign=" + cValign
   ENDIF

   IF nHeight != NIL .AND. HB_ISNUMERIC( nHeight )
      cStr += " height=" + hb_ntos( nHeight )
   ELSEIF nHeight != NIL .AND. HB_ISSTRING( nHeight )
      cStr += " height=" + '"' + nHeight + '"'
   ENDIF

   IF cBgPic != NIL
      cStr += " background=" + '"' + cBgPic + '"'
   ENDIF

   IF nWidth != NIL .AND. HB_ISNUMERIC( nWidth )
      cStr += " width=" + hb_ntos( nWidth )
   ELSEIF nWidth != NIL .AND. HB_ISSTRING( nWidth )
      cStr += " width=" + '"' + nWidth + '"'
   ENDIF

   IF nColspan != NIL .AND. HB_ISNUMERIC( nColspan )
      cStr += " colspan=" + hb_ntos( nColspan )
   ELSEIF nColspan != NIL .AND. HB_ISSTRING( nColspan )
      cStr += " colspan=" + '"' + nColspan + '"'
   ENDIF

   IF clrdrk != NIL
      cStr += " borderColorDark=" + clrdrk
   ENDIF

   IF clrlt != NIL
      cStr += " bordercolorlight=" + clrlt
   ENDIF

   IF cClass != NIL
      cStr += ' Class ="' + cClass + '" '
   ENDIF

   IF nRowspan != NIL .AND. HB_ISNUMERIC( nRowspan )
      cStr += " rowspan=" + hb_ntos( nRowspan )
   ELSEIF nRowspan != NIL .AND. HB_ISSTRING( nRowspan )
      cStr += " rowspan=" + '"' + nRowspan + '"'
   ENDIF

   IF ! lWrap
      cStr += " nowrap"
   ENDIF

   cStr += ">"

   IF ! lNoFont
      cStr += '<font '

      IF nSize != NIL
         cStr += 'size=' + hb_ntos( nSize )
      ENDIF

      IF cFntColor != NIL
         cStr += ' color=' + cFntColor
      ENDIF

      IF ! Empty( cFont )
         cStr += ' face="' + cFont + '"' + ">"
      ELSE
         cStr += ">"
      ENDIF

      ::lFont := .T.
   ENDIF

   ::cStr += cStr

   RETURN Self

/****
*
*     THtml():EndTableCell()
*
*     End a table cell definition.
*
*/

METHOD EndTableCell() CLASS THtml

   IF ::lFont
      ::cStr += "</font></td>" + CRLF()
   ELSE
      ::cStr += "</td>" + CRLF()
   ENDIF

   ::lFont := .F.

   RETURN Self

/****
*
*     THtml():EndTable()
*
*     End a table definition.
*/

METHOD EndTable() CLASS THtml

   ::cStr += "</table>" + CRLF()
   ::cStr += CRLF() + CRLF() + CRLF()

   RETURN Self


/****
*
*     THtml():NewForm()
*
*     Creates a new form
*
*/

METHOD NewForm( cMethod, cAction, cName ) CLASS THtml

   __defaultNIL( @cMethod, "POST" )
   __defaultNIL( @cName, "newForm" )

   ::cStr += CRLF() + "<form"

   IF cMethod != NIL
      ::cStr += ' method="' + cMethod + '"'
   ENDIF

   IF cName != NIL
      ::cStr += ' name="' + cName + '"'
   ENDIF

   IF cAction != NIL
      ::cStr += ' action="' + cAction + '"'
   ENDIF

   ::cStr += ">" + CRLF()

   t_cForm := cName

   RETURN Self

/****
*
*     THtml():FormEdit()
*
*     Adds a form edit field
*
*/

METHOD FormEdit( cType, cName, xValue, nSize ) CLASS THtml

   __defaultNIL( @cType, "edit" )

   ::cStr += '<input type="' + cType + '"'

   IF cName != NIL
      ::cStr += ' Name="' + cName + '"'
   ENDIF

   IF xValue != NIL
      ::cStr += ' Value="' + HtmlAny2Str( xValue ) + '"'
   ENDIF

   IF nSize != NIL
      ::cStr += ' Size="' + HtmlAny2Str( nSize ) + '"'
   ENDIF

   ::cStr += ">"

   RETURN Self

/****
*
*     THtml():FormSubmit()
*
*     Adds a form submit button
*
*/

METHOD FormSubmit( cText ) CLASS THtml

   ::cStr += '<input type="submit" Value="' + cText + '">' + CRLF()

   RETURN Self

/****
*
*     THtml():FormImage()
*
*     Adds a form image button
*
*/

METHOD FormImage( cText, name, file ) CLASS THtml

   HB_SYMBOL_UNUSED( cText )

   ::cStr += '<input type="IMAGE" name="' + name + '" src="' + file + '">' + CRLF()

   RETURN Self

/****
*
*     THtml():FormReset()
*
*     Adds a reset button
*
*/

METHOD FormReset( cText ) CLASS THtml

   ::cStr += '<input type="Reset" Value="' + cText + '">' + CRLF()

   RETURN Self

/****
*
*     THtml():pushButton()
*
*     Insert a standalone push button and assign an action to it
*     Either pass onClick or cCgiApp - not both
*/

METHOD PushButton( cName, cCaption, ;
      cCgiApp, ;
      cOnClick, ;
      cOnFocus, cOnBlur, ;
      cOnMsOver, cOnMsOut, ;
      style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<input type=BUTTON " + CRLF()

   __defaultNIL( @cOnMsOver, "window.status=this.name;" )
   __defaultNIL( @cOnMsOut, "window.status='';" )

   IF cName != NIL
      cStr += "        name=" + cName
   ENDIF

   IF cCaption != NIL
      cStr += "       value=" + cCaption
   ENDIF

   IF style != NIL
      cStr += '       style="' + style + '"'
   ENDIF

   IF ID != NIL
      cStr += '          id="' + ID + '"'
   ENDIF

   IF cOnClick != NIL
      cStr += '     onClick="' + cOnClick + '"'
   ENDIF

   IF cOnFocus != NIL
      cStr += '     onFocus="' + cOnFocus + '"'
   ENDIF

   IF cOnBlur != NIL
      cStr += '      onBlur="' + cOnBlur + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsover + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += '  onMouseOut="' + cOnMsout + '"'
   ENDIF

   IF cCgiApp != NIL
      cStr += '     onClick="location.href=' + cCgiApp + ';"'
   ENDIF

   ::cStr += cStr + ">"

   RETURN Self

/****
*
*     THtml():Button()
*
*     Insert a standalone <button> push button and assign an action to it
*
*/

METHOD Button( cName, cCaption, ;
      cOnClick, ;
      cCGIApp, ;
      cOnMsOver, cOnMsOut, ;
      Style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<button " + CRLF()

   __defaultNIL( @cOnMsOver, "window.status=this.name;" )
   __defaultNIL( @cOnMsOut, "window.status='';" )

   IF cName != NIL
      cStr += "        name=" + cName
   ENDIF

   IF cCaption != NIL
      cStr += "       title=" + cCaption
   ENDIF

   IF style != NIL
      cStr += '       style="' + style + '"'
   ENDIF

   IF ID != NIL
      cStr += '          id="' + ID + '"'
   ENDIF

   IF cOnClick != NIL
      cStr += '     onClick="' + cOnClick + '"'
   ENDIF

   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsover + '"'
   ENDIF

   IF cOnMsOut != NIL
      cStr += '  onMouseOut="' + cOnMsout + '"'
   ENDIF

   IF cCgiApp != NIL
      cStr += '     onClick="location.href=' + cCgiApp + ';"'
   ENDIF

   ::cStr += cStr + ">" + CRLF()

   RETURN Self

/****
*
*     THtml():EndButton()
*
*     End a <button> definition
*
*/

METHOD EndButton() CLASS THtml

   ::cStr += CRLF() + CRLF() + "</button>" + CRLF()

   RETURN Self

/****
*
*     THtml():Marquee()
*
*     Display a scrolling marquee effect
*
*/

METHOD Marquee( cText, cFont, cFntColor, nFntSize, ;
      cAlign, nWidth, nHeight, cbgColor, ;
      cBehavior, cDirection, ;
      nScrollAmt, nScrollDelay, LOOP, ;
      onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS THtml

   __defaultNIL( @cFont, "Verdana" )
   __defaultNIL( @cFntColor, "white" )
   __defaultNIL( @nFntSize, 3 )
   __defaultNIL( @cAlign, "middle" )
   __defaultNIL( @nWidth, 100 )
   __defaultNIL( @cText, "" )
   __defaultNIL( @cBgColor, "black" )
   __defaultNIL( @cBehavior, "scroll" )
   __defaultNIL( @cDirection, "left" )
   __defaultNIL( @nScrollAmt, 5 )
   __defaultNIL( @nScrolldelay, 2 )
   __defaultNIL( @LOOP, 0 )

   ::StartFont( cFont, , , , nFntSize, cFntColor )

   ::cStr += '<marquee align="' + cAlign + '" '
   ::cStr += 'behavior="' + cBehavior + '" '
   ::cStr += 'width="' + hb_ntos( nWidth ) + '%" '
   ::cStr += iif( nHeight != NIL, 'height=' + hb_ntos( nHeight ) + " ", "" )
   ::cStr += 'bgColor="' + cBgColor + '" '
   ::cStr += 'scrollamount="' + hb_ntos( nScrollAmt ) + '" '
   ::cStr += 'scrolldelay="' + hb_ntos( nScrollDelay ) + '" '
   ::cStr += 'loop=' + iif( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + " "
   ::cStr += 'direction="' + cDirection + '" '
   ::cStr += iif( onMsOver != NIL, 'onMouseOver="' + onMsOver + '" ', "" )
   ::cStr += iif( onMsOut != NIL, 'onMouseOut="' + onMsOut + '" ', "" )
   ::cStr += iif( onClick != NIL, 'onClick="' + onClick + '" ', "" )
   ::cStr += iif( onStart != NIL, 'onStart="' + onStart + '" ', "" )
   ::cStr += iif( onFinish != NIL, 'onFinish="' + onFinish + '" ', "" )
   ::cStr += ">"
   ::cStr += cText

   ::cStr += "</marquee>" + CRLF()
   ::EndFont()

   RETURN Self

/****
*
*     THtml():StartMarquee()
*
*     Start a scrolling marquee effect definition
*
*/

METHOD StartMarquee( cFont, cFntColor, nFntSize, ;
      cAlign, nWidth, nHeight, cbgColor, ;
      cBehavior, cDirection, ;
      nScrollAmt, nScrollDelay, LOOP, ;
      onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS THtml

   LOCAL cStr := ""

   __defaultNIL( @cFont, "Verdana" )
   __defaultNIL( @cFntColor, "white" )
   __defaultNIL( @nFntSize, 3 )
   __defaultNIL( @cAlign, "middle" )
   __defaultNIL( @nWidth, 100 )
   __defaultNIL( @cBgColor, "black" )
   __defaultNIL( @cBehavior, "scroll" )
   __defaultNIL( @cDirection, "left" )
   __defaultNIL( @nScrollAmt, 5 )
   __defaultNIL( @nScrolldelay, 2 )

   ::StartFont( cFont, , , , nFntSize, cFntColor )

   cStr += '<marquee align="' + cAlign + '" ' + ;
      'behavior="' + cBehavior + '" ' + ;
      'width="' + hb_ntos( nWidth ) + '%" ' + ;
      iif( nHeight != NIL, 'height=' + hb_ntos( nHeight ) + " ", "" ) + ;
      'bgColor="' + cBgColor + '" ' + ;
      'scrollamount="' + hb_ntos( nScrollAmt ) + '" ' + ;
      'scrolldelay="' + hb_ntos( nScrollDelay ) + '" ' + ;
      'loop=' + iif( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + " " + ;
      'direction="' + cDirection + '" ' + ;
      iif( onMsOver != NIL, 'onMouseOver="' + onMsOver + '" ', "" ) + ;
      iif( onMsOut != NIL, 'onMouseOut="' + onMsOut + '" ', "" ) + ;
      iif( onClick != NIL, 'onClick="' + onClick + '" ', "" ) + ;
      iif( onStart != NIL, 'onStart="' + onStart + '" ', "" ) + ;
      iif( onFinish != NIL, 'onFinish="' + onFinish + '" ', "" ) + ;
      ">" + ;
      CRLF()

   ::cStr += cStr
   ::EndFont()

   RETURN Self

/****
*
*     THtml():EndMarquee()
*
*/

METHOD EndMarquee() CLASS THtml

   ::cStr += "</marquee>" + CRLF()

   RETURN Self

/****
*
*     THtml():iFrame()
*
*     Define an inline frame.
*
*/

METHOD iFrame( name, src, border, marginwidth, marginheight, ;
      scrolling, align, WIDTH, HEIGHT ) CLASS THtml

   LOCAL cStr := "<iframe " + CRLF()

   __defaultNIL( @BORDER, .T. )
   __defaultNIL( @name, "Frame01" )
// __defaultNIL( @align, "vertical" )

   IF name != NIL
      cStr += Space( 5 ) + '        name="' + name + '"' + CRLF()
   ENDIF
   IF src != NIL
      cStr += Space( 5 ) + '         src="' + src + '"' + CRLF()
   ENDIF

   IF BORDER
      cStr += Space( 5 ) + " frameborder='1'" + CRLF()
   ELSE
      cStr += Space( 5 ) + " frameborder='0'" + CRLF()
   ENDIF

   IF scrolling
      cStr += Space( 5 ) + "   scrolling='yes'" + CRLF()
   ELSE
      cStr += Space( 5 ) + "   scrolling='no'" + CRLF()
   ENDIF

   IF marginwidth != NIL
      cStr += Space( 5 ) + " marginwidth='" + hb_ntos( marginWidth ) + "'" + CRLF()
   ENDIF

   IF marginheight != NIL
      cStr += Space( 5 ) + "marginheight='" + hb_ntos( marginheight ) + "'" + CRLF()
   ENDIF

   IF WIDTH != NIL
      cStr += Space( 5 ) + "       width='" + hb_ntos( Width ) + "'" + CRLF()
   ENDIF

   IF HEIGHT != NIL
      cStr += Space( 5 ) + "      height='" + hb_ntos( height ) + "'" + CRLF()
   ENDIF

   IF align != NIL
      cStr += Space( 5 ) + "       align='" + align + "'" + CRLF()
   ENDIF

   cStr += ">" + CRLF()
   cStr += "</iframe>" + CRLF()

   ::cStr += cStr

   RETURN Self

/*   New    Methods   */

METHOD Span( c, Style ) CLASS THtml

   LOCAL cStr := "<span "

   IF style != NIL
      cStr += ' style ="' + Style + '"'
   ENDIF
   cStr += ">" + c + '</span>'
   ::cStr += cStr

   RETURN Self

METHOD Comment( cText ) CLASS THtml

   LOCAL cStr := CRLF() + "<!-- "

   cStr += cText + " -->"
   ::cStr += cStr

   RETURN Self

METHOD AddObject( cType, cClassid, cAling, cCode, lDisable, cCodeBase, cName, nWidth, nHeight ) CLASS THtml

   LOCAL cStr := "<object "

   IF cType != NIL
      cStr += ' type="' + cType + '"' + CRLF()
   ENDIF

   IF cClassId != NIL
      cStr += ' classid="' + cClassId + '"' + CRLF()
   ENDIF

   IF cAling != NIL
      cStr += ' aling ="' + cAling + '"' + CRLF()
   ENDIF

   IF cCode != NIL
      cStr += ' code ="' + cCode + '"' + CRLF()
   ENDIF

   IF lDisable
      cStr += ' DISABLED ' + CRLF()
   ENDIF

   IF cCodebase != NIL
      cStr += ' codebase ="' + cCodebase + '"' + CRLF()
   ENDIF

   IF cName != NIL
      cStr += ' Name ="' + cName + '"' + CRLF()
   ENDIF

   IF nHeight != NIL .AND. HB_ISNUMERIC( nHeight )
      cStr += " height = " + hb_ntos( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .AND. HB_ISSTRING( nHeight )
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .AND. HB_ISNUMERIC( nWidth )
      cStr += " width = " + hb_ntos( nWidth ) + " " + CRLF()
   ELSEIF nWidth != NIL .AND. HB_ISSTRING( nWidth )
      cStr += " width = " + nWidth + " " + CRLF()
   ENDIF

   cStr += " >"
   ::cStr += cStr + CRLF()

   RETURN Self

METHOD EndObject() CLASS THtml

   ::cStr += "</object>" + CRLF()

   RETURN Self

METHOD ADDPARAM( cType, cValue ) CLASS THtml

   ::cStr += '<param name="' + cType + '" value="' + cValue + '">' + CRLF()

   RETURN Self

METHOD PutLinkName( cName ) CLASS THtml

   ::cStr += '<a name="' + cName + '"></a>'

   RETURN Self

/****
*
*     HtmlPageHandle()
*
*     Returns the current HTML page handle
*
*/

FUNCTION HtmlPageHandle()

   RETURN t_nHtm

/****
*
*     HtmlFormName()
*
*     Returns the current ( or last ) form name
*
*/

FUNCTION HtmlFormName()

   RETURN t_cForm

/****
*     HtmlPageObject()
*
*     Return the current THtml() object.
*
*/

FUNCTION HtmlPageObject()

   RETURN t_oPage

/****
*
*     HtmlDecodeUrl()
*
*     Decodes a URL encoded string. Also handles international charsets.
*
*/

FUNCTION HtmlDecodeUrl( cString )

   DO WHILE "%26" $ cString
      cString := Stuff( cString, At( "%26", cString ), 3, "&" )
   ENDDO

   DO WHILE "%2B" $ cString
      cString := Stuff( cString, At( "%2B", cString ), 3, "+" )
   ENDDO

   DO WHILE "%20" $ cString
      cString := Stuff( cString, At( "%20", cString ), 3, " " )
   ENDDO

   DO WHILE "%27" $ cString
      cString := Stuff( cString, At( "%27", cString ), 3, "'" )
   ENDDO

   DO WHILE "+" $ cString
      cString := Stuff( cString, At( "+", cString ), 1, " " )
   ENDDO

   DO WHILE "%2C" $ cString
      cString := Stuff( cString, At( "%2C", cString ), 3, "," )
   ENDDO

   DO WHILE "%21" $ cString
      cString := Stuff( cString, At( "%21", cString ), 3, "!" )
   ENDDO

   DO WHILE "%7E" $ cString
      cString := Stuff( cString, At( "%7E", cString ), 3, "~" )
   ENDDO

   DO WHILE "%23" $ cString
      cString := Stuff( cString, At( "%23", cString ), 3, "#" )
   ENDDO

   DO WHILE "%24" $ cString
      cString := Stuff( cString, At( "%24", cString ), 3, "!" )
   ENDDO

   DO WHILE "%25" $ cString
      cString := Stuff( cString, At( "%25", cString ), 3, "%" )
   ENDDO

   DO WHILE "%5E" $ cString
      cString := Stuff( cString, At( "%5E", cString ), 3, "^" )
   ENDDO

   DO WHILE "%28" $ cString
      cString := Stuff( cString, At( "%28", cString ), 3, "(" )
   ENDDO

   DO WHILE "%29" $ cString
      cString := Stuff( cString, At( "%29", cString ), 3, ")" )
   ENDDO

   DO WHILE "%60" $ cString
      cString := Stuff( cString, At( "%60", cString ), 3, "`" )
   ENDDO

   DO WHILE "%2F" $ cString
      cString := Stuff( cString, At( "%2F", cString ), 3, "/" )
   ENDDO

   RETURN cString

/****
*
*     HtmlJSCmd()
*
*     Inserts inline Javascript source
*
*/

PROCEDURE HtmlJSCmd( nH, cCmd )

   LOCAL cStr := ""

   __defaultNIL( @nH, HtmlPageHandle() )
   __defaultNIL( @cCmd, "" )

   cStr += '<script language=JavaScript 1.2>' + CRLF() + ;
      "<!--" + CRLF()
   cStr += cCmd + CRLF()
   cStr += "//-->" + CRLF() + ;
      "</script>" + CRLF()
   FWrite( nH, cStr )

   RETURN

/****
*
*     HtmlLinkStyle()
*
*/

FUNCTION HtmlLinkStyle( cHoverStyle, cHoverClr, cHoverBG, ;
      cLinkStyle, cLinkClr, cLinkBG )

   LOCAL cStr

   __defaultNIL( @cHoverStyle, "normal" )
   __defaultNIL( @cLinkStyle, "normal" )
   __defaultNIL( @cHoverClr, "white" )
   __defaultNIL( @cHoverBg, "black" )
   __defaultNIL( @cLinkClr, "black" )
   __defaultNIL( @cLinkBg, "white" )
   cStr := ;
      "<!-- A:hover {text-decoration:" + cHoverStyle + ";color:" + cHoverClr + ";background:" + cHoverBG + ;
      ";} A:link {text-decoration:" + cLinkStyle + ";color:" + cLinkClr + ";background:" + cLinkBG + ";}-->"

   RETURN cStr

/****
*
*     HtmlPadL()
*
*/

FUNCTION HtmlPadL( cStr, n )

   LOCAL cRet
   LOCAL nSpaces

   IF n == NIL
      RETURN cStr
   ENDIF

   nSpaces := n - Len( cStr )

   IF n <= 0
      cRet := Right( cStr, n )
   ELSE
      cRet := Replicate( _HTML_SPACE, nSpaces ) + cStr
   ENDIF

   RETURN cRet

/****
*
*     HtmlPadR()
*
*/

FUNCTION HtmlPadR( cStr, n )

   LOCAL cRet
   LOCAL nSpaces

   IF n == NIL
      RETURN cStr
   ENDIF

   nSpaces := n - Len( cStr )

   IF n <= 0
      cRet := Left( cStr, n )
   ELSE
      cRet := cStr + Replicate( _HTML_SPACE, nSpaces )
   ENDIF

   RETURN cRet

//

FUNCTION Any2Str( xVal )

   RETURN  HtmlAny2Str( xVal )

FUNCTION HtmlAny2Str( xVal )

   LOCAL xRet := NIL

   IF HB_ISSTRING( xVal )
      xRet := iif( Empty( xVal ), ".", xVal )

   ELSEIF HB_ISNUMERIC( xVal )
      xRet := hb_ntos( xVal )

   ELSEIF HB_ISOBJECT( xVal )
      xRet := "<" + xVal:CLASSNAME() + ">"

   ELSEIF HB_ISDATE( xVal )
      xRet := DToC( xVal )

   ELSEIF HB_ISLOGICAL( xVal )
      xRet := iif( xVal, "T", "F" )

   ELSEIF HB_ISBLOCK( xVal )
      xRet := "{||...}"

   ELSEIF ValType( xVal ) == NIL
      xRet := "NIL"

   ELSEIF ValType( xVal ) == "U"
      xRet := "<Unknown Value>"

   ENDIF

   RETURN xRet
