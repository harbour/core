/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Main HTML CLASS for HTMLLIB
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

#include "common.ch"
#include "hbclass.ch"
#include "cgi.ch"

STATIC snHtm   := NIL
STATIC scForm  := 0
STATIC soPage  := 0

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

CLASS THtml

   DATA nH
   DATA FName, TITLE
   DATA FontFace INIT "Verdana"
   DATA FontSize INIT 1
   DATA FontColor INIT "black"
   DATA aImages
   Data lCgi  INIT .F.
   Data cStr    Init ""
   DATA BaseURL, BaseTarget
   DATA lFont INIT .F.

   //METHOD New( cFile, cTitle, cLinkTitle, cCharSet, cScriptSRC, ;
               //BGIMAGE, BGCOLOR, txtColor, cJavaCode, ;
               //onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr, ;
               //cStyle, aimages, baseURL, baseTarget, ;
               //nRefresh, cRefreshURL, cStyleScr, lnocache )
   METHOD Newalt(cType)
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

   METHOD SetPageColor( cColor, lBody ) INLINE DEFAULT(lBody ,.T.),::cStr +=  IIF( lBody, '<BODY BGCOLOR="' + cColor + '">', ' BGCOLOR="' + cColor + '" ' )

   METHOD SetTextColor( cColor, lBody ) INLINE DEFAULT(lBody ,.T.),::cStr +=  IIF( lBody, '<BODY TEXT="' + cColor + '">', ' TEXT="' + cColor + '" ' )

   METHOD SetBgImage( cImage, lBody ) INLINE DEFAULT(lBody ,.T.),::cStr +=  IIF( lBody, '<BODY BACKGROUND="' + cImage + '">', ' BACKGROUND="' + cImage + '" ' )

   METHOD CLOSE()

   METHOD SetCenter( lOn ) INLINE ::cStr +=  IIF( lOn, "<CENTER>", "</CENTER>" )

   METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet )

   METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut )

   METHOD DefineFont( cFont, cType, nSize, cColor, lSet )

   METHOD ENDFONT()

   METHOD SAY( str, font, size, type, color, style )

   METHOD Qqout( c ) INLINE DEFAULT( c, "" ), ::cStr +=  c

   METHOD Qout( c ) INLINE DEFAULT( c, "" ), ::cStr +=  CRLF() + c + '<BR>' + CRLF()

   METHOD Write( c ) INLINE DEFAULT( c, "" ), ::cStr +=  c

   METHOD WriteLN( c ) INLINE DEFAULT( c, "" ), ::cStr +=  CRLF() + c + '<BR>' + CRLF()

   METHOD SayColor( t, c ) INLINE DEFAULT( t, "" ), DEFAULT( c, "black" ),;
                    ::cStr +=  '<FONT COLOR="' + c + '">' + t + '</FONT>'

   METHOD Space( n ) INLINE DEFAULT( n, 1 ), ::cStr +=  Replicate( "&nbsp;", n  )

   METHOD PutImage( cImage, nBorder, nHeight, cOnclick, cOnMsOver, cOnMsOut, ;
                    cName, cAlt, cTarget, nWidth, lBreak, ID, MAP, ALING, HSPACE )

   METHOD TEXT( cText, nCols, lWrap ) INLINE DEFAULT( lWrap, .T. ), DEFAULT( nCols, 80 ),;
              ::cStr +=  "<PRE" + IIF( nCols != NIL, ' COLS="' + hb_ntos( nCols ) + "'", "" ) + IIF( lWrap, " WRAP>", ">" ) + CRLF() + cText + CRLF() + "</PRE>" + CRLF()

   METHOD MultiCol( txt, cols, gutter, width ) INLINE DEFAULT( txt, "" ),;
                    DEFAULT( cols, 2 ),;
                    DEFAULT( gutter, 5 ),;
                    DEFAULT( width, 100 ),;
                    ::cStr +=  '<MULTICOL COLS="' + hb_ntos( cols ) + '" GUTTER="' + hb_ntos( gutter ) + '" WIDTH="' + hb_ntos( width ) + '">' ,;
                    ::cStr +=  txt ,;
                    ::cStr +=  "</MULTICOL>"

   METHOD PutHeading( cText, nWeight, lCentered )

   METHOD HLine( nSize, nWidth, lShade, cColor )

   METHOD PutParagraph() INLINE ::cStr +=  "<P> </P>" + CRLF()

   METHOD Paragraph( lStart, cAlign, cStyle )

   METHOD PutBreak() INLINE ::cStr +=  "<BR>" + CRLF()

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

   METHOD NewList() INLINE ::cStr +=  "<UL>" + CRLF()

   METHOD ListItem() INLINE ::cStr +=  "<LI> "

   METHOD EndList() INLINE ::cStr +=  "</UL> "

   METHOD NewForm( cMethod, cAction, cName )

   METHOD FormImage( cText, name, File )

   METHOD FormEdit( cType, cName, xValue, nSize )

   METHOD FormReset( cText )

   METHOD FormSubmit( cText )

   METHOD FormQOut( c ) INLINE ::cStr +=  c + '<BR>' + CRLF()

   METHOD FormQQOut( c ) INLINE ::cStr +=  c + CRLF()

   METHOD EndForm() INLINE ::cStr +=  CRLF() + "</FORM>" + CRLF()

   METHOD Pushbutton( cName, cCaption, cCgiApp, cOnClick, cOnFocus, cOnBlur, cOnMsOver, cOnMsOut, style, ID )

   METHOD endButton()

   METHOD Button( cName, cCaption, cOnClick, cCgiApp, cOnMsOver, cOnMsOut, style, ID )

   METHOD iFrame( name, src, border, marginwidth, marginheight, scrolling, align, WIDTH, HEIGHT )

   METHOD StartJava() INLINE ::cStr +=  '<SCRIPT LANGUAGE="JavaScript">' + CRLF() + "<!--" + CRLF()

   METHOD PutJavaSource( c ) INLINE ::cStr +=  Space( 5 ) + 'SRC="' + c + '"' + CRLF()

   METHOD PutJava( c ) INLINE ::cStr +=  Space( 5 ) + c + CRLF()

   METHOD EndJava() INLINE ::cStr +=  "                  //-->" + CRLF() + "</SCRIPT>" + CRLF()

   METHOD serverCode( c ) INLINE ::cStr +=  "<SERVER>" + Space( 9 ) + c + CRLF() + "</SERVER>" + CRLF()

   METHOD Fwrite( c ) INLINE Fwrite( ::nH, c )

   METHOD FWriteLN( c ) INLINE Fwrite( ::nH, c + CRLF() )

   METHOD Span( c, Style )

   METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, cOnclick, ;
                           cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText )

   METHOD Comment( cText )

   METHOD ADDoBJECT( cType, cClassid, cAling, cCode, lDisable, cCodeBase, ;
                     cName, nWidth, nHeight )

   METHOD ADDPARAM( cType, cValue )

   METHOD EndOBJect()

   METHOD PutLinkName( cName )

   METHOD NewMap( cName ) INLINE ::cStr +=  "<MAP NAME=" + cName + ">"


   METHOD MapArea( Shape, Alt, Coord, Url ) INLINE ;
          ::cStr +=  "<AREA  shape=" + Shape + " alt=" + alt + " coords=" + Coord + " href=" + Url + ">" + CRLF()

   METHOD EndMap() INLINE ::cStr +=  "</MAP>"

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
   nMarginWidth, nMarginLeft ,.T.,cFile)



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
   DEFAULT lCgi to .F.
   ::lCgi := lCgi
   if lCgi
      DEFAULT cTitle TO "New CGI-HTML page"
      DEFAULT cLinkTitle TO cTitle
      DEFAULT cRefreshURL TO ""
      DEFAULT cCharset TO "windows-1251"
      DEFAULT lNocache TO .f.
   else
      DEFAULT cFile TO "file1.htm"
      DEFAULT cTitle TO "New HTML page"
      DEFAULT cLinkTitle TO cTitle
      DEFAULT cRefreshURL TO ""
      DEFAULT cCharset TO "windows-1251"
      DEFAULT lNocache TO .f.
   endif


   ::nH    := STD_OUT
   ::Title := cTitle
   if lCgi
      ::FName := "cgiout.htm"
   else
      ::FName := cFile
   endif
   if lCgi
      ::cStr +=  'Content-Type: text/html' + CRLF() + CRLF()
      fwrite(::nh,::cStr)
   endif
   ::cStr:=''

   ::cStr +=  '<HTML>' + CRLF() + ;
           '<HEAD>' + CRLF() + ;
           '   <TITLE>' + cTitle + ' </TITLE>' + CRLF()

   IF cBaseURL != NIL
      ::cStr +=  "<BASE HREF='" + cBaseURL + "'"

      IF cBaseTarget != NIL
         ::cStr +=  " TARGET='" + cBaseTarget + "'"
      ENDIF

      ::cStr +=  ">" + CRLF()
   ENDIF
/* TOFIX: Luiz please review it
   ::cStr +=  '   <LINK TITLE="' + cLinkTitle + '"' + CRLF() + ;
           '                HREF="mailto:culik@sl.conex.net" >' + CRLF() + ;
           '   <META HTTP-EQUIV="Content-Type" content="text/html; charset=' + cCharset + '">' + CRLF() )
*/
   IF cStyleScr != NIL
      ::cStr +=  '   <LINK HREF="' + cStyleScr + '"' + " rel='STYLESHEET' type='text/css'>" + CRLF()
   ENDIF

   IF nRefresh != NIL
      ::cStr +=  [   <META HTTP-EQUIV="Refresh" CONTENT="] + hb_ntos( nRefresh ) + [; URL=] + cRefreshURL + [">]
   ENDIF

   IF lnocache
      ::cStr +=  [   <META HTTP-EQUIV="pragma" CONTENT="no-cache"> ]
   ENDIF

   IF aJsCode != NIL
      Aeval( aJsCode, { | e | HtmlJsCmd( ::nH, e ) } )
   ENDIF

   IF aScriptSrc != NIL

      FOR i := 1 TO Len( aScriptSrc )
         ::cStr +=  ;
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + aScriptSrc[ i ] + '"></SCRIPT>' + CRLF()
      NEXT

   ENDIF

   IF aServerSrc != NIL

      FOR i := 1 TO Len( aServerSrc )
         ::cStr +=  ;
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + aServerSrc[ i ] + '" RUNAT=SERVER></SCRIPT>' + CRLF()
      NEXT

   ENDIF

   // preload images...
   IF aImages != NIL
      ::aImages := aImages
      ::cStr +=  ;
              '<SCRIPT LANGUAGE="JavaScript">' + CRLF()
      ::cStr +=  '<!--' + CRLF()
      ::cStr +=  "if(document.images)" + CRLF()
      ::cStr +=  "{" + CRLF()
      FOR i := 1 TO Len( aImages )
         ::cStr +=  Space( 5 ) + aImages[ i, 1 ] + "=new Image(100,50);" + CRLF()
         ::cStr +=  Space( 5 ) + aImages[ i, 1 ] + '.src="' + aImages[ i, 2 ] + '";' + CRLF()
      NEXT
      ::cStr +=  "}" + CRLF()

      ::cStr +=  "" + CRLF()
      ::cStr +=  Space( 5 ) + [// Function to 'activate' images.] + CRLF()
      ::cStr +=  Space( 5 ) + "function imageOn(imgName) {" + CRLF()
      ::cStr +=  Space( 5 ) + "        if (document.images) {" + CRLF()
      ::cStr +=  Space( 5 ) + '            imgOn=eval(imgName + "on.src");' + CRLF()
      ::cStr +=  Space( 5 ) + '            document[imgName].src = imgOn;' + CRLF()
      ::cStr +=  Space( 5 ) + "        }" + CRLF()
      ::cStr +=  Space( 5 ) + "}" + CRLF()
      ::cStr +=  CRLF()
      ::cStr +=  Space( 5 ) + "// Function to 'deactivate' images." + CRLF()
      ::cStr +=  Space( 5 ) + "function imageOff(imgName) {" + CRLF()
      ::cStr +=  Space( 5 ) + "        if (document.images) {" + CRLF()
      ::cStr +=  Space( 5 ) + '            imgOff = eval(imgName + "off.src");' + CRLF()
      ::cStr +=  Space( 5 ) + '            document[imgName].src = imgOff;' + CRLF()
      ::cStr +=  Space( 5 ) + "        }" + CRLF()
      ::cStr +=  Space( 5 ) + "}" + CRLF()
      ::cStr +=  CRLF()
      ::cStr +=  Space( 5 ) + "// Function for 'pressed' images." + CRLF()
      ::cStr +=  Space( 5 ) + "function imagePress(imgName) {" + CRLF()
      ::cStr +=  Space( 5 ) + "        if (document.images) {" + CRLF()
      ::cStr +=  Space( 5 ) + '            imgPress = eval(imgName + "press.src");' + CRLF()
      ::cStr +=  Space( 5 ) + '            document[imgName].src = imgPress;' + CRLF()
      ::cStr +=  Space( 5 ) + "        }" + CRLF()
      ::cStr +=  Space( 5 ) + "}" + CRLF()
      ::cStr +=  CRLF()
      ::cStr +=  '//-->' + CRLF()
      ::cStr +=  '</SCRIPT>' + CRLF()

   ENDIF

   IF cStyle != NIL
      ::cStr +=  "<STYLE> " + cStyle + " </STYLE>" + CRLF()
   ENDIF

   ::cStr +=  ;
           '</HEAD>' + CRLF() + ;
           '<BODY'

   IF onLoad != NIL
      ::cStr +=  '   onLoad="' + onLoad + '"'
   ENDIF

   IF NOF != NIL
      ::cStr +=  '   NOF="' + nof + '"'
   ENDIF

   IF onUnLoad != NIL
      ::cStr +=  ' onUnload="' + onUnLoad + '"'
   ENDIF

   IF cLinkClr != NIL
      ::cStr +=  ' link="' + cLinkClr + '"'
   ENDIF

   IF cVLinkClr != NIL
      ::cStr +=  ' vlnk="' + cVLinkClr + '"'
   ENDIF

   IF cALinkClr != NIL
      ::cStr +=  ' alink="' + cALinkClr + '"'
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
      ::cStr +=  ' topMargin=' + hb_ntos( nMarginTop )
   ENDIF

   IF nMarginLeft != NIL
      ::cStr +=  ' LeftMargin=' + hb_ntos( nMarginLeft )
   ENDIF

   IF nMarginHeight != NIL
      ::cStr +=  ' MARGINHEIGHT=' + hb_ntos( nMarginHeight )
   ENDIF

   IF nMarginWidth != NIL
      ::cStr +=  ' MARGINWIDTH=' + hb_ntos( nMarginWidth )
   ENDIF

   ::cStr +=  '>'

   ::cStr +=  CRLF()

   snHtm := ::nH

   soPage := Self

   RETURN self

METHOD NewAlt(cType) CLASS THtml

   ::nH    := STD_OUT
   ::cStr +=  'Content-Type: '+cType + CRLF() + CRLF()

   snHtm := ::nH

   soPage := Self

   RETURN self


/****
*
*     THtml():SetFont()
*
*     obvious...
*/

METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := CRLF() + '<FONT'

   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cColor TO ::fontColor
   DEFAULT lset TO IIF( cFont != NIL, .t., .f. )

   IF cFont != NIL
      cStr += ' FACE="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF nSize != NIL
      cStr += ' SIZE="' + Ltrim( Str( nSize ) ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF

   ENDIF

   IF cColor != NIL
      cStr += ' COLOR= "' + cColor + '">'

      IF lset
         ::fontColor := cColor
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF lBold != NIL
      IIF( lBold, cStr += '<B>', cStr += '</B>' )
   ENDIF

   IF lItalic != NIL
      IIF( lItalic, cStr += '<I>', cStr += '</I>' )
   ENDIF

   IF lULine != NIL
      IIF( lULine, cStr += '<U>', cStr += '</U>' )
   ENDIF

   cStr += '</FONT>'
   ::cStr +=  cStr + CRLF()
   RETURN Self

/****
*
*     THtml():StartFont()
*
*     Begin a font definition. They may be nested but make sure you
*     end the definition appropriately later
*/

METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor, lSet, lPut ) CLASS THtml

   LOCAL cStr := "<FONT "
   DEFAULT lSet TO .t.
   DEFAULT lPut TO .f.
   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cColor TO ::fontColor

   IF cFont != NIL
      cStr += ' FACE="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF lPut

      IF nSize != NIL
         cStr += ' SIZE="' + Ltrim( Str( nSize ) ) + '"'

         IF lSet
            ::fontSize := nSize
         ENDIF

      ENDIF

      IF cColor != NIL
         cStr += ' COLOR= "' + cColor + '">'

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
      IIF( lBold, cStr += '<B>', cStr += '</B>' )
   ENDIF

   IF lItalic != NIL
      IIF( lItalic, cStr += '<I>', cStr += '</I>' )
   ENDIF

   IF lULine != NIL
      IIF( lULine, cStr += '<U>', cStr += '</U>' )
   ENDIF

   ::cStr +=  cStr + CRLF()

   RETURN Self

/****
*
*     THtml():DefineFont()
*
*     Begin a font definition by font type "name".
*     Use ::endFont() to cancel this font
*/

METHOD DefineFont( cFont, cType, nSize, cColor, lSet ) CLASS THtml

   LOCAL cStr := "<FONT "

   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cColor TO ::fontColor
   DEFAULT lset TO IIF( cFont != NIL, .t., .f. )

   IF cFont != NIL
      cStr += ' FACE="' + cFont + '"'

      IF lSet
         ::fontFace := cFont
      ENDIF

   ENDIF

   IF nSize != NIL
      cStr += ' SIZE="' + Ltrim( Str( nSize ) ) + '"'

      IF lSet
         ::fontSize := nSize
      ENDIF

   ENDIF

   IF cColor != NIL
      cStr += ' COLOR= "' + cColor + '">'

      IF lset
         ::fontColor := cColor
      ENDIF

   ELSE
      cStr += ">"
   ENDIF

   IF cType != NIL
      cStr += cType
   ENDIF

   ::cStr +=  cStr + CRLF()

   RETURN Self

/****
*
*     THtml():EndFont()
*
*     End a font definition
*/

METHOD ENDFONT() CLASS THtml

   ::cStr +=  '</font>' + CRLF()

   RETURN Self

/****
*
*     THtml():say()
*
*
*
*/

METHOD SAY( str, font, size, type, color, style ) CLASS THtml

   LOCAL cOut    := ""
   LOCAL lBold   := .F.
   LOCAL lItalic := .F.
   LOCAL lULine  := .F.
   LOCAL lEm     := .f.
   LOCAL lStrong := .f.
   LOCAL nSize   := Size
   DEFAULT str TO ""
   DEFAULT FONT TO ::FontFace
   DEFAULT size TO ::FontSize
   DEFAULT COLOR TO ::FontColor

   IF FONT != NIL .or. Size != NIL .or. COLOR != NIL
      cOut := '<FONT ' + IIF( font != NIL, 'FACE="' + font + '"', '' ) + IIF( color != NIL, ' COLOR=' + color, '' ) + IIF( nSize != NIL, ' SIZE=' + hb_ntos( size ), "" )

      IF Style != NIL
         cOut += '" Style="' + style + '">'
      ELSE
         cOut += '>'
      ENDIF

   ENDIF

   IF HB_ISSTRING( type )

      IF "<" $ type

         IF "<B>" $ type
            lBold := .T.
            cOut  += "<B>"
         ENDIF

         IF "<I>" $ type
            lItalic := .T.
            cOut    += "<I>"
         ENDIF

         IF "<U>" $ type
            lULine := .T.
            cOut   += "<U>"
         ENDIF

         IF "<EM>" $ type
            lEm  := .T.
            cOut += "<EM>"
         ENDIF

         IF "<STRONG>" $ type
            lStrong := .T.
            cOut    += "<STRONG>"
         ENDIF

      ENDIF

   ENDIF

   cOut += str

   IF lBold
      cOut += "</B>"
   ENDIF

   IF lItalic
      cOut += "</I>"
   ENDIF

   IF lULine
      cOut += "</U>"
   ENDIF

   IF lStrong
      cOut += "</STRONG>"
   ENDIF

   IF lEm
      cOut += "</EM>"
   ENDIF

   IF FONT != NIL .or. Size != NIL .or. COLOR != NIL
      cOut += "</FONT>"
   ENDIF

   ::cStr +=  cOut + CRLF()

   RETURN Self

/****
*
*     THtml():paragraph()
*
*
*
*/

METHOD Paragraph( lStart, cAlign, cStyle ) CLASS THtml

   LOCAL cStr := "<P"

   DEFAULT lStart TO .T.
   DEFAULT cAlign TO "LEFT"

   IF lStart
      cStr := "<P ALIGN='" + cAlign + "'"

      IF cStyle != NIL
         cStr += ' STYLE="' + cStyle + '"'
      ENDIF

      cStr += ">"
   ELSE
      cStr := "</P>"
   ENDIF

   cStr += CRLF()
   ::cStr +=  cStr
   RETURN Self

/****
*
*     THtml():HLine()
*
*     Put a Horizontal line
*/

METHOD HLine( nSize, nWidth, lShade, cColor ) CLASS THtml

   DEFAULT nSize TO 3
   DEFAULT nWidth TO 100
   DEFAULT lShade TO .T.

   IF lShade
      ::cStr +=  CRLF() + ;
              '<HR SIZE = ' + hb_ntos( nSize ) + IIF( cColor != NIL, " COLOR  " + cColor, "" ) + ' WIDTH = ' + hb_ntos( nWidth ) + '%>' + ;
              CRLF()
   ELSE
      ::cStr +=  CRLF() + ;
              '<HR NOSHADE SIZE = ' + hb_ntos( nSize ) + IIF( cColor != NIL, " COLOR  " + cColor, "" ) + ' WIDTH = ' + hb_ntos( nWidth ) + '%>' + ;
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

   DEFAULT nWeight TO 3
   DEFAULT lCentered TO .F.

   IF lCentered
      ::cStr +=  "<CENTER>"
   ENDIF

   ::cStr +=  "<H" + hb_ntos( nWeight ) + ">" + cText + "</H" + hb_ntos( nWeight ) + ">" + CRLF()

   IF lCentered
      ::cStr +=  "</CENTER>"
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
   DEFAULT cUrl TO ""
   DEFAULT bld TO .F.
   DEFAULT lBreak TO .f.

   ::cStr +=  ;
           '<A HREF="' + cUrl + '"' + CRLF()

   IF cOnClick != NIL
      ::cStr +=  ;
              Space( 5 ) + 'onClick="' + cOnClick + '"' + CRLF()
   ENDIF
   IF cOnMsOver != NIL
      ::cStr +=  ;
              Space( 5 ) + 'onMouseOver="' + cOnMsOver + '"' + CRLF()
   ENDIF
   IF cOnMsOut != NIL
      ::cStr +=  ;
              Space( 5 ) + 'onMouseOut="' + cOnMsOut + '"' + CRLF()
   ENDIF

   IF cTarget != NIL
      ::cStr +=  ;
              Space( 5 ) + 'TARGET=' + cTarget + CRLF()
   ENDIF

   IF cClass != NIL
      ::cStr +=  ;
              Space( 5 ) + 'CLASS=' + cClass + CRLF()
   ENDIF

   IF bld
      cStr += "<B>" + CRLF()
   ENDIF

   IF FONT != NIL .or. clr != NIL .or. size != NIL .or. style != NIL
      //    cStr +=" Font" +valtype(font)+"color"+valtype(clr)+"size"+valtype(size)+"style"+valtype(style)
      cStr += " <FONT " + CRLF()

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

   IF FONT != NIL .or. clr != NIL .or. size != NIL .or. style != NIL
      cStr += '>' + cText
   ELSE
      cStr += cText
   ENDIF

   ::cStr +=  ;
           '>' + cStr
   IF FONT != NIL .or. clr != NIL .or. size != NIL .or. style != NIL
      ::cStr +=  ;
              '</font>'
   ENDIF

   IF bld
      ::cStr +=  ;
              '</B>'
   ENDIF

   ::cStr +=  ;
           '</A>' + IIF( lBreak, '<br>' + CRLF(), CRLF() )

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
   default lbreak TO .F.

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"' + CRLF()
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"' + CRLF()
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + IIF( HB_ISNUMERIC( nBorder ), hb_ntos( nBorder ), nBorder ) + CRLF()
   ENDIF

   IF nHeight != NIL .and. HB_ISNUMERIC( nHeight )
      cStr += " height = " + hb_ntos( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .and. HB_ISSTRING( nHeight )
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .and. HB_ISNUMERIC( nWidth )
      cStr += " width = " + hb_ntos( nWidth ) + " " + CRLF()
   ELSEIF nWidth != NIL .and. HB_ISSTRING( nWidth )
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
      cStr += ' TARGET=' + cTarget + CRLF()
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

   ::cStr +=  ;
           '<A HREF=' + cUrl + IIF( cClass != NIL, ' class="' + cClass + '"', "" ) + '><IMG SRC="' + cImage + '"' + ;
           cStr + '></A>' + IIF( lBreak, '<br>' + CRLF(), "" )

   RETURN Self

METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
                           cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget, nWidth, lbreak, cClass, cText ) CLASS THtml
   LOCAL cStr := ""
   default lbreak TO .F.
   IF cName != NIL
      cStr += ' NAME= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL
      cStr += " border = " + hb_ntos( nBorder )
   ENDIF

   IF nHeight != NIL .and. HB_ISNUMERIC( nHeight )
      cStr += " height = " + hb_ntos( nHeight ) + " "
   ELSEIF nHeight != NIL .and. HB_ISSTRING( nHeight )
      cStr += " height = " + nHeight + " "
   ENDIF

   IF nWidth != NIL .and. HB_ISNUMERIC( nWidth )
      cStr += " width = " + hb_ntos( nWidth ) + " "
   ELSEIF nWidth != NIL .and. HB_ISSTRING( nWidth )
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
      cStr += ' TARGET=' + cTarget
   ENDIF

   ::cStr +=  ;
           '<A HREF=' + cUrl + IIF( cClass != NIL, ' class="' + cClass + '"', "" ) + '>' + cText + '<IMG SRC="' + cImage + '"' + ;
           cStr + '></A>' + IIF( lBreak, '<br>' + CRLF(), "" )

   RETURN Self

/****
*
*     THtml():putImage()
*
*     Put an Image.
*/

METHOD PutImage( cImage, nBorder, nHeight, ;
                    cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget,;
                    nWidth, lbreak, Id, Map, Aling, hSpace ) CLASS THtml
   LOCAL cStr := ""
   default lbreak TO .F.

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"'
   ENDIF

   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"'
   ENDIF

   IF nBorder != NIL .and. HB_ISNUMERIC( nBorder )
      cStr += " BORDER = " + hb_ntos( nBorder )
   ELSEIF nBorder != NIL .and. HB_ISSTRING( nBorder )
      cStr += " BORDER = " + '"' + nBorder + '"'
   ENDIF

   IF nHeight != NIL .and. HB_ISNUMERIC( nHeight )
      cStr += " HEIGHT = " + hb_ntos( nHeight ) + " "
   ELSEIF nHeight != NIL .and. HB_ISSTRING( nHeight )
      cStr += " HEIGHT = " + '"' + nHeight + '"'
   ENDIF

   IF nWidth != NIL .and. HB_ISNUMERIC( nWidth )
      cStr += " width = " + hb_ntos( nWidth ) + " "
   ELSEIF nWidth != NIL .and. HB_ISSTRING( nWidth )
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

   IF Map != NIL
      cStr += " usemap=" + Map
   ENDIF

   IF cTarget != NIL
      cStr += ' TARGET="' + cTarget + '"'
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

   ::cStr +=  ;
           '<IMG SRC="' + cImage + '"' + ;
           cStr + '>' + IIF( lBreak, "<br>" + CRLF(), "" )

   RETURN Self

/****
*
*     THtml():Close()
*
*     Close an HTML disk file
*
*/

METHOD CLOSE() CLASS THtml
   //::cStr +=  ::cStr
   ::cStr +=  "</body>" + CRLF()
   ::cStr +=  "</html>" + CRLF()

   FWrite(::nh,::cStr)

   IF !::lCgi
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

   LOCAL cStr  := CRLF() + CRLF() + "<TABLE "
   LOCAL xCols := nCols

   DEFAULT l3d TO .T.
   DEFAULT lRuleCols TO .F.
   DEFAULT lRuleRows TO .F.

   IF ColorFore != NIL
      cStr += " bordercolor=" + ColorFore + ' '
   ENDIF

   IF Colorbg != NIL
      cStr += " bgcolor=" + ColorBG + ' '
   ENDIF

   cStr += IIF( nBorder = NIL, "border ", "border=" + hb_ntos( nBorder ) + ' ' )

   IF ncellpadding != NIL
      cStr += ' CellPadding=' + hb_ntos( nCellPadding )
   ENDIF

   IF nCellSpacing != NIL
      cStr += ' CellSpacing=' + hb_ntos( nCellSpacing )
   ENDIF

   IF cAling != NIL
      cStr += ' aling=' + '"' + cAling + '"'
   ENDIF

   cStr += IIF( xCols != NIL, " COLS=" + hb_ntos( nCols ), "" )

   IF nWidth != NIL .and. HB_ISNUMERIC( nWidth )
      cStr += " WIDTH=" + hb_ntos( nWidth )
   ELSEIF nWidth != NIL .and. HB_ISSTRING( nWidth )
      cStr += " WIDTH=" + '"' + nWidth + '"'
   ENDIF

   IF nHeight != NIL .and. HB_ISNUMERIC( nHeight )
      cStr += " HEIGHT=" + hb_ntos( nHeight )
   ELSEIF nHeight != NIL .and. HB_ISSTRING( nHeight )
      cStr += " HEIGHT=" + '"' + nHeight + '"'
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

   IF lRuleCols == .T.
      cStr += " RULES=COLS"
   ELSEIF lRuleRows == .T.
      cStr += " RULES=ROWS"
   ELSEIF lRules == .T.
      cStr += " RULES=ALL"
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
      cStr += ' NOF="' + NOF + '"'
   ENDIF

   cStr += ">" + CRLF()

   ::cStr +=  cStr + CRLF()

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

   LOCAL cStr := Space( 3 ) + "<TH"

   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cFntColor TO ::fontColor

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

   cStr += cHead + IIF( cFont != NIL, '</font>', "" ) + "</th>" + CRLF()

   ::cStr +=  cStr

   RETURN Self

/****
*
*     THtml():NewTableRow()
*
*     Start a table row definition.
*
*/

METHOD NewTableRow( cColor, vAling, aLing ) CLASS THtml

   LOCAL cStr := Space( 5 ) + "<TR"

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
   ::cStr +=  cStr
   RETURN Self

/****
*
*     THtml():EndTableRow()
*
*     End a table row definition.
*
*/

METHOD EndTableRow() CLASS THtml

   ::cStr +=  Space( 5 ) + "</TR>" + CRLF()
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

   LOCAL cStr := Space( 10 ) + "<TD"
   LOCAL cAli := cAlign
   DEFAULT lNoFont TO .T.
   DEFAULT cFont TO ::fontFace
   DEFAULT nSize TO ::fontSize
   DEFAULT cFntColor TO ::fontColor
   DEFAULT cAlign TO "LEFT"
   DEFAULT lWrap TO .T.

   IF cBdrClr != NIL
      cStr += " BORDERCOLOR=" + cBdrClr
   ENDIF

   IF cColor != NIL
      cStr += " BGCOLOR=" + cColor
   ENDIF

   IF cAlign != NIL .and. caLi != NIL
      cStr += " ALIGN=" + cAlign
   ENDIF

   IF cValign != NIL
      cStr += " VALIGN=" + cValign
   ENDIF

   IF nHeight != NIL .and. HB_ISNUMERIC( nHeight )
      cStr += " HEIGHT=" + hb_ntos( nHeight )
   ELSEIF nHeight != NIL .and. HB_ISSTRING( nHeight )
      cStr += " HEIGHT=" + '"' + nHeight + '"'
   ENDIF

   IF cBgPic != NIL
      cStr += " BACKGROUND=" + '"' + cBgPic + '"'
   ENDIF

   IF nWidth != NIL .and. HB_ISNUMERIC( nWidth )
      cStr += " WIDTH=" + hb_ntos( nWidth )
   ELSEIF nWidth != NIL .and. HB_ISSTRING( nWidth )
      cStr += " WIDTH=" + '"' + nWidth + '"'
   ENDIF

   IF nColspan != NIL .and. HB_ISNUMERIC( nColspan )
      cStr += " COLSPAN=" + hb_ntos( nColspan )
   ELSEIF nColspan != NIL .and. HB_ISSTRING( nColspan )
      cStr += " COLSPAN=" + '"' + nColspan + '"'
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

   IF nRowspan != NIL .and. HB_ISNUMERIC( nRowspan )
      cStr += " ROWSPAN=" + hb_ntos( nRowspan )
   ELSEIF nRowspan != NIL .and. HB_ISSTRING( nRowspan )
      cStr += " ROWSPAN=" + '"' + nRowspan + '"'
   ENDIF

   IF lWrap == .F.
      cStr += " NOWRAP"
   ENDIF

   cStr += ">"

   IF !lNoFont
      cStr += '<FONT '

      IF nSize != NIL
         cStr += 'SIZE=' + hb_ntos( nSize )
      ENDIF

      IF cFntColor != NIL
         cStr += ' COLOR=' + cFntColor
      ENDIF

      IF !Empty( cFont )
         cStr += ' FACE="' + cFont + '"' + ">"
      ELSE
         cStr += ">"
      ENDIF

      ::lFont := .t.
   ENDIF

   ::cStr +=  cStr
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
      ::cStr +=  "</font></td>" + CRLF()
   ELSE
      ::cStr +=  "</td>" + CRLF()
   ENDIF

   ::lFont := .f.
   RETURN Self

/****
*
*     THtml():EndTable()
*
*     End a table definition.
*/

METHOD EndTable() CLASS THtml

   ::cStr +=  "</table>" + CRLF()
   ::cStr +=  CRLF() + CRLF() + CRLF()
   RETURN Self


/****
*
*     THtml():NewForm()
*
*     Creates a new form
*
*/

METHOD NewForm( cMethod, cAction, cName ) CLASS THtml

   DEFAULT cMethod TO "POST"
   DEFAULT cName TO "newForm"

   ::cStr +=  CRLF() + "<FORM"

   IF cMethod != NIL
      ::cStr +=  ' METHOD="' + cMethod + '"'
   ENDIF

   IF cName != NIL
      ::cStr +=  ' NAME="' + cName + '"'
   ENDIF

   IF cAction != NIL
      ::cStr +=  ' ACTION="' + cAction + '"'
   ENDIF

   ::cStr +=  '>' + CRLF()

   scForm := cName

   RETURN Self

/****
*
*     THtml():FormEdit()
*
*     Adds a form edit field
*
*/

METHOD FormEdit( cType, cName, xValue, nSize ) CLASS THtml

   DEFAULT cType TO "edit"

   ::cStr +=  '<INPUT Type="' + cType + '"'

   IF cName != NIL
      ::cStr +=  ' Name="' + cName + '"'
   ENDIF

   IF xValue != NIL
      ::cStr +=  ' Value="' + htmlany2sTR( xValue ) + '"'
   ENDIF

   IF nSize != NIL
      ::cStr +=  ' Size="' + htmlany2sTR( nSize ) + '"'
   ENDIF

   ::cStr +=  ">"

   RETURN Self

/****
*
*     THtml():FormSubmit()
*
*     Adds a form submit button
*
*/

METHOD FormSubmit( cText ) CLASS THtml

   ::cStr +=  '<INPUT Type="submit" Value="' + cText + '">' + CRLF()

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

   ::cStr +=  '<INPUT TYPE="IMAGE" NAME="' + name + '" SRC="' + file + '">' + CRLF()

   RETURN Self

/****
*
*     THtml():FormReset()
*
*     Adds a reset button
*
*/

METHOD FormReset( cText ) CLASS THtml

   ::cStr +=  '<INPUT Type="Reset" Value="' + cText + '">' + CRLF()

   RETURN Self

/****
*
*     THtml():pushButton()
*
*     Insert a standalone push button and assign an action to it
*     Either pass onClick or cCgiApp - not both
*/

METHOD Pushbutton( cName, cCaption, ;
                      cCgiApp, ;
                      cOnClick, ;
                      cOnFocus, cOnBlur, ;
                      cOnMsOver, cOnMsOut, ;
                      style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<INPUT TYPE=BUTTON " + CRLF()

   DEFAULT cOnMsOver TO "window.status=this.name;"
   DEFAULT cOnMsOut TO "window.status='';"

   IF cName != NIL
      cStr += "        NAME=" + cName
   ENDIF

   IF cCaption != NIL
      cStr += "       VALUE=" + cCaption
   ENDIF

   IF style != NIL
      cStr += '       STYLE="' + style + '"'
   ENDIF

   IF ID != NIL
      cStr += '          ID="' + ID + '"'
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

   ::cStr +=  cStr + ">"

   RETURN Self

/****
*
*     THtml():Button()
*
*     Insert a standalone <BUTTON> push button and assign an action to it
*
*/

METHOD Button( cName, cCaption, ;
                  cOnClick, ;
                  cCGIApp, ;
                  cOnMsOver, cOnMsOut, ;
                  Style, ID ) CLASS THtml

   LOCAL cStr := CRLF() + "<BUTTON " + CRLF()

   DEFAULT cOnMsOver TO "window.status=this.name;"
   DEFAULT cOnMsOut TO "window.status='';"

   IF cName != NIL
      cStr += "        NAME=" + cName
   ENDIF

   IF cCaption != NIL
      cStr += "       TITLE=" + cCaption
   ENDIF

   IF style != NIL
      cStr += '       STYLE="' + style + '"'
   ENDIF

   IF ID != NIL
      cStr += '          ID="' + ID + '"'
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

   ::cStr +=  cStr + ">" + CRLF()

   RETURN Self

/****
*
*     THtml():EndButton()
*
*     End a <BUTTON> definition
*
*/

METHOD EndButton() CLASS THtml

   ::cStr +=  CRLF() + CRLF() + "</BUTTON>" + CRLF()
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

   DEFAULT cFont TO "Verdana"
   DEFAULT cFntColor TO "white"
   DEFAULT nFntSize TO 3
   DEFAULT cAlign TO "middle"
   DEFAULT nWidth TO 100
   DEFAULT cText TO ""
   DEFAULT cBgColor TO "black"
   DEFAULT cBehavior TO "scroll"
   DEFAULT cDirection TO "left"
   DEFAULT nScrollAmt TO 5
   DEFAULT nScrolldelay TO 2
   DEFAULT LOOP TO 0

   ::StartFont( cFont,,,, nFntSize, cFntColor )

   ::cStr +=  '<MARQUEE align="' + cAlign + '" '
   ::cStr +=  'behavior="' + cBehavior + '" '
   ::cStr +=  'width="' + hb_ntos( nWidth ) + '%" '
   ::cStr +=  IIF( nHeight != NIL, 'height=' + hb_ntos( nHeight ) + " ", "" )
   ::cStr +=  'bgColor="' + cBgColor + '" '
   ::cStr +=  'scrollamount="' + hb_ntos( nScrollAmt ) + '" '
   ::cStr +=  'scrolldelay="' + hb_ntos( nScrollDelay ) + '" '
   ::cStr +=  'loop=' + IIF( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + ' '
   ::cStr +=  'direction="' + cDirection + '" '
   ::cStr +=  IIF( onMsOver != NIL, 'onMouseOver="' + onMsOver + '" ', "" )
   ::cStr +=  IIF( onMsOut != NIL, 'onMouseOut="' + onMsOut + '" ', "" )
   ::cStr +=  IIF( onClick != NIL, 'onClick="' + onClick + '" ', "" )
   ::cStr +=  IIF( onStart != NIL, 'onStart="' + onStart + '" ', "" )
   ::cStr +=  IIF( onFinish != NIL, 'onFinish="' + onFinish + '" ', "" )
   ::cStr +=  '>'
   ::cStr +=  cText

   ::cStr +=  "</MARQUEE>" + CRLF()
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

   DEFAULT cFont TO "Verdana"
   DEFAULT cFntColor TO "white"
   DEFAULT nFntSize TO 3
   DEFAULT cAlign TO "middle"
   DEFAULT nWidth TO 100
   DEFAULT cBgColor TO "black"
   DEFAULT cBehavior TO "scroll"
   DEFAULT cDirection TO "left"
   DEFAULT nScrollAmt TO 5
   DEFAULT nScrolldelay TO 2

   ::StartFont( cFont,,,, nFntSize, cFntColor )

   cStr += '<MARQUEE align="' + cAlign + '" ' + ;
      'behavior="' + cBehavior + '" ' + ;
      'width="' + hb_ntos( nWidth ) + '%" ' + ;
      IIF( nHeight != NIL, 'height=' + hb_ntos( nHeight ) + " ", "" ) + ;
      'bgColor="' + cBgColor + '" ' + ;
      'scrollamount="' + hb_ntos( nScrollAmt ) + '" ' + ;
      'scrolldelay="' + hb_ntos( nScrollDelay ) + '" ' + ;
      'loop=' + IIF( HB_ISNUMERIC( loop ), hb_ntos( loop ), loop ) + ' ' + ;
      'direction="' + cDirection + '" ' + ;
      IIF( onMsOver != NIL, 'onMouseOver="' + onMsOver + '" ', "" ) + ;
      IIF( onMsOut != NIL, 'onMouseOut="' + onMsOut + '" ', "" ) + ;
      IIF( onClick != NIL, 'onClick="' + onClick + '" ', "" ) + ;
      IIF( onStart != NIL, 'onStart="' + onStart + '" ', "" ) + ;
      IIF( onFinish != NIL, 'onFinish="' + onFinish + '" ', "" ) + ;
      '>' + ;
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

   ::cStr +=  "</MARQUEE>" + CRLF()
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

   LOCAL cStr := "<IFRAME " + CRLF()

   DEFAULT BORDER TO .T.
   DEFAULT name TO "Frame01"
   //DEFAULT align  := "vertical"

   IF name != NIL
      cStr += Space( 5 ) + '        NAME="' + name + '"' + CRLF()
   ENDIF
   IF src != NIL
      cStr += Space( 5 ) + '         SRC="' + src + '"' + CRLF()
   ENDIF

   IF BORDER
      cStr += Space( 5 ) + " FRAMEBORDER='1'" + CRLF()
   ELSE
      cStr += Space( 5 ) + " FRAMEBORDER='0'" + CRLF()
   ENDIF

   IF scrolling
      cStr += Space( 5 ) + "   SCROLLING='yes'" + CRLF()
   ELSE
      cStr += Space( 5 ) + "   SCROLLING='no'" + CRLF()
   ENDIF

   IF marginwidth != NIL
      cStr += Space( 5 ) + " MARGINWIDTH='" + hb_ntos( marginWidth ) + "'" + CRLF()
   ENDIF

   IF marginheight != NIL
      cStr += Space( 5 ) + "MARGINHEIGHT='" + hb_ntos( marginheight ) + "'" + CRLF()
   ENDIF

   IF WIDTH != NIL
      cStr += Space( 5 ) + "       WIDTH='" + hb_ntos( Width ) + "'" + CRLF()
   ENDIF

   IF HEIGHT != NIL
      cStr += Space( 5 ) + "      HEIGHT='" + hb_ntos( height ) + "'" + CRLF()
   ENDIF

   IF align != NIL
      cStr += Space( 5 ) + "       ALIGN='" + align + "'" + CRLF()
   ENDIF

   cStr += ">" + CRLF()
   cStr += "</IFRAME>" + CRLF()

   ::cStr +=  cStr

   RETURN Self
/*   New    Methods   */
METHOD Span( c, Style ) CLASS THtml

   LOCAL cStr := "<Span "
   IF style != NIL
      cStr += ' style ="' + Style + '"'
   ENDIF
   cStr += ">" + c + '</span>'
   ::cStr +=  cStr
   RETURN Self

METHOD Comment( cText ) CLASS THtml

   LOCAL cStr := CRLF() + "<!-- "
   cStr += cText + " -->"
   ::cStr +=  cStr
   RETURN Self

METHOD AddObject( cType, cClassid, cAling, cCode, lDisable, cCodeBase, cName, nWidth, nHeight ) CLASS THtml

   LOCAL cStr := "<Object "

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

   IF nHeight != NIL .and. HB_ISNUMERIC( nHeight )
      cStr += " height = " + hb_ntos( nHeight ) + " " + CRLF()
   ELSEIF nHeight != NIL .and. HB_ISSTRING( nHeight )
      cStr += " height = " + nHeight + " " + CRLF()
   ENDIF

   IF nWidth != NIL .and. HB_ISNUMERIC( nWidth )
      cStr += " width = " + hb_ntos( nWidth ) + " " + CRLF()
   ELSEIF nWidth != NIL .and. HB_ISSTRING( nWidth )
      cStr += " width = " + nWidth + " " + CRLF()
   ENDIF

   cStr += " >"
   ::cStr +=  cStr + CRLF()

   RETURN Self

METHOD EndObject() CLASS THtml

   ::cStr +=  "</OBJECT>" + CRLF()

   RETURN Self

METHOD ADDPARAM( cType, cValue ) CLASS THtml

   ::cStr +=  '<param name="' + cType + '" value="' + cValue + '">' + CRLF()

   RETURN Self

METHOD PutLinkName( cName ) CLASS THtml

   LOCAL cStr := '<a name="' + cName + '"></a>'

   ::cStr +=  cStr

   RETURN Self

/****
*
*     HtmlPageHandle()
*
*     Returns the current HTML page handle
*
*/

FUNCTION HtmlPageHandle()

   RETURN snHtm

/****
*
*     HtmlFormName()
*
*     Returns the current ( or last ) form name
*
*/

FUNCTION HtmlFormName()

   RETURN scForm

/****
*     HtmlPageObject()
*
*     Return the current THtml() object.
*
*/

FUNCTION HtmlPageObject()

   RETURN soPage

/****
*
*     HtmlDecodeURL()
*
*     Decodes a URL encoded string. Also handles international charsets.
*
*/

FUNCTION HtmlDecodeURL( cString )

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
*     HtmlJsCmd()
*
*     Inserts inline Javascript source
*
*/

PROCEDURE HtmlJsCmd( nH, cCmd )
   Local cStr := ""
   DEFAULT nH TO HtmlPageHandle()
   DEFAULT cCmd TO ""

   cStr += '<SCRIPT LANGUAGE=JavaScript 1.2>' + CRLF() + ;
           "<!--" + CRLF()
   cStr += cCmd + CRLF()
   cStr += "//-->" + CRLF() + ;
           "</SCRIPT>" + CRLF()
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
   DEFAULT cHoverStyle TO "normal"
   DEFAULT cLinkStyle TO "normal"
   DEFAULT cHoverClr TO "white"
   DEFAULT cHoverBg TO "black"
   DEFAULT cLinkClr TO "black"
   DEFAULT cLinkBg TO "white"
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
      cRet := LEFT( cStr, n )
   ELSE
      cRet := cStr + Replicate( _HTML_SPACE, nSpaces )
   ENDIF

   RETURN cRet


//----------------------------------------------------------------------------
function ANY2STR( xVal )
return  HTMLANY2STR( xVal )

FUNCTION HTMLANY2STR( xVal )

   LOCAL xRet := NIL

   IF HB_ISSTRING( xVal )
      xRet := IIF( Empty( xVal ), ".", xVal )

   ELSEIF HB_ISNUMERIC( xVal )
      xRet := Alltrim( Str( xVal ) )

   ELSEIF HB_ISOBJECT( xVal )
      xRet := "<" + xVal:CLASSNAME() + ">"

   ELSEIF ISDATE( xVal )
      xRet := Dtoc( xVal )

   ELSEIF HB_ISLOGICAL( xVal )
      xRet := iif( xVal, "T", "F" )

   ELSEIF HB_ISBLOCK( xVal )
      xRet := "{||...}"

   ELSEIF Valtype( xVal ) == NIL
      xRet := "NIL"

   ELSEIF Valtype( xVal ) == "U"
      xRet := "<Unknown Value>"

   ENDIF

RETURN xRet
