/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Main HTML CLASS for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
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
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "default.ch"
#include "hbclass.ch"
#include "html.ch"

STATIC saGreek := {}
STATIC snHtm   := NIL //0
STATIC scForm  := 0
STATIC soPage  := 0


/****
*
*     Class HTML()
*
*
*     Constructors :
*
*     Html():New()          Creates a new HTML document
*
*     Html():CGINew()       Creates a new CGI-HTML document
*
*/

CLASS Html

DATA nH
DATA FName, Title
DATA FontFace     INIT "Verdana"    // Note!
DATA FontSize     INIT 1
DATA FontColor    INIT "black"
DATA aImages
DATA BaseURL, BaseTarget
DATA lFont  init .F.

// --- INIT --- //
METHOD New( cFile, cTitle, cLinkTitle, cCharSet, cScriptSRC,;
            bgImage, bgColor, txtColor, cJavaCode,;
            onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr,;
            cStyle, aimages, baseURL, baseTarget,;
            nRefresh, cRefreshURL ,cStyleScr,lnocache)
METHOD CGINew( cTitle, cLinkTitle, cCharSet, cScriptSRC, bgImage, bgColor, txtColor, cJavaCode, onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr,cStyle, aImages, aServerSrc, baseURL, baseTarget,nRefresh, cRefreshURL ,cStyleScr,lnocache)

/* METHOD Debug()         INLINE __clsDebug( self ) NOSELF */

// --- HEADER --- //
METHOD SetPageColor( cColor ) INLINE FWrite(::nH, '<BODY BGCOLOR="'+cColor+'">' )
METHOD SetTextColor( cColor ) INLINE FWrite(::nH, '<BODY TEXT="'+cColor+'">' )
METHOD SetBgImage( cImage )   INLINE FWrite(::nH, '<BODY BACKGROUND="'+cImage+'">' )

// --- END --- //
METHOD Close()
METHOD CGIClose()


// --- FONTS --- //
METHOD SetCenter( lOn ) INLINE FWrite( ::nH, IF( lOn, "<CENTER>", "</CENTER>" ) )
METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor ,lSet)
METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor ,lSet)
METHOD DefineFont( cFont, cType, nSize, cColor , lSet)
METHOD EndFont()


// --- OUTPUT --- //

METHOD Say( str, font, size, type, color ,style)

METHOD QQOut( c )       INLINE ;
                             DEFAULT(c, ""), ;
                             FWrite( ::nH, c )

METHOD QOut( c )        INLINE ;
                             DEFAULT(c, ""), ;
                             FWrite( ::nH, CRLF()+c+'<BR>'+CRLF() )

METHOD Write( c )       INLINE ;
                             DEFAULT(c, ""), ;
                             FWrite( ::nH, c )

METHOD WriteLN( c )     INLINE ;
                             DEFAULT( c, "" ),;
                             FWrite( ::nH, CRLF()+c+'<BR>'+CRLF() )

METHOD SayColor( t, c ) INLINE ;
                             DEFAULT( t, "" ),;
                             DEFAULT( c, "black" ),;
                             FWrite(::nH, '<FONT COLOR="'+c+'">'+t+'</FONT>')

METHOD Space( n )       INLINE ;
                              DEFAULT( n, 1 ),;
                              FWrite( ::nH, replicate( "&nbsp;", n) )

METHOD PutImage( cImage, nBorder, nHeight,;
                 cOnclick, cOnMsOver, cOnMsOut, ;
                 cName, cAlt, cTarget ,nWidth,lbreak)

METHOD Text( cText, nCols, lWrap ) ;
                        INLINE ;
                             DEFAULT( lWrap, .T. ),;
                             DEFAULT( nCols, 80 ),;
                             FWrite( ::nH, "<PRE"+;
                                     IF(nCols!=NIL, ' COLS="'+NUMTRIM(nCols)+"'","")+;
                                        IF( lWrap, " WRAP>", ">")+CRLF()+;
                                         cText+CRLF()+"</PRE>"+CRLF() )


METHOD MultiCol( txt, cols, gutter, width )       INLINE ;
            DEFAULT( txt, "" ),;
            DEFAULT( cols, 2 ),;
            DEFAULT( gutter, 5 ),;
            DEFAULT( width, 100 ),;
            FWrite( ::nH, '<MULTICOL COLS="'+NUMTRIM(cols)+'" GUTTER="'+NUMTRIM(gutter)+'" WIDTH="'+NUMTRIM(width)+'">' ),;
            FWrite( ::nH, txt ),;
            FWrite( ::nH, "</MULTICOL>" ) 

// --- COSMETICS --- //
METHOD PutHeading( cText, nWeight, lCentered )

METHOD HLine( nSize, nWidth, lShade, cColor)
/*                         INLINE ;
                             nSize  := IF( nSize == NIL, 3, nSize ),;
                             nWidth := IF( nWidth == NIL, 90, nWidth ),;
                             FWrite( ::nH, '<P>'+CRLF()+'<HR SIZE = '+NUMTRIM(nSize)+' WIDTH = '+NUMTRIM(nWidth)+'%>')
*/
METHOD PutParagraph()    INLINE FWrite( ::nH, "<P> </P>"+CRLF() )

METHOD Paragraph( l, c, style )

METHOD PutBreak()        INLINE FWrite( ::nH, "<BR>"+CRLF() )


METHOD Marquee( cText, cFont, cFntColor, nFntSize, ;
                cAlign, nWidth, nHeight, cbgColor, ;
                cBehavior, cDirection , ;
                nScrollAmt, nScrollDelay, loop,;
                onMsOver, onMsOut, onClick, onStart, onFinish )

METHOD StartMarquee( cFont, cFntColor, nFntSize, ;
                     cAlign, nWidth, nHeight, cbgColor, ;
                     cBehavior, cDirection , ;
                     nScrollAmt, nScrollDelay, loop, ;
                     onMsOver, onMsOut, onClick, onStart, onFinish )
METHOD EndMarquee()

// --- URLs --- //
METHOD PutTextUrl( cText, cUrl, cOnClick, cOmMsOver, cOnMsout, cTarget ,/*new parameters*/ font,clr,size,style,bld,lbreak,cClass)
METHOD PutImageUrl( cImage, nBorder, nHeight,nWidth, cUrl,;
                    cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget ,nWidth,lbreak,cClass)



// --- TABLES --- //
METHOD DefineTable( nCols, nBorder, nWidth,nHeight, ColorFore, ColorBG, ;
                    l3d, lRuleCols, lRuleRows, ;
                    cClrDark, cClrLight, ncellpadding, ncellspacing,cAling ,lRules,bgImage,cStyle)

METHOD TableHead( cHead, cColor, cAlign, ;
                  cFont, nSize, cFntColor, nHeight, cBgPic )

METHOD NewTableRow(cColor)
METHOD EndTableRow()
METHOD NewTableCell( cAlign, cColor, ;
                     cFont, nSize, cFntColor, nHeight, ;
                     cBgPic, ;
                     nWidth, lWrap,;
                     nCSpan, nRSpan ,cValing,clrdrk,clrlt,cBdrClr,cclass)

METHOD EndTableCell()
METHOD EndTable()


// --- LISTS --- //
method newList()   INLINE FWrite( ::nH, "<UL>"+CRLF() )
method ListItem()  INLINE FWrite( ::nH, "<LI> " )
method EndList()   INLINE FWrite( ::nH, "</UL> " )


// --- FORMS --- //
METHOD NewForm( cMethod, cAction, cName )
METHOD FormImage( cText, name, File )
METHOD FormGet( cType, cName, xValue, nSize )
METHOD FormReset( c )
METHOD FormSubmit( c )
METHOD FormQOut( c )      INLINE FWrite( ::nH, c + '<BR>'+CRLF() )
METHOD FormQQOut( c )     INLINE FWrite( ::nH, c + CRLF() )
METHOD EndForm()          INLINE FWrite( ::nH, CRLF()+"</FORM>"+CRLF() )

METHOD PushButton( cName, cCaption, ;
                   cCgiApp,;
                   cOnClick, ;
                   cOnFocus, cOnBlur, ;
                   cOnMsOver, cOnMsOut,;
                   style, id )

METHOD endButton()

METHOD Button( cName, cCaption, ;
               cOnClick, cCGIApp, ;
               cOnMsOver, cOnMsOut,;
               style, id )
       
METHOD iFrame( name, src, border, ;
               marginwidth, marginheight, ;
               scrolling, allign, ;
               width, height)


// --- JAVA SUPPORT --- //
METHOD StartJava()      INLINE ;
                        FWRITE( ::nH, '<SCRIPT LANGUAGE="JavaScript">'+CRLF()+;
                                      "<!--"+CRLF() )

METHOD PutJavaSource(c) INLINE FWrite( ::nH, SPACE(5)+'SRC="'+ c +'"'+CRLF() )

METHOD PutJava( c )     INLINE FWrite( ::nH, SPACE(5)+c+CRLF() )

METHOD EndJava()        INLINE FWRITE( ::nH, "//-->"+CRLF()+;
                                             "</SCRIPT>"+CRLF() )

METHOD serverCode(c)    INLINE FWRITE( ::nH, "<SERVER>"+;
                                             SPACE(9) + c +CRLF()+;
                                             "</SERVER>"+CRLF() )
       
// standard output

METHOD FWrite( c )      INLINE ;
                               FWrite( ::nH, c )      
METHOD FWriteLN( c )    INLINE ;
                               FWrite( ::nH, c + CRLF() )


METHOD Span(c,Style)
METHOD PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
                    cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget ,nWidth,lbreak,cClass,cText) 
METHOD Comment(cText)
METHOD ADDoBJECT(cType,cClassid,cAling,cCode,lDisable,cCodeBase,cName,nWidth,nHeight)
METHOD ADDPARAM(cName,cValue)
METHOD EndOBJect()
METHOD PutLinkName( cName )
ENDCLASS
             



/****
*
*     Html():New()
*
*     Starts a new HTML disk file.
*/

METHOD New( cFile, cTitle, cLinkTitle, cCharSet, aScriptSRC,;
            bgImage, bgColor, txtColor, aJavaCode,;
            onLoad, onUnload, cLinkClr, cVLinkClr, cALinkClr,;
            cStyle, aImages, cBaseURL, cBaseTarget,;
            nRefresh, cRefreshURL ,cStyleScr,lnocache) CLASS Html

LOCAL i

DEFAULT cFile      := "file1.htm"
DEFAULT cTitle     := "test HTML page"
DEFAULT cLinkTitle := cTitle
DEFAULT cRefreshURL:= ""
DEFAULT cCharset   := "windows-1253"

::nH    := FCreate( cFile )
::Title := cTitle
::FName := cFile

FWRITE( ::nH, '<HTML>'+CRLF() +;
              '<HEAD>'+CRLF() +;
              '   <TITLE>' +cTitle+' </TITLE>'+CRLF() )

IF cBaseURL != NIL
FWRITE( ::nH, "<BASE HREF='"+cBaseURL+"'")
    IF cBaseTarget != NIL
       FWRITE( ::nH, " TARGET='"+cBaseTarget+"'")
    ENDIF
FWRITE( ::nH, ">"+CRLF() )
ENDIF
IF cStyleScr != NIL
   FWRITE( ::nH, '   <LINK HREF="'+cStyleScr+'"'+ "  rel='STYLESHEET' type='text/css'>"+CRLF())
ENDIF

FWRITE( ::nH, '   <LINK TITLE="' +cLinkTitle+'"'+CRLF()+;
              '                HREF="mailto:culik@sl.conex.net" >'+CRLF()+;
              '   <META HTTP-EQUIV="Content-Type" content="text/html; charset='+cCharset+'">'+CRLF() )

IF nRefresh != NIL
   FWrite( ::nH, [   <META HTTP-EQUIV="Refresh" CONTENT="]+NUMTRIM(nRefresh)+[; URL=]+cRefreshURL+[">] )
ENDIF

if lnocache
   FWrite( ::nH, [   <META HTTP-EQUIV="pragma" CONTENT="no-cache"> ])
ENDIF
IF aJavaCode != NIL
   AEVAL( aJavaCode, {|e| JavaCMD( ::nH, e ) } )
ENDIF

IF aScriptSrc != NIL
   FOR i =1 TO LEN( aScriptSrc )
   FWRITE( ::nH, ;
                '<SCRIPT LANGUAGE=JavaScript SRC="'+aScriptSrc[i]+'"></SCRIPT>'+CRLF() )
   NEXT
ENDIF


// preload images...
IF aImages != NIL
   ::aImages := aImages
   FWRITE( ::nH, ;
                 '<SCRIPT LANGUAGE="JavaScript">'+CRLF() )
   FWRITE( ::nH, '<!--'+CRLF() )
   FWrite( ::nH, "if(document.images)"+CRLF() )
   FWrite( ::nH, "{"+CRLF() )
   FOR i=1 TO LEN( aImages )
      FWrite( ::nH, space(5)+aImages[i,1]+"=new Image(100,50);"+CRLF() )
      FWrite( ::nH, space(5)+aImages[i,1]+'.src="'+aImages[i,2]+'";'+CRLF() )
   NEXT
   FWrite( ::nH, "}"+CRLF() )

   FWrite( ::nH, ""+CRLF())
   FWrite( ::nH, space(5)+[// Function to 'activate' images.]+CRLF())
   FWrite( ::nH, space(5)+"function imageOn(imgName) {"+CRLF())
   FWrite( ::nH, space(5)+"        if (document.images) {"+CRLF())
   FWrite( ::nH, space(5)+'            imgOn=eval(imgName + "on.src");'+CRLF())
   FWrite( ::nH, space(5)+'            document[imgName].src = imgOn;'+CRLF())
   FWrite( ::nH, space(5)+"        }"+CRLF())
   FWrite( ::nH, space(5)+"}"+CRLF())
   FWrite( ::nH, CRLF())
   FWrite( ::nH, space(5)+"// Function to 'deactivate' images."+CRLF())
   FWrite( ::nH, space(5)+"function imageOff(imgName) {"+CRLF())
   FWrite( ::nH, space(5)+"        if (document.images) {"+CRLF())
   FWrite( ::nH, space(5)+'            imgOff = eval(imgName + "off.src");'+CRLF())
   FWrite( ::nH, space(5)+'            document[imgName].src = imgOff;'+CRLF())
   FWrite( ::nH, space(5)+"        }"+CRLF())
   FWrite( ::nH, space(5)+"}"+CRLF())
   FWrite( ::nH, CRLF())
   FWrite( ::nH, space(5)+"// Function for 'pressed' images."+CRLF())
   FWrite( ::nH, space(5)+"function imagePress(imgName) {"+CRLF())
   FWrite( ::nH, space(5)+"        if (document.images) {"+CRLF())
   FWrite( ::nH, space(5)+'            imgPress = eval(imgName + "press.src");'+CRLF())
   FWrite( ::nH, space(5)+'            document[imgName].src = imgPress;'+CRLF())
   FWrite( ::nH, space(5)+"        }"+CRLF())
   FWrite( ::nH, space(5)+"}"+CRLF())
   FWrite( ::nH, CRLF())
   FWRITE( ::nH, '//-->'+CRLF() )
   FWRITE( ::nH, '</SCRIPT>'+CRLF() )

ENDIF


IF cStyle != NIL
   FWRITE( ::nH, "<STYLE> "+cStyle+" </STYLE>"+CRLF() )
ENDIF

FWrite( ::nH,;
              CRLF()+'</HEAD>'+;
              CRLF()+'<BODY' )

IF onLoad != NIL
   FWrite( ::nH, '   onLoad="'+onLoad+'"' )
ENDIF

IF onUnLoad != NIL
   FWrite( ::nH, ' onUnload="'+onUnLoad+'"' )
ENDIF

FWrite( ::nH, '>' )
FWrite( ::nH, CRLF() )


IF bgImage  != NIL
   ::SetBgImage( bgImage )
ENDIF

IF bgColor  != NIL
   ::SetPageColor( bgColor )
ENDIF

IF txtColor != NIL
  ::SetTextColor( txtColor )
ENDIF

snHtm  := ::nH

soPage := Self

RETURN self

                                

/****
*
*     Html():CGINew()
*
*     Starts a new CGI-HTML stream file.
*/

METHOD CGINew( cTitle, cLinkTitle, cCharSet, aScriptSRC,;
               bgImage, bgColor, txtColor, aJavaCode, ;
               onLoad, onUnload, ;
               cLinkClr, cVLinkClr, cALinkClr, ;
               cStyle, aImages, aServerSrc, ;
               cBaseURL, cBaseTarget,;
               nRefresh, cRefreshURL,cStyleScr ,lNocache) CLASS Html

LOCAL i


//DEFAULT lAuthenticate := .F.
DEFAULT cTitle        := "CGI HTML page"
DEFAULT cLinkTitle    := cTitle
DEFAULT cRefreshURL:= ""
DEFAULT cCharset   := "windows-1253"

::nH     := STD_OUT       //FCreate( cFile )
::Title  := cTitle
::FName  := "CGIOUT.HTM"

FWRITE( ::nH, 'Content-Type: text/html'+CRLF()+CRLF() )

/*
IF lAuthenticate == .T. .and. ;
   ( EMPTY(GetEnv( "AUTH_USER" )) .OR. EMPTY( GetEnv( "AUTH_PASS" )) )
   FWRITE( ::nH,"<HTML><HEAD</HEAD><BODY>"+CRLF() )
   FWRITE( ::nH,"HTTP/1.0 401 Not Authorized"+CRLF() )
   FWRITE( ::nH,'WWW-Authenticate:Basic Realm="'+cTitle+'"'+CRLF() )
   FWRITE( ::nH,"</BODY></HTML>"+CRLF() )
   FClose( ::nH )
   RETURN Self
ENDIF
*/

FWRITE( ::nH, '<HTML>'+CRLF() +;
              '<HEAD>'+CRLF() +;
              '   <TITLE>' +cTitle+' </TITLE>'+CRLF() )

IF cBaseURL != NIL
FWRITE( ::nH, "<BASE HREF='"+cBaseURL+"'")
    IF cBaseTarget != NIL
       FWRITE( ::nH, " TARGET='"+cBaseTarget+"'")
    ENDIF
FWRITE( ::nH, ">"+CRLF() )
ENDIF

FWRITE( ::nH, '   <LINK TITLE="' +cLinkTitle+'"'+CRLF()+;
              '                HREF="mailto:culik@sl.conex.net" >'+CRLF()+;
              '   <META HTTP-EQUIV="Content-Type" content="text/html; charset='+cCharset+'">'+CRLF() )

IF cStyleScr != NIL
   FWRITE( ::nH, '   <LINK HREF="'+cStyleScr+'"'+ " rel='STYLESHEET' type='text/css'>"+CRLF())
ENDIF
IF nRefresh != NIL
   FWrite( ::nH, [   <META HTTP-EQUIV="Refresh" CONTENT="]+NUMTRIM(nRefresh)+[; URL=]+cRefreshURL+[">] )
ENDIF
if lnocache
   FWrite( ::nH, [   <META HTTP-EQUIV="pragma" CONTENT="no-cache"> ])
ENDIF

IF aJavaCode != NIL
   AEVAL( aJavaCode, {|e| JavaCMD( ::nH, e ) } )
ENDIF

IF aScriptSrc != NIL
   FOR i =1 TO LEN( aScriptSrc )
   FWRITE( ::nH, ;              // RUNAT=SERVER
           '<SCRIPT LANGUAGE=JavaScript SRC="'+aScriptSrc[i]+'"></SCRIPT>'+CRLF() )
   NEXT
ENDIF

IF aServerSrc != NIL
   FOR i =1 TO LEN( aServerSrc )
   FWRITE( ::nH, ;              // RUNAT=SERVER
           '<SCRIPT LANGUAGE=JavaScript SRC="'+aServerSrc[i]+'" RUNAT=SERVER></SCRIPT>'+CRLF() )
   NEXT
ENDIF

// preload images...
IF aImages != NIL
   ::aImages := aImages
   FWRITE( ::nH, ;
                 '<SCRIPT LANGUAGE="JavaScript">'+CRLF() )
   FWRITE( ::nH, '<!--'+CRLF() )
   FWrite( ::nH, "if(document.images)"+CRLF() )
   FWrite( ::nH, "{"+CRLF() )
   FOR i=1 TO LEN( aImages )
      FWrite( ::nH, space(5)+aImages[i,1]+"=new Image(100,50);"+CRLF() )
      FWrite( ::nH, space(5)+aImages[i,1]+'.src="'+aImages[i,2]+'";'+CRLF() )
   NEXT
   FWrite( ::nH, "}"+CRLF() )

   FWrite( ::nH, ""+CRLF())
   FWrite( ::nH, space(5)+[// Function to 'activate' images.]+CRLF())
   FWrite( ::nH, space(5)+"function imageOn(imgName) {"+CRLF())
   FWrite( ::nH, space(5)+"        if (document.images) {"+CRLF())
   FWrite( ::nH, space(5)+'            imgOn=eval(imgName + "on.src");'+CRLF())
   FWrite( ::nH, space(5)+'            document[imgName].src = imgOn;'+CRLF())
   FWrite( ::nH, space(5)+"        }"+CRLF())
   FWrite( ::nH, space(5)+"}"+CRLF())
   FWrite( ::nH, CRLF())
   FWrite( ::nH, space(5)+"// Function to 'deactivate' images."+CRLF())
   FWrite( ::nH, space(5)+"function imageOff(imgName) {"+CRLF())
   FWrite( ::nH, space(5)+"        if (document.images) {"+CRLF())
   FWrite( ::nH, space(5)+'            imgOff = eval(imgName + "off.src");'+CRLF())
   FWrite( ::nH, space(5)+'            document[imgName].src = imgOff;'+CRLF())
   FWrite( ::nH, space(5)+"        }"+CRLF())
   FWrite( ::nH, space(5)+"}"+CRLF())
   FWrite( ::nH, CRLF())
   FWrite( ::nH, space(5)+"// Function for 'pressed' images."+CRLF())
   FWrite( ::nH, space(5)+"function imagePress(imgName) {"+CRLF())
   FWrite( ::nH, space(5)+"        if (document.images) {"+CRLF())
   FWrite( ::nH, space(5)+'            imgPress = eval(imgName + "press.src");'+CRLF())
   FWrite( ::nH, space(5)+'            document[imgName].src = imgPress;'+CRLF())
   FWrite( ::nH, space(5)+"        }"+CRLF())
   FWrite( ::nH, space(5)+"}"+CRLF())
   FWrite( ::nH, CRLF())
   FWRITE( ::nH, '//-->'+CRLF() )
   FWRITE( ::nH, '</SCRIPT>'+CRLF() )

ENDIF

IF cStyle != NIL
   FWRITE( ::nH, "<STYLE> "+cStyle+" </STYLE>"+CRLF() )
ENDIF

FWrite( ::nH,;
              '</HEAD>'+CRLF()+;
              '<BODY' )

IF onLoad != NIL
   FWrite( ::nH, '   onLoad="'+onLoad+'"' )
ENDIF

IF onUnLoad != NIL
   FWrite( ::nH, ' onUnload="'+onUnLoad+'"' )
ENDIF

if cLinkClr !=NIL 
FWrite( ::nH, ' link="'+cLinkClr+'"' )
endif

if cVLinkClr !=NIL 
FWrite( ::nH, ' vlnk="'+cVLinkClr+'"' )
endif
if  cALinkClr !=NIL
FWrite( ::nH, ' alink="'+cALinkClr+'"' )
endif
FWrite( ::nH, '>' )

FWrite( ::nH, CRLF() )

IF bgImage  != NIL
   ::SetBgImage( bgImage )
ENDIF

IF bgColor  != NIL
   ::SetPageColor( bgColor )
ENDIF

IF txtColor != NIL
  ::SetTextColor( txtColor )
ENDIF

snHtm  := ::nH

soPage := Self

RETURN self



/****
*
*     Html():SetFont()
*
*     obvious...
*/

METHOD SetFont( cFont, lBold, lItalic, lULine, nSize, cColor ,lSet) CLASS Html
LOCAL cStr      := CRLF()+'<FONT'

DEFAULT cFont   := ::fontFace
DEFAULT nSize   := ::fontSize
DEFAULT cColor  := ::fontColor
default lset    := IF( cFont != NIL,.t.,.f.)
IF cFont != NIL
   cStr += ' FACE="'+cFont +'"'
   if lSet
   ::fontFace := cFont
   endif
ENDIF
IF nSize != NIL
   cStr += ' SIZE="'+LTRIM(STR(nSize ))+'"'
   if lSet
   ::fontSize := nSize
   endif
ENDIF

IF cColor != NIL
   cStr += ' COLOR= "'+ cColor + '">'
   if lset
   ::fontColor := cColor
   endif
ELSE
   cStr += ">"
ENDIF


IF lBold != NIL
     IF( lBold, cStr += '<B>', cStr += '</B>')
ENDIF
IF lItalic != NIL
     IF( lItalic, cStr += '<I>', cStr += '</I>')
ENDIF

IF lULine != NIL
     IF( lULine, cStr += '<U>', cStr += '</U>')
ENDIF

cStr += '</FONT>'
FWrite( ::nH, cStr + CRLF() )
RETURN Self


/****
*
*     Html():StartFont()
*
*     Begin a font definition. They may be nested but make sure you
*     end the definition appropriately later
*/

METHOD StartFont( cFont, lBold, lItalic, lULine, nSize, cColor ,lSet) CLASS Html
LOCAL cStr      := "<FONT "
default lSet  :=.t.
DEFAULT cFont   := ::fontFace
DEFAULT nSize   := ::fontSize
DEFAULT cColor  := ::fontColor

IF cFont != NIL
   cStr += ' FACE="'+cFont +'"'
   if lSet
   ::fontFace := cFont
   endif
ENDIF
IF nSize != NIL
   cStr += ' SIZE="'+LTRIM(STR(nSize ))+'"'
   if lSet
   ::fontSize := nSize
   endif
ENDIF

IF cColor != NIL
   cStr += ' COLOR= "'+ cColor + '">'
   if lSet
   ::fontColor := cColor
   endif
ELSE
   cStr +=  ">"
ENDIF

IF lBold != NIL
     IF( lBold, cStr += '<B>', cStr += '</B>')
ENDIF
IF lItalic != NIL
     IF( lItalic, cStr += '<I>', cStr += '</I>')
ENDIF

IF lULine != NIL
     IF( lULine, cStr += '<U>', cStr += '</U>')
ENDIF

FWrite( ::nH, cStr + CRLF() )

RETURN Self


/****
*
*     Html():DefineFont()
*
*     Begin a font definition by font type "name".
*     Use ::endFont() to cancel this font
*/

METHOD DefineFont( cFont, cType, nSize, cColor ,lSet) CLASS Html
LOCAL cStr      := "<FONT "

DEFAULT cFont   := ::fontFace
DEFAULT nSize   := ::fontSize
DEFAULT cColor  := ::fontColor
default lset    := if(cFont != NIL,.t.,.f.)
IF cFont != NIL
   cStr += ' FACE="'+cFont +'"'
   if lSet
   ::fontFace := cFont
   endif
ENDIF
IF nSize != NIL
   cStr += ' SIZE="'+LTRIM(STR(nSize ))+'"'
   if lSet
   ::fontSize := nSize
   endif
ENDIF

IF cColor != NIL
   cStr += ' COLOR= "'+ cColor + '">'
   if lset
   ::fontColor := cColor
   endif
ELSE
   cStr += ">"
ENDIF


IF cType != NIL
     cStr += cType
ENDIF

FWrite( ::nH, cStr + CRLF() )

RETURN Self



/****
*
*     Html():EndFont()
*
*     End a font definition
*/

METHOD endFont() CLASS Html
FWrite( ::nH,  '</font>' + CRLF() )
RETURN Self



/****
*
*     Html():say()
*
*
*
*/

METHOD Say( str, font, size, type, color ,style) CLASS Html
LOCAL cOut := ""
LOCAL lBold := .F., lItalic:= .F., lULine := .F.,lEm:=.f.,lStrong:=.f.
Local nSize:=Size
DEFAULT str  := ""
DEFAULT font := ::FontFace
DEFAULT size := ::FontSize
DEFAULT color:= ::FontColor
if Font != nil .or. Size !=Nil .or. color != Nil
cOut := '<FONT ' + if(font!=nil, 'FACE="'+font+'"','')+ if(color!=nil,' COLOR='+color,'')+ if(nSize !=nil, ' SIZE='+NUMTRIM( size ),"")

if Style != NIL
    cOut += '" Style="' +style +'">'
Else
    cOut += '>'
Endif
endif
IF VALTYPE( type ) == "C"
   IF "<" $ type
      IF "<B>" $ type
         lBold := .T.
         cOut+= "<B>"
      ENDIF
      IF "<I>" $ type
         lItalic := .T.
         cOut+= "<I>"
      ENDIF
      IF "<U>" $ type
         lULine := .T.
         cOut+= "<U>"
      ENDIF
      IF "<EM>" $ type
         lEm := .T.
         cOut+= "<EM>"
      ENDIF
      IF "<STRONG>" $ type
         lStrong := .T.
         cOut+= "<STRONG>"
      ENDIF

   ENDIF
ENDIF

cOut += str

IF lBold
   cOut+= "</B>"
ENDIF
IF lItalic
   cOut+= "</I>"
ENDIF
IF lULine
   cOut+= "</U>"
ENDIF

IF lStrong
   cOut+= "</STRONG>"
ENDIF
IF lEm
   cOut+= "</EM>"
ENDIF
if Font != nil .or. Size !=Nil .or. color != Nil
cOut += "</FONT>"
endif
FWrite( ::nH, cOut+CRLF() )

RETURN Self




/****
*
*     Html():paragraph()
*
*
*
*/

METHOD Paragraph( lStart, cAlign, cStyle ) CLASS Html
LOCAL cStr := "<P"

DEFAULT( lStart, .T.)
DEFAULT( cAlign, "LEFT")

IF lStart
   cStr := "<P ALIGN='" + cAlign +"'"
   IF cStyle != NIL
      cStr += ' STYLE="'+cStyle +'"'
   ENDIF
   cStr += ">"
ELSE
   cStr := "</P>"
ENDIF

cStr += CRLF()
FWrite( ::nH, cStr )
RETURN Self



/****
*
*     Html():HLine()
*
*     Put a Horizontal line
*/

Method HLine( nSize, nWidth, lShade, cColor) CLASS Html
DEFAULT nSize  := 3
DEFAULT nWidth := 100
DEFAULT lShade := .T.

IF lShade
FWrite( ::nH,  CRLF()+;
              '<HR SIZE = '+NUMTRIM(nSize)+ if(cColor !=NIL," COLOR  " + cColor,"")              +' WIDTH = '+NUMTRIM(nWidth)+'%>'+ ;
              CRLF() )
ELSE
FWrite( ::nH,  CRLF()+;
              '<HR NOSHADE SIZE = '+NUMTRIM(nSize)+ if(cColor !=NIL," COLOR  " + cColor,"")+' WIDTH = '+NUMTRIM(nWidth)+'%>'+ ;
              CRLF() )
ENDIF

RETURN Self



/****
*
*     Html():PutHeading()
*
*     Put an HTML heading ( large text )
*/

METHOD PutHeading( cText, nWeight, lCentered ) CLASS Html
DEFAULT nWeight   := 3
DEFAULT lCentered := .F.

IF lCentered
FWrite( ::nH, "<CENTER>" )
ENDIF

FWrite( ::nH, "<H"+NUMTRIM(nWeight)+">"+cText+"</H"+NUMTRIM(nWeight)+">"+CRLF() )

IF lCentered
FWrite( ::nH, "</CENTER>" )
ENDIF

RETURN Self




/****
*
*     Html():putTextURL()
*
*     Put a text link.
*/

METHOD PutTextUrl( cText         , cUrl                                        , cOnClick, cOnMsOver, cOnMsout, cTarget , font, clr, size, style, bld,lbreak,cClass) CLASS Html

LOCAL cStr := ""
DEFAULT cUrl := ""
DEFAULT bld := .F.

FWrite( ::nH, ;
        '<A HREF="'+cUrl+'"'+crlf() )

IF cOnClick != NIL
FWrite( ::nH, ;
        SPACE(5)+'onClick="'+cOnClick+'"'+CRLF() )
ENDIF
IF cOnMsOver != NIL
FWrite( ::nH, ;
        SPACE(5)+'onMouseOver="'+cOnMsOver+'"'+CRLF() )
ENDIF
IF cOnMsOut != NIL
FWrite( ::nH, ;
        SPACE(5)+'onMouseOut="'+cOnMsOut+'"'+CRLF() )
ENDIF

IF cTarget != NIL
FWrite( ::nH, ;
        SPACE(5)+'TARGET='+cTarget+CRLF() )
ENDIF
if cClass != Nil
FWrite( ::nH, ;
        SPACE(5)+'CLASS='+cClass+CRLF() )
ENDIF

IF bld
    cStr += "<B>" +CRLF()
Endif
 IF    font != NIL .or.  clr != NIL .or. size != NIL .or. style != NIL
//    cStr +=" Font" +valtype(font)+"color"+valtype(clr)+"size"+valtype(size)+"style"+valtype(style)
    cStr += " <FONT "+CRLF()
    IF Font != NIL
        cStr += ' face="' +Font +'"'
    Endif
    IF clr != NIL
        cStr += ' color=' +clr 
    Endif
    IF size != NIL
        cStr += ' size=' +NUMTRIM(size) 
    Endif
    IF style != NIL
        cStr += ' style="' +style +'"'
    Endif

Endif
 IF    font != NIL .or. clr != NIL .or. size != NIL .or. style != NIL
cStr +='>' + cText
Else
cStr += cText
Endif
FWrite (::nH,;
        '>'+cStr )
 IF    font != NIL .or.  clr != NIL .or. size != NIL .or. style != NIL
    FWrite (::nH,;
        '</font>')
Endif
IF bld
    FWrite (::nH,;
        '</B>')   
Endif

FWrite( ::nH, ;
        '</A>'+if(lBreak,'<br>'+CRLF(),CRLF() ))


RETURN Self




/****
*
*     Html():putImageURL()
*
*     Put an Image link.
*/

Method PutImageUrl( cImage, nBorder, nHeight, cUrl, ;
                    cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget ,nWidth,lbreak,cClass) CLASS Html
LOCAL cStr := ""

IF cName != NIL
   cStr += ' NAME= "'+cName + '"' +CRLF()
ENDIF
IF cAlt != NIL
   cStr += ' ALT= "'+cAlt+ '"' +CRLF()
ENDIF
IF nBorder != NIL
   cStr += " border = "+if(valtype(nBorder)=="N",NUMTRIM(nBorder),nBorder)  +CRLF()
ENDIF
IF nHeight != NIL .and. valtype(nHeight)=="N"
   cStr += " height = "+NUMTRIM(nHeight) + " "  +CRLF()
ELSEIF nHeight != NIL .and. valtype(nHeight)=="C"
   cStr += " height = "+nHeight +" " +CRLF()
ENDIF

IF  nWidth!= NIL .and. valtype(nWidth)=="N"
   cStr += " width = "+NUMTRIM(nWidth) + " " +CRLF()
ELSEIF  nWidth!= NIL .and. valtype(nWidth)=="C"
   cStr += " width = "+nWidth + " "  +CRLF()
endif
IF cOnClick != NIL
   cStr += ' onClick="'+cOnClick+'"' +CRLF()
ENDIF
IF cOnMsOver != NIL
   cStr += ' onMouseOver="'+cOnMsOver+'"'  +CRLF()
ENDIF
IF cOnMsOut != NIL
   cStr += ' onMouseOut="'+cOnMsOut+'"' +CRLF()
ENDIF

IF cTarget != NIL
cStr += ' TARGET='+cTarget +CRLF()
ENDIF

FWrite( ::nH, ;
        '<A HREF='+cUrl+ if(cClass!=nil,' class="'+cClass+'"',"")+  '><IMG SRC="'+cImage+'"'+ ;
         cStr +'></A>'+ if(lBreak,'<br>'+CRLF(),"" ))

RETURN Self

Method PutTextImageUrl( cImage, nBorder, nHeight, cUrl, ;
                    cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget ,nWidth,lbreak,cClass,cText) CLASS Html
LOCAL cStr := ""

IF cName != NIL
   cStr += ' NAME= "'+cName + '"'
ENDIF
IF cAlt != NIL
   cStr += ' ALT= "'+cAlt+ '"'
ENDIF
IF nBorder != NIL
   cStr += " border = "+NUMTRIM(nBorder) 
ENDIF
IF nHeight != NIL .and. valtype(nHeight)=="N"
   cStr += " height = "+NUMTRIM(nHeight) + " " 
ELSEIF nHeight != NIL .and. valtype(nHeight)=="C"
   cStr += " height = "+nHeight +" "
ENDIF

IF  nWidth!= NIL .and. valtype(nWidth)=="N"
   cStr += " width = "+NUMTRIM(nWidth) + " "
ELSEIF  nWidth!= NIL .and. valtype(nWidth)=="C"
   cStr += " width = "+nWidth + " " 
endif
IF cOnClick != NIL
   cStr += ' onClick="'+cOnClick+'"'
ENDIF
IF cOnMsOver != NIL
   cStr += ' onMouseOver="'+cOnMsOver+'"' 
ENDIF
IF cOnMsOut != NIL
   cStr += ' onMouseOut="'+cOnMsOut+'"'
ENDIF

IF cTarget != NIL
cStr += ' TARGET='+cTarget
ENDIF

FWrite( ::nH, ;
        '<A HREF='+cUrl+ if(cClass!=nil,' class="'+cClass+'"',"")+  '>'+cText+ '<IMG SRC="'+cImage+'"'+ ;
         cStr +'></A>'+ if(lBreak,'<br>'+CRLF(),"" ))

RETURN Self



/****
*
*     Html():putImage()
*
*     Put an Image.
*/

Method PutImage( cImage, nBorder, nHeight, ;
                 cOnclick, cOnMsOver, cOnMsOut, cName, cAlt, cTarget ,nWidth,lbreak) CLASS Html
LOCAL cStr := ""

IF cName != NIL
   cStr += ' NAME= "'+cName + '"'
ENDIF
IF cAlt != NIL
   cStr += ' ALT= "'+cAlt+ '"'
ENDIF
IF nBorder != NIL .and. valtype(nBorder)=="N"
   cStr += " BORDER = "+NUMTRIM(nBorder) 
ELSEIF nBorder != NIL .and. valtype(nBorder)=="C"
   cStr += " BORDER = "+ '"'+nBorder+'"' 
ENDIF

IF nHeight != NIL .and. valtype(nHeight)=="N"
   cStr += " HEIGHT = "+NUMTRIM(nHeight) + " " 
ELSEIF nHeight != NIL .and. valtype(nHeight)=="C"
   cStr += " HEIGHT = "+'"'+nHeight + '"' 
ENDIF
IF  nWidth!= NIL .and. valtype(nWidth)=="N"
   cStr += " width = "+NUMTRIM(nWidth) + " " 
ELSEIF  nWidth!= NIL .and. valtype(nWidth)=="C"
   cStr += " width = "+nWidth + " " 
endif

IF cOnClick != NIL
   cStr += ' onClick="'+cOnClick+'"' 
ENDIF
IF cOnMsOver != NIL
   cStr += ' onMouseOver="'+cOnMsOver+'"'
ENDIF
IF cOnMsOut != NIL
   cStr += ' onMouseOut="'+cOnMsOut+'"'
ENDIF

IF cTarget != NIL
cStr += ' TARGET="'+cTarget+'"'
ENDIF
FWrite( ::nH, ;
        '<IMG SRC="'+cImage+'"'+;
         cStr +'>'+if(lBreak,'<br>'+CRLF(),"" ) )

RETURN Self





/****
*
*     Html():Close()
*
*     Close an HTML disk file
*
*/

METHOD Close() CLASS Html
FWrite( ::nH, "</body>"+CRLF() )
FWrite( ::nH, "</html>"+CRLF() )
FClose( ::nH )
RETURN Self


/****
*
*     Html():CGIClose()
*
*     Close a CGI-HTML stream file
*/

METHOD cgiClose() CLASS Html
FWrite( ::nH, "</body>"+CRLF() )
FWrite( ::nH, "</html>"+CRLF() )
FWrite( ::nH, CRLF() )
RETURN Self



/****
*
*     Html():defineTable()
*
*     Start an HTML table definition.
*
*
*/

METHOD DefineTable( nCols, nBorder, nWidth, nHeight, ColorFore, ColorBG, ;
                    l3d, lRuleCols, lRuleRows, cClrDark, cClrLight,cClrBorder, ;
                    nCellPadding, nCellSpacing,cAling,lRules,bgImage,cStyle) CLASS Html

LOCAL cStr := "<!-- Table Definition -->"+ CRLF()+CRLF()+"<TABLE "
Local xCols:=nCols
DEFAULT ColorBG   := "#9196A0"  //CLR_WHITE
DEFAULT nCols     := 1
DEFAULT nWidth    := 100
DEFAULT nBorder   := 1
DEFAULT l3d       := .T.
DEFAULT lRuleCols := .F.
DEFAULT lRuleRows := .F.

IF ColorFore != NIL
   cStr += " bordercolor="+ColorFore+' '
ENDIF

cStr += " bgcolor="+ColorBG+' '
cStr += IF( nBorder = NIL, "border ", "border="+NUMTRIM(nBorder)+' ')
// ??? --> cStr += "frame=ALL "
if ncellpadding != Nil
cStr += ' CellPadding='+NUMTRIM(nCellPadding)
Endif
if nCellSpacing != Nil
cStr += ' CellSpacing='+NUMTRIM(nCellSpacing)
Endif
if cAling != Nil
cStr +=' aling='+'"'+ cAling +'"'
endif

//cStr += "rowspan = 1 "    + CRLF()
//cStr += "colspan = 1 "    + CRLF()
cStr += if(xCols != nil," COLS=" + NUMTRIM( nCols ) ,"")
if nWidth !=NIL .and. VALTYPE(nWidth)=="N"
    cStr += " WIDTH=" + NUMTRIM( nWidth ) 
elseif nWidth !=NIL .and. VALTYPE(nWidth)=="C"
    cStr += " WIDTH=" +'"' + nWidth +'"' 
Endif


if nHeight !=NIL .and. VALTYPE(nHeight)=="N"
    cStr += " HEIGHT=" + NUMTRIM( nHeight ) 
elseif nHeight !=NIL .and. VALTYPE(nHeight)=="C"
    cStr += " HEIGHT=" +'"' + nHeight +'"' 
Endif

IF l3d
cStr += ' bordercolorlight=#000000 ' +;
        ' bordercolordark=#FFFFFF '  
ENDIF

IF cClrDark != NIL
cStr += ' bordercolordark='+cClrDark 
ENDIF

IF cClrLight != NIL
cStr += ' bordercolorlight='+cClrLight 
ENDIF

IF cClrBorder != NIL
cStr += ' bordercolor='+cClrBorder 
ENDIF

IF lRuleCols == .T.
   cStr += " RULES=COLS"
ELSEIF lRuleRows == .T.
   cStr += " RULES=ROWS"
ELSEif lRules == .T.                                  
   cStr += " RULES=ALL"
ENDIF
if bgImage != NIL
    cStr+= ' background="'+bgImage+'" '
endif
if cStyle != NIL
cStr += ' style ="'+cStyle+'" '
endif
cStr += ">"+CRLF()

// rules=cols
// rules=rows
// rules=all

FWrite( ::nH, cStr + CRLF() )

RETURN Self



/****
*
*     Html():tableHead()
*
*     Define a table column Header.
*
*/

METHOD TableHead( cHead, cColor, cAlign, ;
                  cFont, nSize, cFntColor, nHeight, cBgPic ) CLASS Html

LOCAL cStr := SPACE(3)+"<TH"

DEFAULT cFont     := ::fontFace
DEFAULT nSize     := ::fontSize
DEFAULT cFntColor := ::fontColor


IF cColor != NIL
  cStr += " bgcolor=" + '"' + cColor + '"'
ENDIF
IF cAlign != NIL
  cStr += " align=" + '"' + cAlign + '"'
ENDIF
IF nHeight != NIL
  cStr += " height=" + '"' + NUMTRIM(nHeight) + '"'
ENDIF
IF cBgPic != NIL
  cStr += " background=" + '"' + cBgPic + '"'
ENDIF

cStr += ">"

IF cFont != NIL
   cStr+= '<font face="'+cFont+'"'
   IF nSize != NIL
      cStr += ' size="'+NUMTRIM(nSize)+'"'
   ENDIF
   IF cFntColor != NIL
      cStr += ' color="'+cFntColor+'"'
   ENDIF
   cStr += ">"
ENDIF


cStr += cHead + IF( cFont !=NIL, '</font>', "")+"</th>"+CRLF()
//cStr += cHead + '</font>'+"</th>"+CRLF()

FWrite(::nH, cStr )

RETURN Self



/****
*
*     Html():newTableRow()
*
*     Start a table row definition.
*
*/

METHOD NewTableRow( cColor ) CLASS Html
LOCAL cStr := SPACE(5)+"<TR"

IF cColor != NIL
  cStr += " bgcolor=" +  cColor 
ENDIF
cStr += ">"+CRLF()
FWrite(::nH, cStr )
RETURN Self




/****
*
*     Html():endTableRow()
*
*     End a table row definition.
*
*/

METHOD EndTableRow() CLASS Html
FWrite(::nH, SPACE(5)+"</TR>"+CRLF() )
RETURN Self




/****
*
*     Html():newTableCell()
*
*     Start a table cell definition.
*
*/

METHOD NewTableCell( cAlign, cColor, ;
                     cFont, nSize, cFntColor, nHeight, ;
                     cBgPic, nWidth, lWrap, ;
                     nColspan, nRowspan,cValign ,clrdrk,clrlt,cBdrClr,cClass) CLASS Html

LOCAL cStr := SPACE(10)+"<TD"
Local cAli:=cAlign
DEFAULT cFont     := ::fontFace
DEFAULT nSize     := ::fontSize
DEFAULT cFntColor := ::fontColor
DEFAULT cAlign    := "LEFT"
DEFAULT lWrap     := .T.
if cBdrClr != NIL
    cStr += " BORDERCOLOR=" + cBdrClr 
endif
IF cColor != NIL
  cStr += " BGCOLOR=" + cColor 
ENDIF

IF cAlign != NIL .and. caLi !=Nil
  cStr += " ALIGN=" +  cAlign 
ENDIF
IF cValign != NIL
  cStr += " VALIGN=" +  cValign 
ENDIF

IF nHeight != NIL .and. VALTYPE(nHeight)="N"
  cStr += " HEIGHT=" + NUMTRIM(nHeight) 
ELSEIF nHeight != NIL .and. VALTYPE(nHeight)="C"
  cStr += " HEIGHT=" + '"' + nHeight + '"'
ENDIF

IF cBgPic != NIL
  cStr += " BACKGROUND=" + '"' + cBgPic + '"'
ENDIF

IF nWidth != NIL .and. VALTYPE(nWidth)="N"
  cStr += " WIDTH=" + NUMTRIM(nWidth) 
ELSEIF nWidth != NIL .and. VALTYPE(nWidth)="C"
  cStr += " WIDTH=" + '"' + nWidth + '"'
ENDIF

IF nColspan != NIL .and. VALTYPE(nColspan)="N"
  cStr += " COLSPAN=" + NUMTRIM(nColspan) 
ELSEIF nColspan != NIL .and. VALTYPE(nColspan)="C"
  cStr += " COLSPAN=" + '"' + nColspan + '"'
ENDIF


IF clrdrk != NIL
  cStr += " borderColorDark=" +  clrdrk 
ENDIF

IF clrlt != NIL
  cStr += " bordercolorlight=" +  clrlt 
ENDIF

if cClass != NIL
cStr += ' Class ="'+cClass+'" '
endif

IF nRowspan != NIL .and. VALTYPE(nRowspan)="N"
  cStr += " ROWSPAN=" +  NUMTRIM(nRowspan) 
ELSEIF nRowspan != NIL .and. VALTYPE(nRowspan)="C"
  cStr += " ROWSPAN=" + '"' + nRowspan + '"'
ENDIF


IF lWrap == .F.
  cStr += " NOWRAP"
ENDIF

cStr += ">"

IF cFont != NIL .or. nSize != NIL .or. cFntColor != NIL
   cStr+= '<FONT '
   IF nSize != NIL
      cStr += 'SIZE='+NUMTRIM(nSize)
   ENDIF
   IF cFntColor != NIL
      cStr += ' COLOR='+cFntColor
   ENDIF
   IF !empty( cFont)
      cStr += ' FACE="'+cFont+'"'+ ">"
   ELSE
       cStr += ">"
   ENDIF
   ::lFont:=.t.
ENDIF

FWrite(::nH, cStr )
RETURN Self



/****
*
*     Html():endTableCell()
*
*     End a table cell definition.
*
*/

METHOD EndTableCell() CLASS Html
if ::lFont
FWrite(::nH, "</font></td>"+CRLF() )
else
FWrite(::nH, "</td>"+CRLF() )
endif
::lFont:=.f.
RETURN Self




/****
*
*     Html():endTable()
*
*     End a table definition.
*/

METHOD EndTable()     CLASS Html
FWrite(::nH, "</table>"+CRLF() )
FWrite(::nH, CRLF()+"<!-- End of Table -->"+ CRLF()+CRLF() )
RETURN Self





//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
//   FORMS...
//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ


/****
*
*     Html():newForm()
*
*     Creates a new form
*
*/

METHOD NewForm( cMethod, cAction, cName ) CLASS Html

DEFAULT cMethod := "POST"
DEFAULT cName   := "newForm"

FWRITE( ::nH, CRLF()+"<FORM")

IF cMethod != NIL
FWRITE( ::nH, ' METHOD="'+cMethod+'"' )
ENDIF

IF cName != NIL
FWRITE( ::nH, ' NAME="'+cName+'"' )
ENDIF

IF cAction != NIL
FWRITE( ::nH, ' ACTION="'+cAction+'"' )
ENDIF

FWRITE( ::nH, '>'+CRLF() )

scForm := cName

RETURN Self



/****
*
*     Html():FormGet()
*
*     Adds a form edit field
*
*/

METHOD FormGet( cType, cName, xValue, nSize ) CLASS Html
default cType := "edit"

FWrite( ::nH, '<INPUT Type="'+cType+'"' )

IF cName != NIL
FWrite( ::nH, ' Name="'+cName+'"' )
ENDIF

IF xValue != NIL
FWrite( ::nH, ' Value="'+ANY2STR( xValue )+'"' )
ENDIF

IF nSize != NIL
FWrite( ::nH, ' Size="'+ANY2STR( nSize )+'"' )
ENDIF

FWrite( ::nH, ">" )
RETURN Self



/****
*
*     Html():FormSubmit()
*
*     Adds a form submit button
*
*/

METHOD FormSubmit( cText ) CLASS Html
FWrite( ::nH, '<INPUT Type="submit" Value="'+cText+'">'+CRLF() )
RETURN Self





/****
*
*     Html():FormImage()
*
*     Adds a form image button
*
*/

METHOD FormImage( cText, name, File ) CLASS Html
FWrite( ::nH, '<INPUT TYPE="IMAGE" NAME="'+name+'" SRC="'+file+'">'+CRLF() )
RETURN Self


/****
*
*     Html():FormReset()
*
*     Adds a reset button
*
*/

METHOD FormReset( cText ) CLASS Html

FWrite( ::nH, '<INPUT Type="Reset" Value="'+cText+'">'+CRLF() )

RETURN Self




/****
*
*     Html():pushButton()
*
*     Insert a standalone push button and assign an action to it
*     Either pass onClick or cCgiApp - not both
*/

METHOD PushButton( cName, cCaption, ;
                   cCgiApp,;
                   cOnClick, ;
                   cOnFocus, cOnBlur, ;
                   cOnMsOver, cOnMsOut,;
                   style, id ) CLASS Html

LOCAL cStr := CRLF()+"<INPUT TYPE=BUTTON "+CRLF()

DEFAULT cOnMsOver := "window.status=this.name;"
DEFAULT cOnMsOut  := "window.status='';"

IF cName != NIL
   cStr += "        NAME="+cName
ENDIF

IF cCaption != NIL
   cStr += "       VALUE="+cCaption
ENDIF

IF style != NIL
   cStr += '       STYLE="'+style+'"'
ENDIF

IF id != NIL
   cStr += '          ID="'+id+'"'
ENDIF

IF cOnClick != NIL
   cStr += '     onClick="'+cOnClick+'"'
ENDIF

IF cOnFocus != NIL
   cStr += '     onFocus="'+cOnFocus+'"'
ENDIF

IF cOnBlur != NIL
   cStr += '      onBlur="'+cOnBlur+'"'
ENDIF

IF cOnMsOver != NIL
   cStr += ' onMouseOver="'+cOnMsover+'"'
ENDIF

IF cOnMsOut != NIL
   cStr += '  onMouseOut="'+cOnMsout+'"'
ENDIF

IF cCgiApp != NIL
   cStr += '     onClick="location.href='+cCgiApp+';"'
ENDIF

FWrite( ::nH, cStr +">" )

RETURN Self



/****
*
*     Html():Button()
*
*     Insert a standalone <BUTTON> push button and assign an action to it
*
*/

METHOD Button( cName, cCaption, ;
               cOnClick, ;
               cCGIApp, ;
               cOnMsOver, cOnMsOut,;
               Style, Id ) CLASS Html

LOCAL cStr := CRLF()+"<BUTTON "+CRLF()

DEFAULT cOnMsOver := "window.status=this.name;"
DEFAULT cOnMsOut  := "window.status='';"

IF cName != NIL
   cStr += "        NAME="+cName
ENDIF

IF cCaption != NIL
   cStr += "       TITLE="+cCaption
ENDIF

IF style != NIL
   cStr += '       STYLE="'+style+'"'
ENDIF

IF id != NIL
   cStr += '          ID="'+id+'"'
ENDIF

IF cOnClick != NIL
   cStr += '     onClick="'+cOnClick+'"'
ENDIF
                              
IF cOnMsOver != NIL
   cStr += ' onMouseOver="'+cOnMsover+'"'
ENDIF

IF cOnMsOut != NIL
   cStr += '  onMouseOut="'+cOnMsout+'"'
ENDIF

IF cCgiApp != NIL
   cStr += '     onClick="location.href='+cCgiApp+';"'
ENDIF

FWrite( ::nH, cStr +">"+CRLF() )

RETURN Self



/****
*
*     Html():EndButton()
*
*     End a <BUTTON> definition
*
*/

METHOD EndButton() CLASS Html
FWrite( ::nH, CRLF()+CRLF()+"</BUTTON>"+CRLF() )
RETURN Self




/****
*
*     Html():Marquee()
*
*     Display a scrolling marquee effect
*
*/

METHOD Marquee( cText, cFont, cFntColor, nFntSize, ;
                cAlign, nWidth, nHeight, cbgColor, ;
                cBehavior, cDirection , ;
                nScrollAmt, nScrollDelay, loop,;
                onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS Html


LOCAL cStr := ""

DEFAULT cFont        := "Verdana"
DEFAULT cFntColor    := "white"
DEFAULT nFntSize     := 3
DEFAULT cAlign       := "middle"
DEFAULT nWidth       := 100
DEFAULT cText        := ""
DEFAULT cBgColor     := "black"
DEFAULT cBehavior    := "scroll"   // "slide" "alternate"
DEFAULT cDirection   := "left"   // "slide" "alternate"
DEFAULT nScrollAmt   := 5
DEFAULT nScrolldelay := 2
DEFAULT loop         := 0

::StartFont( cFont,,,, nFntSize, cFntColor )

FWrite( ::nH, '<MARQUEE align="'+cAlign+'" ')
FWrite( ::nH,        'behavior="'+cBehavior+'" ')
FWrite( ::nH,        'width="'+NUMTRIM(nWidth)+'%" ')
FWrite( ::nH,        IF( nHeight != NIL, 'height='+NUMTRIM(nHeight)+" ", ""))
FWrite( ::nH,        'bgColor="'+cBgColor+'" ')
FWrite( ::nH,        'scrollamount="'+NUMTRIM(nScrollAmt)+'" ')
FWrite( ::nH,        'scrolldelay="'+NUMTRIM(nScrollDelay)+'" ')
FWrite( ::nH,        'loop='+IF( VALTYPE(loop) == "N", NUMTRIM(loop), loop )+' ')
FWrite( ::nH,        'direction="'+cDirection+'" ')
FWrite( ::nH,        IF( onMsOver != NIL, 'onMouseOver="'+onMsOver+'" ', ""))
FWrite( ::nH,        IF( onMsOut  != NIL, 'onMouseOut="'+onMsOut+'" ', ""))
FWrite( ::nH,        IF( onClick  != NIL, 'onClick="'+onClick+'" ', ""))
FWrite( ::nH,        IF( onStart  != NIL, 'onStart="'+onStart+'" ', ""))
FWrite( ::nH,                IF( onFinish != NIL, 'onFinish="'+onFinish+'" ', ""))
FWrite( ::nH,        '>')
FWrite( ::nH,        cText )

FWrite( ::nH, "</MARQUEE>"+CRLF() )
// FWrite( ::nH, cStr )
::EndFont()

RETURN Self



/****
*
*     Html():StartMarquee()
*
*     Start a scrolling marquee effect definition
*
*/

METHOD StartMarquee( cFont, cFntColor, nFntSize, ;
                     cAlign, nWidth, nHeight, cbgColor, ;
                     cBehavior, cDirection , ;
                     nScrollAmt, nScrollDelay, loop, ;
                     onMsOver, onMsOut, onClick, onStart, onFinish ) CLASS Html

LOCAL cStr := ""

DEFAULT cFont        := "Verdana"
DEFAULT cFntColor    := "white"
DEFAULT nFntSize     := 3
DEFAULT cAlign       := "middle"
DEFAULT nWidth       := 100
DEFAULT cBgColor     := "black"
DEFAULT cBehavior    := "scroll"   // "slide" "alternate"
DEFAULT cDirection   := "left"   // "slide" "alternate"
DEFAULT nScrollAmt   := 5
DEFAULT nScrolldelay := 2
//DEFAULT loop         := -1

::StartFont( cFont,,,, nFntSize, cFntColor )

cStr += '<MARQUEE align="'+cAlign+'" '+;
        'behavior="'+cBehavior+'" '+;
        'width="'+NUMTRIM(nWidth)+'%" '+;
        IF( nHeight != NIL, 'height='+NUMTRIM(nHeight)+" ", "")+;
        'bgColor="'+cBgColor+'" '+;
        'scrollamount="'+NUMTRIM(nScrollAmt)+'" '+;
        'scrolldelay="'+NUMTRIM(nScrollDelay)+'" '+;
        'loop='+IF( VALTYPE(loop) == "N", NUMTRIM(loop), loop )+' '+;
        'direction="'+cDirection+'" '+;
        IF( onMsOver != NIL, 'onMouseOver="'+onMsOver+'" ', "")+;
        IF( onMsOut  != NIL, 'onMouseOut="'+onMsOut+'" ', "")+;
        IF( onClick  != NIL, 'onClick="'+onClick+'" ', "")+;
        IF( onStart  != NIL, 'onStart="'+onStart+'" ', "")+;
        IF( onFinish != NIL, 'onFinish="'+onFinish+'" ', "")+;
        '>'+;
        CRLF()

FWrite( ::nH, cStr )
::EndFont()

RETURN Self


/****
*
*     Html():endMarquee()
*
*
*
*/

METHOD EndMarquee() CLASS Html
FWrite( ::nH, "</MARQUEE>"+CRLF() )
RETURN Self




/****
*
*     Html():iFrame()
*
*     Define an inline frame.
*
*/

METHOD iFrame( name, src, border, marginwidth, marginheight, ;
               scrolling, align, width, height) CLASS Html

LOCAL cStr := "<IFRAME "+CRLF()

DEFAULT border := .T.
DEFAULT name   := "Frame01"
//DEFAULT align  := "vertical"

IF name != NIL
   cStr += SPACE(5)+'        NAME="'+name+'"'+CRLF()
ENDIF
IF src != NIL
   cStr += SPACE(5)+'         SRC="'+src+'"'+CRLF()
ENDIF

IF border
   cStr += SPACE(5)+" FRAMEBORDER='1'"+CRLF()
ELSE
   cStr += SPACE(5)+" FRAMEBORDER='0'"+CRLF()
ENDIF

IF scrolling
   cStr += SPACE(5)+"   SCROLLING='yes'"+CRLF()
ELSE
   cStr += SPACE(5)+"   SCROLLING='no'"+CRLF()
ENDIF

IF marginwidth !=NIL
   cStr += SPACE(5)+" MARGINWIDTH='"+NUMTRIM(marginWidth)+"'"+CRLF()
ENDIF

IF marginheight != NIL
   cStr += SPACE(5)+"MARGINHEIGHT='"+NUMTRIM(marginheight)+"'"+CRLF()
ENDIF

IF width !=NIL
   cStr += SPACE(5)+"       WIDTH='"+NUMTRIM(Width)+"'"+CRLF()
ENDIF

IF height != NIL
   cStr += SPACE(5)+"      HEIGHT='"+NUMTRIM(height)+"'"+CRLF()
ENDIF

IF align != NIL
   cStr += SPACE(5)+"       ALIGN='"+align+"'"+CRLF()
ENDIF


cStr += ">"+CRLF()
cStr += "</IFRAME>"+CRLF()

FWrite( ::nH, cStr )

RETURN Self
/*   New    Methods   */
Method Span( c, Style ) Class html

     Local cStr := "<Span "
     If style != NIL
        cStr += ' style ="' + Style + '"'
     Endif
     cStr += ">" + c + '</span>'
     Fwrite( ::nh, cStr )
Return Self

Method Comment( cText ) Class html

     Local cStr := CRLF() + "<!-- "
     cStr += cText + " -->"
     Fwrite( ::nh, cStr )
Return Self

Method ADDOBJECT( cType, cClassid, cAling, cCode, lDisable, cCodeBase, cName, nWidth, nHeight ) Class HTML

     Local cStr := "<Object "
     If cType != Nil
        cStr += ' type="' + cType + '"' + CRLF()
     Endif
     If cClassId != NIL
        cStr += ' classid="' + cClassId + '"' + CRLF()
     Endif

     If cAling != Nil
        cStr += ' aling ="' + cAling + '"' + CRLF()
     Endif
     If cCode != NIL
        cStr += ' code ="' + cCode + '"' + CRLF()
     Endif
     If lDisable
        cStr += ' DISABLED ' + CRLF()
     Endif
     If cCodebase != NIL
        cStr += ' codebase ="' + cCodebase + '"' + CRLF()
     Endif
     If cName != NIL
        cStr += ' Name ="' + cName + '"' + CRLF()
     Endif
     If nHeight != NIL .and. Valtype( nHeight ) == "N"
        cStr += " height = " + NUMTRIM( nHeight ) + " " + CRLF()
     Elseif nHeight != NIL .and. Valtype( nHeight ) == "C"
        cStr += " height = " + nHeight + " " + CRLF()
     Endif

     If nWidth != NIL .and. Valtype( nWidth ) == "N"
        cStr += " width = " + NUMTRIM( nWidth ) + " " + CRLF()
     Elseif nWidth != NIL .and. Valtype( nWidth ) == "C"
        cStr += " width = " + nWidth + " " + CRLF()
     Endif
     cStr += " >"
     Fwrite( ::nh, cStr + CRLF() )

Return Self
Method ENDOBJECT() Class HTML

     Fwrite( ::nh, "</OBJECT>" + CRLF() )
Return Self
Method ADDPARAM( cType, cValue ) Class HTML

     Fwrite( ::nh, '<param name="' + cType + '" value="' + cValue + '">' + CRLF() )
Return Self

Method PutLinkName( cName ) Class html

     Local cStr := '<a name="'+cName+'"></a>'
     Fwrite( ::nh, cStr )
Return Self

  /*
METHOD MultiCol( txt, cols, gutter, width ) Class Html
            DEFAULT( txt, "" )
            DEFAULT( cols, 2 )
            DEFAULT( gutter, 5 )
            DEFAULT( width, 100 )
            FWrite( ::nH, '<MULTICOL COLS="'+NUMTRIM(cols)+'" GUTTER="'+NUMTRIM(gutter)+'" WIDTH="'+NUMTRIM(width)+'">' )
            FWrite( ::nH, txt )
            FWrite( ::nH, "</MULTICOL>" )
Return Self

*/


//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
// International Support...
//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ


#define GREEK_ALPHABET  {;
                         "Á","Â","Ã","Ä","Å","Æ","Ç","È","É","Ê","Ë","Ì","Í","Î","Ï","Ð","Ñ","Ó","Ô","Õ","Ö","×","Ø","Ù",;   // A-—
                         "á","â","ã","ä","å","æ","ç","è","é","ê","ë","ì","í","î","ï","ð","ñ","ó","ò","ô","õ","ö","÷","ø",;   // ˜-¯
                         "ù","Ü","Ý","Þ","ú","ß","ü","ý","û","þ", "¢","¸","¹","º","¼","¾","¿","Ú","Û" ;   // à-ð
                        }

/****
*
*     InitGreek()
*
*     Initializes the international languages support.
*
*     Uses GREEK_ALPHABET array as a match pattern. Replace with your
*     own character set.
*/

FUNCTION initGreek()
LOCAL i,n
LOCAL aGreek := GREEK_ALPHABET
LOCAL aArr   := ARRAY( 255 )

FOR i = 1 TO 255
    aArr[i] := CHR(i)
NEXT

n := 1
FOR i = 128 TO 175
    aArr[i] := aGreek[n]
    n++
NEXT

FOR i=224 to 240
    aArr[i] := aGreek[n]
    n++
next
aArr[244] := aGreek[n] ; n++
aArr[245] := aGreek[n]

RETURN( aArr )



/****
*
*     Greek2Html()
*
*     Converts International characters to HTML
*/

FUNCTION Greek2Html( cText )
LOCAL i, cStr := ""

IF EMPTY( saGreek )
   saGreek := initGreek()
ENDIF
FOR I=1 to LEN( cText )
   cStr += saGreek[ ASC( substr( cText, i, 1 ) ) ]
NEXT

RETURN( cStr )




/****
*
*     PageHandle()
*
*     Returns the current HTML page handle
*
*/

FUNCTION PageHandle()
RETURN snHtm


/****
*
*     CurrentForm()
*
*     Returns the current ( or last ) form name
*
*/

FUNCTION CurrentForm()
RETURN scForm




/****
*     oPage()
*
*     Return the current HTML() object.
*
*/


FUNCTION oPage()
RETURN soPage



/****
*
*     ParseCGIVar()
*
*     Separates elements of a CGI query environment variable
*
*/

FUNCTION ParseCGIVar( cEnvVar )
   cEnvVar := DecodeURL(cEnvVar)

   IF "=" $ cEnvVar .AND. LEN(cEnvVar) > AT("=", cEnvVar)
      cEnvVar := ALLTRIM(SUBSTR(cEnvVar, AT("=", cEnvVar) + 1))
   ELSE
      cEnvVar := ""
   ENDIF

RETURN cEnvVar




/****
*
*     DecodeURL()
*
*     Decodes a URL encoded string. Also handles international charsets.
*
*/

FUNCTION DecodeURL(cString)
LOCAL i
LOCAL aGreek := GREEK_CGI

   DO WHILE "%26" $ cString
      cString := STUFF(cString, AT("%26", cString), 3, "&")
   ENDDO

   DO WHILE "%2B" $ cString
      cString := STUFF(cString, AT("%2B", cString), 3, "+")
   ENDDO

   DO WHILE "%20" $ cString
      cString := STUFF(cString, AT("%20", cString), 3, " ")
   ENDDO

   DO WHILE "%27" $ cString
      cString := STUFF(cString, AT("%27", cString), 3, "'")
   ENDDO

   DO WHILE "+" $ cString
      cString := STUFF(cString, AT("+", cString), 1, " ")
   ENDDO

   DO WHILE "%2C" $ cString
      cString := STUFF(cString, AT("%2C", cString), 3, ",")
   ENDDO

   DO WHILE "%21" $ cString
      cString := STUFF(cString, AT("%21", cString), 3, "!")
   ENDDO

   DO WHILE "%7E" $ cString
      cString := STUFF(cString, AT("%7E", cString), 3, "~")
   ENDDO

   DO WHILE "%23" $ cString
      cString := STUFF(cString, AT("%23", cString), 3, "#")
   ENDDO

   DO WHILE "%24" $ cString
      cString := STUFF(cString, AT("%24", cString), 3, "!")
   ENDDO

   DO WHILE "%25" $ cString
      cString := STUFF(cString, AT("%25", cString), 3, "%")
   ENDDO

   DO WHILE "%5E" $ cString
      cString := STUFF(cString, AT("%5E", cString), 3, "^")
   ENDDO

   DO WHILE "%28" $ cString
      cString := STUFF(cString, AT("%28", cString), 3, "(")
   ENDDO

   DO WHILE "%29" $ cString
      cString := STUFF(cString, AT("%29", cString), 3, ")")
   ENDDO

   DO WHILE "%60" $ cString
      cString := STUFF(cString, AT("%60", cString), 3, "`")
   ENDDO

   DO WHILE "%2F" $ cString
      cString := STUFF(cString, AT("%2F", cString), 3, "/")
   ENDDO

   FOR i=1 TO LEN( aGreek )
       DO WHILE aGreek[i,2] $ cString
          cString := STUFF(cString, AT( aGreek[i,2], cString), 3, aGreek[i,1] )
       ENDDO
   NEXT


RETURN cString



/****
*
*     JavaCMD()
*
*     Inserts inline Javascript source
*
*/


PROC JavaCMD( nH, cCmd )

 DEFAULT nH   := pageHandle()
 DEFAULT cCmd := ""

 FWRITE( nH, '<SCRIPT LANGUAGE=JavaScript 1.2>'+CRLF()+;
             "<!--"+CRLF() )
 FWRITE( nH, cCmd + CRLF() )
 FWRITE( nH, "//-->"+CRLF()+;
             "</SCRIPT>"+CRLF() )

RETURN



/****
*
*     linkStyle()
*
*
*
*/
      
FUNCTION linkStyle( cHoverStyle, cHoverClr, cHoverBG, ;
                    cLinkStyle, cLinkClr, cLinkBG )

LOCAL cStr := ""
DEFAULT cHoverStyle := "normal"
DEFAULT cLinkStyle  := "normal"
DEFAULT cHoverClr   := "white"
DEFAULT cHoverBg    := "black"
DEFAULT cLinkClr    := "black"
DEFAULT cLinkBg     := "white"
cStr := ;
      "<!-- A:hover {text-decoration:"+cHoverStyle+";color:"+cHoverClr+";background:"+cHoverBG+;
        ";} A:link {text-decoration:"+cLinkStyle+";color:"+cLinkClr+";background:"+cLinkBG+";}-->"

 // A:visited {font:8pt/11pt verdana; color:#4e4e4e;}


RETURN cStr



//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION ANY2STR( xVal )

Local xRet := NIL

IF ValType( xVal ) == "C"
    xRet := IF( EMPTY( xVal ), htmlSpace(2), xVal )

elseIf ValType( xVal ) == "N"
    xRet := alltrim(STR(xVal))

elseIf ValType( xVal ) == "O"
    xRet := "<"+xVal:CLASSNAME()+">"

elseIf ValType( xVal ) == "D"
    xRet := DTOC(xVal)

elseIf ValType( xVal ) == "L"
    xRet := LTOC(xVal)

elseIf ValType( xVal ) == "B"
    xRet := "{||...}"

elseIf ValType( xVal ) == NIL
    xRet := "NIL"

elseIf ValType( xVal ) == "U"
    xRet := "<Unknown Value>"

ENDIF

Return( xRet )



//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION HTMLANY2STR( xVal )

Local xRet := NIL

IF ValType( xVal ) == "C"
    xRet := IF( EMPTY( xVal ), ".", xVal )

elseIf ValType( xVal ) == "N"
    xRet := alltrim(STR(xVal))

elseIf ValType( xVal ) == "O"
    xRet := "<"+xVal:CLASSNAME()+">"

elseIf ValType( xVal ) == "D"
    xRet := DTOC(xVal)

elseIf ValType( xVal ) == "L"
    xRet := LTOC(xVal)

elseIf ValType( xVal ) == "B"
    xRet := "{||...}"

elseIf ValType( xVal ) == NIL
    xRet := "NIL"

elseIf ValType( xVal ) == "U"
    xRet := "<Unknown Value>"

ENDIF

Return( xRet )





//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
FUNCTION ListAsArray( cList, cDelimiter )
   LOCAL nPos              // Position of cDelimiter in cList
   LOCAL aList := {}       // Define an empty array

   DEFAULT cDelimiter TO ","

   // Loop while there are more items to extract
   DO WHILE ( nPos := AT( cDelimiter, cList )) != 0
      
      // Add the item to aList and remove it from cList
      AADD( aList, ALLTRIM(SUBSTR( cList, 1, nPos - 1 )))
      cList := SUBSTR( cList, nPos + 1 )

   ENDDO
   AADD( aList, cList )                         // Add final element

   RETURN ( aList )                             // Return the array

               


//*** EOF ***//
