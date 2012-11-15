/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Main HTML include File Definition of all html lib commands
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

#ifndef _HTML_CH

#include "simpleio.ch"
#include "htmlform.ch"
#include "htmlclrs.ch"
#include "cgi.ch"

#xtranslate CRLF( <str> ) => ( <str> + Chr( 13 ) + Chr( 10 ) )
#xtranslate CRLF()        => ( Chr( 13 ) + Chr( 10 ) )

/*
#xcommand DEFINE HTML                ;
          [FILE <file>]              ;
          [TITLE <title>]            ;
          [LINKTITLE <linktitle>]    ;
          [CHARSET <charset>]        ;
          [JAVASOURCE <javasrc,...>] ;
          [JAVACODE <javacode,...>]  ;
          [BGIMAGE <bgimg>]          ;
          [BGCOLOR <bgcolor>]        ;
          [TEXTCOLOR <txtcolor>]     ;
          [ONLOAD <onload>]          ;
          [ONOPEN <onload>]          ;
          [ONUNLOAD <onunload>]      ;
          [ONCLOSE <onunload>]       ;
          [LINKCOLOR <lcolor>]       ;
          [VLINKCOLOR <vlcolor>]     ;
          [ALINKCOLOR <alcolor>]     ;
          [STYLE <cStyle>]           ;
          [IMAGES <aImages,...>]     ;
          [BASEURL <baseurl>]        ;
          [BASETARGET <basetarget>]  ;
          [STYLESHEET <cStyleScr>]   ;
          [REFRESH <nRefresh>]       ;
          [REFRESHURL <cRefreshURL>]            ;
          [<lcache:NOCACHE>]           ;
          OF <oHtml>                 ;
          =>                         ;
          <oHtml> := THtml():new( <file>,<title>,<linktitle>,<charset>,;
                                 [{<(javasrc)>}], ;
                                 [<bgimg>], [<bgcolor>], [<txtcolor>],;
                                 [{<(javacode)>}],;
                                 [<(onload)>], [<(onunload)>], ;
                                 [<(lcolor)>],[<(vlcolor)>],[<(alcolor)>],;
                                 [<(cStyle)>], [<aImages>],;
                                 [<baseurl>], [<basetarget>] ,;
                                 [<nRefresh>], [<cRefreshURL>],;
                                 <cStyleScr>,<.lcache.>)
*/
#xcommand DEFINE HTML                 ;
          [FILE <file>]              ;
          [TITLE <title>]            ;
          [LINKTITLE <linktitle>]    ;
          [CHARSET <charset>]        ;
          [JAVASOURCE <javasrc,...>] ;
          [JAVACODE <javacode,...>]  ;
          [BGIMAGE <bgimg>]          ;
          [BGCOLOR <bgcolor>]        ;
          [TEXTCOLOR <txtcolor>]     ;
          [ONLOAD <onload>]          ;
          [ONOPEN <onload>]          ;
          [ONUNLOAD <onunload>]      ;
          [ONCLOSE <onunload>]       ;
          [LINKCOLOR <lcolor>]       ;
          [VLINKCOLOR <vlcolor>]     ;
          [ALINKCOLOR <alcolor>]     ;
          [STYLE <cStyle>]           ;
          [IMAGES <aImages>]         ;
          [SERVERSOURCE <srvr,...>]  ;
          [BASEURL <baseurl>]        ;
          [BASETARGET <basetarget>]  ;
          [REFRESH <nrefr> [REFRESHURL <refrURL>] ] ;
          [STYLESHEET <cStyleScr>]   ;
          [<lcache:NOCACHE>]           ;
          [NOF <nof> ]               ;
          [TOPMARGIN  <nMarginTop>];
          [LEFTMARGIN  <nMarginLeft>] ;
          [MARGINHEIGHT <nMarginHeight>];
          [MARGINWIDTH <nMarginWidth>] ;
          OF <oHtml>                 ;
          =>                         ;
          <oHtml> := THtml():new(<title>, <linktitle>, <charset>,;
                                   [{<(javasrc)>}], ;
                                   [<bgimg>], [<bgcolor>], [<txtcolor>],;
                                   [{<(javacode)>}],;
                                   [<(onload)>], [<(onunload)>], ;
                                   [<(lcolor)>], [<(vlcolor)>], [<(alcolor)>],;
                                   [<(cStyle)>], [<aImages>], [{<(srvr)>}],;
                                   [<baseurl>], [<basetarget>], ;
                                   <nrefr>, <refrURL>, <cStyleScr>, <.lcache.>, <nof>,;
                                   <nMarginTop>, <nMarginHeight>, <nMarginWidth>, <nMarginLeft> ,;
                                   .F.,<file>)

#xcommand DEFINE CGI                 ;
          [FILE <file>]              ;
          [TITLE <title>]            ;
          [LINKTITLE <linktitle>]    ;
          [CHARSET <charset>]        ;
          [JAVASOURCE <javasrc,...>] ;
          [JAVACODE <javacode,...>]  ;
          [BGIMAGE <bgimg>]          ;
          [BGCOLOR <bgcolor>]        ;
          [TEXTCOLOR <txtcolor>]     ;
          [ONLOAD <onload>]          ;
          [ONOPEN <onload>]          ;
          [ONUNLOAD <onunload>]      ;
          [ONCLOSE <onunload>]       ;
          [LINKCOLOR <lcolor>]       ;
          [VLINKCOLOR <vlcolor>]     ;
          [ALINKCOLOR <alcolor>]     ;
          [STYLE <cStyle>]           ;
          [IMAGES <aImages>]         ;
          [SERVERSOURCE <srvr,...>]  ;
          [BASEURL <baseurl>]        ;
          [BASETARGET <basetarget>]  ;
          [REFRESH <nrefr> [REFRESHURL <refrURL>] ] ;
          [STYLESHEET <cStyleScr>]   ;
          [<lcache:NOCACHE>]           ;
          [NOF <nof> ]               ;
          [TOPMARGIN  <nMarginTop>];
          [LEFTMARGIN  <nMarginLeft>] ;
          [MARGINHEIGHT <nMarginHeight>];
          [MARGINWIDTH <nMarginWidth>] ;
          OF <oHtml>                 ;
          =>                         ;
          <oHtml> := THtml():new(<title>, <linktitle>, <charset>,;
                                   [{<(javasrc)>}], ;
                                   [<bgimg>], [<bgcolor>], [<txtcolor>],;
                                   [{<(javacode)>}],;
                                   [<(onload)>], [<(onunload)>], ;
                                   [<(lcolor)>], [<(vlcolor)>], [<(alcolor)>],;
                                   [<(cStyle)>], [<aImages>], [{<(srvr)>}],;
                                   [<baseurl>], [<basetarget>], ;
                                   <nrefr>, <refrURL>, <cStyleScr>, <.lcache.>, <nof>,;
                                   <nMarginTop>, <nMarginHeight>, <nMarginWidth>, <nMarginLeft> ,;
                                   .T., <file> )

          // [<auth:AUTHENTICATE>]      ;
          // [<.auth.>]


#xcommand DEFINE TABLE               ;
          [COLS <cols>]              ;
          [BORDER <border>]          ;
          [WIDTH <width>]            ;
          [HEIGHT <height>]          ;
          [COLORFORE <clrfore>]      ;
          [BGCOLOR <clrbg>]          ;
          [COLORBG <clrbg>]          ;
          [COLORDARK <clrdrk>]       ;
          [COLORLIGHT <clrlt>]       ;
          [BORDERCOLOR <cClrBorder>] ;
          [BGIMAGE <bgImage>]        ;
          [<aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [CELLPADDING <nCellPadding>] ;
          [CELLSPACING <nCellSpacing>] ;
          [STYLE <cStyle>]           ;
          [<d:3D>]                   ;
          [<c:RCOLS>]                ;
          [<r:RROWS>]                ;
          [<x:RALL>]                 ;
          [ID <id> ]                 ;
          [NOF <nof> ]               ;
          OF <oHtm>                  ;
          =>                         ;
          <oHtm>:defineTable( <cols>, <border>, <width>,<height>, ;
                              <clrfore>, <clrbg>, ;
                              <.d.>, <.c.>, <.r.>,;
                              <clrdrk>, <clrlt>, <cClrBorder>,  ;
                              <nCellPadding>, <nCellSpacing>, ;
                              __HTML_ALING__ [<aln>], <.x.>, <bgImage>, ;
                              <cStyle>, <id> , <nof>)


#xcommand DEFINE TABLE HEADER        ;
          [TEXT <cHead>]             ;
          [COLOR <cColor>]           ;
          [<aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [FONT <cFont>]             ;
          [SIZE <nSize>]             ;
          [FONTCOLOR <cFntColor>]    ;
          [HEIGHT <nHeight>]         ;
          OF <oHtm>                  ;
          =>                         ;
          <oHtm>:TableHead( <cHead>, <cColor>, __HTML_ALING__ [<aln>], <cFont>, ;
                            <nSize>, <cFntColor>, <nHeight> )


#xcommand DEFINE CELL                ;
          [COLOR <cColor>]           ;
          [ALING <aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [FONT <cFont>]             ;
          [SIZE <nSize>]             ;
          [FONTCOLOR <cFntColor>]    ;
          [HEIGHT <nHeight>]         ;
          [IMAGE <img>]              ;
          [BGIMAGE <img>]            ;
          [WIDTH <width>]            ;
          [COLORDARK <clrdrk>]       ;
          [COLORLIGHT <clrlt>]       ;
          [ROWSPAN <rspan>]          ;
          [COLSPAN <cspan>]          ;
          [<nowrap:NOWRAP>]          ;
          [<valn:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [BORDERCOLOR <bclrlt>]       ;
          [CLASS <cClass>]           ;
          OF <oHtm>                  ;
          =>                         ;
          <oHtm>:newTableCell( __HTML_ALING__ [<aln>], <cColor>, <cFont>, ;
                               <nSize>, <cFntColor>, <nHeight>, ;
                               <img>, <width>, ! <.nowrap.>, ;
                               <cspan>, <rspan> ,__HTML_ALING__ [<valn>], <clrdrk>, <clrlt>, <bclrlt>, <cClass>)


#xcommand TABLE CELL                 ;
          [COLOR <cColor>]           ;
          [FONT <cFont>]             ;
          [SIZE <nSize>]             ;
          [FONTCOLOR <cFntColor>]    ;
          [HEIGHT <nHeight>]         ;
          [IMAGE <img>]              ;
          [WIDTH <width>]            ;
          [ROWSPAN <rspan>]          ;
          [COLSPAN <cspan>]          ;
          [<nowrap:NOWRAP>]          ;
          [<aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          OF <oHtm>                  ;
          =>                         ;
          <oHtm>:newTableCell( __HTML_ALING__ [<aln>], <cColor>, <cFont>, ;
                               <nSize>, <cFntColor>, <nHeight>, ;
                               <img>, <width>, ! <.nowrap.>, ;
                               <cspan>, <rspan> )


#xcommand TABLE ROW                 ;
          [COLOR <cColor>]           ;
          [<aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [ALIGN <aln1:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          OF <oHtm>                  ;
          =>                         ;
          <oHtm>:NewTableRow( <cColor>, __HTML_ALING__ [<aln>], __HTML_ALING__ [<aln1>] );

#xcommand DEFINE FONT [<cFont>]       ;
          [<ftype:BOLD,ITALIC,ULINE,UNDERLINE>] ;
          [SIZE <s>]                  ;
          [COLOR <c>]                 ;
          [OF <oHtm>]                 ;
          =>                          ;
          <oHtm>:defineFont( [<(cFont)>], __HTML_FTYPE__ [<ftype>], ;
                             [<s>], [<c>] )


#xcommand SET FONT [<cFont>]   ;
          [<bold:BOLD>]        ;
          [<itl:ITALIC>]       ;
          [<uln:UNDERLINE>]    ;
          [SIZE <s>]           ;
          [COLOR <c>]          ;
          [<lset:NOTSET>]      ;
          [OF <oHtm>]          ;
          =>                   ;
          <oHtm>:SetFont( [<(cFont)>], [<.bold.>], ;
                          [<.itl.>], [<.uln.>], ;
                          [<s>], [<c>], ! <.lset.> )


#xcommand START FONT [<cFont>] ;
          [<bold:BOLD>]        ;
          [<itl:ITALIC>]       ;
          [<uln:UNDERLINE>]    ;
          [SIZE <s>]           ;
          [COLOR <c>]          ;
          [<lset:NOTSET>]      ;
          [OF <oHtm>]          ;
          =>                   ;
          <oHtm>:StartFont( [<(cFont)>], [<.bold.>], ;
                          [<.itl.>], [<.uln.>], ;
                          [<s>], [<c>], ! <.lset.> )


#xcommand FONTEND[<cFont>]   ;
          [OF <oHtm>]          ;
          =>                   ;
          <oHtm>:EndFont()

#xcommand END FONT   ;
          [OF <oHtm>]          ;
          =>                   ;
          <oHtm>:EndFont()


#xcommand SAY <str>              ;
          [FONT <fnt> ]          ;
          [TYPE <type>]          ;
          [SIZE <size>]          ;
          [STYLE <style>]        ;
          [COLOR <clr>]          ;
          <of:OF,IN> <oHtm>      ;
          =>                     ;
          <oHtm>:Say( <str>, <fnt>, <size>, <type>, <clr>, <style>)


#xcommand PUSH BUTTON            ;
          [NAME <name>]          ;
          [CAPTION <caption>]    ;
          [ONCLICK <onclick>]    ;
          [ONFOCUS <onfocus>]    ;
          [ONBLUR <onblur>]      ;
          [ONMSOVER <onmsov>]    ;
          [ONMSOUT <onmsou>]     ;
          [CGIAPP <cgiapp>]      ;
          [STYLE <style>]        ;
          [ID <id>]              ;
          [OF <oHtm>]            ;
          =>                     ;
          <oHtm>:PushButton( <(name)>, <(caption)>, ;
                             [<(cgiapp)>], [<(onclick)>], ;
                             [<(onfocus)>], [<(onblur)>],;
                             [<(onmsov)>], [<(onmsou)>],;
                             [<(style)>], [<(id)>] )

#xcommand BUTTON                 ;
          [NAME <name>]          ;
          [CAPTION <caption>]    ;
          [ONCLICK <onclick>]    ;
          [ONMSOVER <onmsov>]    ;
          [ONMSOUT <onmsou>]     ;
          [CGIAPP <cgiapp>]      ;
          [STYLE <style>]        ;
          [ID <id>]              ;
          [OF <oHtm>]            ;
          =>;
          <oHtm>:Button( <(name)>, <(caption)>,      ;
                         [<(onclick)>],[<(cgiapp)>], ;
                         [<(onmsov)>], [<(onmsou)>],;
                         [<(style)>], [<(id)>] )


#xcommand END BUTTON OF <oHtm> ;
          =>;
          <oHtm>:endButton()


#xcommand IMAGE <image>             ;
          URL <url>                 ;
          [BORDER <border>]         ;
          [HEIGHT <height>]         ;
          [WIDTH  <width>]          ;
          [ONCLICK <onclick>]       ;
          [ONMOUSEOVER <onmsover>]  ;
          [ONMOUSEOUT <onmsout>]    ;
          [NAME <name>]             ;
          [TARGET <target>]         ;
          [ALT <alt>]               ;
          [<break:BREAK>]             ;
          OF <oHtm>                 ;
          =>                        ;
          <oHtm>:putImageURL( <image>, <border>, <height>, <url>,;
                              <onclick>, <onmsover>, <onmsout>,  ;
                              <name>, <alt>, <target>, <width>,<.break.>)


#xcommand IMAGE <image>             ;
          [BORDER <border>]         ;
          [HEIGHT <height>]         ;
          [WIDTH <width>]           ;
          [ONCLICK <onclick>]       ;
          [ONMOUSEOVER <onmsover>]  ;
          [ONMOUSEOUT <onmsout>]    ;
          [NAME <name>]             ;
          [TARGET <target>]         ;
          [ALT <alt>]               ;
          [<break:BREAK>]           ;
          [ID <iD>  ]               ;
          [MAP <map> ]              ;
          [HSPACE <hspace>]         ;
          [ALIGN <aln1:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          OF <oHtm>                 ;
          =>                        ;
          <oHtm>:putImage( <image>, <border>, <height>,;
                           <onclick>, <onmsover>, <onmsout>, ;
                           <name>, <alt>, ;
                           <target>, <width>, <.break.>, <iD>, <map>, __HTML_ALING__ [<aln1>] , <hspace>)

#xcommand LINK <url>                ;
          [TEXT <text>]             ;
          [FONT <font>]             ;
          [FONTCOLOR <clr>]         ;
          [SIZE <size>]             ;
          [STYLE <style>]           ;
          [<bld:BOLD>]              ;
          [ONCLICK <onclick>]       ;
          [ONMOUSEOVER <onmsover>]  ;
          [ONMOUSEOUT <onmsout>]    ;
          [TARGET <target>]         ;
          [<break:BREAK>]           ;
          [CLASS <cClass>]          ;
          OF <oHtm>                 ;
          =>                        ;
          <oHtm>:putTextURL( <text>, <url>, ;
                                <onclick>, <onmsover>, <onmsout>, ;
                             <target>, <font>, <clr>, <size>, <style>, <.bld.> ,<.break.>, <cClass>)

#xcommand LINK <url>                ;
          [IMAGE <image>]           ;
          [WIDTH <width>]           ;
          [HEIGHT <height>]         ;
          [ONCLICK <onclick>]       ;
          [BORDER  <border>]        ;
          [ONMOUSEOVER <onmsover>]  ;
          [ONMOUSEOUT <onmsout>]    ;
          [NAME <name>]             ;
          [ALT <alt>]               ;
          [TARGET <target>]         ;
          [<break:BREAK>]           ;
          [CLASS <cClass>]          ;
          [ID <id>]                 ;
          [HSPACE <hspace>]         ;
          [ALIGN <aln1:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          OF <oHtm>                 ;
          =>                        ;
          <oHtm>:putImageURL( <image>,<border>,<height>,<url>,;
                             <onclick>, <onmsover>, <onmsout>, <name>, <alt>, ;
                             <target>, <width>, <.break.>, <cClass>, <id>, < hspace >, __HTML_ALING__ [<aln1>]  )


#xcommand NEW FRAMEPAGE         ;
          [TITLE <title>]       ;
          [FILE <file>]         ;
          OF <oFrm>             ;
          =>                    ;
          <oFrm>:=THtmlFrameSet():New( <file>, <title> )


#xcommand FRAMESET              ;
          [TITLE <title>]       ;
          [FILE <file>]         ;
          [ROWS <rows,...>]     ;
          [COLS <cols,...>]     ;
          [ONLOAD <onload>]     ;
          [ONUNLOAD <onunload>] ;
          OF <oFrm>             ;
          =>                    ;
          <oFrm>:StartSet( [{<(rows)>}], [{<(cols)>}], ;
                           [<(onload)>], [<(onunload)>] )



#xcommand FRAME                                        ;
          [NAME <name>]                                ;
          [URL <url>]                                  ;
          [<brd:NOBORDER>]                             ;
          [<res:NORESIZE>]                             ;
          [<scr:SCROLLBAR>]                            ;
          [SCROLLING <scrl:YES,NO,ON,OFF,AUTO>]        ;
          [TARGET <target>]                            ;
          [MARGINWIDTH <mw>]                           ;
          [MARGINHEIGHT <mh>]                          ;
          OF <oFrm>                                    ;
          =>                                           ;
          <oFrm>:frame( [<name>], [<url>],             ;
                        ! <.brd.>, ! <.res.>, [<.scr.>], ;
                        [<mw>], [<mh>], [<target>], ;
                        __HTML_SCROLL__ [<scrl>] )

#xcommand ENDSET <oFrm>        => <oFrm>:EndSet()
#xcommand END FRAMEPAGE <oFrm> => <oFrm>:End()


#xcommand MARQUEE <cText>                    ;
          [FONT <cFont>]                     ;
          [FONTCOLOR <cFntColor>]            ;
          [FONTSIZE <nFntSize>]              ;
          [<pos:TOP,MIDDLE,BOTTOM>]          ;
          [WIDTH <nWidth>]                   ;
          [HEIGHT <nHeight>]                 ;
          [BGCOLOR <cbgColor>]               ;
          [<bhv:SCROLL,SLIDE,ALT,ALTERNATE>] ;
          [<dir:LEFT,RIGHT>]                 ;
          [SCROLLAMT <nScrollAmt>]           ;
          [SCROLLDELAY <nScrollDel>]         ;
          [LOOP <loop>]                      ;
          [ONMSOVER <onmsover>]              ;
          [ONMSOUT <onmsout>]                ;
          [ONCLICK <onclick>]                ;
          [ONSTART <onstart>]                ;
          [ONFINISH <onfinish>]              ;
          OF <oHtm>                          ;
          =>                                 ;
          <oHtm>:Marquee( <cText>, <cFont>, <cFntColor>, <nFntSize>, ;
                   __HTML_POS__ [<pos>], <nWidth>, <nHeight>, <cbgColor>, ;
                   __HTML_BEHAVE__ [<bhv>], __HTML_DIR__ [<dir>], ;
                   <nScrollAmt>, <nScrollDel>, <loop>,;
                   [<(onmsover)>], [<(onmsout)>], [<(onclick)>], ;
                   [<(onstart)>], [<(onfinish)>] )


#xcommand START MARQUEE                      ;
          [FONT <cFont>]                     ;
          [FONTCOLOR <cFntColor>]            ;
          [FONTSIZE <nFntSize>]              ;
          [<pos:TOP,MIDDLE,BOTTOM>]          ;
          [WIDTH <nWidth>]                   ;
          [HEIGHT <nHeight>]                 ;
          [BGCOLOR <cbgColor>]               ;
          [<bhv:SCROLL,SLIDE,ALT,ALTERNATE>] ;
          [<dir:LEFT,RIGHT>]                 ;
          [SCROLLAMT <nScrollAmt>]           ;
          [SCROLLDELAY <nScrollDel>]         ;
          [LOOP <loop>]                      ;
          [ONMSOVER <onmsover>]              ;
          [ONMSOUT <onmsout>]                ;
          [ONCLICK <onclick>]                ;
          [ONSTART <onstart>]                ;
          [ONFINISH <onfinish>]              ;
          OF <oHtm>                          ;
          =>                                 ;
          <oHtm>:StartMarquee( <cFont>, <cFntColor>, <nFntSize>, ;
                   __HTML_POS__ [<pos>], <nWidth>, <nHeight>, <cbgColor>, ;
                   __HTML_BEHAVE__ [<bhv>], __HTML_DIR__ [<dir>], ;
                   <nScrollAmt>, <nScrollDel>, <loop>,;
                   [<(onmsover)>], [<(onmsout)>], [<(onclick)>], ;
                   [<(onstart)>], [<(onfinish)>] )



#xcommand END MARQUEE OF <oHtm>  => <oHtm>:endMarquee()



#xcommand INLINE FRAME                ;
          [NAME <name>]               ;
          [SRC <url>]                 ;
          [URL <url>]                 ;
          [HEIGHT <height>]           ;
          [WIDTH <width>]             ;
          [MARGINHEIGHT <mheight>]    ;
          [MARGINWIDTH <mwidth>]      ;
          [<scr:SCROLLING>]           ;
          [<brd:NOBORDER>]            ;
          [ALIGN <align>]             ;
          OF <oHtm>                   ;
          =>                          ;
          <oHtm>:iFrame( <name>, <url>, ! <.brd.>, ;
                         <mwidth>, <mheight>, ;
                         <.scr.>, <align>, ;
                         <width>, <height> )



#xcommand COUNTER            ;
          [NUMBER <num>]     ;
          [DIGITS <dig>]     ;
          [COLOR <clr>]      ;
          [WIDTH <w>]        ;
          [BORDER <b>]       ;
          [FOLDER <folder>]  ;
          OF <oHtm>          ;
          =>                 ;
          PutCounter( <oHtm>, <num>, <folder>, <dig>, <w>, <clr>, <b> )

/*******************************************/
/*            New Commands                 */
/*******************************************/

#xcommand LINKS <url>                ;
          [TEXT  <text>]            ;
          [IMAGE <image>]           ;
          [WIDTH <width>]           ;
          [HEIGHT <height>]         ;
          [ONCLICK <onclick>]       ;
          [BORDER  <border>]        ;
          [ONMOUSEOVER <onmsover>]  ;
          [ONMOUSEOUT <onmsout>]    ;
          [NAME <name>]             ;
          [ALT <alt>]               ;
          [TARGET <target>]         ;
          [<break:BREAK>]           ;
          [CLASS <cClass>]          ;
          OF <oHtm>                 ;
          =>                        ;
          <oHtm>:putTextImageURL( <image>, <border>, <height>, <url>,;
                             <onclick>, <onmsover>, <onmsout>, <name>, <alt> , ;
                             <target>,<width>, <.break.>, <cClass>, <text>)
#xcommand SPAN <text>           ;
          [STYLE <cStyle>]      ;
          OF <oHtm>             ;
          =>                    ;
          <oHtm>:Span(<text>,<cStyle>)

#xcommand Comment <text>        ;
          OF <oHtm>             ;
          =>                    ;
          <oHtm>:Comment(<text>)

#xcommand LINKNAME <cName>      ;
          OF <oHtm>             ;
          =>                    ;
          <oHtm>:PutLinkName(<cName>)


#xcommand CREATE OBJECT          ;
          [NAME <cName>]         ;
          [TYPE <cType>]         ;
          [CLASSID <cClassid>]   ;
          [CODE <cCode>]         ;
          [CODEBASE <cCodeBase>] ;
          [HEIGHT<nHeight>]      ;
          [WIDTH <nWidth>]       ;
          [<lDisable:DISABLED>]  ;
          [<aln:LEFT,RIGHT,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSBOTTOM,BASELINE>];
          OF <oHtm>              ;
          =>                     ;
          <oHtm>:ADDOBJECT(<cType>,<cClassid>,__HTML_ALING__ [<aln>],<cCode>,<.lDisable.>,<cCodeBase>,<cName>,<nWidth>,<nHeight>)

#xcommand OBJECT PARAM   ;
          NAME <cName>   ;
          VALUE <cValue> ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:ADDPARAM(<cName>,<cValue>)

#xcommand END OBJECT     ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:ENDOBJECT()

#xcommand MAPEND         ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:EndMap()

#xcommand CELLEND        ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:EndTableCell( .F. )

#xcommand ROWEND         ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:EndTableRow()

#xcommand TABLEEND       ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:EndTable()

#xcommand MAP <n> ;
          OF <ohtm> ;
          => ;
          <ohtm>:NewMap( <(n)> )

#xcommand AREA ;
          [SHAPE <Shape> ];
          [ALT   <Alt> ];
          [COORD <coord> ];
          [URL <Url> ];
          OF <ohtm> ;
          =>;
          <ohtm>:MapArea( <(Shape)>, <Alt>, <coord>, <Url> )

#xtranslate __HTML_SCROLL__                   => "AUTO"
#xtranslate __HTML_SCROLL__ <scrl:NO>         => "NO"
#xtranslate __HTML_SCROLL__ <scrl:OFF>        => "NO"
#xtranslate __HTML_SCROLL__ <scrl:ON>         => "YES"
#xtranslate __HTML_SCROLL__ <scrl:YES>        => "YES"
#xtranslate __HTML_SCROLL__ <scrl:AUTO>       => "AUTO"


#xtranslate __HTML_ALING__                    =>  NIL
#xtranslate __HTML_ALING__ <aln:LEFT>         => "LEFT"
#xtranslate __HTML_ALING__ <aln:RIGHT>        => "RIGHT"
#xtranslate __HTML_ALING__ <aln:CENTER>       => "center"
#xtranslate __HTML_ALING__ <aln:MIDDLE>       => "MIDDLE"
#xtranslate __HTML_ALING__ <aln:TOP>          => "TOP"
#xtranslate __HTML_ALING__ <aln:TEXTTOP>      => "TEXTTOP"
#xtranslate __HTML_ALING__ <aln:BOTTOM>       => "BOTTOM"
#xtranslate __HTML_ALING__ <aln:ABSMIDDLE>    => "ABSMIDDLE"
#xtranslate __HTML_ALING__ <aln:ABSCENTER>    => "ABSMIDDLE"
#xtranslate __HTML_ALING__ <aln:ABSBOTTOM>    => "ABSBOTTOM"
#xtranslate __HTML_ALING__ <aln:BASELINE>     => "BASELINE"

#xtranslate __HTML_POS__                      => NIL
#xtranslate __HTML_POS__ <pos:TOP>            => "TOP"
#xtranslate __HTML_POS__ <pos:MIDDLE>         => "MIDDLE"
#xtranslate __HTML_POS__ <pos:BOTTOM>         => "BOTTOM"

#xtranslate __HTML_DIR__                      => NIL
#xtranslate __HTML_DIR__ <dir:LEFT>           => "LEFT"
#xtranslate __HTML_DIR__ <dir:RIGHT>          => "RIGHT"

#xtranslate __HTML_BEHAVE__                   => NIL
#xtranslate __HTML_BEHAVE__ <bhv:SCROLL>      => "SCROLL"
#xtranslate __HTML_BEHAVE__ <bhv:SLIDE>       => "SLIDE"
#xtranslate __HTML_BEHAVE__ <bhv:ALTERNATE>   => "ALTERNATE"
#xtranslate __HTML_BEHAVE__ <bhv:ALT>         => "ALTERNATE"

#xtranslate __HTML_FTYPE__                    => NIL
#xtranslate __HTML_FTYPE__ <ftype:ITALIC>     => "<I>"
#xtranslate __HTML_FTYPE__ <ftype:BOLD>       => "<B>"
#xtranslate __HTML_FTYPE__ <ftype:ULINE>      => "<U>"
#xtranslate __HTML_FTYPE__ <ftype:UNDERLINE>  => "<U>"


#define _HTML_CH
#endif
