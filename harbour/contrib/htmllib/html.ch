/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HTML.CH Main HTML include File Definition of all html lib commands
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

#ifndef _HTML_CH

#include "simpleIO.ch"
#include "forms.ch"
#include "Colors.ch"

#xTranslate  DEFAULT( <p>, <v> ) => <p> := IF( <p> == NIL, <v>, <p> )


#define STD_IN       0
#define STD_OUT      1


#define _WHITE_BLUE_STYLE   "<!-- "+;
                                " A:visited {text-decoration:none;color:blue;background:none;} "+;
                                " A:link {text-decoration:none;color:blue;background:none;} "+;
                                " A:active {text-decoration:none;color:blue;background:none;} "+;
                                " A:hover {text-decoration:none;color:white;background:blue;} "+;
                                "-->"
#define _BLUE_WHITE_STYLE   "<!-- "+;
                                 "A:visited {text-decoration:none;color:white;background:none;} "+;
                                 "A:link {text-decoration:none;color:white;background:none;} "+;
                                 "A:active {text-decoration:none;color:white;background:none;} "+;
                                 "A:hover {text-decoration:none;color:blue;background:white;}  "+;
                                 "-->"
#define _WHITE_RED_STYLE    "<!-- "+;
                                 "A:visited {text-decoration:none;color:red;background:none;} "+;
                                 "A:link {text-decoration:none;color:red;background:none;} "+;
                                 "A:active {text-decoration:none;color:red;background:none;} "+;
                                 "A:hover {text-decoration:none;color:white;background:red;} "+;
                                 "-->"
#define _WHITE_BLACK_STYLE  "<!-- "+;
                                 "A:visited {text-decoration:none;color:black;background:none;}"+;
                                 "A:link {text-decoration:none;color:black;background:none;} "+;
                                 "A:active {text-decoration:none;color:black;background:none;}"+;
                                 "A:hover {text-decoration:none;color:white;background:black;} "+;
                                 " -->"

#define LINE_BREAK   "<BR>"
#define BOLD_ON      "<B>"
#define BOLD_OFF     "</B>"

#define _HTML_SPACE  chr(38)+"nbsp;"

#xTranslate HTMLSpace( <n> )     => replicate( "&nbsp;", <n> )  //"&#32;"

#xTranslate :putLink( <c>, <u> ) => :putTextURL( <c>, <u> )



#define CLR_LIGHT_YELLOW  "#fffffc0"
#define CLR_DARK_YELLOW   "#fffffcc"
#define CLR_DARKER_YELLOW "#fffff80"
#define CLR_LIGHT_BLUE    "#DEEFEF"  //"00000ff"
#define CLR_MAGENTA       "#FFD0FF"
#define CLR_CYAN          "#D0FFFFF"
#define CLR_LIGHT_GRAY    "#F0F0F0"

#define CLR_BLACK         "black"      //"#0000000"
#define CLR_MAROON        "maroon"
#define CLR_GREEN         "green"
#define CLR_OLIVE         "olive"
#define CLR_NAVY          "navy"
#define CLR_PURPLE        "purple"
#define CLR_TEAL          "teal"
#define CLR_GRAY          "gray"
#define CLR_SILVER        "silver"
#define CLR_RED           "red"
#define CLR_LIME          "lime"
#define CLR_YELLOW        "yellow"
#define CLR_BLUE          "blue"
#define CLR_FUCHSIA       "fuchsia"
#define CLR_AQUA          "aqua"
#define CLR_WHITE         "white"      //"#fffffff"


#xCommand DEFINE HTML                ;
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
          [<lcache:NOCACHE>]           ;
          OF <oHtml>                 ;
          =>                         ;
          <oHtml> := html():new( <file>,<title>,<linktitle>,<charset>,;
                                 [{<(javasrc)>}], ;
                                 [<bgimg>], [<bgcolor>], [<txtcolor>],;
                                 [{<(javacode)>}],;
                                 [<(onload)>], [<(onunload)>], ;
                                 [<(lcolor)>],[<(vlcolor)>],[<(alcolor)>],;
                                 [<(cStyle)>], [<aImages>],;
                                 [<baseurl>], [<basetarget>] ,<cStyleScr>,<.lcache.>)

#xCommand DEFINE CGI                 ;
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
          OF <oHtml>                 ;
          =>                         ;
          <oHtml> := html():CGInew(<title>, <linktitle>, <charset>,;
                                   [{<(javasrc)>}], ;
                                   [<bgimg>], [<bgcolor>], [<txtcolor>],;
                                   [{<(javacode)>}],;
                                   [<(onload)>], [<(onunload)>], ;
                                   [<(lcolor)>],[<(vlcolor)>],[<(alcolor)>],;
                                   [<(cStyle)>], [<aImages>], [{<(srvr)>}],;
                                   [<baseurl>], [<basetarget>], ;
                                   <nrefr>, <refrURL> ,<cStyleScr>,<.lcache.>)
          // [<auth:AUTHENTICATE>]      ;
          // [<.auth.>]


#xCommand DEFINE TABLE               ;
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
          OF <oHtm>                  ;
          =>                         ;
          <oHtm>:defineTable( <cols>, <border>, <width>,<height>, ;
                              <clrfore>, <clrbg>, ;
                              <.d.>, <.c.>, <.r.>,;
                              <clrdrk>, <clrlt>, <cClrBorder>,  ;
                              <nCellPadding>,<nCellSpacing>,__ALIGN__ [<aln>],<.x.>,<bgImage>,<cStyle>)


#xCommand DEFINE TABLE HEADER        ;
          [TEXT <cHead>]             ;
          [COLOR <cColor>]           ;
          [<aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [FONT <cFont>]             ;
          [SIZE <nSize>]             ;
          [FONTCOLOR <cFntColor>]    ;
          [HEIGHT <nHeight>]         ;
          OF <oHtm>                  ;
          =>                         ;
          <oHtm>:TableHead( <cHead>, <cColor>, __ALIGN__ [<aln>], <cFont>, ;
                            <nSize>, <cFntColor>, <nHeight> )


#xCommand DEFINE CELL                ;
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
          <oHtm>:newTableCell( __ALIGN__ [<aln>], <cColor>, <cFont>, ;
                               <nSize>, <cFntColor>, <nHeight>,      ;
                               <img>, <width>, !<.nowrap.>,          ;
                               <cspan>, <rspan> ,__ALIGN__ [<valn>],<clrdrk>,<clrlt>,<bclrlt>,<cClass>)


#xCommand TABLE CELL                 ;
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
          <oHtm>:newTableCell( __ALIGN__ [<aln>], <cColor>, <cFont>, ;
                               <nSize>, <cFntColor>, <nHeight>, ;
                               <img>, <width>, !<.nowrap.>,;
                               <cspan>, <rspan> )


#xCommand DEFINE FONT [<cFont>]       ;
          [<ftype:BOLD,ITALIC,ULINE,UNDERLINE>] ;
          [SIZE <s>]                  ;
          [COLOR <c>]                 ;
          [OF <oHtm>]                 ;
          =>                          ;
          <oHtm>:defineFont( [<(cFont)>], __FTYPE__ [<ftype>], ;
                             [<s>], [<c>] )


#xCommand SET FONT [<cFont>]   ;
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
                          [<s>], [<c>],!<.lset.> )


#xCommand START FONT [<cFont>] ;
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
                          [<s>], [<c>] ,!<.lset.> )


#xCommand END FONT [<cFont>]   ;
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


#xCommand END BUTTON OF <oHtm> ;
          =>;
          <oHtm>:endButton()


#xCommand IMAGE <image>             ;
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
          [<break:BREAK>]           ;
          OF <oHtm>                 ;
          =>                        ;
          <oHtm>:putImageURL( <image>, <border>, <height>, <url>,;
                              <onclick>, <onmsover>, <onmsout>,  ;
                              <name>, <alt>, <target>, <width>,<.break.>)


#xCommand IMAGE <image>             ;
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
          OF <oHtm>                 ;
          =>                        ;
          <oHtm>:putImage( <image>, <border>, <height>,;
                           <onclick>, <onmsover>, <onmsout>, ;
                           <name>, <alt>, ;
                           <target>,<width> ,<.break.>)

#xCommand LINK <url>                ;
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
                             <target>,<font>,<clr>,<size>,<style>,<.bld.> ,<.break.>,<cClass>)

#xCommand LINK <url>                ;
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
          <oHtm>:putImageURL( <image>,<border>,<height>,<url>,;
                             <onclick>, <onmsover>, <onmsout>,<name>,<alt>, ;
                             <target> ,<width>,<.break.>,<cClass>)


#xCommand NEW FRAMEPAGE         ;
          [TITLE <title>]       ;
          [FILE <file>]         ;
          OF <oFrm>             ;
          =>                    ;
          <oFrm>:=frameSet():New( <file>, <title>)


#xCommand FRAMESET              ;
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



#xCommand FRAME                                        ;
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
                        !<.brd.>, !<.res.>, [<.scr.>], ;
                        [<mw>], [<mh>], [<target>],    ;
                        __SCROLL__ [<scrl>] )

#xCommand ENDSET <oFrm>        => <oFrm>:EndSet()
#xCommand END FRAMEPAGE <oFrm> => <oFrm>:End()


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
                   __POS__ [<pos>], <nWidth>, <nHeight>, <cbgColor>, ;
                   __BEHAVE__ [<bhv>], __DIR__ [<dir>], ;
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
                   __POS__ [<pos>], <nWidth>, <nHeight>, <cbgColor>, ;
                   __BEHAVE__ [<bhv>], __DIR__ [<dir>], ;
                   <nScrollAmt>, <nScrollDel>, <loop>,;
                   [<(onmsover)>], [<(onmsout)>], [<(onclick)>], ;
                   [<(onstart)>], [<(onfinish)>] )



#xCommand END MARQUEE OF <oHtm>  => <oHtm>:endMarquee()



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
          <oHtm>:iFrame( <name>, <url>, !<.brd.>, ;
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
          putCounter( <oHtm>, <num>, <folder>, <dig>, <w>, <clr>, <b> )

/*******************************************/
/*            New Commands                 */
/*******************************************/

#xCommand LINKS <url>                ;
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
          <oHtm>:putTextImageURL( <image>,<border>,<height>,<url>,;
                             <onclick>, <onmsover>, <onmsout>,<name>,<alt>, ;
                             <target> ,<width>,<.break.>,<cClass>,<text>)
#xCommand SPAN <text>           ;
          [STYLE <cStyle>]      ;
          OF <oHtm>             ;
          =>                    ;
          <oHtm>:Span(<text>,<cStyle>)

#xCommand Comment <text>        ;
          OF <oHtm>             ;
          =>                    ;
          <oHtm>:Comment(<text>)          

#xCommand LINKNAME <cName>      ;
          OF <oHtm>             ;
          =>                    ;
          <oHtm>:PutLinkName(<cName>)


#xCommand CREATE OBJECT          ;
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
          <oHtm>:ADDOBJECT(<cType>,<cClassid>,__ALING__ [<aln>],<cCode>,<.lDisable.>,<cCodeBase>,<cName>,<nWidth>,<nHeight>)

#xCommand OBJECT PARAM   ;
          NAME <cName>   ;
          VALUE <cValue> ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:ADDPARAM(<cName>,<cValue>)

#xCommand END OBJECT     ;
          OF <oHtm>      ;
          =>             ;
          <oHtm>:ENDOBJECT()



#xtranslate __SCROLL__                => "AUTO"
#xtranslate __SCROLL__ <scrl:NO>      => "NO"
#xtranslate __SCROLL__ <scrl:OFF>     => "NO"
#xtranslate __SCROLL__ <scrl:ON>      => "YES"
#xtranslate __SCROLL__ <scrl:YES>     => "YES"
#xtranslate __SCROLL__ <scrl:AUTO>    => "AUTO"


#xtranslate __ALIGN__                 =>  NIL
#xtranslate __ALIGN__ <aln:LEFT>      => "LEFT"
#xtranslate __ALIGN__ <aln:RIGHT>     => "RIGHT"
#xtranslate __ALIGN__ <aln:CENTER>    => "center"
#xtranslate __ALIGN__ <aln:MIDDLE>    => "MIDDLE"
#xtranslate __ALIGN__ <aln:TOP>       => "TOP"
#xtranslate __ALIGN__ <aln:TEXTTOP>   => "TEXTTOP"
#xtranslate __ALIGN__ <aln:BOTTOM>    => "BOTTOM"
#xtranslate __ALIGN__ <aln:ABSMIDDLE> => "ABSMIDDLE"
#xtranslate __ALIGN__ <aln:ABSCENTER> => "ABSMIDDLE"
#xtranslate __ALIGN__ <aln:ABSBOTTOM> => "ABSBOTTOM"
#xtranslate __ALIGN__ <aln:BASELINE>  => "BASELINE"

#xtranslate __POS__                    => NIL
#xtranslate __POS__ <pos:TOP>          => "TOP"
#xtranslate __POS__ <pos:MIDDLE>       => "MIDDLE"
#xtranslate __POS__ <pos:BOTTOM>       => "BOTTOM"

#xtranslate __DIR__                    => NIL
#xtranslate __DIR__ <dir:LEFT>         => "LEFT"
#xtranslate __DIR__ <dir:RIGHT>        => "RIGHT"

#xtranslate __BEHAVE__                 => NIL
#xtranslate __BEHAVE__ <bhv:SCROLL>    => "SCROLL"
#xtranslate __BEHAVE__ <bhv:SLIDE>     => "SLIDE"
#xtranslate __BEHAVE__ <bhv:ALTERNATE> => "ALTERNATE"
#xtranslate __BEHAVE__ <bhv:ALT>       => "ALTERNATE"

#xtranslate __FTYPE__                    => NIL
#xtranslate __FTYPE__ <ftype:ITALIC>     => "<I>"
#xtranslate __FTYPE__ <ftype:BOLD>       => "<B>"
#xtranslate __FTYPE__ <ftype:ULINE>      => "<U>"
#xtranslate __FTYPE__ <ftype:UNDERLINE>  => "<U>"
                                    

#define  GREEK_CGI  {;
                    {"Ä","%C1"},;
                    {"Å","%C2"},;
                    {"Ç","%C3"},;
                    {"É","%C4"},;
                    {"Ñ","%C5"},;
                    {"Ö","%C6"},;
                    {"Ü","%C7"},;
                    {"á","%C8"},;
                    {"à","%C9"},;
                    {"â","%CA"},;
                    {"ä","%CB"},;
                    {"ã","%CC"},;
                    {"å","%CD"},;
                    {"ç","%CE"},;
                    {"é","%CF"},;
                    {"è","%D0"},;
                    {"ê","%D1"},;
                    {"ë","%D3"},;
                    {"í","%D4"},;
                    {"ì","%D5"},;
                    {"î","%D6"},;
                    {"ï","%D7"},;
                    {"ñ","%D8"},;
                    {"ó","%D9"},;
                    {"ò","%E1"},;
                    {"ô","%E2"},;
                    {"ö","%E3"},;
                    {"õ","%E4"},;
                    {"ú","%E5"},;
                    {"ù","%E6"},;
                    {"û","%E7"},;
                    {"ü","%E8"},;
                    {"†","%E9"},;
                    {"°","%EA"},;
                    {"¢","%EB"},;
                    {"£","%EC"},;
                    {"§","%ED"},;
                    {"•","%EE"},;
                    {"¶","%EF"},;
                    {"ß","%F0"},;
                    {"®","%F1"},;
                    {"©","%F3"},;
                    {"´","%F4"},;
                    {"¨","%F5"},;
                    {"≠","%F6"},;
                    {"Æ","%F7"},;
                    {"Ø","%F8"},;
                    {"‡","%F9"},;
                    {"™","%F2"},;
                    {"·","%DC"},;
                    {"‚","%DD"},;
                    {"„","%DE"},;
                    {"Â","%DF"},;
                    {"Ê","%FC"},;
                    {"Á","%FD"},;
                    {"È","%FE"} ;
                    }


#define _HTML_CH
#endif
