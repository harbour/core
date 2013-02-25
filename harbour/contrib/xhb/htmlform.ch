/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Include file to create forms
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

#ifndef _FORMS_CH

#xcommand DEFINE FORM <oFrm>                                       ;
          [NAME <name>]                                            ;
          [METHOD <method>]                                        ;
          [ACTION <action>]                                        ;
          [ENCTYPE <enctype>]                                      ;
          [TARGET <target>]                                        ;
          [ONSUBMIT <onsubmit>]                                    ;
          [ONRESET <onreset>]                                      ;
          [<frame:FRAME>]                                          ;
          [CAPTION <cap>]                                          ;
          [CAPCOLOR <capclr>]                                      ;
          [CAPFONTCOLOR <capfntclr>]                               ;
          [CAPIMAGE <capimage>]                                    ;
          [BGIMAGE <bgimg>]                                        ;
          [FONTCOLOR <fntclr>]                                     ;
          [COLOR <clr>]                                            ;
          [WIDTH <w>]                                              ;
          =>                                                       ;
          <oFrm> := THtmlForm():New( [<name>], [<action>], [<method>],  ;
                                <.frame.>, <cap> )                ;;
          [<oFrm>:setTarget( <(target)> ) ;]                       ;
          [<oFrm>:setEncType( <(enctype)> ) ;]                     ;
          [<oFrm>:setCapClr( <(capclr)> ) ;]                       ;
          [<oFrm>:setCapFntClr( <(capfntclr)> ) ;]                 ;
          [<oFrm>:setCapImage( <(capimage)> ) ;]                   ;
          [<oFrm>:setBgImage( <(bgimg)> ) ;]                       ;
          [<oFrm>:setFontColor( <(fntclr)> ) ;]                    ;
          [<oFrm>:setFrmColor( <(clr)> ) ;]                        ;
          [<oFrm>:setwidth( <w> ) ;]                               ;
          [<oFrm>:setAction( <(action)> ) ;]                       ;
          [<oFrm>:setOnSubmit( <(onsubmit)> ) ;]                   ;
          [<oFrm>:setOnReset( <(onreset)> ) ]


#xcommand ACTIVATE <oFrm> ;
          =>;
          <oFrm>:Put( .T. ) ; <oFrm>:End()


// --> Controls

#xcommand CONTROL ;
          [OF <obj>];
          [<typ:EDIT,TEXT,TEXTAREA,PASSWORD,BUTTON,IMAGE,CHECKBOX,CHECK,HIDDEN,RADIO,FILE,RESET,SUBMIT,SELECT,LISTBOX>] ;
          [ALIGN <aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [WRAP <wrap:OFF,VIRTUAL,PHYSICAL,NORMAL>] ;
          [NAME <name>]                       ;
          [VALUE <value>]                     ;
          [SIZE <size>]                       ;
          [MAXCHARS <maxchars>]               ;
          [ROWS <rows>]                       ;
          [COLS <cols>]                       ;
          [ONCHANGE <onchange>]               ;
          [ONSELECT <onselect>]               ;
          [ONFOCUS <onfocus>]                 ;
          [ONBLUR <onblur>]                   ;
          [ONCLICK <onclick>]                 ;
          [ONMOUSEOVER <onmsover>]            ;
          [ONMOUSEOUT <onmsout>]              ;
          [ONMOUSEUP <onmsup>]                ;
          [ONMOUSEDOWN <onmsdn>]              ;
          [ONKEYDOWN <onkdown>]               ;
          [ONKEYUP <onkup>]                   ;
          [ONKEYPRESS <onkprs>]               ;
          [PICTURE <pic>]                     ;
          [<chk:CHECKED>]                     ;
          [<dis:DISABLED>]                    ;
          [<ro:READONLY>]                     ;
          [<mult:MULTIPLE>]                   ;
          [CAPTION <cap>]                     ;
          [STYLE <style>]                     ;
          [ID <id>]                           ;
          [<l:LABEL>]                         ;
          IN <oForm>                          ;
          =>                                  ;
          <oForm>:addControl( ;
                  [ <obj>:= ] THtmlControl():SetControl(;
                  <(name)>, <rows>, <cols>, <size>, <maxchars>, ;
                  <(value)>, ;
                  <(onfocus)>, <(onblur)>, <(onchange)>, <(onselect)>, ;
                  <(onclick)>, <(onmsover)>, <(onmsout)>, <(onmsdn)>, <(onmsup)>, ;
                  <(onkdown)>, <(onkup)>, <(onkprs)>, ;
                  <(pic)>, <(cap)>, ;
                  <.dis.>, <.ro.>, <.mult.>, <.chk.>, ;
                  __ALIGN__ [<aln>], ;
                  __WRAP__ [<wrap>], ;
                  __TYPE__ [<typ>], ;
                  <style>, <id>, <.l.> ) )


#xcommand DEFINE <typ:EDIT,TEXT,TEXTAREA,PASSWORD,BUTTON,IMAGE,CHECKBOX,CHECK,HIDDEN,RADIO,FILE,RESET,SUBMIT,SELECT,LISTBOX> <oCtr>;
          [WRAP <wrap:OFF,VIRTUAL,PHYSICAL,NORMAL>] ;
          [ALIGN <aln:LEFT,RIGHT,CENTER,MIDDLE,TOP,TEXTTOP,BOTTOM,ABSMIDDLE,ABSCENTER,ABSBOTTOM,BASELINE>];
          [NAME <name>]                            ;
          [SIZE <size>]                            ;
          [MAXCHARS <maxchars>]                    ;
          [VALUE <value>]                          ;
          [ROWS <rows>]                            ;
          [COLS <cols>]                            ;
          [SOURCE <source>]                        ;
          [ONCHANGE <onchange>]                    ;
          [ONSELECT <onselect>]                    ;
          [ONFOCUS <onfocus>]                      ;
          [ONBLUR <onblur>]                        ;
          [ONCLICK <onclick>]                      ;
          [ONMOUSEOVER <onmsover>]                 ;
          [ONMOUSEOUT <onmsout>]                   ;
          [ONKEYDOWN <onkdown>]                    ;
          [ONKEYUP <onkup>]                        ;
          [ONKEYPRESS <onkprs>]                    ;
          [PICTURE <pic>]                          ;
          [<mult:MULTIPLE>]                        ;
          [<checked:CHECKED>]                      ;
          [<dis:DISABLED>]                         ;
          [<ro:READONLY>]                          ;
          [CAPTION <cap>]                          ;
          [STYLE <style>]                          ;
          [ID <id>]                                ;
          [<l:LABEL>]                              ;
          IN <oForm>                               ;
          =>                                       ;
          <oCtr> := THtmlControl()                 ;;
          [<oCtr>:SetName( <(name)> ) ;]           ;
          [<oCtr>:SetSize( <size> ) ;]             ;
          [<oCtr>:SetRows( <rows> ) ;]             ;
          [<oCtr>:SetCols( <cols> ) ;]             ;
          [<oCtr>:SetAlign( __ALIGN__ <aln> );]    ;
          [<oCtr>:SetWrap( __WRAP__ <wrap> );]     ;
          [<oCtr>:SetSource( <(source)> );]        ;
          [<oCtr>:SetMaxChars( <maxchars> );]      ;
          [<oCtr>:SetValue( <(value)> );]          ;
          [<oCtr>:SetOnFocus( <(onfocus)> );]      ;
          [<oCtr>:SetOnBlur( <(onblur)> );]        ;
          [<oCtr>:SetOnChange( <(onchange)> );]    ;
          [<oCtr>:SetOnSelect( <(onselect)> );]    ;
          [<oCtr>:SetonClick( <(onclick)> );]      ;
          [<oCtr>:SetonMsOver( <(onmsover)> );]    ;
          [<oCtr>:SetonMsOut( <(onmsout)> );]      ;
          [<oCtr>:SetonKDown( <(onkdown)> );]      ;
          [<oCtr>:SetonKUp( <(onkup)> );]          ;
          [<oCtr>:SetonKPress( <(onkprs)> );]      ;
          [<oCtr>:SetPicture( <(pic)> );]          ;
          [<oCtr>:SetCaption( <cap> );]            ;
          [<oCtr>:SetStyle( <(style)> );]          ;
          [<oCtr>:SetId( <(id)> );]                ;
          [<oCtr>:SetChecked( <.checked.> );]      ;
          [<oCtr>:SetMultiple( <.mult.> );]        ;
          [<oCtr>:SetDisabled( <.dis.> );]         ;
          [<oCtr>:SetReadonly( <.ro.> );]          ;
          [<oCtr>:SetLabel( <.l.> );]              ;
          <oCtr>:Type := __TYPE__ <typ>            ;;
          <oForm>:AddControl( <oCtr> )

#xcommand DEFINE OPTION <text> [OF <oSelect>] ;
          [VALUE <value>]                     ;
          [LABEL <label>]                     ;
          [<sel:SELECTED>]                    ;
          [<dis:DISABLED>]                    ;
          =>;
          <oSelect>:AddOption( <text>, <value>, <label>, <.sel.>, <.dis.> )

#xcommand ADD OPTION <text> [OF <oSelect>]    ;
          [VALUE <value>]                     ;
          [LABEL <label>]                     ;
          [<sel:SELECTED>]                    ;
          [<dis:DISABLED>]                    ;
          =>;
          <oSelect>:AddOption( <text>, <value>, <label>, <.sel.>, <.dis.> )


// --> Literals

#xcommand LINE BREAK                       ;
          IN <oForm>                       ;
          =>                               ;
          <oForm>:AddControl( "<br />" + CRLF() )


#xcommand LINE IN <oForm>                  ;
          =>                               ;
          <oForm>:AddControl( CRLF() + "<hr width = 100%>" + CRLF() )

#xcommand SPACE <n> IN <oForm>             ;
          =>                               ;
          <oForm>:AddControl( Replicate( "&nbsp;", <n> ) )


#xcommand TEXT <c> IN <oForm>              ;
          =>                               ;
          <oForm>:AddControl( <c> )

#xcommand SCRIPT <c> IN <oForm>            ;
          =>                               ;
          <oForm>:AddControl( <c> )


#xcommand START GROUP <c> IN <oForm> => ;
          <oForm>:AddControl( CRLF() + "<fieldset><legend>" + <c> + "</legend>" + CRLF() )

#xcommand END GROUP IN <oForm> => ;
          <oForm>:AddControl( CRLF() + "</fieldset>" + CRLF() )


#xtranslate __TYPE__                  => "TEXT"
#xtranslate __TYPE__ <typ:EDIT>       => "TEXT"
#xtranslate __TYPE__ <typ:TEXT>       => "TEXT"
#xtranslate __TYPE__ <typ:TEXTAREA>   => "TEXTAREA"
#xtranslate __TYPE__ <typ:PASSWORD>   => "PASSWORD"
#xtranslate __TYPE__ <typ:IMAGE>      => "IMAGE"
#xtranslate __TYPE__ <typ:BUTTON>     => "BUTTON"
#xtranslate __TYPE__ <typ:CHECKBOX>   => "CHECKBOX"
#xtranslate __TYPE__ <typ:CHECK>      => "CHECKBOX"
#xtranslate __TYPE__ <typ:HIDDEN>     => "HIDDEN"
#xtranslate __TYPE__ <typ:RADIO>      => "RADIO"
#xtranslate __TYPE__ <typ:FILE>       => "FILE"
#xtranslate __TYPE__ <typ:RESET>      => "RESET"
#xtranslate __TYPE__ <typ:SUBMIT>     => "SUBMIT"
#xtranslate __TYPE__ <typ:SELECT>     => "SELECT"

#xtranslate __WRAP__                  => NIL
#xtranslate __WRAP__ <wrap:OFF>       => "OFF"
#xtranslate __WRAP__ <wrap:VIRTUAL>   => "VIRTUAL"
#xtranslate __WRAP__ <wrap:PHYSICAL>  => "PHYSICAL"
#xtranslate __WRAP__ <wrap:NORMAL>    => "NORMAL"

#xtranslate __ALIGN__                 =>  NIL
#xtranslate __ALIGN__ <aln:LEFT>      => "LEFT"
#xtranslate __ALIGN__ <aln:RIGHT>     => "RIGHT"
#xtranslate __ALIGN__ <aln:CENTER>    => "MIDDLE"
#xtranslate __ALIGN__ <aln:MIDDLE>    => "MIDDLE"
#xtranslate __ALIGN__ <aln:TOP>       => "TOP"
#xtranslate __ALIGN__ <aln:TEXTTOP>   => "TEXTTOP"
#xtranslate __ALIGN__ <aln:BOTTOM>    => "BOTTOM"
#xtranslate __ALIGN__ <aln:ABSMIDDLE> => "ABSMIDDLE"
#xtranslate __ALIGN__ <aln:ABSCENTER> => "ABSMIDDLE"
#xtranslate __ALIGN__ <aln:ABSBOTTOM> => "ABSBOTTOM"
#xtranslate __ALIGN__ <aln:BASELINE>  => "BASELINE"


#define _FORMS_CH
#endif
