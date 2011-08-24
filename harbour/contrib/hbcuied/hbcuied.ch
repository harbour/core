/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CUI Forms Editor
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                       Harbour CUI Editor Source
 *
 *                             Pritpal Bedi
 *                               13Aug2011
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#ifndef __HBEDCUI_CH
#define __HBEDCUI_CH

#define THE_FILL                                  chr( 177 )


#define OBJ_TYPE                                  1     //   N   1

#define OBJ_ROW                                   2     //   N   3
#define OBJ_COL                                   3     //   N   3
#define OBJ_TO_ROW                                4     //   N   3
#define OBJ_TO_COL                                5     //   N   3

#define OBJ_TEXT                                  6     //   C  15
#define OBJ_EQN                                   6     //   C 240
#define OBJ_NAME                                  6     //   C 240

#define OBJ_F_TYPE                                7     //   C   1
#define OBJ_F_LEN                                 8     //   N   3
#define OBJ_F_DEC                                 9     //   N   2
#define OBJ_F_PIC                                 10    //   C  20
#define OBJ_COLOR                                 11    //   C   7

#define OBJ_WHEN                                  12    //   C   7
#define OBJ_BORDER                                12    //   C   7
#define OBJ_BOX_SHAPE                             12    //   C  --

#define OBJ_VALID                                 13    //   C   7
#define OBJ_PATTERN                               13    //   C  10

#define OBJ_ID                                    14    //   C  15
#define OBJ_SECTION                               15    //   N   4
#define OBJ_SEC_ROW                               16    //   N   3
#define OBJ_OBJ_UNIQUE                            17    //   N   4
#define OBJ_MDL_F_TYPE                            18    //   C  10
//
#define OBJ_INIT_VRBLS                            18


#define OBJ_REFRESH_ALL                           1
#define OBJ_REFRESH_LINE                          2
#define OBJ_REFRESH_NIL                           0

#define OBJ_O_BOX                                 1
#define OBJ_O_LINE                                2
#define OBJ_O_TEXT                                3
#define OBJ_O_FIELD                               4
#define OBJ_O_EXP                                 5
#define OBJ_O_BMP                                 6

#define OBJ_MODE_SELECT                           1
#define OBJ_MODE_MOVE                             2
#define OBJ_MODE_IDLE                             0

#translate B_MSG ;
      [ <msg,...> ] ;
      [ AT <r1> [, <c1> ] ] ;
      [ TO <r2> [, <c2> ] ] ;
      [ WIDTH <w>       ] ;
      [ DEPTH <d>       ] ;
      [ COLOR <clr>     ] ;
      [ CHOOSE <ch,...> ] ;
      [ CHOOSECOLOR <chClr> ] ;
      [ CHCOLOR <chClr> ] ;
      [ INTO <ret>      ] ;
      [ WAIT <wait>     ] ;
      [ <rest:RESTORE,REST> ] ;
      [ <paste:PASTE>   ] ;
      [ <shadow:SHADOW> ] ;
      [ TRIGGER <trg>   ] ;
      [ INITIAL <init>  ] ;
      [ SELECTABLES <sel> ] ;
      [ ABORT <abr>     ] ;
      [ <selections:SELECTIONS> ] ;
      [ <leftright:LEFTRIGHT>   ] ;
      [ <cent:CENTER,CENTRE>    ] ;
      [ TAGGED <tag_>   ] ;
      [ <num:NUMERIC>   ] ;
      [ HELP <hlp>      ] ;
      [ EXECUTE <ex_>   ] ;
      [ NUMBERED <num_> ] ;
      [ <lNoXpp:NOXPP>  ] ;
      [ WINDOW <oWin>   ] ;
      [ ICON <cIcon>    ] ;
      [ WVT <lWvt>      ] ;
      [ ALIGN <nAlign>  ] ;
   => ;
      [<ret> := ] VouchMsgBox (<r1>, <c1>, <r2>, <c2>, <w>, <d>, ;
         {<msg>}, <clr>, {<ch>}, <chClr>, <wait>,  <.rest.>, ;
         <.paste.>, <.shadow.>, <trg>, <init>, <sel>, <abr>, ;
         <.selections.>, <.leftright.>, <.cent.>, <tag_>,<.num.>,;
         <hlp>,<ex_>,<num_>,<.lNoXpp.>,<oWin>,<cIcon>,<lWvt>,<nAlign> )


#xtranslate B_GETS ;
            HEADERS <hed> VALUES <val> ;
            [ SELECTABLES <sel> ] ;
            [ AT <r1> [, <c1> ] ] ;
            [ TO <r2> [, <c2> ] ] ;
            [ TITLE   <ttl> ] ;
            [ INTO    <ret> ] ;
            [ WHEN    <whn> ] ;
            [ VALID   <vld> ] ;
            [ PICTURE <pic> ] ;
            [ HELP    <hlp> ] ;
            [ ORDER   <ord> ] ;
            => ;
   [<ret> := ] VouchGetArray(<hed>, <val>, <sel>, <r1>, <c1>, <r2>, <c2>, ;
                        <ttl>, <whn>, <vld>, <pic>, <hlp>, <ord> )


#xtranslate B_CRT <nTop>,<nLeft>,<nBottom>,<nRight> ;
            [ TITLE    <ttl>     ] ;
            [ ICON    <icon>     ] ;
            [ <lModal:MODAL>     ] ;
            [ <lRowCols:RESIZEROWCOLS> ] ;
            [ <lHidden:HIDDEN>   ] ;
            [ <lCenter:CENTER>   ] ;
            [ AT <nRow>,<nCol>   ] ;
            [ <lNoTitleBar:NOTITLEBAR> ] ;
            INTO <oCrt> ;
            => ;
   <oCrt> := Vou_CreateOCrt( <nTop>, <nLeft>, <nBottom>, <nRight>, <ttl>, <icon>, ;
                             <.lModal.>, <.lRowCols.>, <.lHidden.>, <.lCenter.>, ;
                             <nRow>, <nCol>, <.lNoTitleBar.> )

#define COMPILE( cStr )    &( "{|v,w,x| " + cStr + " }" )

#define CHECKMARK                                 chr( 251 )

#endif
