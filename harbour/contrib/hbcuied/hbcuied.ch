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

#define THE_FILL                                  'ø'

#define OBJ_TYPE                                  1     //   N   1
#define OBJ_ROW                                   2     //   N   3
#define OBJ_COL                                   3     //   N   3
#define OBJ_TEXT                                  4     //   C  15
#define OBJ_BOX_SHAPE                             4     //   C  --
#define OBJ_COLOR                                 5     //   C   7
#define OBJ_TO_ROW                                6     //   N   3
#define OBJ_TO_COL                                7     //   N   3
#define OBJ_ID                                    8     //   C  15
#define OBJ_SECTION                               9     //   N   4
#define OBJ_ALIAS                                 10    //   C   8
#define OBJ_FIELD                                 11    //   N   3
#define OBJ_EQN                                   12    //   C 240
#define OBJ_F_TYPE                                13    //   C   1
#define OBJ_F_LEN                                 14    //   N   3
#define OBJ_F_DEC                                 15    //   N   2
#define OBJ_F_PIC                                 16    //   C  20
#define OBJ_ALIGN                                 17    //   C   1
#define OBJ_PITCH                                 18    //   N   2
#define OBJ_FONT                                  19    //   C   8
#define OBJ_BOLD                                  20    //   L   1
#define OBJ_ITALIC                                21    //   L   1
#define OBJ_UNDERLN                               22    //   L   1
#define OBJ_S_SCRPT                               23    //   L   1
#define OBJ_U_SCRPT                               24    //   L   1
#define OBJ_HALF_H                                25    //   L   1
#define OBJ_PRN_LEN                               26    //   N   3
#define OBJ_ZERO                                  27    //   L   1
#define OBJ_REPEATED                              28    //   C   6
#define OBJ_VERTICLE                              29    //   L   1
#define OBJ_WRAP_SEMI                             30    //   L   1
#define OBJ_FOR                                   31    //   C  80
#define OBJ_SEC_ROW                               32    //   N   3
#define OBJ_ATTRB                                 33    //   C   8
#define OBJ_VAL                                   34    //   C   1
#define OBJ_OBJ_UNIQUE                            35    //   N   4
#define OBJ_MDL_F_TYPE                            36    //   N   2
#define OBJ_POINT                                 37    //   N   2
#define OBJ_COL_JUST                              38    //   N   2
#define OBJ_PATTERN                               39    //   C  10
#define OBJ_BORDER                                40    //   N   5 2

#define OBJ_INIT_VRBLS                            40

#define OBJ_LEN_TYPE                              1
#define OBJ_LEN_ROW                               3
#define OBJ_LEN_COL                               3
#define OBJ_LEN_TEXT                              15
#define OBJ_LEN_COLOR                             7
#define OBJ_LEN_TO_ROW                            3
#define OBJ_LEN_TO_COL                            3
#define OBJ_LEN_ID                                15
#define OBJ_LEN_SECTION                           4
#define OBJ_LEN_ALIAS                             8
#define OBJ_LEN_FIELD                             3
#define OBJ_LEN_EQN                               240
#define OBJ_LEN_F_TYPE                            1
#define OBJ_LEN_F_LEN                             3
#define OBJ_LEN_F_DEC                             2
#define OBJ_LEN_F_PIC                             20
#define OBJ_LEN_ALIGN                             1
#define OBJ_LEN_PITCH                             2
#define OBJ_LEN_FONT                              8
#define OBJ_LEN_BOLD                              1
#define OBJ_LEN_ITALIC                            1
#define OBJ_LEN_UNDERLN                           1
#define OBJ_LEN_S_SCRPT                           1
#define OBJ_LEN_U_SCRPT                           1
#define OBJ_LEN_HALF_H                            1
#define OBJ_LEN_PRN_LEN                           3
#define OBJ_LEN_ZERO                              1
#define OBJ_LEN_REPEATED                          6
#define OBJ_LEN_VERTICLE                          1
#define OBJ_LEN_WRAP_SEMI                         1
#define OBJ_LEN_FOR                               80
#define OBJ_LEN_SEC_ROW                           3
#define OBJ_LEN_ATTRB                             8
#define OBJ_LEN_VAL                               1
#define OBJ_LEN_OBJ_UNIQUE                        4
#define OBJ_LEN_MDL_F_TYPE                        2
#define OBJ_LEN_POINT                             2
#define OBJ_LEN_COL_JUST                          3   //   N   3
#define OBJ_LEN_PATTERN                           10  //   C  10
#define OBJ_LEN_BORDER                            5   //   N   5 2

#define OBJ_OS_TYPE                               1     //   N   1
#define OBJ_OS_ROW                                2     //   N   3
#define OBJ_OS_COL                                5     //   N   3
#define OBJ_OS_TEXT                               8     //   C  15
#define OBJ_OS_COLOR                              23    //   C   7
#define OBJ_OS_TO_ROW                             30    //   N   3
#define OBJ_OS_TO_COL                             33    //   N   3
#define OBJ_OS_ID                                 36    //   C  15
#define OBJ_OS_SECTION                            51    //   N   4
#define OBJ_OS_ALIAS                              55    //   C   8
#define OBJ_OS_FIELD                              63    //   N   3
#define OBJ_OS_EQN                                66    //   C 240
#define OBJ_OS_F_TYPE                             306   //   C   1
#define OBJ_OS_F_LEN                              307   //   N   3
#define OBJ_OS_F_DEC                              310   //   N   2
#define OBJ_OS_F_PIC                              312   //   C  20
#define OBJ_OS_ALIGN                              332   //   C   1
#define OBJ_OS_PITCH                              333   //   N   2
#define OBJ_OS_FONT                               335   //   C   8
#define OBJ_OS_BOLD                               343   //   L   1
#define OBJ_OS_ITALIC                             344   //   L   1
#define OBJ_OS_UNDERLN                            345   //   L   1
#define OBJ_OS_S_SCRPT                            346   //   L   1
#define OBJ_OS_U_SCRPT                            347   //   L   1
#define OBJ_OS_HALF_H                             348   //   L   1
#define OBJ_OS_PRN_LEN                            349   //   N   3
#define OBJ_OS_ZERO                               352   //   L   1
#define OBJ_OS_REPEATED                           353   //   C   6
#define OBJ_OS_VERTICLE                           359   //   L   1
#define OBJ_OS_WRAP_SEMI                          360   //   L   1
#define OBJ_OS_FOR                                361   //   C  80
#define OBJ_OS_SEC_ROW                            441   //   N   3
#define OBJ_OS_ATTRB                              444   //   C   8
#define OBJ_OS_VAL                                452   //   C   1
#define OBJ_OS_OBJ_UNIQUE                         453   //   N   4
#define OBJ_OS_MDL_F_TYPE                         457   //   N   2
#define OBJ_OS_POINT                              459   //   N   2
#define OBJ_OS_COL_JUST                           461   //   N   3
#define OBJ_OS_PATTERN                            464   //   C  10
#define OBJ_OS_BORDER                             474   //   N   5 2
//                                               ----
//   TOTAL                                        474

#define VV_ID                                     1     //   N   1
#define VV_FIELD                                  2     //   N   3
#define VV_F_TYPE                                 3     //   C   1
#define VV_F_LEN                                  4     //   N   3
#define VV_F_DEC                                  5     //   N   2
#define VV_ATTRB                                  6     //   C   8
#define VV_EQN                                    7     //   C 240
#define VV_PRN_LEN                                8     //   N   3
#define VV_F_PIC                                  9     //   C  20
#define VV_PITCH                                  10    //   N   2
#define VV_FONT                                   11    //   C   8
#define VV_BOLD                                   12    //   L   1
#define VV_ITALIC                                 13    //   L   1
#define VV_UNDERLN                                14    //   L   1
#define VV_S_SCRPT                                15    //   L   1
#define VV_U_SCRPT                                16    //   L   1
#define VV_HALF_H                                 17    //   L   1
#define VV_ALIGN                                  18    //   C   1
#define VV_COLOR                                  19    //   C   7
#define VV_ZERO                                   20    //   L   1
#define VV_REPEATED                               21    //   C   6
#define VV_VERTICLE                               22    //   L   1
#define VV_WRAP_SEMI                              23    //   L   1
#define VV_FOR                                    24    //   C  80
#define VV_OBJ_UNIQUE                             25    //   N   4
#define VV_MDL_F_TYPE                             26    //   N   2
#define VV_POINT                                  27    //   N   2
#define VV_COL_JUST                               28
#define VV_PATTERN                                29
#define VV_BORDER                                 30

#define VV_INIT_VRBLS                             30

#define OBJ_REFRESH_ALL                           1
#define OBJ_REFRESH_LINE                          2
#define OBJ_REFRESH_NIL                           0

#define OBJ_O_BOX                                 1
#define OBJ_O_LINE                                2
#define OBJ_O_TEXT                                3
#define OBJ_O_FIELD                               4
#define OBJ_O_EXP                                 5
#define OBJ_O_BMP                                 6

//   Properties will be based on these attributes
#define DGN_MODULE                                1
#define DGN_REPORT                                2
#define DGN_DOCUMENT                              3
#define DGN_LABEL                                 4
#define DGN_SCREEN                                5

#define OBJ_MODE_SELECT                           1
#define OBJ_MODE_MOVE                             2
#define OBJ_MODE_IDLE                             0

#define SCT_ORDER                                 1
#define SCT_ID                                    2
#define SCT_SAY                                   3
#define SCT_ROWS                                  4
#define SCT_COLOR                                 5
#define SCT_EQN                                   6
#define SCT_EJECT                                 7
#define SCT_RESET                                 8
#define SCT_INIT_VRBLS                            8


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
