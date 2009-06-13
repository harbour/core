/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the WVT*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.xharbour.org http://www.harbour-project.org
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
/*                                 Xbp.ch                               */
/*----------------------------------------------------------------------*/

#ifndef  _XBP_CH

#define _XBP_CH

/*----------------------------------------------------------------------*/

#define DA_MODE                                    1  /* Experimental */

#define objTypeNone                                0
#define objTypeCrt                                 1
#define objTypeWindow                              2
#define objTypeActiveX                             3
#define objTypeDialog                              4
#define objTypeToolBar                             5
#define objTypeToolBarButton                       6
#define objTypeMenu                                7
#define objTypeStatusBar                           8
#define objTypePushButton                          9
#define objTypeComboBox                           10
#define objTypeListBox                            11
#define objTypeStatic                             12
#define objTypeTreeView                           13
#define objTypeTreeViewItem                       14
#define objTypeCheckBox                           15
#define objTypeRadioButton                        16
#define objType3State                             17
#define objTypeSLE                                18
#define objTypeMLE                                19
#define objTypeDA                                 20
#define objTypeScrollBar                          21
#define objTypeTabPage                            22

/*----------------------------------------------------------------------*/
/*  NMHDR info */

#define NMH_code                                  1
#define NMH_controlID                             2
#define NMH_hWnd                                  3
#define NMH_dwItemSpec                            4  /* NMHMOUSE    */
#define NMH_action                                4  /* NMHTREEVIEW */

/*----------------------------------------------------------------------*/

#define EVENT_HANDELLED                           0
#define EVENT_UNHANDELLED                         1

/*----------------------------------------------------------------------*/

#define XBP_AUTOSIZE                              -1

#define XBPALIGN_TOP                              0
#define XBPALIGN_LEFT                             0
#define XBPALIGN_BOTTOM                           8
#define XBPALIGN_RIGHT                            2
#define XBPALIGN_HCENTER                          1
#define XBPALIGN_VCENTER                          4
#define XBPALIGN_WORDBREAK                        16

#define XBPFRAME_NONE                             0
#define XBPFRAME_RECT                             1
#define XBPFRAME_BOX                              2
#define XBPFRAME_RAISED                           16
#define XBPFRAME_RECESSED                         32
#define XBPFRAME_THICK                            128
#define XBPFRAME_DASHED                           256
#define XBPFRAME_DOTTED                           512

#define XBPTOOLBAR_STYLE_STANDARD                 0
#define XBPTOOLBAR_STYLE_FLAT                     1
#define XBPTOOLBAR_STYLE_VERTICAL                 2

#define XBPTOOLBAR_BUTTON_DEFAULT                 0

/*      Statusbar Manipulation Constants         */
#define XBPSTATUSBAR_AUTOSIZE_NONE                0
#define XBPSTATUSBAR_AUTOSIZE_SPRING              1
#define XBPSTATUSBAR_AUTOSIZE_CONTENTS            2

#define XBPSTATUSBAR_BEVEL_NONE                   0
#define XBPSTATUSBAR_BEVEL_INSET                  1
#define XBPSTATUSBAR_BEVEL_RAISED                 2

#define XBPSTATUSBAR_PANEL_TEXT                   0
#define XBPSTATUSBAR_PANEL_CAPSLOCK               1
#define XBPSTATUSBAR_PANEL_NUMLOCK                2
#define XBPSTATUSBAR_PANEL_INSERT                 3
#define XBPSTATUSBAR_PANEL_SCROLL                 4
#define XBPSTATUSBAR_PANEL_TIME                   5
#define XBPSTATUSBAR_PANEL_DATE                   6
#define XBPSTATUSBAR_PANEL_KANA                   7

#define XBPSTATIC_TYPE_TEXT                       1
#define XBPSTATIC_TYPE_GROUPBOX                   2
#define XBPSTATIC_TYPE_ICON                       3
#define XBPSTATIC_TYPE_SYSICON                    4
#define XBPSTATIC_TYPE_BITMAP                     5
#define XBPSTATIC_TYPE_FGNDRECT                   6
#define XBPSTATIC_TYPE_BGNDRECT                   7
#define XBPSTATIC_TYPE_FGNDFRAME                  8
#define XBPSTATIC_TYPE_BGNDFRAME                  9
#define XBPSTATIC_TYPE_HALFTONERECT               10
#define XBPSTATIC_TYPE_HALFTONEFRAME              11
#define XBPSTATIC_TYPE_RAISEDBOX                  12
#define XBPSTATIC_TYPE_RECESSEDBOX                13
#define XBPSTATIC_TYPE_RAISEDRECT                 14
#define XBPSTATIC_TYPE_RECESSEDRECT               15
#define XBPSTATIC_TYPE_RAISEDLINE                 16
#define XBPSTATIC_TYPE_RECESSEDLINE               17

#define XBPSTATIC_FRAMETHIN                       1
#define XBPSTATIC_FRAMETHICK                      2

#define XBPDLG_FRAMESTAT_MINIMIZED                1
#define XBPDLG_FRAMESTAT_MAXIMIZED                2
#define XBPDLG_FRAMESTAT_NORMALIZED               3

#define XBPSTATIC_TEXT_LEFT                       XBPALIGN_LEFT
#define XBPSTATIC_TEXT_RIGHT                      XBPALIGN_RIGHT
#define XBPSTATIC_TEXT_CENTER                     XBPALIGN_HCENTER
#define XBPSTATIC_TEXT_TOP                        XBPALIGN_TOP
#define XBPSTATIC_TEXT_VCENTER                    XBPALIGN_VCENTER
#define XBPSTATIC_TEXT_BOTTOM                     XBPALIGN_BOTTOM
#define XBPSTATIC_TEXT_WORDBREAK                  XBPALIGN_WORDBREAK

#define XBPSTATIC_BITMAP_TILED                    1
#define XBPSTATIC_BITMAP_SCALED                   2

#define XBP_DRAW_NORMAL                           0

#define XBPLISTBOX_MM_SINGLE                      1

/* Generic Pres Parameters */
#define XBP_PP_FGCLR                              2
#define XBP_PP_BGCLR                              4
#define XBP_PP_COMPOUNDNAME                       15
#define XBP_PP_FONT                               16

#define XBPSLE_LEFT                               1
#define XBPSLE_RIGHT                              2
#define XBPSLE_CENTER                             3

/* SCROLLBAR */
#define XBPSCROLL_HORIZONTAL                      1
#define XBPSCROLL_VERTICAL                        2

#define XBPSB_PREVPOS                             1
#define XBPSB_NEXTPOS                             2
#define XBPSB_PREVPAGE                            3
#define XBPSB_NEXTPAGE                            4
#define XBPSB_SLIDERTRACK                         5
#define XBPSB_ENDTRACK                            6
#define XBPSB_ENDSCROLL                           7
#define XBPSB_TOP                                 11
#define XBPSB_BOTTOM                              12

#define XBPTABPAGE_TAB_BOTTOM                     2
#define XBPTABPAGE_TAB_TOP                        4

#define XBPTOOLBAR_BUTTON_SEPARATOR               4

#define XBPMENUBAR_MIS_BUTTONSEPARATOR            512
#define XBPMENUBAR_MIS_STATIC                     256
#define XBPMENUBAR_MIS_SEPARATOR                  4

#define XBPMENUBAR_MIA_NODISMISS                  32
#define XBPMENUBAR_MIA_FRAMED                     4096
#define XBPMENUBAR_MIA_CHECKED                    8192
#define XBPMENUBAR_MIA_DISABLED                   16384
#define XBPMENUBAR_MIA_HILITED                    32768
#define XBPMENUBAR_MIA_DEFAULT                    65536
#define XBPMENUBAR_MIA_OWNERDRAW                  131072

#endif /* #ifndef _XBP_CH */

/*----------------------------------------------------------------------*/
