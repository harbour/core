/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the WVT*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.xharbour.org http://harbour-project.org
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
/*-*/
/*                               WvgParts.ch                            */
/*-*/

#ifndef  _WVG_CH

#define _WVG_CH

/*-*/

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

/*-*/
/*  NMHDR info */

#define NMH_code                                  1
#define NMH_controlID                             2
#define NMH_hWnd                                  3
#define NMH_dwItemSpec                            4  /* NMHMOUSE    */
#define NMH_action                                4  /* NMHTREEVIEW */

/*-*/

#define EVENT_HANDELLED                           0
#define EVENT_UNHANDELLED                         1

/*-*/

#define WVG_AUTOSIZE                              -1

#define WVGALIGN_TOP                              0
#define WVGALIGN_LEFT                             0
#define WVGALIGN_BOTTOM                           8
#define WVGALIGN_RIGHT                            2
#define WVGALIGN_HCENTER                          1
#define WVGALIGN_VCENTER                          4
#define WVGALIGN_WORDBREAK                        16

#define WVGFRAME_NONE                             0
#define WVGFRAME_RECT                             1
#define WVGFRAME_BOX                              2
#define WVGFRAME_RAISED                           16
#define WVGFRAME_RECESSED                         32
#define WVGFRAME_THICK                            128
#define WVGFRAME_DASHED                           256
#define WVGFRAME_DOTTED                           512

#define WVGTOOLBAR_STYLE_STANDARD                 0
#define WVGTOOLBAR_STYLE_FLAT                     1
#define WVGTOOLBAR_STYLE_VERTICAL                 2

#define WVGTOOLBAR_BUTTON_DEFAULT                 0

/*      Statusbar Manipulation Constants         */
#define WVGSTATUSBAR_AUTOSIZE_NONE                0
#define WVGSTATUSBAR_AUTOSIZE_SPRING              1
#define WVGSTATUSBAR_AUTOSIZE_CONTENTS            2

#define WVGSTATUSBAR_BEVEL_NONE                   0
#define WVGSTATUSBAR_BEVEL_INSET                  1
#define WVGSTATUSBAR_BEVEL_RAISED                 2

#define WVGSTATUSBAR_PANEL_TEXT                   0
#define WVGSTATUSBAR_PANEL_CAPSLOCK               1
#define WVGSTATUSBAR_PANEL_NUMLOCK                2
#define WVGSTATUSBAR_PANEL_INSERT                 3
#define WVGSTATUSBAR_PANEL_SCROLL                 4
#define WVGSTATUSBAR_PANEL_TIME                   5
#define WVGSTATUSBAR_PANEL_DATE                   6
#define WVGSTATUSBAR_PANEL_KANA                   7

#define WVGSTATIC_TYPE_TEXT                       1
#define WVGSTATIC_TYPE_GROUPBOX                   2
#define WVGSTATIC_TYPE_ICON                       3
#define WVGSTATIC_TYPE_SYSICON                    4
#define WVGSTATIC_TYPE_BITMAP                     5
#define WVGSTATIC_TYPE_FGNDRECT                   6
#define WVGSTATIC_TYPE_BGNDRECT                   7
#define WVGSTATIC_TYPE_FGNDFRAME                  8
#define WVGSTATIC_TYPE_BGNDFRAME                  9
#define WVGSTATIC_TYPE_HALFTONERECT               10
#define WVGSTATIC_TYPE_HALFTONEFRAME              11
#define WVGSTATIC_TYPE_RAISEDBOX                  12
#define WVGSTATIC_TYPE_RECESSEDBOX                13
#define WVGSTATIC_TYPE_RAISEDRECT                 14
#define WVGSTATIC_TYPE_RECESSEDRECT               15
#define WVGSTATIC_TYPE_RAISEDLINE                 16
#define WVGSTATIC_TYPE_RECESSEDLINE               17

#define WVGSTATIC_FRAMETHIN                       1
#define WVGSTATIC_FRAMETHICK                      2

#define WVGDLG_FRAMESTAT_MINIMIZED                1
#define WVGDLG_FRAMESTAT_MAXIMIZED                2
#define WVGDLG_FRAMESTAT_NORMALIZED               3

#define WVGSTATIC_TEXT_LEFT                       WVGALIGN_LEFT
#define WVGSTATIC_TEXT_RIGHT                      WVGALIGN_RIGHT
#define WVGSTATIC_TEXT_CENTER                     WVGALIGN_HCENTER
#define WVGSTATIC_TEXT_TOP                        WVGALIGN_TOP
#define WVGSTATIC_TEXT_VCENTER                    WVGALIGN_VCENTER
#define WVGSTATIC_TEXT_BOTTOM                     WVGALIGN_BOTTOM
#define WVGSTATIC_TEXT_WORDBREAK                  WVGALIGN_WORDBREAK

#define WVGSTATIC_BITMAP_TILED                    1
#define WVGSTATIC_BITMAP_SCALED                   2

#define WVG_DRAW_NORMAL                           0

#define WVGLISTBOX_MM_SINGLE                      1

/* Generic Pres Parameters */
#define WVG_PP_FGCLR                              2
#define WVG_PP_BGCLR                              4
#define WVG_PP_COMPOUNDNAME                       15
#define WVG_PP_FONT                               16

#define WVGSLE_LEFT                               1
#define WVGSLE_RIGHT                              2
#define WVGSLE_CENTER                             3

#define WVGMENUBAR_MIS_SEPARATOR                  4
#define WVGMENUBAR_MIS_BUTTONSEPARATOR            512

/* SCROLLBAR */
#define WVGSCROLL_HORIZONTAL                      1
#define WVGSCROLL_VERTICAL                        2

#define WVGSB_PREVPOS                             1
#define WVGSB_NEXTPOS                             2
#define WVGSB_PREVPAGE                            3
#define WVGSB_NEXTPAGE                            4
#define WVGSB_SLIDERTRACK                         5
#define WVGSB_ENDTRACK                            6
#define WVGSB_ENDSCROLL                           7
#define WVGSB_TOP                                 11
#define WVGSB_BOTTOM                              12

#define WVGTABPAGE_TAB_BOTTOM                     2
#define WVGTABPAGE_TAB_TOP                        4

#define WVGTOOLBAR_BUTTON_SEPARATOR               4

#define WVGCOMBO_SIMPLE                           1
#define WVGCOMBO_DROPDOWN                         2
#define WVGCOMBO_DROPDOWNLIST                     3

#endif /* #ifndef _WVG_CH */

/*-*/
