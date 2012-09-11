/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the WVG*Classes
 *
 * Copyright 2004 Pritpal Bedi <pritpal@vouchcac.com>
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

#ifndef _HBGTWVG_CH
#define _HBGTWVG_CH

/*----------------------------------------------------------------------*/
/*                 Extended GT Manipulation Constants                   */
/*----------------------------------------------------------------------*/
#if 0  /*Already in hbgtinfo.ch */
   #define HB_GTE_ACTIVATE                1
   #define HB_GTE_SETFOCUS                2
   #define HB_GTE_KILLFOCUS               3
   #define HB_GTE_CLOSE                   4
   #define HB_GTE_RESIZED                 5
#endif
#define HB_GTE_MOUSE                      6
#define HB_GTE_KEYBOARD                   7
#define HB_GTE_TIMER                      8
#define HB_GTE_MENU                       9
#define HB_GTE_NOTIFY                    10
#define HB_GTE_COMMAND                   11
#define HB_GTE_CTLCOLOR                  12
#define HB_GTE_PAINT                     13
#define HB_GTE_GUIPARTS                  14
#define HB_GTE_HSCROLL                   15
#define HB_GTE_VSCROLL                   16
#define HB_GTE_ANY                       17
#define HB_GTE_KEYTOITEM                 18
#define HB_GTE_CREATED                   19
#define HB_GTE_CLOSED                    20

#define HB_GTI_SETFONT                   71
#define HB_GTI_PRESPARAMS                72
#define HB_GTI_ENABLE                    73
#define HB_GTI_DISABLE                   74
#define HB_GTI_SETFOCUS                  75
#define HB_GTI_DEFERPAINT                76
#define HB_GTI_ACTIVATESELECTCOPY        77
#define HB_GTI_SETPOSANDSIZE             78
#define HB_GTI_REFRESH                   79
#define HB_GTI_NOTIFIERBLOCKGUI          80
#define HB_GTI_MAXIMIZABLE               81

/* Presentation Parameters | HB_GTI_PRESPARAMS */
#define HB_GTI_PP_EXSTYLE                 1
#define HB_GTI_PP_STYLE                   2
#define HB_GTI_PP_X                       3
#define HB_GTI_PP_Y                       4
#define HB_GTI_PP_WIDTH                   5
#define HB_GTI_PP_HEIGHT                  6
#define HB_GTI_PP_PARENT                  7
#define HB_GTI_PP_VISIBLE                 8
#define HB_GTI_PP_ROWCOLS                 9
#define HB_GTI_PP_WNDTYPE                10

#define HB_GTI_PP_SIZE                   10

#define HB_WNDTYPE_CRT                    0
#define HB_WNDTYPE_DIALOG                 1

#define HB_GTI_SPEC                    1000

/* Window Specifications | HB_GTI_SPEC */
#define HB_GTS_WINDOWHANDLE               1
#define HB_GTS_CENTERWINDOW               2
#define HB_GTS_PROCESSMESSAGES            3
#define HB_GTS_KEYBOARD                   4
#define HB_GTS_RESETWINDOW                5
#define HB_GTS_WNDSTATE                   6
#define HB_GTS_SETTIMER                   7
#define HB_GTS_KILLTIMER                  8
#define HB_GTS_SETPOSITION                9
#define HB_GTS_SHOWWINDOW                10
#define HB_GTS_UPDATEWINDOW              11
#define HB_GTS_SYSTRAYICON               12
#define HB_GTS_FACTOR                    13


/* Window States | HB_GTS_WNDSTATE */
#define HB_GTS_WS_SETONTOP                1
#define HB_GTS_WS_SETASNORMAL             2
#define HB_GTS_WS_MINIMIZED               3
#define HB_GTS_WS_MAXIMIZED               4
#define HB_GTS_WS_HIDDEN                  5
#define HB_GTS_WS_NORMAL                  6

/* SysTrayIcon Types | HB_GTS_SYSTRAYICON */
#define HB_GTS_NIT_FILE                   0
#define HB_GTS_NIT_RESOURCEBYNAME         1
#define HB_GTS_NIT_RESOURCEBYID           2

/* ShowWindow modes | HB_GTS_SHOWWINDOW */
#define HB_GTS_SW_HIDE                    0
#define HB_GTS_SW_NORMAL                  1
#define HB_GTS_SW_MINIMIZED               2
#define HB_GTS_SW_MAXIMIZED               3
#define HB_GTS_SW_RESTORE                 9

#define HB_GTI_GUI                     1001

/* Constants to manage CUI-GUI objects  */
#define GOBJ_OBJSTATE_ENABLED             1
#define GOBJ_OBJSTATE_DISABLED            2
#define GOBJ_OBJSTATE_HIDDEN              3
#define GOBJ_OBJSTATE_HILIGHTED           4

#define GOBJ_OBJDATA_TEXT                 1
#define GOBJ_OBJDATA_PICTURE              2
#define GOBJ_OBJDATA_HFONT                3
#define GOBJ_OBJDATA_HPEN                 4
#define GOBJ_OBJDATA_HBRUSH               5
#define GOBJ_OBJDATA_COLORTEXT            6
#define GOBJ_OBJDATA_COLORBK              7
#define GOBJ_OBJDATA_IMAGE                8
#define GOBJ_OBJDATA_BLOCK                9

#define GOBJ_IMAGESOURCE_SLOT             1
#define GOBJ_IMAGESOURCE_RESOURCE         2
#define GOBJ_IMAGESOURCE_FILE             3


/* CUI-GUI Draw Objects */
#define GOBJ_OBJTYPE_BOXRAISED            1
#define GOBJ_OBJTYPE_BOXRECESSED          2
#define GOBJ_OBJTYPE_BOXGET               3
#define GOBJ_OBJTYPE_BOXGROUP             4
#define GOBJ_OBJTYPE_BOXGROUPRAISED       5
#define GOBJ_OBJTYPE_PICTURE              6
#define GOBJ_OBJTYPE_LINE                 7
#define GOBJ_OBJTYPE_LINEEX               8
#define GOBJ_OBJTYPE_LABEL                9
#define GOBJ_OBJTYPE_LABELEX              10
#define GOBJ_OBJTYPE_OUTLINE              11
#define GOBJ_OBJTYPE_ELLIPSE              12
#define GOBJ_OBJTYPE_RECTANGLE            13
#define GOBJ_OBJTYPE_ROUNDRECT            14
#define GOBJ_OBJTYPE_COLORRECT            15
#define GOBJ_OBJTYPE_SHADEDRECT           16
#define GOBJ_OBJTYPE_TEXTBOX              17
#define GOBJ_OBJTYPE_OUTLINEEX            18
#define GOBJ_OBJTYPE_GRIDVERT             19
#define GOBJ_OBJTYPE_GRIDHORZ             20

#define GOBJ_OBJTYPE_OBJECT               25  /* One of the above objects */

/*----------------------------------------------------------------------*/

#endif /* _HBGTWVG_CH */
