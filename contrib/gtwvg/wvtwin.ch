/*
 * Header file for the WVT*Classes
 *
 * Copyright 2004-2011 Pritpal Bedi <bedipritpal@hotmail.com>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#ifndef _WVGWIN_CH
#define _WVGWIN_CH

#include "hbwin.ch"

#ifndef __NO_LEGACY__
#include "wvtwinlg.ch"
#endif

/* wvt_DrawButton() constants */
#define WVT_BTN_FORMAT_RAISED                     0  // Default
#define WVT_BTN_FORMAT_RECESSED                   1
#define WVT_BTN_FORMAT_FLAT                       2
#define WVT_BTN_FORMAT_OUTLINED                   3

#define WVT_BTN_IMAGE_TOP                         0  // Default
#define WVT_BTN_IMAGE_LEFT                        1
#define WVT_BTN_IMAGE_BOTTOM                      2
#define WVT_BTN_IMAGE_RIGHT                       3

/* wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor ) */

/* nOrient */
#define WVT_LINE_HORZ                             0  // Default
#define WVT_LINE_VERT                             1

// nFormat
#define WVT_LINE_RAISED                           0  // Default
#define WVT_LINE_RECESSED                         1
#define WVT_LINE_PLAIN                            2

// nAlign
#define WVT_LINE_CENTER                           0  // Default
#define WVT_LINE_TOP                              1
#define WVT_LINE_BOTTOM                           2
#define WVT_LINE_LEFT                             3
#define WVT_LINE_RIGHT                            4

// nStyle
#define WVT_LINE_SOLID                            0  // Default
#define WVT_LINE_DASH                             1
#define WVT_LINE_DOT                              2
#define WVT_LINE_DASHDOT                          3
#define WVT_LINE_DASHDOTDOT                       4

// Standard Mouse Pointer Shape Constants
#define WVT_IDC_ARROW                             1
#define WVT_IDC_IBEAM                             2
#define WVT_IDC_WAIT                              3
#define WVT_IDC_CROSS                             4
#define WVT_IDC_UPARROW                           5
#define WVT_IDC_SIZE                              6
#define WVT_IDC_ICON                              7
#define WVT_IDC_SIZENWSE                          8
#define WVT_IDC_SIZENESW                          9
#define WVT_IDC_SIZEWE                            10
#define WVT_IDC_SIZENS                            11
#define WVT_IDC_SIZEALL                           12
#define WVT_IDC_NO                                13
#define WVT_IDC_HAND                              14
#define WVT_IDC_APPSTARTING                       15
#define WVT_IDC_HELP                              16

/* - */

#define WVT_BLOCK_IMAGE                           1
#define WVT_BLOCK_BOX                             2
#define WVT_BLOCK_LABEL                           3
#define WVT_BLOCK_GRID_V                          4
#define WVT_BLOCK_GRID_H                          5
#define WVT_BLOCK_BUTTON                          6
#define WVT_BLOCK_GETS                            7
#define WVT_BLOCK_LINE                            8
#define WVT_BLOCK_STATUSBAR                       9
#define WVT_BLOCK_TOOLBAR                         10
#define WVT_BLOCK_STATIC                          11

/* - */

#define DLG_OBJ_BROWSE                            1
#define DLG_OBJ_PICTURE                           2
#define DLG_OBJ_LINE                              3
#define DLG_OBJ_RECT                              4
#define DLG_OBJ_GETS                              5
#define DLG_OBJ_BUTTON                            6
#define DLG_OBJ_STATUSBAR                         7
#define DLG_OBJ_PANEL                             8
#define DLG_OBJ_LABEL                             9
#define DLG_OBJ_STATIC                            10
#define DLG_OBJ_TOOLBAR                           11
#define DLG_OBJ_IMAGE                             12
#define DLG_OBJ_PUSHBUTTON                        13
#define DLG_OBJ_CONSOLE                           14
#define DLG_OBJ_SCROLLBAR                         15
#define DLG_OBJ_BANNER                            16
#define DLG_OBJ_TEXTBOX                           17
#define DLG_OBJ_PROGRESSBAR                       18

/* - */

#define TLB_BUTTON_TYPE_IMAGE                     0
#define TLB_BUTTON_TYPE_SEPARATOR                 1
#define TLB_BUTTON_TYPE_TEXT                      2

/* - */

#define WVT_STATIC_LINE                           1
#define WVT_STATIC_BOXRAISED                      2
#define WVT_STATIC_BOXRECESSED                    3
#define WVT_STATIC_BOXGROUP                       4
#define WVT_STATIC_BOXGROUPRAISED                 5
#define WVT_STATIC_RECTANGLE                      6
#define WVT_STATIC_ROUNDRECT                      7
#define WVT_STATIC_FOCUSRECT                      8
#define WVT_STATIC_OUTLINE                        9
#define WVT_STATIC_ELLIPSE                        10
#define WVT_STATIC_SHADEDRECT                     11

#define WVT_SCROLLBAR_VERT                        1
#define WVT_SCROLLBAR_HORZ                        2

#define WVT_SCROLLBUTTON_TOP                      1
#define WVT_SCROLLBUTTON_LEFT                     2
#define WVT_SCROLLBUTTON_BOTTOM                   3
#define WVT_SCROLLBUTTON_RIGHT                    4
#define WVT_SCROLL_THUMB                          5

/* WvtMenu() defines [Peter Rees] */
#define WVT_MENU_TYPE                             1
#define WVT_MENU_IDENTIFIER                       2
#define WVT_MENU_CAPTION                          3
#define WVT_MENU_ACTION                           4
#define WVT_MENU_MENUOBJ                          4

// Windows Specific

#define LVM_FIRST                                 0x1000  // ListView messages
#define TV_FIRST                                  0x1100  // TreeView messages
#define TVN_FIRST                                 -400
#define HDM_FIRST                                 0x1200  // Header messages
#define TCM_FIRST                                 0x1300  // Tab control messages


#define CCM_FIRST                                 0x2000
#define CCM_LAST                                  ( CCM_FIRST + 0x200 )
#define CCM_SETBKCOLOR                            8193
#define CCM_SETCOLORSCHEME                        8194
#define CCM_GETCOLORSCHEME                        8195
#define CCM_GETDROPTARGET                         8196
#define CCM_SETUNICODEFORMAT                      8197
#define CCM_GETUNICODEFORMAT                      8198
#define CCM_SETVERSION                            0x2007
#define CCM_GETVERSION                            0x2008
#define CCM_SETNOTIFYWINDOW                       0x2009
#define CCM_SETWINDOWTHEME                        0x200b
#define CCM_DPISCALE                              0x200c

/* Dialog styles */
#define DS_ABSALIGN                               0x01
#define DS_SYSMODAL                               0x02
#define DS_3DLOOK                                 0x04
#define DS_FIXEDSYS                               0x08
#define DS_NOFAILCREATE                           0x10
#define DS_LOCALEDIT                              0x20
#define DS_SETFONT                                0x40
#define DS_MODALFRAME                             0x80
#define DS_NOIDLEMSG                              0x100
#define DS_CONTROL                                0x400
#define DS_CENTER                                 0x800
#define DS_CENTERMOUSE                            0x1000
#define DS_CONTEXTHELP                            0x2000

/* Button Control Styles */
#define BS_PUSHBUTTON                             0x00000000
#define BS_DEFPUSHBUTTON                          0x00000001
#define BS_CHECKBOX                               0x00000002
#define BS_AUTOCHECKBOX                           0x00000003
#define BS_RADIOBUTTON                            0x00000004
#define BS_3STATE                                 0x00000005
#define BS_AUTO3STATE                             0x00000006
#define BS_GROUPBOX                               0x00000007
#define BS_USERBUTTON                             0x00000008
#define BS_AUTORADIOBUTTON                        0x00000009
#define BS_OWNERDRAW                              0x0000000B
#define BS_LEFTTEXT                               0x00000020

#define BS_TEXT                                   0x00000000
#define BS_ICON                                   0x00000040
#define BS_BITMAP                                 0x00000080
#define BS_LEFT                                   0x00000100
#define BS_RIGHT                                  0x00000200
#define BS_CENTER                                 0x00000300
#define BS_TOP                                    0x00000400
#define BS_BOTTOM                                 0x00000800
#define BS_VCENTER                                0x00000C00
#define BS_PUSHLIKE                               0x00001000
#define BS_MULTILINE                              0x00002000
#define BS_NOTIFY                                 0x00004000
#define BS_FLAT                                   0x00008000
#define BS_RIGHTBUTTON                            BS_LEFTTEXT

/* User Button Notification Codes */
#define BN_CLICKED                                0
#define BN_PAINT                                  1
#define BN_HILITE                                 2
#define BN_UNHILITE                               3
#define BN_DISABLE                                4
#define BN_DOUBLECLICKED                          5
#define BN_PUSHED                                 BN_HILITE
#define BN_UNPUSHED                               BN_UNHILITE
#define BN_DBLCLK                                 BN_DOUBLECLICKED
#define BN_SETFOCUS                               6
#define BN_KILLFOCUS                              7

/* Button Control Messages */
#define BM_GETCHECK                               240
#define BM_SETCHECK                               241
#define BM_GETSTATE                               242
#define BM_SETSTATE                               243
#define BM_SETSTYLE                               244
#define BM_CLICK                                  245
#define BM_GETIMAGE                               246
#define BM_SETIMAGE                               247

#define BST_UNCHECKED                             0
#define BST_CHECKED                               1
#define BST_INDETERMINATE                         2
#define BST_PUSHED                                4
#define BST_FOCUS                                 8

/* Edit Control Styles */
#define ES_LEFT                                   0
#define ES_CENTER                                 1
#define ES_RIGHT                                  2
#define ES_MULTILINE                              4
#define ES_UPPERCASE                              8
#define ES_LOWERCASE                              16
#define ES_PASSWORD                               32
#define ES_AUTOVSCROLL                            64
#define ES_AUTOHSCROLL                            128
#define ES_NOHIDESEL                              256
#define ES_OEMCONVERT                             1024
#define ES_READONLY                               2048
#define ES_WANTRETURN                             4096
#define ES_NUMBER                                 8192

/* Edit Control Notification Codes */
#define EN_SETFOCUS                               0x0100
#define EN_KILLFOCUS                              0x0200
#define EN_CHANGE                                 0x0300
#define EN_UPDATE                                 0x0400
#define EN_ERRSPACE                               0x0500
#define EN_MAXTEXT                                0x0501
#define EN_HSCROLL                                0x0601
#define EN_VSCROLL                                0x0602

/* Edit Control Messages */
#define EM_GETSEL                                 176
#define EM_SETSEL                                 177
#define EM_GETRECT                                178
#define EM_SETRECT                                179
#define EM_SETRECTNP                              180
#define EM_SCROLL                                 181
#define EM_LINESCROLL                             182
#define EM_SCROLLCARET                            183
#define EM_GETMODIFY                              184
#define EM_SETMODIFY                              185
#define EM_GETLINECOUNT                           186
#define EM_LINEINDEX                              187
#define EM_SETHANDLE                              188
#define EM_GETHANDLE                              189
#define EM_GETTHUMB                               190
#define EM_LINELENGTH                             193
#define EM_REPLACESEL                             194
#define EM_GETLINE                                196
#define EM_LIMITTEXT                              197
#define EM_CANUNDO                                198
#define EM_UNDO                                   199
#define EM_FMTLINES                               200
#define EM_LINEFROMCHAR                           201
#define EM_SETTABSTOPS                            203
#define EM_SETPASSWORDCHAR                        204
#define EM_EMPTYUNDOBUFFER                        205
#define EM_GETFIRSTVISIBLELINE                    206
#define EM_SETREADONLY                            207
#define EM_SETWORDBREAKPROC                       208
#define EM_GETWORDBREAKPROC                       209
#define EM_GETPASSWORDCHAR                        210
#define EM_SETMARGINS                             211
#define EM_GETMARGINS                             212
#define EM_SETLIMITTEXT                           EM_LIMITTEXT
#define EM_GETLIMITTEXT                           213
#define EM_POSFROMCHAR                            214
#define EM_CHARFROMPOS                            215

/* Combo Box styles */
#define CBS_SIMPLE                                0x0001
#define CBS_DROPDOWN                              0x0002
#define CBS_DROPDOWNLIST                          0x0003
#define CBS_OWNERDRAWFIXED                        0x0010
#define CBS_OWNERDRAWVARIABLE                     0x0020
#define CBS_AUTOHSCROLL                           0x0040
#define CBS_OEMCONVERT                            0x0080
#define CBS_SORT                                  0x0100
#define CBS_HASSTRINGS                            0x0200
#define CBS_NOINTEGRALHEIGHT                      0x0400
#define CBS_DISABLENOSCROLL                       0x0800
#define CBS_UPPERCASE                             0x2000
#define CBS_LOWERCASE                             0x4000

/* Combo Box Notification Codes */
#define CBN_ERRSPACE                              -1
#define CBN_SELCHANGE                             1
#define CBN_DBLCLK                                2
#define CBN_SETFOCUS                              3
#define CBN_KILLFOCUS                             4
#define CBN_EDITCHANGE                            5
#define CBN_EDITUPDATE                            6
#define CBN_DROPDOWN                              7
#define CBN_CLOSEUP                               8
#define CBN_SELENDOK                              9
#define CBN_SELENDCANCEL                          10

/* Combo Box messages */
#define CB_GETEDITSEL                             320
#define CB_LIMITTEXT                              321
#define CB_SETEDITSEL                             322
#define CB_ADDSTRING                              323
#define CB_DELETESTRING                           324
#define CB_DIR                                    325
#define CB_GETCOUNT                               326
#define CB_GETCURSEL                              327
#define CB_GETLBTEXT                              328
#define CB_GETLBTEXTLEN                           329
#define CB_INSERTSTRING                           330
#define CB_RESETCONTENT                           331
#define CB_FINDSTRING                             332
#define CB_SELECTSTRING                           333
#define CB_SETCURSEL                              334
#define CB_SHOWDROPDOWN                           335
#define CB_GETITEMDATA                            336
#define CB_SETITEMDATA                            337
#define CB_GETDROPPEDCONTROLRECT                  338
#define CB_SETITEMHEIGHT                          339
#define CB_GETITEMHEIGHT                          340
#define CB_SETEXTENDEDUI                          341
#define CB_GETEXTENDEDUI                          342
#define CB_GETDROPPEDSTATE                        343
#define CB_FINDSTRINGEXACT                        344
#define CB_SETLOCALE                              345
#define CB_GETLOCALE                              346
#define CB_GETTOPINDEX                            347
#define CB_SETTOPINDEX                            348
#define CB_GETHORIZONTALEXTENT                    349
#define CB_SETHORIZONTALEXTENT                    350
#define CB_GETDROPPEDWIDTH                        351
#define CB_SETDROPPEDWIDTH                        352
#define CB_INITSTORAGE                            353
#define CB_MULTIPLEADDSTRING                      0x0163
#define CB_GETCOMBOBOXINFO                        0x0164
#define CB_MSGMAX                                 0x0165

/* Combo Box return Values */
#define CB_OKAY                                   0
#define CB_ERR                                    -1
#define CB_ERRSPACE                               -2

/* Static Control Constants */
#define SS_LEFT                                   0x00000000
#define SS_CENTER                                 0x00000001
#define SS_RIGHT                                  0x00000002
#define SS_ICON                                   0x00000003
#define SS_BLACKRECT                              0x00000004
#define SS_GRAYRECT                               0x00000005
#define SS_WHITERECT                              0x00000006
#define SS_BLACKFRAME                             0x00000007
#define SS_GRAYFRAME                              0x00000008
#define SS_WHITEFRAME                             0x00000009
#define SS_USERITEM                               0x0000000A
#define SS_SIMPLE                                 0x0000000B
#define SS_LEFTNOWORDWRAP                         0x0000000C
#define SS_OWNERDRAW                              0x0000000D
#define SS_BITMAP                                 0x0000000E
#define SS_ENHMETAFILE                            0x0000000F
#define SS_ETCHEDHORZ                             0x00000010
#define SS_ETCHEDVERT                             0x00000011
#define SS_ETCHEDFRAME                            0x00000012
#define SS_TYPEMASK                               0x0000001F
#define SS_NOPREFIX                               0x00000080  // Don't do "&" character translation
#define SS_NOTIFY                                 0x00000100
#define SS_CENTERIMAGE                            0x00000200
#define SS_RIGHTJUST                              0x00000400
#define SS_REALSIZEIMAGE                          0x00000800
#define SS_SUNKEN                                 0x00001000

#define SS_ENDELLIPSIS                            16384
#define SS_PATHELLIPSIS                           32768
#define SS_WORDELLIPSIS                           49152
#define SS_ELLIPSISMASK                           49152

/* Static Control Mesages */
#define STM_SETICON                               368
#define STM_GETICON                               369
#define STM_SETIMAGE                              370
#define STM_GETIMAGE                              371

#define STN_CLICKED                               0
#define STN_DBLCLK                                1
#define STN_ENABLE                                2
#define STN_DISABLE                               3

/* Listbox messages */
#define LB_ADDSTRING                              384
#define LB_INSERTSTRING                           385
#define LB_DELETESTRING                           386
#define LB_SELITEMRANGEEX                         387
#define LB_RESETCONTENT                           388
#define LB_SETSEL                                 389
#define LB_SETCURSEL                              390
#define LB_GETSEL                                 391
#define LB_GETCURSEL                              392
#define LB_GETTEXT                                393
#define LB_GETTEXTLEN                             394
#define LB_GETCOUNT                               395
#define LB_SELECTSTRING                           396
#define LB_DIR                                    397
#define LB_GETTOPINDEX                            398
#define LB_FINDSTRING                             399
#define LB_GETSELCOUNT                            400
#define LB_GETSELITEMS                            401
#define LB_SETTABSTOPS                            402
#define LB_GETHORIZONTALEXTENT                    403
#define LB_SETHORIZONTALEXTENT                    404
#define LB_SETCOLUMNWIDTH                         405
#define LB_ADDFILE                                406
#define LB_SETTOPINDEX                            407
#define LB_GETITEMRECT                            408
#define LB_GETITEMDATA                            409
#define LB_SETITEMDATA                            410
#define LB_SELITEMRANGE                           411
#define LB_SETANCHORINDEX                         412
#define LB_GETANCHORINDEX                         413
#define LB_SETCARETINDEX                          414
#define LB_GETCARETINDEX                          415
#define LB_SETITEMHEIGHT                          416
#define LB_GETITEMHEIGHT                          417
#define LB_FINDSTRINGEXACT                        418
#define LB_SETLOCALE                              421
#define LB_GETLOCALE                              422
#define LB_SETCOUNT                               423
#define LB_INITSTORAGE                            424
#define LB_ITEMFROMPOINT                          425

/* Listbox Styles */
#define LBS_NOTIFY                                1
#define LBS_SORT                                  2
#define LBS_NOREDRAW                              4
#define LBS_MULTIPLESEL                           8
#define LBS_OWNERDRAWFIXED                        16
#define LBS_OWNERDRAWVARIABLE                     32
#define LBS_HASSTRINGS                            64
#define LBS_USETABSTOPS                           128
#define LBS_NOINTEGRALHEIGHT                      256
#define LBS_MULTICOLUMN                           512
#define LBS_WANTKEYBOARDINPUT                     1024
#define LBS_EXTENDEDSEL                           2048
#define LBS_DISABLENOSCROLL                       4096
#define LBS_NODATA                                8192
#define LBS_NOSEL                                 16384
#define LBS_STANDARD                              ( LBS_NOTIFY + LBS_SORT + WS_VSCROLL + WS_BORDER )

/* Listbox Notification Codes */
#define LBN_ERRSPACE                              -2
#define LBN_SELCHANGE                             1
#define LBN_DBLCLK                                2
#define LBN_SELCANCEL                             3
#define LBN_SETFOCUS                              4
#define LBN_KILLFOCUS                             5

/* Stock Logical Objects */
#define WHITE_BRUSH                               0
#define LTGRAY_BRUSH                              1
#define GRAY_BRUSH                                2
#define DKGRAY_BRUSH                              3
#define BLACK_BRUSH                               4
#define NULL_BRUSH                                5
#define HOLLOW_BRUSH                              NULL_BRUSH

#define WHITE_PEN                                 6
#define BLACK_PEN                                 7
#define NULL_PEN                                  8

#define OEM_FIXED_FONT                            10
#define ANSI_FIXED_FONT                           11
#define ANSI_VAR_FONT                             12
#define SYSTEM_FONT                               13
#define DEVICE_DEFAULT_FONT                       14
#define DEFAULT_PALETTE                           15
#define SYSTEM_FIXED_FONT                         16
#define DEFAULT_GUI_FONT                          17

// Pen Styles
#define PS_ENDCAP_ROUND                           0
#define PS_ENDCAP_SQUARE                          256
#define PS_ENDCAP_FLAT                            512
#define PS_ENDCAP_MASK                            3840

#define PS_JOIN_ROUND                             0
#define PS_JOIN_BEVEL                             4096
#define PS_JOIN_MITER                             8192
#define PS_JOIN_MASK                              61440

#define PS_COSMETIC                               0
#define PS_GEOMETRIC                              65536
#define PS_TYPE_MASK                              983040

#define BOLD_FONTTYPE                             0x0100
#define ITALIC_FONTTYPE                           0x0200
#define REGULAR_FONTTYPE                          0x0400
#define SCREEN_FONTTYPE                           0x2000
#define PRINTER_FONTTYPE                          0x4000
#define SIMULATED_FONTTYPE                        0x8000

// flags (CHOOSECOLOR structure)
#define CC_RGBINIT                                0x00000001
#define CC_FULLOPEN                               0x00000002
#define CC_PREVENTFULLOPEN                        0x00000004
#define CC_SHOWHELP                               0x00000008
#define CC_ENABLEHOOK                             0x00000010
#define CC_ENABLETEMPLATE                         0x00000020
#define CC_ENABLETEMPLATEHANDLE                   0x00000040
#define CC_SOLIDCOLOR                             0x00000080  // WINVER >= 0x0400
#define CC_ANYCOLOR                               0x00000100  // WINVER >= 0x0400

/* Common Control Constants */
#define CCS_TOP                                   1
#define CCS_NOMOVEY                               2
#define CCS_BOTTOM                                3
#define CCS_NORESIZE                              4
#define CCS_NOPARENTALIGN                         8
#define CCS_ADJUSTABLE                            32
#define CCS_NODIVIDER                             64
#define CCS_VERT                                  128
#define CCS_LEFT                                  ( CCS_VERT + CCS_TOP )
#define CCS_RIGHT                                 ( CCS_VERT + CCS_BOTTOM )
#define CCS_NOMOVEX                               ( CCS_VERT + CCS_NOMOVEY )

#define TOOLBARCLASSNAME                          "ToolbarWindow32"
#define STATUSCLASSNAME                           "msctls_statusbar32"

/* Toolbar messages */
#define TB_ADDBITMAP                              ( WIN_WM_USER + 19 )
#define TB_SAVERESTOREA                           ( WIN_WM_USER + 26 )
#define TB_SAVERESTOREW                           ( WIN_WM_USER + 76 )
#define TB_CUSTOMIZE                              ( WIN_WM_USER + 27 )
#define TB_ADDSTRINGA                             ( WIN_WM_USER + 28 )
#define TB_ADDSTRINGW                             ( WIN_WM_USER + 77 )
#define TB_GETITEMRECT                            ( WIN_WM_USER + 29 )
#define TB_BUTTONSTRUCTSIZE                       ( WIN_WM_USER + 30 )
#define TB_SETBUTTONSIZE                          ( WIN_WM_USER + 31 )
#define TB_SETBITMAPSIZE                          ( WIN_WM_USER + 32 )
#define TB_AUTOSIZE                               ( WIN_WM_USER + 33 )
#define TB_GETTOOLTIPS                            ( WIN_WM_USER + 35 )
#define TB_SETTOOLTIPS                            ( WIN_WM_USER + 36 )
#define TB_SETPARENT                              ( WIN_WM_USER + 37 )
#define TB_SETROWS                                ( WIN_WM_USER + 39 )
#define TB_GETROWS                                ( WIN_WM_USER + 40 )
#define TB_GETBITMAPFLAGS                         ( WIN_WM_USER + 41 )
#define TB_SETCMDID                               ( WIN_WM_USER + 42 )
#define TB_CHANGEBITMAP                           ( WIN_WM_USER + 43 )
#define TB_GETBITMAP                              ( WIN_WM_USER + 44 )
#define TB_GETBUTTONTEXTA                         ( WIN_WM_USER + 45 )
#define TB_GETBUTTONTEXTW                         ( WIN_WM_USER + 75 )
#define TB_REPLACEBITMAP                          ( WIN_WM_USER + 46 )
#define TB_SETINDENT                              ( WIN_WM_USER + 47 )
#define TB_SETIMAGELIST                           ( WIN_WM_USER + 48 )
#define TB_GETIMAGELIST                           ( WIN_WM_USER + 49 )
#define TB_LOADIMAGES                             ( WIN_WM_USER + 50 )
#define TB_GETRECT                                ( WIN_WM_USER + 51 )    // wParam is the Cmd instead of index
#define TB_SETHOTIMAGELIST                        ( WIN_WM_USER + 52 )
#define TB_GETHOTIMAGELIST                        ( WIN_WM_USER + 53 )
#define TB_SETDISABLEDIMAGELIST                   ( WIN_WM_USER + 54 )
#define TB_GETDISABLEDIMAGELIST                   ( WIN_WM_USER + 55 )
#define TB_SETSTYLE                               ( WIN_WM_USER + 56 )
#define TB_GETSTYLE                               ( WIN_WM_USER + 57 )
#define TB_GETBUTTONSIZE                          ( WIN_WM_USER + 58 )
#define TB_SETBUTTONWIDTH                         ( WIN_WM_USER + 59 )
#define TB_SETMAXTEXTROWS                         ( WIN_WM_USER + 60 )
#define TB_GETTEXTROWS                            ( WIN_WM_USER + 61 )

#define TB_GETBUTTONTEXT                          TB_GETBUTTONTEXTW
#define TB_SAVERESTORE                            TB_SAVERESTOREW
#define TB_ADDSTRING                              TB_ADDSTRINGW

#define TB_GETOBJECT                              ( WIN_WM_USER + 62 )    // wParam == IID, lParam void **ppv
#define TB_GETHOTITEM                             ( WIN_WM_USER + 71 )
#define TB_SETHOTITEM                             ( WIN_WM_USER + 72 )    // wParam == iHotItem
#define TB_SETANCHORHIGHLIGHT                     ( WIN_WM_USER + 73 )    // wParam == TRUE/FALSE
#define TB_GETANCHORHIGHLIGHT                     ( WIN_WM_USER + 74 )
#define TB_MAPACCELERATORA                        ( WIN_WM_USER + 78 )    // wParam == ch, lParam int * pidBtn


#define TB_GETINSERTMARK                          ( WIN_WM_USER + 79 )    // lParam == LPTBINSERTMARK
#define TB_SETINSERTMARK                          ( WIN_WM_USER + 80 )    // lParam == LPTBINSERTMARK
#define TB_INSERTMARKHITTEST                      ( WIN_WM_USER + 81 )    // wParam == LPPOINT lParam == LPTBINSERTMARK
#define TB_MOVEBUTTON                             ( WIN_WM_USER + 82 )
#define TB_GETMAXSIZE                             ( WIN_WM_USER + 83 )    // lParam == LPSIZE
#define TB_SETEXTENDEDSTYLE                       ( WIN_WM_USER + 84 )    // For TBSTYLE_EX_*
#define TB_GETEXTENDEDSTYLE                       ( WIN_WM_USER + 85 )    // For TBSTYLE_EX_*
#define TB_GETPADDING                             ( WIN_WM_USER + 86 )
#define TB_SETPADDING                             ( WIN_WM_USER + 87 )
#define TB_SETINSERTMARKCOLOR                     ( WIN_WM_USER + 88 )
#define TB_GETINSERTMARKCOLOR                     ( WIN_WM_USER + 89 )

#define TB_SETCOLORSCHEME                         CCM_SETCOLORSCHEME  // lParam is color scheme
#define TB_GETCOLORSCHEME                         CCM_GETCOLORSCHEME  // fills in COLORSCHEME pointed to by lParam

#define TB_SETUNICODEFORMAT                       CCM_SETUNICODEFORMAT
#define TB_GETUNICODEFORMAT                       CCM_GETUNICODEFORMAT

#define TB_MAPACCELERATORW                        ( WIN_WM_USER + 90 )    // wParam == ch, lParam int * pidBtn
#define TB_MAPACCELERATOR                         TB_MAPACCELERATORW

#define TBIMHT_AFTER                              1                   // TRUE = insert After iButton, otherwise before
#define TBIMHT_BACKGROUND                         2                   // TRUE iff missed buttons completely

#define TBBF_LARGE                                1

#define TBIF_IMAGE                                1
#define TBIF_TEXT                                 2
#define TBIF_STATE                                4
#define TBIF_STYLE                                8
#define TBIF_LPARAM                               16
#define TBIF_COMMAND                              32
#define TBIF_SIZE                                 64

#define TBIF_BYINDEX                              2147483648          // this specifies that the wparam in Get/SetButtonInfo is an index, not id

#define TBBUTTONINFO                              TBBUTTONINFOW
#define LPTBBUTTONINFO                            LPTBBUTTONINFOW

#define TB_GETBUTTONINFOW                         ( WIN_WM_USER + 63 )
#define TB_SETBUTTONINFOW                         ( WIN_WM_USER + 64 )
#define TB_GETBUTTONINFOA                         ( WIN_WM_USER + 65 )
#define TB_SETBUTTONINFOA                         ( WIN_WM_USER + 66 )
#define TB_GETBUTTONINFO                          TB_GETBUTTONINFOW
#define TB_SETBUTTONINFO                          TB_SETBUTTONINFOW

#define TB_INSERTBUTTONW                          ( WIN_WM_USER + 67 )
#define TB_ADDBUTTONSW                            ( WIN_WM_USER + 68 )
#define TB_ADDBUTTONSA                            ( WIN_WM_USER + 20 )
#define TB_HITTEST                                ( WIN_WM_USER + 69 )
#define TB_INSERTBUTTON                           TB_INSERTBUTTONW
#define TB_ADDBUTTONS                             TB_ADDBUTTONSW

#define TB_SETDRAWTEXTFLAGS                       ( WIN_WM_USER + 70 )
#define TB_GETSTRING                              ( WIN_WM_USER + 92 )

#define TBN_FIRST                                 -700
#define TBN_GETBUTTONINFOA                        ( TBN_FIRST - 0 )
#define TBN_BEGINDRAG                             ( TBN_FIRST - 1 )
#define TBN_ENDDRAG                               ( TBN_FIRST - 2 )
#define TBN_BEGINADJUST                           ( TBN_FIRST - 3 )
#define TBN_ENDADJUST                             ( TBN_FIRST - 4 )
#define TBN_RESET                                 ( TBN_FIRST - 5 )
#define TBN_QUERYINSERT                           ( TBN_FIRST - 6 )
#define TBN_QUERYDELETE                           ( TBN_FIRST - 7 )
#define TBN_TOOLBARCHANGE                         ( TBN_FIRST - 8 )
#define TBN_CUSTHELP                              ( TBN_FIRST - 9 )
#define TBN_DROPDOWN                              ( TBN_FIRST - 10 )
#define TBN_GETOBJECT                             ( TBN_FIRST - 12 )
#define TBN_HOTITEMCHANGE                         ( TBN_FIRST - 13 )
#define TBN_DRAGOUT                               ( TBN_FIRST - 14 )
#define TBN_DELETINGBUTTON                        ( TBN_FIRST - 15 )
#define TBN_GETDISPINFOA                          ( TBN_FIRST - 16 )
#define TBN_GETDISPINFOW                          ( TBN_FIRST - 17 )
#define TBN_GETINFOTIPA                           ( TBN_FIRST - 18 )
#define TBN_GETINFOTIPW                           ( TBN_FIRST - 19 )
#define TBN_GETBUTTONINFOW                        ( TBN_FIRST - 20 )
#define TBN_RESTORE                               ( TBN_FIRST - 21 )
#define TBN_SAVE                                  ( TBN_FIRST - 22 )
#define TBN_INITCUSTOMIZE                         ( TBN_FIRST - 23 )


/* Toolbar Control Constants */
#define TBSTATE_CHECKED                           1
#define TBSTATE_PRESSED                           2
#define TBSTATE_ENABLED                           4
#define TBSTATE_HIDDEN                            8
#define TBSTATE_INDETERMINATE                     16
#define TBSTATE_WRAP                              32
#define TBSTATE_ELLIPSES                          64
#define TBSTATE_MARKED                            128

#define TBSTYLE_BUTTON                            0
#define TBSTYLE_SEP                               1
#define TBSTYLE_CHECK                             2
#define TBSTYLE_GROUP                             4
#define TBSTYLE_CHECKGROUP                        ( TBSTYLE_GROUP + TBSTYLE_CHECK )
#define TBSTYLE_DROPDOWN                          8
#define TBSTYLE_AUTOSIZE                          16
#define TBSTYLE_NOPREFIX                          32

#define TBSTYLE_TOOLTIPS                          256
#define TBSTYLE_WRAPABLE                          512
#define TBSTYLE_ALTDRAG                           1024
#define TBSTYLE_FLAT                              2048
#define TBSTYLE_LIST                              4096
#define TBSTYLE_CUSTOMERASE                       8192
#define TBSTYLE_REGISTERDROP                      16384
#define TBSTYLE_TRANSPARENT                       32768

#define BTNS_BUTTON                               TBSTYLE_BUTTON
#define BTNS_SEP                                  TBSTYLE_SEP
#define BTNS_CHECK                                TBSTYLE_CHECK
#define BTNS_GROUP                                TBSTYLE_GROUP
#define BTNS_CHECKGROUP                           TBSTYLE_CHECKGROUP
#define BTNS_DROPDOWN                             TBSTYLE_DROPDOWN
#define BTNS_AUTOSIZE                             TBSTYLE_AUTOSIZE
#define BTNS_NOPREFIX                             TBSTYLE_NOPREFIX
#define BTNS_SHOWTEXT                             64                  // ignored unless TBSTYLE_EX_MIXEDBUTTONS is set
#define BTNS_WHOLEDROPDOWN                        128                 // draw drop-down arrow, but without split arrow section

#define TBSTYLE_EX_DRAWDDARROWS                   1
#define TBSTYLE_EX_MIXEDBUTTONS                   8
#define TBSTYLE_EX_HIDECLIPPEDBUTTONS             16                  // don't show partially obscured buttons
#define TBSTYLE_EX_DOUBLEBUFFER                   0x00000080

#define NM_FIRST                                  ( 0 -  0 )          // generic to all controls
#define NM_LAST                                   ( 0 - 99 )

#define NM_OUTOFMEMORY                            ( NM_FIRST - 1 )
#define NM_CLICK                                  ( NM_FIRST - 2 )    // uses NMCLICK struct
#define NM_DBLCLK                                 ( NM_FIRST - 3 )
#define NM_RETURN                                 ( NM_FIRST - 4 )
#define NM_RCLICK                                 ( NM_FIRST - 5 )    // uses NMCLICK struct
#define NM_RDBLCLK                                ( NM_FIRST - 6 )
#define NM_SETFOCUS                               ( NM_FIRST - 7 )
#define NM_KILLFOCUS                              ( NM_FIRST - 8 )
#define NM_CUSTOMDRAW                             ( NM_FIRST - 12 )
#define NM_HOVER                                  ( NM_FIRST - 13 )
#define NM_NCHITTEST                              ( NM_FIRST - 14 )   // uses NMMOUSE struct
#define NM_KEYDOWN                                ( NM_FIRST - 15 )   // uses NMKEY struct
#define NM_RELEASEDCAPTURE                        ( NM_FIRST - 16 )
#define NM_SETCURSOR                              ( NM_FIRST - 17 )   // uses NMMOUSE struct
#define NM_CHAR                                   ( NM_FIRST - 18 )   // uses NMCHAR struct
#define NM_TOOLTIPSCREATED                        ( NM_FIRST - 19 )   // notify of when the tooltips window is create
#define NM_LDOWN                                  ( NM_FIRST - 20 )
#define NM_RDOWN                                  ( NM_FIRST - 21 )

#define SBARS_SIZEGRIP                            256
#define SBARS_TOOLTIPS                            2048

// SCROLLBARS

#define SB_HORZ                                   0
#define SB_VERT                                   1
#define SB_CTL                                    2
#define SB_BOTH                                   3

#define SB_LINELEFT                               0
#define SB_LINERIGHT                              1
#define SB_PAGELEFT                               2
#define SB_PAGERIGHT                              3
#define SB_LEFT                                   6
#define SB_RIGHT                                  7
//
#define SB_LINEUP                                 0
#define SB_LINEDOWN                               1
#define SB_PAGEUP                                 2
#define SB_PAGEDOWN                               3
#define SB_TOP                                    6
#define SB_BOTTOM                                 7
//
#define SB_THUMBPOSITION                          4
#define SB_THUMBTRACK                             5
#define SB_ENDSCROLL                              8

#define SBS_BOTTOMALIGN                           4
#define SBS_HORZ                                  0
#define SBS_LEFTALIGN                             2
#define SBS_RIGHTALIGN                            4
#define SBS_SIZEBOX                               8
#define SBS_SIZEBOXBOTTOMRIGHTALIGN               4
#define SBS_SIZEBOXTOPLEFTALIGN                   2
#define SBS_SIZEGRIP                              16
#define SBS_TOPALIGN                              2
#define SBS_VERT                                  1


#define SB_SETTEXTA                               ( WIN_WM_USER + 1 )
#define SB_SETTEXTW                               ( WIN_WM_USER + 11 )
#define SB_GETTEXTA                               ( WIN_WM_USER + 2 )
#define SB_GETTEXTW                               ( WIN_WM_USER + 13 )
#define SB_GETTEXTLENGTHA                         ( WIN_WM_USER + 3 )
#define SB_GETTEXTLENGTHW                         ( WIN_WM_USER + 12 )

#define SB_GETTEXT                                SB_GETTEXTW
#define SB_SETTEXT                                SB_SETTEXTW
#define SB_GETTEXTLENGTH                          SB_GETTEXTLENGTHW
#define SB_SETTIPTEXT                             SB_SETTIPTEXTW
#define SB_GETTIPTEXT                             SB_GETTIPTEXTW

#define SB_SETPARTS                               ( WIN_WM_USER + 4 )
#define SB_GETPARTS                               ( WIN_WM_USER + 6 )
#define SB_GETBORDERS                             ( WIN_WM_USER + 7 )
#define SB_SETMINHEIGHT                           ( WIN_WM_USER + 8 )
#define SB_SIMPLE                                 ( WIN_WM_USER + 9 )
#define SB_GETRECT                                ( WIN_WM_USER + 10 )
#define SB_ISSIMPLE                               ( WIN_WM_USER + 14 )
#define SB_SETICON                                ( WIN_WM_USER + 15 )
#define SB_SETTIPTEXTA                            ( WIN_WM_USER + 16 )
#define SB_SETTIPTEXTW                            ( WIN_WM_USER + 17 )
#define SB_GETTIPTEXTA                            ( WIN_WM_USER + 18 )
#define SB_GETTIPTEXTW                            ( WIN_WM_USER + 19 )
#define SB_GETICON                                ( WIN_WM_USER + 20 )
#define SB_SETUNICODEFORMAT                       CCM_SETUNICODEFORMAT
#define SB_GETUNICODEFORMAT                       CCM_GETUNICODEFORMAT

#define SBT_OWNERDRAW                             0x1000
#define SBT_NOBORDERS                             0x0100
#define SBT_POPOUT                                0x0200
#define SBT_RTLREADING                            0x0400
#define SBT_NOTABPARSING                          0x0800

#define SB_SETBKCOLOR                             CCM_SETBKCOLOR      // lParam = bkColor

#define SBN_SIMPLEMODECHANGE                      ( SBN_FIRST - 0 )

#define SB_SIMPLEID                               0x00ff

/* - */

#define ILC_COLOR                                 0
#define ILC_COLOR4                                4
#define ILC_COLOR8                                8
#define ILC_COLOR16                               16
#define ILC_COLOR24                               24
#define ILC_COLOR32                               32
#define ILC_COLORDDB                              254
#define ILC_MASK                                  1
#define ILC_PALETTE                               2048

// Tab Pages

#define TCS_SCROLLOPPOSITE                        0x0001  // assumes multiline tab
#define TCS_BOTTOM                                0x0002
#define TCS_RIGHT                                 0x0002
#define TCS_MULTISELECT                           0x0004  // allow multi-select in button mode
#define TCS_FLATBUTTONS                           0x0008
#define TCS_FORCEICONLEFT                         0x0010
#define TCS_FORCELABELLEFT                        0x0020
#define TCS_HOTTRACK                              0x0040
#define TCS_VERTICAL                              0x0080
#define TCS_TABS                                  0x0000
#define TCS_BUTTONS                               0x0100
#define TCS_SINGLELINE                            0x0000
#define TCS_MULTILINE                             0x0200
#define TCS_RIGHTJUSTIFY                          0x0000
#define TCS_FIXEDWIDTH                            0x0400
#define TCS_RAGGEDRIGHT                           0x0800
#define TCS_FOCUSONBUTTONDOWN                     0x1000
#define TCS_OWNERDRAWFIXED                        0x2000
#define TCS_TOOLTIPS                              0x4000
#define TCS_FOCUSNEVER                            0x8000

// Tree View Constants

#define WC_TREEVIEWA                              "SysTreeView32"
#define WC_TREEVIEWW                              "SysTreeView32"
#define WC_TREEVIEW                               WC_TREEVIEWW

#define TVS_HASBUTTONS                            1
#define TVS_HASLINES                              2
#define TVS_LINESATROOT                           4
#define TVS_EDITLABELS                            8
#define TVS_DISABLEDRAGDROP                       16
#define TVS_SHOWSELALWAYS                         32
#define TVS_RTLREADING                            64

#define TVS_NOTOOLTIPS                            128
#define TVS_CHECKBOXES                            256
#define TVS_TRACKSELECT                           512
#define TVS_SINGLEEXPAND                          1024
#define TVS_INFOTIP                               2048
#define TVS_FULLROWSELECT                         4096
#define TVS_NOSCROLL                              8192
#define TVS_NONEVENHEIGHT                         16384
#define TVS_NOHSCROLL                             32768  // TVS_NOSCROLL overrides this

#define TVIF_TEXT                                 1
#define TVIF_IMAGE                                2
#define TVIF_PARAM                                4
#define TVIF_STATE                                8
#define TVIF_HANDLE                               16
#define TVIF_SELECTEDIMAGE                        32
#define TVIF_CHILDREN                             64
#define TVIF_INTEGRAL                             128
#define TVIS_SELECTED                             2
#define TVIS_CUT                                  4
#define TVIS_DROPHILITED                          8
#define TVIS_BOLD                                 16
#define TVIS_EXPANDED                             32
#define TVIS_EXPANDEDONCE                         64
#define TVIS_EXPANDPARTIAL                        128

#define TVIS_OVERLAYMASK                          3840
#define TVIS_STATEIMAGEMASK                       61440
#define TVIS_USERMASK                             61440

#define I_CHILDRENCALLBACK                        -1

#define LPTV_ITEMW                                LPTVITEMW
#define LPTV_ITEMA                                LPTVITEMA
#define TV_ITEMW                                  TVITEMW
#define TV_ITEMA                                  TVITEMA

#define LPTV_ITEM                                 LPTVITEM
#define TV_ITEM                                   TVITEM

#define TVITEM                                    TVITEMW
#define LPTVITEM                                  LPTVITEMW

#define TVI_ROOT                                  -0x10000
#define TVI_FIRST                                 -0x0FFFF
#define TVI_LAST                                  -0x0FFFE
#define TVI_SORT                                  -0x0FFFD

#define LPTV_INSERTSTRUCTA                        LPTVINSERTSTRUCTA
#define LPTV_INSERTSTRUCTW                        LPTVINSERTSTRUCTW
#define TV_INSERTSTRUCTA                          TVINSERTSTRUCTA
#define TV_INSERTSTRUCTW                          TVINSERTSTRUCTW

#define TV_INSERTSTRUCT                           TVINSERTSTRUCT
#define LPTV_INSERTSTRUCT                         LPTVINSERTSTRUCT

#define TVM_INSERTITEMA                           ( TV_FIRST + 0 )
#define TVM_INSERTITEMW                           ( TV_FIRST + 50 )
#define TVM_INSERTITEM                            TVM_INSERTITEMW
#define TVM_DELETEITEM                            ( TV_FIRST + 1 )
#define TVM_EXPAND                                ( TV_FIRST + 2 )

#define TVE_COLLAPSE                              1
#define TVE_EXPAND                                2
#define TVE_TOGGLE                                3
#define TVE_EXPANDPARTIAL                         16384
#define TVE_COLLAPSERESET                         32768

#define TVM_GETITEMRECT                           ( TV_FIRST + 4 )
#define TVM_GETCOUNT                              ( TV_FIRST + 5 )
#define TVM_GETINDENT                             ( TV_FIRST + 6 )
#define TVM_SETINDENT                             ( TV_FIRST + 7 )
#define TVM_GETIMAGELIST                          ( TV_FIRST + 8 )
#define TVSIL_NORMAL                              0
#define TVSIL_STATE                               2

#define TVM_SETIMAGELIST                          ( TV_FIRST + 9 )
#define TVM_GETNEXTITEM                           ( TV_FIRST + 10 )

#define TVGN_ROOT                                 0
#define TVGN_NEXT                                 1
#define TVGN_PREVIOUS                             2
#define TVGN_PARENT                               3
#define TVGN_CHILD                                4
#define TVGN_FIRSTVISIBLE                         5
#define TVGN_NEXTVISIBLE                          6
#define TVGN_PREVIOUSVISIBLE                      7
#define TVGN_DROPHILITE                           8
#define TVGN_CARET                                9
#define TVGN_LASTVISIBLE                          10

#define TVM_SELECTITEM                            ( TV_FIRST + 11 )
#define TVM_GETITEMA                              ( TV_FIRST + 12 )
#define TVM_GETITEMW                              ( TV_FIRST + 62 )

#define TVM_GETITEM                               TVM_GETITEMW

#define TVM_SETITEMA                              ( TV_FIRST + 13 )
#define TVM_SETITEMW                              ( TV_FIRST + 63 )

#define TVM_SETITEM                               TVM_SETITEMW

#define TVM_EDITLABELA                            ( TV_FIRST + 14 )
#define TVM_EDITLABELW                            ( TV_FIRST + 65 )
#define TVM_EDITLABEL                             TVM_EDITLABELW
#define TVM_GETEDITCONTROL                        ( TV_FIRST + 15 )
#define TVM_GETVISIBLECOUNT                       ( TV_FIRST + 16 )
#define TVM_HITTEST                               ( TV_FIRST + 17 )

#define LPTV_HITTESTINFO                          LPTVHITTESTINFO
#define TV_HITTESTINFO                            TVHITTESTINFO

#define TVHT_NOWHERE                              1
#define TVHT_ONITEMICON                           2
#define TVHT_ONITEMLABEL                          4
#define TVHT_ONITEM                               ( TVHT_ONITEMICON + TVHT_ONITEMLABEL + TVHT_ONITEMSTATEICON )
#define TVHT_ONITEMINDENT                         8
#define TVHT_ONITEMBUTTON                         16
#define TVHT_ONITEMRIGHT                          32
#define TVHT_ONITEMSTATEICON                      64

#define TVHT_ABOVE                                256
#define TVHT_BELOW                                512
#define TVHT_TORIGHT                              1024
#define TVHT_TOLEFT                               2048

#define TVM_CREATEDRAGIMAGE                       ( TV_FIRST + 18 )
#define TVM_SORTCHILDREN                          ( TV_FIRST + 19 )
#define TVM_ENSUREVISIBLE                         ( TV_FIRST + 20 )
#define TVM_SORTCHILDRENCB                        ( TV_FIRST + 21 )
#define TVM_ENDEDITLABELNOW                       ( TV_FIRST + 22 )
#define TVM_GETISEARCHSTRINGA                     ( TV_FIRST + 23 )
#define TVM_GETISEARCHSTRINGW                     ( TV_FIRST + 64 )
#define TVM_GETISEARCHSTRING                      TVM_GETISEARCHSTRINGW

#define TVM_SETTOOLTIPS                           ( TV_FIRST + 24 )
#define TVM_GETTOOLTIPS                           ( TV_FIRST + 25 )
#define TVM_SETINSERTMARK                         ( TV_FIRST + 26 )
#define TVM_SETUNICODEFORMAT                      CCM_SETUNICODEFORMAT
#define TVM_GETUNICODEFORMAT                      CCM_GETUNICODEFORMAT
#define TVM_SETITEMHEIGHT                         ( TV_FIRST + 27 )
#define TVM_GETITEMHEIGHT                         ( TV_FIRST + 28 )
#define TVM_SETBKCOLOR                            ( TV_FIRST + 29 )
#define TVM_SETTEXTCOLOR                          ( TV_FIRST + 30 )
#define TVM_GETBKCOLOR                            ( TV_FIRST + 31 )
#define TVM_GETTEXTCOLOR                          ( TV_FIRST + 32 )
#define TVM_SETSCROLLTIME                         ( TV_FIRST + 33 )
#define TVM_GETSCROLLTIME                         ( TV_FIRST + 34 )
#define TVM_SETINSERTMARKCOLOR                    ( TV_FIRST + 37 )
#define TVM_GETINSERTMARKCOLOR                    ( TV_FIRST + 38 )
#define TVM_GETITEMSTATE                          ( TV_FIRST + 39 )
#define TVM_SETLINECOLOR                          ( TV_FIRST + 40 )
#define TVM_GETLINECOLOR                          ( TV_FIRST + 41 )

#define LPTV_SORTCB                               LPTVSORTCB
#define TV_SORTCB                                 TVSORTCB

#define LPNM_TREEVIEWA                            LPNMTREEVIEWA
#define LPNM_TREEVIEWW                            LPNMTREEVIEWW
#define NM_TREEVIEWW                              NMTREEVIEWW
#define NM_TREEVIEWA                              NMTREEVIEWA

#define LPNM_TREEVIEW                             LPNMTREEVIEW
#define NM_TREEVIEW                               NMTREEVIEW
#define NMTREEVIEW                                NMTREEVIEWW
#define LPNMTREEVIEW                              LPNMTREEVIEWW

#define TVN_SELCHANGINGA                          ( TVN_FIRST - 1 )
#define TVN_SELCHANGINGW                          ( TVN_FIRST - 50 )
#define TVN_SELCHANGEDA                           ( TVN_FIRST - 2 )
#define TVN_SELCHANGEDW                           ( TVN_FIRST - 51 )

#define TVC_UNKNOWN                               0
#define TVC_BYMOUSE                               1
#define TVC_BYKEYBOARD                            2

#define TVN_GETDISPINFOA                          ( TVN_FIRST - 3 )
#define TVN_GETDISPINFOW                          ( TVN_FIRST - 52 )
#define TVN_SETDISPINFOA                          ( TVN_FIRST - 4 )
#define TVN_SETDISPINFOW                          ( TVN_FIRST - 53 )

#define TVIF_DI_SETITEM                           4096

#define TV_DISPINFOA                              NMTVDISPINFOA
#define TV_DISPINFOW                              NMTVDISPINFOW
#define TV_DISPINFO                               NMTVDISPINFO

#define NMTVDISPINFO                              NMTVDISPINFOW
#define LPNMTVDISPINFO                            LPNMTVDISPINFOW

#define TVN_ITEMEXPANDINGA                        ( TVN_FIRST - 5 )
#define TVN_ITEMEXPANDINGW                        ( TVN_FIRST - 54 )
#define TVN_ITEMEXPANDEDA                         ( TVN_FIRST - 6 )
#define TVN_ITEMEXPANDEDW                         ( TVN_FIRST - 55 )
#define TVN_BEGINDRAGA                            ( TVN_FIRST - 7 )
#define TVN_BEGINDRAGW                            ( TVN_FIRST - 56 )
#define TVN_BEGINRDRAGA                           ( TVN_FIRST - 8 )
#define TVN_BEGINRDRAGW                           ( TVN_FIRST - 57 )
#define TVN_DELETEITEMA                           ( TVN_FIRST - 9 )
#define TVN_DELETEITEMW                           ( TVN_FIRST - 58 )
#define TVN_BEGINLABELEDITA                       ( TVN_FIRST - 10 )
#define TVN_BEGINLABELEDITW                       ( TVN_FIRST - 59 )
#define TVN_ENDLABELEDITA                         ( TVN_FIRST - 11 )
#define TVN_ENDLABELEDITW                         ( TVN_FIRST - 60 )
#define TVN_KEYDOWN                               ( TVN_FIRST - 12 )

#define TVN_GETINFOTIPA                           ( TVN_FIRST - 13 )
#define TVN_GETINFOTIPW                           ( TVN_FIRST - 14 )
#define TVN_SINGLEEXPAND                          ( TVN_FIRST - 15 )

#define TVNRET_DEFAULT                            0
#define TVNRET_SKIPOLD                            1
#define TVNRET_SKIPNEW                            2

#define TV_KEYDOWN                                NMTVKEYDOWN

#define TVN_SELCHANGING                           TVN_SELCHANGINGW
#define TVN_SELCHANGED                            TVN_SELCHANGEDW
#define TVN_GETDISPINFO                           TVN_GETDISPINFOW
#define TVN_SETDISPINFO                           TVN_SETDISPINFOW
#define TVN_ITEMEXPANDING                         TVN_ITEMEXPANDINGW
#define TVN_ITEMEXPANDED                          TVN_ITEMEXPANDEDW
#define TVN_BEGINDRAG                             TVN_BEGINDRAGW
#define TVN_BEGINRDRAG                            TVN_BEGINRDRAGW
#define TVN_DELETEITEM                            TVN_DELETEITEMW
#define TVN_BEGINLABELEDIT                        TVN_BEGINLABELEDITW
#define TVN_ENDLABELEDIT                          TVN_ENDLABELEDITW

#define TVN_GETINFOTIP                            TVN_GETINFOTIPW
#define NMTVGETINFOTIP                            NMTVGETINFOTIPW
#define LPNMTVGETINFOTIP                          LPNMTVGETINFOTIPW

#define TVCDRF_NOIMAGES                           65536

/* - */

#define R2_BLACK                                  1   // 0
#define R2_NOTMERGEPEN                            2   // DPon
#define R2_MASKNOTPEN                             3   // DPna
#define R2_NOTCOPYPEN                             4   // PN
#define R2_MASKPENNOT                             5   // PDna
#define R2_NOT                                    6   // Dn
#define R2_XORPEN                                 7   // DPx
#define R2_NOTMASKPEN                             8   // DPan
#define R2_MASKPEN                                9   // DPa
#define R2_NOTXORPEN                              10  // DPxn
#define R2_NOP                                    11  // D
#define R2_MERGENOTPEN                            12  // DPno
#define R2_COPYPEN                                13  // P
#define R2_MERGEPENNOT                            14  // PDno
#define R2_MERGEPEN                               15  // DPo
#define R2_WHITE                                  16  // 1
#define R2_LAST                                   16

/* - */

#define TOOLTIPS_CLASS                            "tooltips_class32"

#define TTS_ALWAYSTIP                             0x01
#define TTS_NOPREFIX                              0x02
#define TTS_NOANIMATE                             0x10
#define TTS_NOFADE                                0x20
#define TTS_BALLOON                               0x40

#define TTF_IDISHWND                              0x0001

#define TTF_CENTERTIP                             0x0002
#define TTF_RTLREADING                            0x0004
#define TTF_SUBCLASS                              0x0010
#define TTF_TRACK                                 0x0020
#define TTF_ABSOLUTE                              0x0080
#define TTF_TRANSPARENT                           0x0100
#define TTF_DI_SETITEM                            0x8000       // valid only on the TTN_NEEDTEXT callback

#define TTDT_AUTOMATIC                            0
#define TTDT_RESHOW                               1
#define TTDT_AUTOPOP                              2
#define TTDT_INITIAL                              3

#define TTI_NONE                                  0
#define TTI_INFO                                  1
#define TTI_WARNING                               2
#define TTI_ERROR                                 3

#define TTM_ACTIVATE                              ( WIN_WM_USER + 1 )
#define TTM_SETDELAYTIME                          ( WIN_WM_USER + 3 )
#define TTM_ADDTOOLA                              ( WIN_WM_USER + 4 )
#define TTM_ADDTOOLW                              ( WIN_WM_USER + 50 )
#define TTM_DELTOOLA                              ( WIN_WM_USER + 5 )
#define TTM_DELTOOLW                              ( WIN_WM_USER + 51 )
#define TTM_NEWTOOLRECTA                          ( WIN_WM_USER + 6 )
#define TTM_NEWTOOLRECTW                          ( WIN_WM_USER + 52 )
#define TTM_RELAYEVENT                            ( WIN_WM_USER + 7 )

#define TTM_GETTOOLINFOA                          ( WIN_WM_USER + 8 )
#define TTM_GETTOOLINFOW                          ( WIN_WM_USER + 53 )

#define TTM_SETTOOLINFOA                          ( WIN_WM_USER + 9 )
#define TTM_SETTOOLINFOW                          ( WIN_WM_USER + 54 )

#define TTM_HITTESTA                              ( WIN_WM_USER + 10 )
#define TTM_HITTESTW                              ( WIN_WM_USER + 55 )
#define TTM_GETTEXTA                              ( WIN_WM_USER + 11 )
#define TTM_GETTEXTW                              ( WIN_WM_USER + 56 )
#define TTM_UPDATETIPTEXTA                        ( WIN_WM_USER + 12 )
#define TTM_UPDATETIPTEXTW                        ( WIN_WM_USER + 57 )
#define TTM_GETTOOLCOUNT                          ( WIN_WM_USER + 13 )
#define TTM_ENUMTOOLSA                            ( WIN_WM_USER + 14 )
#define TTM_ENUMTOOLSW                            ( WIN_WM_USER + 58 )
#define TTM_GETCURRENTTOOLA                       ( WIN_WM_USER + 15 )
#define TTM_GETCURRENTTOOLW                       ( WIN_WM_USER + 59 )
#define TTM_WINDOWFROMPOINT                       ( WIN_WM_USER + 16 )
#define TTM_TRACKACTIVATE                         ( WIN_WM_USER + 17 )  // wParam = TRUE/FALSE start end  lparam = LPTOOLINFO
#define TTM_TRACKPOSITION                         ( WIN_WM_USER + 18 )  // lParam = dwPos
#define TTM_SETTIPBKCOLOR                         ( WIN_WM_USER + 19 )
#define TTM_SETTIPTEXTCOLOR                       ( WIN_WM_USER + 20 )
#define TTM_GETDELAYTIME                          ( WIN_WM_USER + 21 )
#define TTM_GETTIPBKCOLOR                         ( WIN_WM_USER + 22 )
#define TTM_GETTIPTEXTCOLOR                       ( WIN_WM_USER + 23 )
#define TTM_SETMAXTIPWIDTH                        ( WIN_WM_USER + 24 )
#define TTM_GETMAXTIPWIDTH                        ( WIN_WM_USER + 25 )
#define TTM_SETMARGIN                             ( WIN_WM_USER + 26 )  // lParam = lprc
#define TTM_GETMARGIN                             ( WIN_WM_USER + 27 )  // lParam = lprc
#define TTM_POP                                   ( WIN_WM_USER + 28 )
#define TTM_UPDATE                                ( WIN_WM_USER + 29 )
#define TTM_GETBUBBLESIZE                         ( WIN_WM_USER + 30 )
#define TTM_ADJUSTRECT                            ( WIN_WM_USER + 31 )
#define TTM_SETTITLEA                             ( WIN_WM_USER + 32 )  // wParam = TTI_*, lParam = char* szTitle
#define TTM_SETTITLEW                             ( WIN_WM_USER + 33 )  // wParam = TTI_*, lParam = wchar* szTitle

#define TTM_ADDTOOL                               TTM_ADDTOOLW
#define TTM_DELTOOL                               TTM_DELTOOLW
#define TTM_NEWTOOLRECT                           TTM_NEWTOOLRECTW
#define TTM_GETTOOLINFO                           TTM_GETTOOLINFOW
#define TTM_SETTOOLINFO                           TTM_SETTOOLINFOW
#define TTM_HITTEST                               TTM_HITTESTW
#define TTM_GETTEXT                               TTM_GETTEXTW
#define TTM_UPDATETIPTEXT                         TTM_UPDATETIPTEXTW
#define TTM_ENUMTOOLS                             TTM_ENUMTOOLSW
#define TTM_GETCURRENTTOOL                        TTM_GETCURRENTTOOLW
#define TTM_SETTITLE                              TTM_SETTITLEW

/* - */

#define CW_USEDEFAULT                             0x80000000

/* - */

#define PBM_SETRANGE                              ( WIN_WM_USER + 1 )
#define PBM_SETPOS                                ( WIN_WM_USER + 2 )
#define PBM_DELTAPOS                              ( WIN_WM_USER + 3 )
#define PBM_SETSTEP                               ( WIN_WM_USER + 4 )
#define PBM_STEPIT                                ( WIN_WM_USER + 5 )
#define PBM_SETRANGE32                            1030
#define PBM_GETRANGE                              1031
#define PBM_GETPOS                                1032
#define PBM_SETBARCOLOR                           1033
#define PBM_SETBKCOLOR                            CCM_SETBKCOLOR

#define PBS_SMOOTH                                1
#define PBS_VERTICAL                              4

#endif
