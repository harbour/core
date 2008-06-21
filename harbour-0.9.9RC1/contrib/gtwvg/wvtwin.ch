/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the WVT*Classes
 *
 * Copyright 2004 Pritpal Bedi <pritpal@vouchcac.com>
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
//-------------------------------------------------------------------//
//
//                         GTWVT Specific
//
//-------------------------------------------------------------------//
//
//   Wvt_DrawButton() constants
//
#define WVT_BTN_FORMAT_RAISED            0    // Default
#define WVT_BTN_FORMAT_RECESSED          1
#define WVT_BTN_FORMAT_FLAT              2
#define WVT_BTN_FORMAT_OUTLINED          3

#define WVT_BTN_IMAGE_TOP                0    // Default
#define WVT_BTN_IMAGE_LEFT               1
#define WVT_BTN_IMAGE_BOTTOM             2
#define WVT_BTN_IMAGE_RIGHT              3

//-------------------------------------------------------------------//
//
//   Wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat,;
//                 nAlign, nStyle, nThick, nColor )
//
//   nOrient
#define WVT_LINE_HORZ                    0    // Default
#define WVT_LINE_VERT                    1

//   nFormat
#define WVT_LINE_RAISED                  0    // Default
#define WVT_LINE_RECESSED                1
#define WVT_LINE_PLAIN                   2

//   nAlign
#define WVT_LINE_CENTER                  0    // Default
#define WVT_LINE_TOP                     1
#define WVT_LINE_BOTTOM                  2
#define WVT_LINE_LEFT                    3
#define WVT_LINE_RIGHT                   4

//   nStyle
#define WVT_LINE_SOLID                   0    // Default
#define WVT_LINE_DASH                    1
#define WVT_LINE_DOT                     2
#define WVT_LINE_DASHDOT                 3
#define WVT_LINE_DASHDOTDOT              4

//-------------------------------------------------------------------//
//
//              Standard Mouse Pointer Shape Constants
//
#define WVT_IDC_ARROW                    1
#define WVT_IDC_IBEAM                    2
#define WVT_IDC_WAIT                     3
#define WVT_IDC_CROSS                    4
#define WVT_IDC_UPARROW                  5
#define WVT_IDC_SIZE                     6
#define WVT_IDC_ICON                     7
#define WVT_IDC_SIZENWSE                 8
#define WVT_IDC_SIZENESW                 9
#define WVT_IDC_SIZEWE                  10
#define WVT_IDC_SIZENS                  11
#define WVT_IDC_SIZEALL                 12
#define WVT_IDC_NO                      13
#define WVT_IDC_HAND                    14
#define WVT_IDC_APPSTARTING             15
#define WVT_IDC_HELP                    16

//-------------------------------------------------------------------//

#define WVT_BLOCK_IMAGE                  1
#define WVT_BLOCK_BOX                    2
#define WVT_BLOCK_LABEL                  3
#define WVT_BLOCK_GRID_V                 4
#define WVT_BLOCK_GRID_H                 5
#define WVT_BLOCK_BUTTON                 6
#define WVT_BLOCK_GETS                   7
#define WVT_BLOCK_LINE                   8
#define WVT_BLOCK_STATUSBAR              9
#define WVT_BLOCK_TOOLBAR               10
#define WVT_BLOCK_STATIC                11

//-------------------------------------------------------------------//

#define DLG_OBJ_BROWSE                   1
#define DLG_OBJ_PICTURE                  2
#define DLG_OBJ_LINE                     3
#define DLG_OBJ_RECT                     4
#define DLG_OBJ_GETS                     5
#define DLG_OBJ_BUTTON                   6
#define DLG_OBJ_STATUSBAR                7
#define DLG_OBJ_PANEL                    8
#define DLG_OBJ_LABEL                    9
#define DLG_OBJ_STATIC                  10
#define DLG_OBJ_TOOLBAR                 11
#define DLG_OBJ_IMAGE                   12
#define DLG_OBJ_PUSHBUTTON              13
#define DLG_OBJ_CONSOLE                 14
#define DLG_OBJ_SCROLLBAR               15
#define DLG_OBJ_BANNER                  16
#define DLG_OBJ_TEXTBOX                 17
#define DLG_OBJ_PROGRESSBAR             18

//-------------------------------------------------------------------//

#define TLB_BUTTON_TYPE_IMAGE            0
#define TLB_BUTTON_TYPE_SEPARATOR        1
#define TLB_BUTTON_TYPE_TEXT             2

//-------------------------------------------------------------------//

#define WVT_STATIC_LINE                  1
#define WVT_STATIC_BOXRAISED             2
#define WVT_STATIC_BOXRECESSED           3
#define WVT_STATIC_BOXGROUP              4
#define WVT_STATIC_BOXGROUPRAISED        5
#define WVT_STATIC_RECTANGLE             6
#define WVT_STATIC_ROUNDRECT             7
#define WVT_STATIC_FOCUSRECT             8
#define WVT_STATIC_OUTLINE               9
#define WVT_STATIC_ELLIPSE              10
#define WVT_STATIC_SHADEDRECT           11

#define WVT_SCROLLBAR_VERT               1
#define WVT_SCROLLBAR_HORZ               2

#define WVT_SCROLLBUTTON_TOP             1
#define WVT_SCROLLBUTTON_LEFT            2
#define WVT_SCROLLBUTTON_BOTTOM          3
#define WVT_SCROLLBUTTON_RIGHT           4
#define WVT_SCROLL_THUMB                 5

//-------------------------------------------------------------------//
//
// wvtmenu defines  .  Peter Rees
//
#define WVT_MENU_TYPE                    1
#define WVT_MENU_IDENTIFIER              2
#define WVT_MENU_CAPTION                 3
#define WVT_MENU_ACTION                  4
#define WVT_MENU_MENUOBJ                 4

//-------------------------------------------------------------------//

#define ISBLOCK( x )      valtype( x ) == 'B'

#define RGB( nR,nG,nB )   ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )

//-------------------------------------------------------------------//
//
//                          Windows Specific
//
//-------------------------------------------------------------------//
//
//                   Menu Manipulation Constants
//
#define MF_INSERT                        0
#define MF_CHANGE                      128
#define MF_APPEND                      256
#define MF_DELETE                      512
#define MF_REMOVE                     4096

#define MF_BYCOMMAND                     0
#define MF_BYPOSITION                 1024

#define MF_SEPARATOR                  2048

#define MF_ENABLED                       0
#define MF_GRAYED                        1
#define MF_DISABLED                      2

#define MF_UNCHECKED                     0
#define MF_CHECKED                       8
#define MF_USECHECKBITMAPS             512

#define MF_STRING                        0
#define MF_BITMAP                        4
#define MF_OWNERDRAW                   256

#define MF_POPUP                        16
#define MF_MENUBARBREAK                 32
#define MF_MENUBREAK                    64

#define MF_UNHILITE                      0
#define MF_HILITE                      128

//-------------------------------------------------------------------//

#define TPM_LEFTBUTTON              0x0000
#define TPM_RIGHTBUTTON             0x0002

#define TPM_LEFTALIGN               0x0000
#define TPM_CENTERALIGN             0x0004
#define TPM_RIGHTALIGN              0x0008

#define TPM_TOPALIGN                0x0000
#define TPM_VCENTERALIGN            0x0010
#define TPM_BOTTOMALIGN             0x0020

#define TPM_HORIZONTAL              0x0000     /* Horz alignment matters more */
#define TPM_VERTICAL                0x0040     /* Vert alignment matters more */
#define TPM_NONOTIFY                0x0080     /* Don't send any notification msgs */
#define TPM_RETURNCMD               0x0100

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * ShowWindow() Commands
 */
#define SW_HIDE                          0
#define SW_SHOWNORMAL                    1
#define SW_NORMAL                        1
#define SW_SHOWMINIMIZED                 2
#define SW_SHOWMAXIMIZED                 3
#define SW_MAXIMIZE                      3
#define SW_SHOWNOACTIVATE                4
#define SW_SHOW                          5
#define SW_MINIMIZE                      6
#define SW_SHOWMINNOACTIVE               7
#define SW_SHOWNA                        8
#define SW_RESTORE                       9
#define SW_SHOWDEFAULT                  10
#define SW_FORCEMINIMIZE                11
#define SW_MAX                          11

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 *  Window Messages
 */
#define WM_USER                       1024

#define WM_CREATE                        1
#define WM_DESTROY                       2
#define WM_MOVE                          3
#define WM_SIZE                          5
#define WM_ACTIVATE                      6
#define WM_SETFOCUS                      7
#define WM_KILLFOCUS                     8
#define WM_ENABLE                       10
#define WM_SETREDRAW                    11
#define WM_SETTEXT                      12
#define WM_GETTEXT                      13
#define WM_GETTEXTLENGTH                14
#define WM_PAINT                        15
#define WM_CLOSE                        16   // 0x0010
#define WM_QUIT                         18   // 0x0012
#define WM_ERASEBKGND                   20   // 0x0014
#define WM_SYSCOLORCHANGE               21   // 0x0015
#define WM_SHOWWINDOW                   24   // 0x0018
#define WM_WININICHANGE                 26   // 0x001A
#define WM_DEVMODECHANGE                27   // 0x001B
#define WM_ACTIVATEAPP                  28   // 0x001C
#define WM_FONTCHANGE                   29   // 0x001D
#define WM_TIMECHANGE                   30   // 0x001E
#define WM_CANCELMODE                   31   // 0x001F
#define WM_SETCURSOR                    32   // 0x0020
#define WM_MOUSEACTIVATE                33   // 0x0021
#define WM_CHILDACTIVATE                34   // 0x0022
#define WM_QUEUESYNC                    35   // 0x0023
#define WM_GETMINMAXINFO                36

#define WM_PAINTICON                    38
#define WM_ICONERASEBKGND               39
#define WM_NEXTDLGCTL                   40
#define WM_SPOOLERSTATUS                42
#define WM_DRAWITEM                     43   // 0x002B
#define WM_MEASUREITEM                  44
#define WM_DELETEITEM                   45
#define WM_VKEYTOITEM                   46
#define WM_CHARTOITEM                   47
#define WM_SETFONT                      48
#define WM_GETFONT                      49
#define WM_SETHOTKEY                    50
#define WM_GETHOTKEY                    51
#define WM_QUERYDRAGICON                55
#define WM_COMPAREITEM                  57
#define WM_GETOBJECT                    61
#define WM_COMPACTING                   65   // 0x0041
#define WM_COMMNOTIFY                   68   // 0x0044  /* no longer suported */
#define WM_WINDOWPOSCHANGING            70   // 0x0046
#define WM_WINDOWPOSCHANGED             71   // 0x0047
#define WM_POWER                        72
#define WM_NOTIFY                       78   // 0x004E
#define WM_INPUTLANGCHANGEREQUEST       79   // 0x0050
#define WM_INPUTLANGCHANGE              80   // 0x0051
#define WM_TCARD                        81   // 0x0052
#define WM_HELP                         82   // 0x0053
#define WM_USERCHANGED                  83   // 0x0054
#define WM_NOTIFYFORMAT                 84   // 0x0055

#define WM_CONTEXTMENU                 123   // 0x007B
#define WM_STYLECHANGING               124   // 0x007C
#define WM_STYLECHANGED                125   // 0x007D
#define WM_DISPLAYCHANGE               126   // 0x007E
#define WM_GETICON                     127   // 0x007F
#define WM_SETICON                     128   // 0x0080

#define WM_NCCREATE                    129
#define WM_NCDESTROY                   130
#define WM_NCCALCSIZE                  131
#define WM_NCHITTEST                   132
#define WM_NCPAINT                     133
#define WM_NCACTIVATE                  134
#define WM_GETDLGCODE                  135

#define WM_NCMOUSEMOVE                 160
#define WM_NCLBUTTONDOWN               161
#define WM_NCLBUTTONUP                 162

#define WM_NCLBUTTONDBLCLK             163

#define WM_NCRBUTTONDOWN               164
#define WM_NCRBUTTONUP                 165
#define WM_NCRBUTTONDBLCLK             166
#define WM_NCMBUTTONDOWN               167
#define WM_NCMBUTTONUP                 168
#define WM_NCMBUTTONDBLCLK             169

#define WM_KEYDOWN                     256    // 0x0100
#define WM_KEYUP                       257    // 0x0101

#define WM_KEYFIRST                    256
#define WM_CHAR                        258
#define WM_DEADCHAR                    259
#define WM_SYSKEYDOWN                  260
#define WM_SYSKEYUP                    261
#define WM_SYSCHAR                     262
#define WM_SYSDEADCHAR                 263
#define WM_KEYLAST                     264

#define WM_INITDIALOG                  272
#define WM_COMMAND                     273
#define WM_SYSCOMMAND                  274
#define WM_TIMER                       275
#define WM_HSCROLL                     276
#define WM_VSCROLL                     277

#define WM_INITMENUPOPUP               279
#define WM_MENUSELECT                  287
#define WM_MENUCOMMAND                 294

#define WM_CTLCOLORMSGBOX              306
#define WM_CTLCOLOREDIT                307
#define WM_CTLCOLORLISTBOX             308
#define WM_CTLCOLORBTN                 309
#define WM_CTLCOLORDLG                 310
#define WM_CTLCOLORSCROLLBAR           311
#define WM_CTLCOLORSTATIC              312

#define WM_MOUSEFIRST                  512
#define WM_MOUSEMOVE                   512
#define WM_LBUTTONDOWN                 513
#define WM_LBUTTONUP                   514
#define WM_LBUTTONDBLCLK               515
#define WM_RBUTTONDOWN                 516
#define WM_RBUTTONUP                   517
#define WM_RBUTTONDBLCLK               518
#define WM_MBUTTONDOWN                 519
#define WM_MBUTTONUP                   520
#define WM_MBUTTONDBLCLK               521
#define WM_MOUSEWHEEL                  522
#define WM_XBUTTONDOWN                 523
#define WM_XBUTTONUP                   524
#define WM_XBUTTONDBLCLK               525

#define WM_PARENTNOTIFY                528
#define WM_ENTERMENULOOP               529
#define WM_EXITMENULOOP                530

#define WM_NEXTMENU                    531
#define WM_SIZING                      532
#define WM_CAPTURECHANGED              533
#define WM_MOVING                      534
#define WM_POWERBROADCAST              536

#define WM_MDICREATE                   544
#define WM_MDIDESTROY                  545
#define WM_MDIACTIVATE                 546
#define WM_MDIRESTORE                  547
#define WM_MDINEXT                     548
#define WM_MDIMAXIMIZE                 549
#define WM_MDITILE                     550
#define WM_MDICASCADE                  551
#define WM_MDIICONARRANGE              552
#define WM_MDIGETACTIVE                553

#define WM_MDISETMENU                  560
#define WM_ENTERSIZEMOVE               561
#define WM_EXITSIZEMOVE                562
#define WM_DROPFILES                   563
#define WM_MDIREFRESHMENU              564

#define WM_CUT                         768
#define WM_COPY                        769
#define WM_PASTE                       770
#define WM_CLEAR                       771
#define WM_UNDO                        772
#define WM_RENDERFORMAT                773
#define WM_RENDERALLFORMATS            774
#define WM_DESTROYCLIPBOARD            775
#define WM_DRAWCLIPBOARD               776
#define WM_PAINTCLIPBOARD              777
#define WM_VSCROLLCLIPBOARD            778
#define WM_SIZECLIPBOARD               779
#define WM_ASKCBFORMATNAME             780
#define WM_CHANGECBCHAIN               781
#define WM_HSCROLLCLIPBOARD            782
#define WM_QUERYNEWPALETTE             783
#define WM_PALETTEISCHANGING           784
#define WM_PALETTECHANGED              785
#define WM_HOTKEY                      786

//-------------------------------------------------------------------//
/*
 * Window Styles
 */
#define WS_OVERLAPPED                    0
#define WS_POPUP                2147483648    // 0x80000000L
#define WS_CHILD                1073741824    // 0x40000000L
#define WS_MINIMIZE              536870912    // 0x20000000L
#define WS_VISIBLE               268435456    // 0x10000000L
#define WS_DISABLED              134217728    // 0x08000000L
#define WS_BORDER                  8388608    // 0x00800000L
#define WS_DLGFRAME                4194304    // 0x00400000L
#define WS_VSCROLL                 2097152    // 0x00200000L
#define WS_HSCROLL                 1048576    // 0x00100000L
#define WS_SYSMENU                  524288    // 0x00080000L
#define WS_THICKFRAME               262144    // 0x00040000L
#define WS_GROUP                    131072    // 0x00020000L
#define WS_TABSTOP                   65536    // 0x00010000L
#define WS_CLIPSIBLINGS           67108864
#define WS_CLIPCHILDREN           33554432
#define WS_MAXIMIZE               16777216
#define WS_CAPTION                12582912    /* WS_BORDER | WS_DLGFRAME  */

#define WS_MINIMIZEBOX              131072
#define WS_MAXIMIZEBOX               65536

#define WS_TILED             WS_OVERLAPPED
#define WS_ICONIC              WS_MINIMIZE
#define WS_SIZEBOX           WS_THICKFRAME
#define WS_TILEDWINDOW WS_OVERLAPPEDWINDOW

#define WS_OVERLAPPEDWINDOW (WS_OVERLAPPED + ;
                                WS_CAPTION + ;
                                WS_SYSMENU + ;
                             WS_THICKFRAME + ;
                            WS_MINIMIZEBOX + ;
                            WS_MAXIMIZEBOX )

//-------------------------------------------------------------------//
/*
 * Extended Window Styles
 */
#define WS_EX_DLGMODALFRAME              1    // 0x00000001L
#define WS_EX_NOPARENTNOTIFY             4    // 0x00000004L
#define WS_EX_TOPMOST                    8    // 0x00000008L
#define WS_EX_ACCEPTFILES               16    // 0x00000010L
#define WS_EX_TRANSPARENT               32    // 0x00000020L
#define WS_EX_MDICHILD                  64    // 0x00000040L
#define WS_EX_TOOLWINDOW               128    // 0x00000080L
#define WS_EX_WINDOWEDGE               256    // 0x00000100L
#define WS_EX_CLIENTEDGE               512    // 0x00000200L
#define WS_EX_CONTEXTHELP             1024    // 0x00000400L

#define WS_EX_RIGHT                   4096    // 0x00001000L
#define WS_EX_LEFT                       0    // 0x00000000L
#define WS_EX_RTLREADING              8192    // 0x00002000L
#define WS_EX_LTRREADING                 0    // 0x00000000L
#define WS_EX_LEFTSCROLLBAR          16384    // 0x00004000L
#define WS_EX_RIGHTSCROLLBAR             0    // 0x00000000L

#define WS_EX_CONTROLPARENT          65536    // 0x00010000L
#define WS_EX_STATICEDGE            131072    // 0x00020000L
#define WS_EX_APPWINDOW             262144    // 0x00040000L

#define WS_EX_OVERLAPPEDWINDOW  (WS_EX_WINDOWEDGE + WS_EX_CLIENTEDGE)
#define WS_EX_PALETTEWINDOW     (WS_EX_WINDOWEDGE + WS_EX_TOOLWINDOW + WS_EX_TOPMOST)

#define WS_EX_LAYERED               524288    // 0x00080000
#define WS_EX_NOINHERITLAYOUT      1048576    // 0x00100000L // Disable inheritence of mirroring by children
#define WS_EX_LAYOUTRTL            4194304    // 0x00400000L // Right to left mirroring
#define WS_EX_NOACTIVATE         134217728    // 0x08000000L

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * Dialog styles
 */
#define DS_ABSALIGN                      1    // 0x01
#define DS_SYSMODAL                      2    // 0x02
#define DS_3DLOOK                        4    // 0x04
#define DS_FIXEDSYS                      8    // 0x08
#define DS_NOFAILCREATE                 16    // 0x10
#define DS_LOCALEDIT                    32    // 0x20
#define DS_SETFONT                      64    // 0x40
#define DS_MODALFRAME                  128    // 0x80
#define DS_NOIDLEMSG                   256    // 0x100
#define DS_CONTROL                    1024    // 0x400
#define DS_CENTER                     2048    // 0x800
#define DS_CENTERMOUSE                4096    // 0x1000
#define DS_CONTEXTHELP                8192    // 0x2000

//-------------------------------------------------------------------//
/*
 * Dialog Box Command IDs
 */
#define IDOK                             1
#define IDCANCEL                         2
#define IDABORT                          3
#define IDRETRY                          4
#define IDIGNORE                         5
#define IDYES                            6
#define IDNO                             7
#define IDTRYAGAIN                      10 // (WINVER >= 0x0500)
#define IDCONTINUE                      11 // (WINVER >= 0x0500)

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * Button Control Styles
 */
#define BS_PUSHBUTTON                    0    // 0x00000000L
#define BS_DEFPUSHBUTTON                 1    // 0x00000001L
#define BS_CHECKBOX                      2    // 0x00000002L
#define BS_AUTOCHECKBOX                  3    // 0x00000003L
#define BS_RADIOBUTTON                   4    // 0x00000004L
#define BS_3STATE                        5    // 0x00000005L
#define BS_AUTO3STATE                    6    // 0x00000006L
#define BS_GROUPBOX                      7    // 0x00000007L
#define BS_USERBUTTON                    8    // 0x00000008L
#define BS_AUTORADIOBUTTON               9    // 0x00000009L
#define BS_OWNERDRAW                    11    // 0x0000000BL
#define BS_LEFTTEXT                     32    // 0x00000020L

//-------------------------------------------------------------------//
/*
 * User Button Notification Codes
 */
#define BN_CLICKED                       0
#define BN_PAINT                         1
#define BN_HILITE                        2
#define BN_UNHILITE                      3
#define BN_DISABLE                       4
#define BN_DOUBLECLICKED                 5
#define BN_PUSHED                BN_HILITE
#define BN_UNPUSHED            BN_UNHILITE
#define BN_DBLCLK         BN_DOUBLECLICKED
#define BN_SETFOCUS                      6
#define BN_KILLFOCUS                     7

//-------------------------------------------------------------------//
/*
 * Button Control Messages
 */
#define BM_GETCHECK                    240
#define BM_SETCHECK                    241
#define BM_GETSTATE                    242
#define BM_SETSTATE                    243
#define BM_SETSTYLE                    244
#define BM_CLICK                       245
#define BM_GETIMAGE                    246
#define BM_SETIMAGE                    247

#define BST_UNCHECKED                    0
#define BST_CHECKED                      1
#define BST_INDETERMINATE                2
#define BST_PUSHED                       4
#define BST_FOCUS                        8

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * Edit Control Styles
 */
#define ES_LEFT                          0
#define ES_CENTER                        1
#define ES_RIGHT                         2
#define ES_MULTILINE                     4
#define ES_UPPERCASE                     8
#define ES_LOWERCASE                    16
#define ES_PASSWORD                     32
#define ES_AUTOVSCROLL                  64
#define ES_AUTOHSCROLL                 128
#define ES_NOHIDESEL                   256
#define ES_OEMCONVERT                 1024
#define ES_READONLY                   2048
#define ES_WANTRETURN                 4096
#define ES_NUMBER                     8192

//-------------------------------------------------------------------//
/*
 * Edit Control Notification Codes
 */
#define EN_SETFOCUS                    256    // 0x0100
#define EN_KILLFOCUS                   512    // 0x0200
#define EN_CHANGE                      768    // 0x0300
#define EN_UPDATE                     1024    // 0x0400
#define EN_ERRSPACE                   1280    // 0x0500
#define EN_MAXTEXT                    1281    // 0x0501
#define EN_HSCROLL                    1537    // 0x0601
#define EN_VSCROLL                    1538    // 0x0602

//----------------------------------------------------------------------//
/*
 * Edit Control Messages
 */
#define EM_GETSEL                      176
#define EM_SETSEL                      177
#define EM_GETRECT                     178
#define EM_SETRECT                     179
#define EM_SETRECTNP                   180
#define EM_SCROLL                      181
#define EM_LINESCROLL                  182
#define EM_SCROLLCARET                 183
#define EM_GETMODIFY                   184
#define EM_SETMODIFY                   185
#define EM_GETLINECOUNT                186
#define EM_LINEINDEX                   187
#define EM_SETHANDLE                   188
#define EM_GETHANDLE                   189
#define EM_GETTHUMB                    190
#define EM_LINELENGTH                  193
#define EM_REPLACESEL                  194
#define EM_GETLINE                     196
#define EM_LIMITTEXT                   197
#define EM_CANUNDO                     198
#define EM_UNDO                        199
#define EM_FMTLINES                    200
#define EM_LINEFROMCHAR                201
#define EM_SETTABSTOPS                 203
#define EM_SETPASSWORDCHAR             204
#define EM_EMPTYUNDOBUFFER             205
#define EM_GETFIRSTVISIBLELINE         206
#define EM_SETREADONLY                 207
#define EM_SETWORDBREAKPROC            208
#define EM_GETWORDBREAKPROC            209
#define EM_GETPASSWORDCHAR             210
#define EM_SETMARGINS                  211
#define EM_GETMARGINS                  212
#define EM_SETLIMITTEXT       EM_LIMITTEXT
#define EM_GETLIMITTEXT                213
#define EM_POSFROMCHAR                 214
#define EM_CHARFROMPOS                 215

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * Combo Box styles
 */
#define CBS_SIMPLE                       1    // 0x0001L
#define CBS_DROPDOWN                     2    // 0x0002L
#define CBS_DROPDOWNLIST                 3    // 0x0003L
#define CBS_OWNERDRAWFIXED              16    // 0x0010L
#define CBS_OWNERDRAWVARIABLE           32    // 0x0020L
#define CBS_AUTOHSCROLL                 64    // 0x0040L
#define CBS_OEMCONVERT                 128    // 0x0080L
#define CBS_SORT                       256    // 0x0100L
#define CBS_HASSTRINGS                 512    // 0x0200L
#define CBS_NOINTEGRALHEIGHT          1024    // 0x0400L
#define CBS_DISABLENOSCROLL           2048    // 0x0800L
#define CBS_UPPERCASE                 8192    // 0x2000L
#define CBS_LOWERCASE                16384    // 0x4000L

//-------------------------------------------------------------------//
/*
 * Combo Box Notification Codes
 */
#define CBN_ERRSPACE                  (-1)
#define CBN_SELCHANGE                    1
#define CBN_DBLCLK                       2
#define CBN_SETFOCUS                     3
#define CBN_KILLFOCUS                    4
#define CBN_EDITCHANGE                   5
#define CBN_EDITUPDATE                   6
#define CBN_DROPDOWN                     7
#define CBN_CLOSEUP                      8
#define CBN_SELENDOK                     9
#define CBN_SELENDCANCEL                10

//-------------------------------------------------------------------//
/*
 * Combo Box messages
 */
#define CB_GETEDITSEL                  320
#define CB_LIMITTEXT                   321
#define CB_SETEDITSEL                  322
#define CB_ADDSTRING                   323
#define CB_DELETESTRING                324
#define CB_DIR                         325
#define CB_GETCOUNT                    326
#define CB_GETCURSEL                   327
#define CB_GETLBTEXT                   328
#define CB_GETLBTEXTLEN                329
#define CB_INSERTSTRING                330
#define CB_RESETCONTENT                331
#define CB_FINDSTRING                  332
#define CB_SELECTSTRING                333
#define CB_SETCURSEL                   334
#define CB_SHOWDROPDOWN                335
#define CB_GETITEMDATA                 336
#define CB_SETITEMDATA                 337
#define CB_GETDROPPEDCONTROLRECT       338
#define CB_SETITEMHEIGHT               339
#define CB_GETITEMHEIGHT               340
#define CB_SETEXTENDEDUI               341
#define CB_GETEXTENDEDUI               342
#define CB_GETDROPPEDSTATE             343
#define CB_FINDSTRINGEXACT             344
#define CB_SETLOCALE                   345
#define CB_GETLOCALE                   346
#define CB_GETTOPINDEX                 347
#define CB_SETTOPINDEX                 348
#define CB_GETHORIZONTALEXTENT         349
#define CB_SETHORIZONTALEXTENT         350
#define CB_GETDROPPEDWIDTH             351
#define CB_SETDROPPEDWIDTH             352
#define CB_INITSTORAGE                 353

//-------------------------------------------------------------------//
/*
 * Combo Box return Values
 */
#define CB_OKAY                          0
#define CB_ERR                        (-1)
#define CB_ERRSPACE                   (-2)

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * Static Control Constants
 */
#define SS_LEFT                          0    // 0x00000000L
#define SS_CENTER                        1    // 0x00000001L
#define SS_RIGHT                         2    // 0x00000002L
#define SS_ICON                          3    // 0x00000003L
#define SS_BLACKRECT                     4    // 0x00000004L
#define SS_GRAYRECT                      5    // 0x00000005L
#define SS_WHITERECT                     6    // 0x00000006L
#define SS_BLACKFRAME                    7    // 0x00000007L
#define SS_GRAYFRAME                     8    // 0x00000008L
#define SS_WHITEFRAME                    9    // 0x00000009L
#define SS_USERITEM                     10    // 0x0000000AL
#define SS_SIMPLE                       11    // 0x0000000BL
#define SS_LEFTNOWORDWRAP               12    // 0x0000000CL
#define SS_OWNERDRAW                    13    // 0x0000000DL
#define SS_BITMAP                       14    // 0x0000000EL
#define SS_ENHMETAFILE                  15    // 0x0000000FL
#define SS_ETCHEDHORZ                   16    // 0x00000010L
#define SS_ETCHEDVERT                   17    // 0x00000011L
#define SS_ETCHEDFRAME                  18    // 0x00000012L
#define SS_TYPEMASK                     31    // 0x0000001FL
#define SS_NOPREFIX                    128    // Don't do "&" character translation
#define SS_NOTIFY                      256    // 0x00000100L
#define SS_CENTERIMAGE                 512    // 0x00000200L
#define SS_RIGHTJUST                  1024    // 0x00000400L
#define SS_REALSIZEIMAGE              2048    // 0x00000800L
#define SS_SUNKEN                     4096    // 0x00001000L

#define SS_ENDELLIPSIS               16384
#define SS_PATHELLIPSIS              32768
#define SS_WORDELLIPSIS              49152
#define SS_ELLIPSISMASK              49152

//-------------------------------------------------------------------//
/*
 * Static Control Mesages
 */
#define STM_SETICON                    368
#define STM_GETICON                    369
#define STM_SETIMAGE                   370
#define STM_GETIMAGE                   371

#define STN_CLICKED                      0
#define STN_DBLCLK                       1
#define STN_ENABLE                       2
#define STN_DISABLE                      3

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * Listbox messages
 */
#define LB_ADDSTRING                   384
#define LB_INSERTSTRING                385
#define LB_DELETESTRING                386
#define LB_SELITEMRANGEEX              387
#define LB_RESETCONTENT                388
#define LB_SETSEL                      389
#define LB_SETCURSEL                   390
#define LB_GETSEL                      391
#define LB_GETCURSEL                   392
#define LB_GETTEXT                     393
#define LB_GETTEXTLEN                  394
#define LB_GETCOUNT                    395
#define LB_SELECTSTRING                396
#define LB_DIR                         397
#define LB_GETTOPINDEX                 398
#define LB_FINDSTRING                  399
#define LB_GETSELCOUNT                 400
#define LB_GETSELITEMS                 401
#define LB_SETTABSTOPS                 402
#define LB_GETHORIZONTALEXTENT         403
#define LB_SETHORIZONTALEXTENT         404
#define LB_SETCOLUMNWIDTH              405
#define LB_ADDFILE                     406
#define LB_SETTOPINDEX                 407
#define LB_GETITEMRECT                 408
#define LB_GETITEMDATA                 409
#define LB_SETITEMDATA                 410
#define LB_SELITEMRANGE                411
#define LB_SETANCHORINDEX              412
#define LB_GETANCHORINDEX              413
#define LB_SETCARETINDEX               414
#define LB_GETCARETINDEX               415
#define LB_SETITEMHEIGHT               416
#define LB_GETITEMHEIGHT               417
#define LB_FINDSTRINGEXACT             418
#define LB_SETLOCALE                   421
#define LB_GETLOCALE                   422
#define LB_SETCOUNT                    423
#define LB_INITSTORAGE                 424
#define LB_ITEMFROMPOINT               425

//-------------------------------------------------------------------//
/*
 * Listbox Styles
 */
#define LBS_NOTIFY                       1
#define LBS_SORT                         2
#define LBS_NOREDRAW                     4
#define LBS_MULTIPLESEL                  8
#define LBS_OWNERDRAWFIXED              16
#define LBS_OWNERDRAWVARIABLE           32
#define LBS_HASSTRINGS                  64
#define LBS_USETABSTOPS                128
#define LBS_NOINTEGRALHEIGHT           256
#define LBS_MULTICOLUMN                512
#define LBS_WANTKEYBOARDINPUT         1024
#define LBS_EXTENDEDSEL               2048
#define LBS_DISABLENOSCROLL           4096
#define LBS_NODATA                    8192
#define LBS_NOSEL                    16384
#define LBS_STANDARD          ( LBS_NOTIFY + LBS_SORT + WS_VSCROLL + WS_BORDER )

//-------------------------------------------------------------------//
/*
 * Listbox Notification Codes
 */
#define LBN_ERRSPACE                  (-2)
#define LBN_SELCHANGE                    1
#define LBN_DBLCLK                       2
#define LBN_SELCANCEL                    3
#define LBN_SETFOCUS                     4
#define LBN_KILLFOCUS                    5

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * MessageBox() Flags
 */
#define MB_OK                            0
#define MB_OKCANCEL                      1
#define MB_ABORTRETRYIGNORE              2
#define MB_YESNOCANCEL                   3
#define MB_YESNO                         4
#define MB_RETRYCANCEL                   5
#define MB_CANCELTRYCONTINUE             6

#define MB_ICONHAND                     16
#define MB_ICONQUESTION                 32
#define MB_ICONEXCLAMATION              48
#define MB_ICONASTERISK                 64

#define MB_USERICON                    128
#define MB_ICONWARNING  MB_ICONEXCLAMATION
#define MB_ICONERROR           MB_ICONHAND

#define MB_ICONINFORMATION MB_ICONASTERISK
#define MB_ICONSTOP            MB_ICONHAND

#define MB_DEFBUTTON1                    0
#define MB_DEFBUTTON2                  256
#define MB_DEFBUTTON3                  512
#define MB_DEFBUTTON4                  768

#define MB_APPLMODAL                     0
#define MB_SYSTEMMODAL                4096
#define MB_TASKMODAL                  8192
#define MB_HELP                      16384    // Help Button

#define MB_NOFOCUS                   32768
#define MB_SETFOREGROUND             65536
#define MB_DEFAULT_DESKTOP_ONLY     131072

#define MB_TOPMOST                  262144
#define MB_RIGHT                    524288
#define MB_RTLREADING              1048576

#define MB_TYPEMASK                     15
#define MB_ICONMASK                    240
#define MB_DEFMASK                    3840
#define MB_MODEMASK                  12288
#define MB_MISCMASK                  49152

//-------------------------------------------------------------------//
/*
 * Stock Logical Objects
 */
#define WHITE_BRUSH                      0
#define LTGRAY_BRUSH                     1
#define GRAY_BRUSH                       2
#define DKGRAY_BRUSH                     3
#define BLACK_BRUSH                      4
#define NULL_BRUSH                       5
#define HOLLOW_BRUSH            NULL_BRUSH

#define WHITE_PEN                        6
#define BLACK_PEN                        7
#define NULL_PEN                         8

#define OEM_FIXED_FONT                  10
#define ANSI_FIXED_FONT                 11
#define ANSI_VAR_FONT                   12
#define SYSTEM_FONT                     13
#define DEVICE_DEFAULT_FONT             14
#define DEFAULT_PALETTE                 15
#define SYSTEM_FIXED_FONT               16
#define DEFAULT_GUI_FONT                17

//-------------------------------------------------------------------//
/*
 * WM_SETICON / WM_GETICON Type Codes
 */
#define ICON_SMALL                       0
#define ICON_BIG                         1

//-------------------------------------------------------------------//

#define IMAGE_BITMAP                     0
#define IMAGE_ICON                       1
#define IMAGE_CURSOR                     2
#define IMAGE_ENHMETAFILE                3

//-------------------------------------------------------------------//
/*
 * DrawText() Format Flags
 */
#define DT_TOP                           0
#define DT_LEFT                          0
#define DT_CENTER                        1
#define DT_RIGHT                         2
#define DT_VCENTER                       4
#define DT_BOTTOM                        8
#define DT_WORDBREAK                    16
#define DT_SINGLELINE                   32
#define DT_EXPANDTABS                   64
#define DT_TABSTOP                     128
#define DT_NOCLIP                      256
#define DT_EXTERNALLEADING             512
#define DT_CALCRECT                   1024
#define DT_NOPREFIX                   2048
#define DT_INTERNAL                   4096
#define DT_EDITCONTROL                8192
#define DT_PATH_ELLIPSIS             16384
#define DT_END_ELLIPSIS              32768
#define DT_MODIFYSTRING              65536
#define DT_RTLREADING               131072
#define DT_WORD_ELLIPSIS            262144
#define DT_NOFULLWIDTHCHARBREAK     524288
#define DT_HIDEPREFIX              1048576
#define DT_PREFIXONLY              2097152

//-------------------------------------------------------------------//
// Brush Styles
#define BS_SOLID                         0
#define BS_NULL                          1
#define BS_HOLLOW                  BS_NULL
#define BS_HATCHED                       2
#define BS_PATTERN                       3
#define BS_INDEXED                       4
#define BS_DIBPATTERN                    5
#define BS_DIBPATTERNPT                  6
#define BS_PATTERN8X8                    7
#define BS_DIBPATTERN8X8                 8
#define BS_MONOPATTERN                   9

// Hatch Styles
#define HS_HORIZONTAL                    0       // -----
#define HS_VERTICAL                      1       // |||||
#define HS_FDIAGONAL                     2       // \\\\\
#define HS_BDIAGONAL                     3       // /////
#define HS_CROSS                         4       // +++++
#define HS_DIAGCROSS                     5       // xxxxx

// Pen Styles
#define PS_SOLID                         0
#define PS_DASH                          1       // -------
#define PS_DOT                           2       // .......
#define PS_DASHDOT                       3       // _._._._
#define PS_DASHDOTDOT                    4       // _.._.._
#define PS_NULL                          5
#define PS_INSIDEFRAME                   6
#define PS_USERSTYLE                     7
#define PS_ALTERNATE                     8
#define PS_STYLE_MASK                   15

#define PS_ENDCAP_ROUND                  0
#define PS_ENDCAP_SQUARE               256
#define PS_ENDCAP_FLAT                 512
#define PS_ENDCAP_MASK                3840

#define PS_JOIN_ROUND                    0
#define PS_JOIN_BEVEL                 4096
#define PS_JOIN_MITER                 8192
#define PS_JOIN_MASK                 61440

#define PS_COSMETIC                      0
#define PS_GEOMETRIC                 65536
#define PS_TYPE_MASK                983040

//-------------------------------------------------------------------//
// font weight values
#define FW_DONTCARE                      0
#define FW_THIN                        100
#define FW_EXTRALIGHT                  200
#define FW_ULTRALIGHT                  200
#define FW_LIGHT                       300
#define FW_NORMAL                      400
#define FW_REGULAR                     400
#define FW_MEDIUM                      500
#define FW_SEMIBOLD                    600
#define FW_DEMIBOLD                    600
#define FW_BOLD                        700
#define FW_EXTRABOLD                   800
#define FW_ULTRABOLD                   800
#define FW_HEAVY                       900
#define FW_BLACK                       900

//-------------------------------------------------------------------//
// font quality values
#define DEFAULT_QUALITY                  0
#define DRAFT_QUALITY                    1
#define PROOF_QUALITY                    2
#define NONANTIALIASED_QUALITY           3 // (WINVER >= 0x0400)
#define ANTIALISED_QUALITY               4 // (WINVER >= 0x0400)

//-------------------------------------------------------------------//
// flags (CHOOSECOLOR structure)
#define CC_RGBINIT                       1 // 0x00000001
#define CC_FULLOPEN                      2 // 0x00000002
#define CC_PREVENTFULLOPEN               4 // 0x00000004
#define CC_SHOWHELP                      8 // 0x00000008
#define CC_ENABLEHOOK                   16 // 0x00000010
#define CC_ENABLETEMPLATE               32 // 0x00000020
#define CC_ENABLETEMPLATEHANDLE         64 // 0x00000040
#define CC_SOLIDCOLOR                  128 // 0x00000080 // WINVER >= 0x0400
#define CC_ANYCOLOR                    256 // 0x00000100 // WINVER >= 0x0400

//-------------------------------------------------------------------//
/*
 * Window field offsets for GetWindowLong()
 */
#define GWL_WNDPROC                    (-4)
#define GWL_HINSTANCE                  (-6)
#define GWL_HWNDPARENT                 (-8)
#define GWL_STYLE                     (-16)
#define GWL_EXSTYLE                   (-20)
#define GWL_USERDATA                  (-21)
#define GWL_ID                        (-12)

#define DWL_MSGRESULT                     0
#define DWL_DLGPROC                       4
#define DWL_USER                          8

//--------------------------------------------------------------------//
/*
 *  Virtual Key Codes
 */
#define VK_LBUTTON                        1
#define VK_RBUTTON                        2
#define VK_CANCEL                         3
#define VK_MBUTTON                        4
#define VK_BACK                           8
#define VK_TAB                            9
#define VK_CLEAR                         12
#define VK_RETURN                        13
#define VK_SHIFT                         16
#define VK_CONTROL                       17
#define VK_MENU                          18
#define VK_PAUSE                         19
#define VK_CAPITAL                       20
#define VK_ESCAPE                        27
#define VK_SPACE                         32
#define VK_PRIOR                         33
#define VK_NEXT                          34
#define VK_END                           35
#define VK_HOME                          36
#define VK_LEFT                          37
#define VK_UP                            38
#define VK_RIGHT                         39
#define VK_DOWN                          40
#define VK_SELECT                        41
#define VK_PRINT                         42
#define VK_EXECUTE                       43
#define VK_SNAPSHOT                      44
#define VK_INSERT                        45
#define VK_DELETE                        46
#define VK_HELP                          47
#define VK_NUMPAD0                       96
#define VK_NUMPAD1                       97
#define VK_NUMPAD2                       98
#define VK_NUMPAD3                       99
#define VK_NUMPAD4                      100
#define VK_NUMPAD5                      101
#define VK_NUMPAD6                      102
#define VK_NUMPAD7                      103
#define VK_NUMPAD8                      104
#define VK_NUMPAD9                      105
#define VK_MULTIPLY                     106
#define VK_ADD                          107
#define VK_SEPARATOR                    108
#define VK_SUBTRACT                     109
#define VK_DECIMAL                      110
#define VK_DIVIDE                       111
#define VK_F1                           112
#define VK_F2                           113
#define VK_F3                           114
#define VK_F4                           115
#define VK_F5                           116
#define VK_F6                           117
#define VK_F7                           118
#define VK_F8                           119
#define VK_F9                           120
#define VK_F10                          121
#define VK_F11                          122
#define VK_F12                          123
#define VK_F13                          124
#define VK_F14                          125
#define VK_F15                          126
#define VK_F16                          127
#define VK_F17                          128
#define VK_F18                          129
#define VK_F19                          130
#define VK_F20                          131
#define VK_F21                          132
#define VK_F22                          133
#define VK_F23                          134
#define VK_F24                          135
#define VK_NUMLOCK                      144
#define VK_SCROLL                       145

//----------------------------------------------------------------------//
/*
 *  File Open/Save Dialog Constants
 */
#define OFN_READONLY                      1
#define OFN_OVERWRITEPROMPT               2
#define OFN_HIDEREADONLY                  4
#define OFN_NOCHANGEDIR                   8
#define OFN_SHOWHELP                     16
#define OFN_ENABLEHOOK                   32
#define OFN_ENABLETEMPLATE               64
#define OFN_ENABLETEMPLATEHANDLE        128
#define OFN_NOVALIDATE                  256
#define OFN_ALLOWMULTISELECT            512
#define OFN_EXTENSIONDIFFERENT         1024
#define OFN_PATHMUSTEXIST              2048
#define OFN_FILEMUSTEXIST              4096
#define OFN_CREATEPROMPT               8192
#define OFN_SHAREAWARE                16384
#define OFN_NOREADONLYRETURN          32768
#define OFN_NOTESTFILECREATE          65536
#define OFN_NONETWORKBUTTON          131072
#define OFN_NOLONGNAMES              262144  // force no long names for 4.x modules
#define OFN_EXPLORER                 524288  // new look commdlg
#define OFN_NODEREFERENCELINKS      1048576
#define OFN_LONGNAMES               2097152  // force long names for 3.x modules
#define OFN_ENABLEINCLUDENOTIFY     4194304  // send include message to callback
#define OFN_ENABLESIZING            8388608
#define OFN_DONTADDTORECENT        33554432
#define OFN_FORCESHOWHIDDEN       268435456  // Show All files including System and hidden files

//----------------------------------------------------------------------//
