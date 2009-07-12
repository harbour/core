/*
 * $Id$
 */

#ifndef _XBP_CH

#if defined( __HB_OUTDEBUG__ )
   #if defined( __PLATFORM__WINDOWS ) .AND. defined( __HB_WINDEBUG__ )
      #xtranslate HB_OUTDEBUG( [<x>] ) => wapi_OutputDebugString( <x> )
   #else
      #xtranslate HB_OUTDEBUG( [<x>] ) => hb_TraceString( <x> )
   #endif
#else
   #xtranslate HB_OUTDEBUG( [<x>] ) => iif( .T.,, )
#endif

/*----------------------------------------------------------------------*/

#define QT_PTROF( oObj )                          ( oObj:pPtr )

#define HBXBP_EVENT_HANDLED                       0
#define HBXBP_EVENT_UNHANDLED                     1

#define XBTOQT_FROM_XB                            1
#define XBTOQT_FROM_QT                            2

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

#define XBP_PP_FGCLR                              2
#define XBP_PP_BGCLR                              4
#define XBP_PP_HILITE_FGCLR                       6
#define XBP_PP_HILITE_BGCLR                       8
#define XBP_PP_DISABLED_FGCLR                     10
#define XBP_PP_DISABLED_BGCLR                     12
#define XBP_PP_BORDER_CLR                         14
#define XBP_PP_COMPOUNDNAME                       15
#define XBP_PP_FONT                               16
#define XBP_PP_ACTIVE_CLR                         19
#define XBP_PP_INACTIVE_CLR                       21
#define XBP_PP_ACTIVETEXT_FGCLR                   23
#define XBP_PP_ACTIVETEXT_BGCLR                   25
#define XBP_PP_INACTIVETEXT_FGCLR                 27
#define XBP_PP_INACTIVETEXT_BGCLR                 29
#define XBP_PP_CAPTION                            50
#define XBP_PP_ALIGNMENT                          52
#define XBP_PP_ORIGIN                             300

#define XBP_PP_MENU_FGCLR                         32
#define XBP_PP_MENU_BGCLR                         34
#define XBP_PP_MENU_HILITE_FGCLR                  36
#define XBP_PP_MENU_HILITE_BGCLR                  38
#define XBP_PP_MENU_DISABLED_FGCLR                40
#define XBP_PP_MENU_DISABLED_BGCLR                42


#define XBPSYSCLR_BUTTONTEXT                      ( -58 )     //
#define XBPSYSCLR_INFOBACKGROUND                  ( -57 )     //
#define XBPSYSCLR_INFOTEXT                        ( -56 )     //
#define XBPSYSCLR_3DHIGHLIGHT                     ( -55 )     // QPalette::Light
#define XBPSYSCLR_3DLIGHT                         ( -54 )     // QPalette::MidLight
#define XBPSYSCLR_3DFACE                          ( -53 )     // QPalette::Button
#define XBPSYSCLR_3DSHADOW                        ( -52 )     // QPalette::Mid
#define XBPSYSCLR_3DDARKSHADOW                    ( -51 )     // QPalette::Shadow
#define XBPSYSCLR_SHADOWHILITEBGND                ( -50 )     //
#define XBPSYSCLR_SHADOWHILITEFGND                ( -49 )     //
#define XBPSYSCLR_SHADOWTEXT                      ( -48 )     //
#define XBPSYSCLR_ENTRYFIELD                      ( -47 )     // QPalette_Base
#define XBPSYSCLR_MENUDISABLEDTEXT                ( -46 )     //
#define XBPSYSCLR_MENUHILITE                      ( -45 )     //
#define XBPSYSCLR_MENUHILITEBGND                  ( -44 )     //
#define XBPSYSCLR_PAGEBACKGROUND                  ( -43 )     // QPalette_Base
#define XBPSYSCLR_FIELDBACKGROUND                 ( -42 )     // QPalette_Base
#define XBPSYSCLR_BUTTONLIGHT                     ( -41 )     // QPalette::Light
#define XBPSYSCLR_BUTTONMIDDLE                    ( -40 )     // QPalette::MidLight
#define XBPSYSCLR_BUTTONDARK                      ( -39 )     // QPalette::Mid
#define XBPSYSCLR_BUTTONDEFAULT                   ( -38 )     // QPalette::Button
#define XBPSYSCLR_TITLEBOTTOM                     ( -37 )     //
#define XBPSYSCLR_SHADOW                          ( -36 )     // QPalette::Shadow
#define XBPSYSCLR_ICONTEXT                        ( -35 )     //
#define XBPSYSCLR_DIALOGBACKGROUND                ( -34 )     // QPalette::Button
#define XBPSYSCLR_HILITEFOREGROUND                ( -33 )     // QPalette::HighlightedText
#define XBPSYSCLR_HILITEBACKGROUND                ( -32 )     // QPalette::Highlight
#define XBPSYSCLR_INACTIVETITLETEXTBGND           ( -31 )     //
#define XBPSYSCLR_ACTIVETITLETEXTBGND             ( -30 )     //
#define XBPSYSCLR_INACTIVETITLETEXT               ( -29 )     //
#define XBPSYSCLR_ACTIVETITLETEXT                 ( -28 )     //
#define XBPSYSCLR_OUTPUTTEXT                      ( -27 )     //
#define XBPSYSCLR_WINDOWSTATICTEXT                ( -26 )     //
#define XBPSYSCLR_SCROLLBAR                       ( -25 )     //
#define XBPSYSCLR_BACKGROUND                      ( -24 )     // QPalette::Window
#define XBPSYSCLR_ACTIVETITLE                     ( -23 )     //
#define XBPSYSCLR_INACTIVETITLE                   ( -22 )     //
#define XBPSYSCLR_MENU                            ( -21 )     //
#define XBPSYSCLR_WINDOW                          ( -20 )     // QPalette_Window
#define XBPSYSCLR_WINDOWFRAME                     ( -19 )     //
#define XBPSYSCLR_MENUTEXT                        ( -18 )     //
#define XBPSYSCLR_WINDOWTEXT                      ( -17 )     // QPalette_WindowText
#define XBPSYSCLR_TITLETEXT                       ( -16 )     //
#define XBPSYSCLR_ACTIVEBORDER                    ( -15 )     //
#define XBPSYSCLR_INACTIVEBORDER                  ( -14 )     //
#define XBPSYSCLR_APPWORKSPACE                    ( -13 )     //
#define XBPSYSCLR_HELPBACKGROUND                  ( -12 )     // QPalette_ToolTipBase
#define XBPSYSCLR_HELPTEXT                        ( -11 )     // QPalette_ToolTipText
#define XBPSYSCLR_HELPHILITE                      ( -10 )     // QPalette::Highlight

#define XBPSYSCLR_TRANSPARENT                     ( -255 )    // Qt_transparent

#define XBP_DISP_MODELESS                         1
#define XBP_DISP_APPMODAL                         2
#define XBP_DISP_SYSMODAL                         3

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

#define XBPCOMBO_SIMPLE                           1
#define XBPCOMBO_DROPDOWN                         2
#define XBPCOMBO_DROPDOWNLIST                     3

#define XBP_MK_LBUTTON                            1
#define XBP_MK_RBUTTON                            2
#define XBP_MK_SHIFT                              4
#define XBP_MK_CONTROL                            8
#define XBP_MK_MBUTTON                            16

#define XBPWINDOW_POINTERTYPE_POINTER             1
#define XBPWINDOW_POINTERTYPE_SYSPOINTER          2
#define XBPWINDOW_POINTERTYPE_ICON                3

#define XBPSTATIC_SYSICON_DEFAULT                 0
#define XBPSTATIC_SYSICON_ARROW                   1
#define XBPSTATIC_SYSICON_TEXT                    2
#define XBPSTATIC_SYSICON_WAIT                    3
#define XBPSTATIC_SYSICON_SIZE                    4
#define XBPSTATIC_SYSICON_MOVE                    5
#define XBPSTATIC_SYSICON_SIZENWSE                6
#define XBPSTATIC_SYSICON_SIZENESW                7
#define XBPSTATIC_SYSICON_SIZEWE                  8
#define XBPSTATIC_SYSICON_SIZENS                  9

#define XBPSTATIC_SYSICON_APPICON                 10
#define XBPSTATIC_SYSICON_ICONINFORMATION         11
#define XBPSTATIC_SYSICON_ICONQUESTION            12
#define XBPSTATIC_SYSICON_ICONERROR               13
#define XBPSTATIC_SYSICON_ICONWARNING             14
#define XBPSTATIC_SYSICON_ILLEGAL                 18
#define XBPSTATIC_SYSICON_FILE                    19
#define XBPSTATIC_SYSICON_FOLDER                  20
#define XBPSTATIC_SYSICON_MULTFILE                21
#define XBPSTATIC_SYSICON_PROGRAM                 22
#define XBPSTATIC_SYSICON_DISPLAY_PTRS            22
#define XBPSTATIC_SYSICON_PENFIRST                23
#define XBPSTATIC_SYSICON_PENLAST                 39


#define XBP_REJECT                                0
#define XBP_ALLOW                                 1

#define XBPBMP_FORMAT_WIN2X                       256
#define XBPBMP_FORMAT_WIN3X                       512
#define XBPBMP_FORMAT_OS21X                       XBPBMP_FORMAT_WIN2X
#define XBPBMP_FORMAT_OS22X                       1024
#define XBPBMP_FORMAT_GIF                         4096
#define XBPBMP_FORMAT_JPG                         8192
#define XBPBMP_FORMAT_PNG                         16384

#define XBPBMP_FORMAT_DEFAULT                     XBPBMP_FORMAT_WIN3X

#define XBPBMP_DEF_COMPRESSION                    -1


#define  XBPPDLG_PRINT_MARK                       1
#define  XBPPDLG_PRINT_ALLPAGES                   2
#define  XBPPDLG_PRINT_PAGERANGE                  3

#define  XBPPDLG_MODE_DRIVER                      1
#define  XBPPDLG_MODE_APPLICATION                 2

#define XBP_SCROLLBAR_NONE                        0
#define XBP_SCROLLBAR_HORIZ                       1
#define XBP_SCROLLBAR_VERT                        2

#define XBP_APPEARANCE_FLAT                       0
#define XBP_APPEARANCE_3D                         1

#define XBPRTF_ALIGN_LEFT                         0
#define XBPRTF_ALIGN_RIGHT                        1
#define XBPRTF_ALIGN_CENTER                       2

#define XBPRTF_MATCH_WHOLEWORD                    2
#define XBPRTF_MATCH_CASE                         4
#define XBPRTF_MATCH_NOHILIGHT                    8


/*----------------------------------------------------------------------*/
#define _XBP_CH
#endif
