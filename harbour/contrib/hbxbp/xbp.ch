/*
 * $Id$
 */

#ifndef _XBP_CH

/*----------------------------------------------------------------------*/

#include "hbqtcore.ch"
#include "hbqtgui.ch"

#include "hbtrace.ch"

/*----------------------------------------------------------------------*/

#define HBXBP_EVENT_HANDLED                       0
#define HBXBP_EVENT_UNHANDLED                     1

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
#define XBPLISTBOX_MM_MULTIPLE                    2
#define XBPLISTBOX_MM_EXTENDED                    3

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


#define XBPPDLG_PRINT_MARK                        1
#define XBPPDLG_PRINT_ALLPAGES                    2
#define XBPPDLG_PRINT_PAGERANGE                   3

#define XBPPDLG_MODE_DRIVER                       1
#define XBPPDLG_MODE_APPLICATION                  2

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


#define XBP_PP_CGRP_ROWWIDTH                      100
#define XBP_PP_CGRP_ROWHEIGHT                     101
#define XBP_PP_CGRP_CELLWIDTH                     102
#define XBP_PP_CGRP_CELLHEIGHT                    103
#define XBP_PP_CGRP_CELLALIGNMENT                 104
#define XBP_PP_CGRP_HSEPARATOR                    105
#define XBP_PP_CGRP_VSEPARATOR                    106
#define XBP_PP_CGRP_FRAMELAYOUT                   107
#define XBP_PP_CGRP_HILITEFRAMELAYOUT             108
#define XBP_PP_CGRP_CELLFRAMELAYOUT               109

#define XBPCOL_TYPE_ICON                          1
#define XBPCOL_TYPE_BITMAP                        2
#define XBPCOL_TYPE_SYSICON                       3
#define XBPCOL_TYPE_TEXT                          4
#define XBPCOL_TYPE_FILEICON                      5
#define XBPCOL_TYPE_FILEMINIICON                  6
#define XBPCOL_TYPE_MULTILINETEXT                 7

#define XBPLINE_NONE                              1
#define XBPLINE_NORMAL                            2
#define XBPLINE_DASHED                            4
#define XBPLINE_DOTTED                            8

#define XBPCOL_SEP_NONE                           XBPLINE_NONE
#define XBPCOL_SEP_LINE                           XBPLINE_NORMAL
#define XBPCOL_SEP_DASHED                         XBPLINE_DASHED
#define XBPCOL_SEP_DOTTED                         XBPLINE_DOTTED

#define XBPCOL_DA_FGCLR                           1
#define XBPCOL_DA_BGCLR                           2
#define XBPCOL_DA_HILITE_FGCLR                    3
#define XBPCOL_DA_HILITE_BGCLR                    4
#define XBPCOL_DA_COMPOUNDNAME                    5
#define XBPCOL_DA_ROWWIDTH                        6
#define XBPCOL_DA_ROWHEIGHT                       7
#define XBPCOL_DA_CELLWIDTH                       8
#define XBPCOL_DA_CELLHEIGHT                      9
#define XBPCOL_DA_CELLALIGNMENT                   10
#define XBPCOL_DA_ROWSEPARATOR                    11
#define XBPCOL_DA_COLSEPARATOR                    12
#define XBPCOL_DA_FRAMELAYOUT                     13
#define XBPCOL_DA_CELLFRAMELAYOUT                 14
#define XBPCOL_DA_HILITEFRAMELAYOUT               15
#define XBPCOL_DA_CHARWIDTH                       16

#define XBPCOL_DA_COUNT                           16

#define XBPCOL_HFA_FGCLR                          1
#define XBPCOL_HFA_BGCLR                          2
#define XBPCOL_HFA_COMPOUNDNAME                   3
#define XBPCOL_HFA_FRAMELAYOUT                    4
#define XBPCOL_HFA_ALIGNMENT                      5
#define XBPCOL_HFA_HEIGHT                         6
#define XBPCOL_HFA_CAPTION                        7

#define XBPCOL_HFA_COUNT                          7

#define XBP_PP_COL_DA_BASE                        100
#define XBP_PP_COL_DA_FGCLR                       ( XBP_PP_COL_DA_BASE + XBPCOL_DA_FGCLR             )
#define XBP_PP_COL_DA_BGCLR                       ( XBP_PP_COL_DA_BASE + XBPCOL_DA_BGCLR             )
#define XBP_PP_COL_DA_HILITE_FGCLR                ( XBP_PP_COL_DA_BASE + XBPCOL_DA_HILITE_FGCLR      )
#define XBP_PP_COL_DA_HILITE_BGCLR                ( XBP_PP_COL_DA_BASE + XBPCOL_DA_HILITE_BGCLR      )
#define XBP_PP_COL_DA_COMPOUNDNAME                ( XBP_PP_COL_DA_BASE + XBPCOL_DA_COMPOUNDNAME      )
#define XBP_PP_COL_DA_ROWWIDTH                    ( XBP_PP_COL_DA_BASE + XBPCOL_DA_ROWWIDTH          )
#define XBP_PP_COL_DA_ROWHEIGHT                   ( XBP_PP_COL_DA_BASE + XBPCOL_DA_ROWHEIGHT         )
#define XBP_PP_COL_DA_CELLWIDTH                   ( XBP_PP_COL_DA_BASE + XBPCOL_DA_CELLWIDTH         )
#define XBP_PP_COL_DA_CELLHEIGHT                  ( XBP_PP_COL_DA_BASE + XBPCOL_DA_CELLHEIGHT        )
#define XBP_PP_COL_DA_CELLALIGNMENT               ( XBP_PP_COL_DA_BASE + XBPCOL_DA_CELLALIGNMENT     )
#define XBP_PP_COL_DA_ROWSEPARATOR                ( XBP_PP_COL_DA_BASE + XBPCOL_DA_ROWSEPARATOR      )
#define XBP_PP_COL_DA_COLSEPARATOR                ( XBP_PP_COL_DA_BASE + XBPCOL_DA_COLSEPARATOR      )
#define XBP_PP_COL_DA_FRAMELAYOUT                 ( XBP_PP_COL_DA_BASE + XBPCOL_DA_FRAMELAYOUT       )
#define XBP_PP_COL_DA_CELLFRAMELAYOUT             ( XBP_PP_COL_DA_BASE + XBPCOL_DA_CELLFRAMELAYOUT   )
#define XBP_PP_COL_DA_HILITEFRAMELAYOUT           ( XBP_PP_COL_DA_BASE + XBPCOL_DA_HILITEFRAMELAYOUT )
#define XBP_PP_COL_DA_CHARWIDTH                   ( XBP_PP_COL_DA_BASE + XBPCOL_DA_CHARWIDTH         )

#define XBP_PP_COL_HA_BASE                        ( XBP_PP_COL_DA_BASE + XBPCOL_DA_COUNT             )
#define XBP_PP_COL_HA_FGCLR                       ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_FGCLR            )
#define XBP_PP_COL_HA_BGCLR                       ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_BGCLR            )
#define XBP_PP_COL_HA_COMPOUNDNAME                ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_COMPOUNDNAME     )
#define XBP_PP_COL_HA_FRAMELAYOUT                 ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_FRAMELAYOUT      )
#define XBP_PP_COL_HA_ALIGNMENT                   ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_ALIGNMENT        )
#define XBP_PP_COL_HA_HEIGHT                      ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_HEIGHT           )
#define XBP_PP_COL_HA_CAPTION                     ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_CAPTION          )

#define XBP_PP_COL_FA_BASE                        ( XBP_PP_COL_HA_BASE + XBPCOL_HFA_COUNT            )
#define XBP_PP_COL_FA_FGCLR                       ( XBP_PP_COL_FA_BASE + XBPCOL_HFA_FGCLR            )
#define XBP_PP_COL_FA_BGCLR                       ( XBP_PP_COL_FA_BASE + XBPCOL_HFA_BGCLR            )
#define XBP_PP_COL_FA_COMPOUNDNAME                ( XBP_PP_COL_FA_BASE + XBPCOL_HFA_COMPOUNDNAME     )
#define XBP_PP_COL_FA_FRAMELAYOUT                 ( XBP_PP_COL_FA_BASE + XBPCOL_HFA_FRAMELAYOUT      )
#define XBP_PP_COL_FA_ALIGNMENT                   ( XBP_PP_COL_FA_BASE + XBPCOL_HFA_ALIGNMENT        )
#define XBP_PP_COL_FA_HEIGHT                      ( XBP_PP_COL_FA_BASE + XBPCOL_HFA_HEIGHT           )
#define XBP_PP_COL_FA_CAPTION                     ( XBP_PP_COL_FA_BASE + XBPCOL_HFA_CAPTION          )

#define XBPBRW_CURSOR_NONE                        1
#define XBPBRW_CURSOR_CELL                        2
#define XBPBRW_CURSOR_ROW                         3

#define XBPBRW_Navigate_NextLine                  1
#define XBPBRW_Navigate_PrevLine                  2
#define XBPBRW_Navigate_NextPage                  3
#define XBPBRW_Navigate_PrevPage                  4
#define XBPBRW_Navigate_GoTop                     5
#define XBPBRW_Navigate_GoBottom                  6
#define XBPBRW_Navigate_Skip                      7    // MsgPar2 == <nSkip>
#define XBPBRW_Navigate_NextCol                   8
#define XBPBRW_Navigate_PrevCol                   9
#define XBPBRW_Navigate_FirstCol                  10
#define XBPBRW_Navigate_LastCol                   11
#define XBPBRW_Navigate_GoPos                     12  // MsgPar2 == <nNewPercentPos>
#define XBPBRW_Navigate_SkipCols                  13  // MsgPar2 == <nColsToSkip>
#define XBPBRW_Navigate_GotoItem                  14  // MsgPar2 == <aRowCol>
#define XBPBRW_Navigate_GotoRecord                15  // MsgPar2 == <nRecordId>

#define XBPBRW_Pan_Left                           1
#define XBPBRW_Pan_Right                          2
#define XBPBRW_Pan_FirstCol                       3
#define XBPBRW_Pan_LastCol                        4
#define XBPBRW_Pan_Track                          5

#define XBPDLG_NO_BORDER                          1
#define XBPDLG_SIZEBORDER                         2
#define XBPDLG_THINBORDER                         3
#define XBPDLG_DLGBORDER                          4
#define XBPDLG_RAISEDBORDERTHICK                  5
#define XBPDLG_RAISEDBORDERTHIN                   6
#define XBPDLG_RECESSEDBORDERTHICK                7
#define XBPDLG_RECESSEDBORDERTHIN                 8
#define XBPDLG_RAISEDBORDERTHICK_FIXED            9
#define XBPDLG_RAISEDBORDERTHIN_FIXED             10
#define XBPDLG_RECESSEDBORDERTHICK_FIXED          11
#define XBPDLG_RECESSEDBORDERTHIN_FIXED           12

#define XBPDLG_ORIGIN_OWNER                       1
#define XBPDLG_ORIGIN_SCREEN                      2
#define XBPDLG_ORIGIN_MOUSE                       3

#define XBP_ORIGIN_TOPLEFT                        1
#define XBP_ORIGIN_BOTTOMLEFT                     2

#define XBPCLPBRD_TEXT                            1
#define XBPCLPBRD_BITMAP                          2

#define APPTYPE_PM                                3

/*----------------------------------------------------------------------*/
/*                      Harbour Parts Constants                         */
/*----------------------------------------------------------------------*/

#define HBPLAYOUT_TYPE_HORZBOX                    1
#define HBPLAYOUT_TYPE_VERTBOX                    2
#define HBPLAYOUT_TYPE_GRID                       3
#define HBPLAYOUT_TYPE_FORM                       4

#define HBPLAYOUT_TYPE_MAX                        4

/*----------------------------------------------------------------------*/
/*                             HbpMdiArea()                             */
/*----------------------------------------------------------------------*/

#define HBPMDI_MODE_SUBWINDOWS                    0
#define HBPMDI_MODE_TABBED                        1

#define HBPMDI_STYLE_ORGANIZED                    0
#define HBPMDI_STYLE_CASCADED                     1
#define HBPMDI_STYLE_TILED                        2
#define HBPMDI_STYLE_MAXIMIZED                    3
#define HBPMDI_STYLE_TILEDVERT                    4
#define HBPMDI_STYLE_TILEDHORZ                    5

/*----------------------------------------------------------------------*/

#define XBP_STAT_INIT                             0
#define XBP_STAT_CREATE                           1
#define XBP_STAT_FAILURE                          2

/*----------------------------------------------------------------------*/

#define XBP_BEGIN_GROUP                           1
#define XBP_WITHIN_GROUP                          2
#define XBP_END_GROUP                             3
#define XBP_NO_GROUP                              4

/*----------------------------------------------------------------------*/

#define XBPMB_OK                                  0
#define XBPMB_OKCANCEL                            1
#define XBPMB_RETRYCANCEL                         5
#define XBPMB_ABORTRETRYIGNORE                    2
#define XBPMB_YESNO                               4
#define XBPMB_YESNOCANCEL                         3
#define XBPMB_CANCEL                              0
#define XBPMB_ENTER                               0
#define XBPMB_ENTERCANCEL                         1
#define XBPMB_HELP                                16384

// Defines for the style
#define XBPMB_NOICON                              0
#define XBPMB_QUESTION                            32
#define XBPMB_WARNING                             48
#define XBPMB_INFORMATION                         64
#define XBPMB_CRITICAL                            16

#define XBPMB_APPMODAL                            (0 + 65536)
#define XBPMB_SYSMODAL                            (4096 + 65536)
#define XBPMB_MOVEABLE                            65536

// Defbutton
#define XBPMB_DEFBUTTON1                          0
#define XBPMB_DEFBUTTON2                          256
#define XBPMB_DEFBUTTON3                          512

// Confirm box return codes
#define XBPMB_RET_OK                              1
#define XBPMB_RET_CANCEL                          2
#define XBPMB_RET_ABORT                           3
#define XBPMB_RET_RETRY                           4
#define XBPMB_RET_IGNORE                          5
#define XBPMB_RET_YES                             6
#define XBPMB_RET_NO                              7
#define XBPMB_RET_ENTER                           9
#define XBPMB_RET_ERROR                           65535

/*----------------------------------------------------------------------*/

#define _XBP_CH
#endif
