/*
 * $Id$
 */

#ifndef _GRA_CH
#define _GRA_CH

#define  GRA_CLR_INVALID                          ( -6 )    //
#define  GRA_CLR_FALSE                            ( -5 )    //
#define  GRA_CLR_TRUE                             ( -4 )    //
#define  GRA_CLR_DEFAULT                          ( -3 )    //
#define  GRA_CLR_BACKGROUND                       ( -2 )    //
#define  GRA_CLR_NEUTRAL                          ( -1 )    //
#define  GRA_CLR_WHITE                            0         //    Qt_white
#define  GRA_CLR_BLACK                            1         //    Qt_black
#define  GRA_CLR_BLUE                             2         //    Qt_blue
#define  GRA_CLR_RED                              3         //    Qt_red
#define  GRA_CLR_PINK                             4         //    Qt_magenta
#define  GRA_CLR_GREEN                            5         //    Qt_green
#define  GRA_CLR_CYAN                             6         //    Qt_cyan
#define  GRA_CLR_YELLOW                           7         //    Qt_yellow
#define  GRA_CLR_DARKGRAY                         8         //    Qt_darkGray
#define  GRA_CLR_DARKBLUE                         9         //    Qt_darkBlue
#define  GRA_CLR_DARKRED                          10        //    Qt_darkRed
#define  GRA_CLR_DARKPINK                         11        //    Qt_darkMagenta
#define  GRA_CLR_DARKGREEN                        12        //    Qt_darkGreen
#define  GRA_CLR_DARKCYAN                         13        //    Qt_darkCyan
#define  GRA_CLR_BROWN                            14        //    Qt_darkYellow
#define  GRA_CLR_PALEGRAY                         15        //    Qt_lightGray  Qt_gray  Qt_darkGray

#define  GRA_FILL                                 1
#define  GRA_OUTLINE                              2
#define  GRA_OUTLINEFILL                          3

#define  GRA_AA_COLOR                             1
#define  GRA_AA_BACKCOLOR                         2
#define  GRA_AA_MIXMODE                           3
#define  GRA_AA_BGMIXMODE                         4
#define  GRA_AA_SYMBOL                            5

#define  GRA_AA_COUNT                             5

#define  GRA_SYM_DENSE1                           1000
#define  GRA_SYM_DENSE2                           1001
#define  GRA_SYM_DENSE3                           1002
#define  GRA_SYM_DENSE4                           1003
#define  GRA_SYM_DENSE5                           1004
#define  GRA_SYM_DENSE6                           1005
#define  GRA_SYM_DENSE7                           1006
#define  GRA_SYM_DENSE8                           1007
#define  GRA_SYM_VERT                             131073
#define  GRA_SYM_HORIZ                            131072
#define  GRA_SYM_DIAG1                            131075
#define  GRA_SYM_DIAG2                            1008
#define  GRA_SYM_DIAG3                            131074
#define  GRA_SYM_DIAG4                            1009
#define  GRA_SYM_NOSHADE                          65536
#define  GRA_SYM_SOLID                            0
#define  GRA_SYM_HALFTONE                         1010
#define  GRA_SYM_HATCH                            131076
#define  GRA_SYM_DIAGHATCH                        131077
#define  GRA_SYM_BLANK                            65536

#define  GRA_SYM_DEFAULT                          GRA_SYM_SOLID

#define  GRA_AM_COLOR                             1
#define  GRA_AM_BACKCOLOR                         2
#define  GRA_AM_MIXMODE                           3
#define  GRA_AM_BGMIXMODE                         4
#define  GRA_AM_SYMBOL                            5
#define  GRA_AM_BOX                               6

#define  GRA_AM_COUNT                             6

#define  GRA_MARKSYM_CROSS                        1
#define  GRA_MARKSYM_PLUS                         2
#define  GRA_MARKSYM_DIAMOND                      3
#define  GRA_MARKSYM_SQUARE                       4
#define  GRA_MARKSYM_SIXPOINTSTAR                 5
#define  GRA_MARKSYM_EIGHTPOINTSTAR               6
#define  GRA_MARKSYM_SOLIDDIAMOND                 7
#define  GRA_MARKSYM_SOLIDSQUARE                  8
#define  GRA_MARKSYM_DOT                          9
#define  GRA_MARKSYM_SMALLCIRCLE                  10
#define  GRA_MARKSYM_BLANK                        64

#define  GRA_MARKSYM_DEFAULT                      GRA_MARKSYM_CROSS


#define  GRA_AL_COLOR                             1
#define  GRA_AL_MIXMODE                           2
#define  GRA_AL_WIDTH                             3
#define  GRA_AL_TYPE                              4

#define  GRA_AL_COUNT                             4
#define  GRA_LINETYPE_DOT                         2
#define  GRA_LINETYPE_SHORTDASH                   1
#define  GRA_LINETYPE_DASHDOT                     3
#define  GRA_LINETYPE_DOUBLEDOT                   GRA_LINETYPE_DOT
#define  GRA_LINETYPE_LONGDASH                    GRA_LINETYPE_SHORTDASH
#define  GRA_LINETYPE_DASHDOUBLEDOT               4
#define  GRA_LINETYPE_SOLID                       0
#define  GRA_LINETYPE_INVISIBLE                   5
#define  GRA_LINETYPE_ALTERNATE                   GRA_LINETYPE_DASHDOT

#define  GRA_LINETYPE_DEFAULT                     GRA_LINETYPE_SOLID

#define  GRA_LINEWIDTH_NORMAL                     0
#define  GRA_LINEWIDTH_THICK                      2

#define  GRA_LINEWIDTH_DEFAULT                    GRA_LINEWIDTH_NORMAL

#define  GRA_AS_COLOR                             1
#define  GRA_AS_BACKCOLOR                         2
#define  GRA_AS_MIXMODE                           3
#define  GRA_AS_BGMIXMODE                         4
#define  GRA_AS_BOX                               5
#define  GRA_AS_ANGLE                             6
#define  GRA_AS_SHEAR                             7
#define  GRA_AS_DIRECTION                         8
#define  GRA_AS_HORIZALIGN                        9
#define  GRA_AS_VERTALIGN                         10
#define  GRA_AS_EXTRA                             11
#define  GRA_AS_BREAK_EXTRA                       12

#define  GRA_AS_COUNT                             12

#define  GRA_CHDIRN_LEFTRIGHT                     1
#define  GRA_CHDIRN_TOPBOTTOM                     2
#define  GRA_CHDIRN_RIGHTLEFT                     3
#define  GRA_CHDIRN_BOTTOMTOP                     4

#define  GRA_CHDIRN_DEFAULT                       GRA_CHDIRN_LEFTRIGHT

#define  GRA_HALIGN_LEFT                          0
#define  GRA_HALIGN_CENTER                        6
#define  GRA_HALIGN_RIGHT                         2
#define  GRA_HALIGN_STANDARD                      GRA_HALIGN_LEFT
#define  GRA_HALIGN_NORMAL                        GRA_HALIGN_LEFT
#define  GRA_VALIGN_NORMAL                        1
#define  GRA_VALIGN_TOP                           2
#define  GRA_VALIGN_HALF                          3
#define  GRA_VALIGN_BASE                          4
#define  GRA_VALIGN_BOTTOM                        5
#define  GRA_VALIGN_STANDARD                      6

#define  GRA_FGMIX_OR                             15
#define  GRA_FGMIX_OVERPAINT                      13
#define  GRA_FGMIX_LEAVEALONE                     11
#define  GRA_FGMIX_XOR                            7
#define  GRA_FGMIX_AND                            9
#define  GRA_FGMIX_SUBTRACT                       3
#define  GRA_FGMIX_MASKSRCNOT                     5
#define  GRA_FGMIX_ZERO                           1
#define  GRA_FGMIX_NOTMERGESRC                    2
#define  GRA_FGMIX_NOTXORSRC                      10
#define  GRA_FGMIX_INVERT                         6
#define  GRA_FGMIX_MERGESRCNOT                    14
#define  GRA_FGMIX_NOTCOPYSRC                     4
#define  GRA_FGMIX_MERGENOTSRC                    12
#define  GRA_FGMIX_NOTMASKSRC                     8
#define  GRA_FGMIX_ONE                            16

#define  GRA_FGMIX_DEFAULT                        GRA_FGMIX_OVERPAINT

#define  GRA_BGMIX_OVERPAINT                      2
#define  GRA_BGMIX_LEAVEALONE                     1
#define  GRA_BGMIX_OR                             GRA_BGMIX_LEAVEALONE
#define  GRA_BGMIX_XOR                            GRA_BGMIX_LEAVEALONE

#define  GRA_BGMIX_DEFAULT                        GRA_BGMIX_LEAVEALONE

#define  GRA_NUMCLR_RESERVED                      15

#define  GRA_SEG_MODIFIABLE                       1
#define  GRA_SEG_NIL                              2

#define  GRA_SEG_LOWER_PRI                        (-1)
#define  GRA_SEG_HIGHER_PRI                       1

#define  GRA_DM_DRAW                              1
#define  GRA_DM_RETAIN                            2
#define  GRA_DM_DRAWANDRETAIN                     3

#define  GRA_TRANSFORM_REPLACE                    0
#define  GRA_TRANSFORM_ADD                        1
#define  GRA_TRANSFORM_PREEMPT                    2

#define  GRA_PATH_FILL_ALTERNATE                  1
#define  GRA_PATH_FILL_WINDING                    2
#define  GRA_PATH_FILL_EXCL                       GRA_PATH_FILL_ALTERNATE

#define  GRA_PATH_CLIP_ALTERNATE                  0
#define  GRA_PATH_CLIP_WINDING                    2
#define  GRA_PATH_CLIP_INCL                       0
#define  GRA_PATH_CLIP_EXCL                       8
#define  GRA_PATH_CLIP_COMBINE                    4

#define  GRA_BLT_ROP_SRCCOPY                      13369376
#define  GRA_BLT_ROP_SRCPAINT                     15597702
#define  GRA_BLT_ROP_SRCAND                       8913094
#define  GRA_BLT_ROP_SRCINVERT                    6684742
#define  GRA_BLT_ROP_SRCERASE                     4457256
#define  GRA_BLT_ROP_NOTSRCCOPY                   3342344
#define  GRA_BLT_ROP_NOTSRCERASE                  1114278
#define  GRA_BLT_ROP_MERGECOPY                    12583114
#define  GRA_BLT_ROP_MERGEPAINT                   12255782
#define  GRA_BLT_ROP_PATCOPY                      15728673
#define  GRA_BLT_ROP_PATPAINT                     16452105
#define  GRA_BLT_ROP_PATINVERT                    5898313
#define  GRA_BLT_ROP_DSTINVERT                    5570569
#define  GRA_BLT_ROP_ZERO                         66
#define  GRA_BLT_ROP_ONE                          16711778

#define  GRA_BLT_BBO_OR                           2
#define  GRA_BLT_BBO_AND                          1
#define  GRA_BLT_BBO_IGNORE                       3


#define  GRA_PU_ARBITRARY                         4
#define  GRA_PU_PIXEL                             8
#define  GRA_PU_LOMETRIC                          12
#define  GRA_PU_HIMETRIC                          16
#define  GRA_PU_LOENGLISH                         20
#define  GRA_PU_HIENGLISH                         24
#define  GRA_PU_TWIPS                             28

#define  GRA_PO_ORGTOPLEFT                        1
#define  GRA_PO_ORGBOTTOMLEFT                     2

#define  XBPPS_MODE_NORMAL                        0
#define  XBPPS_MODE_HIGH_TXT_PRECISION            1

#define  XBPPS_MODE_HIGH_PRECISION                XBPPS_MODE_HIGH_TXT_PRECISION

#define  GRA_CLRMODE_COMPATIBLE                   0
#define  GRA_CLRMODE_STANDARD                     1
#define  GRA_CLRMODE_DEFCOLORTAB                  3
#define  GRA_CLRMODE_DITHER                       8

#endif
