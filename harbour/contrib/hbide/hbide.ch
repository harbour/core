/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://harbour-project.org
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

#ifndef __HBIDE_CH
#define __HBIDE_CH

#include "hbtrace.ch"

#undef HB_TR_ALWAYS
#define HB_TR_ALWAYS HB_TR_DEBUG

#define UI_MODE_FUNC                              0
#define UI_MODE_UIC                               1
#define UI_MODE_UI                                2
#define UI_MODE_DEFAULT                           UI_MODE_FUNC

#define _EOL                                      Chr( 10 )

#define HBIDE_USE_UIC

#define HBIDE_ANIMATION_NONE                      0
#define HBIDE_ANIMATION_GRADIENT                  1
#define HBIDE_ANIMATION_GRADIENT_BLUE             2

#define HBIDE_RUN_MODE_INI                        1
#define HBIDE_RUN_MODE_HBP                        2
#define HBIDE_RUN_MODE_PRG                        3

/* .hbi structure constants */
#define PRJ_PRP_PROPERTIES                        1
#define PRJ_PRP_FLAGS                             2
#define PRJ_PRP_SOURCES                           3
#define PRJ_PRP_METADATA                          4
//
#define PRJ_PRP_SECTIONS                          4

#define PRJ_PRP_TYPE                              1
#define PRJ_PRP_TITLE                             2
#define PRJ_PRP_LOCATION                          3
#define PRJ_PRP_WRKFOLDER                         4
#define PRJ_PRP_DSTFOLDER                         5
#define PRJ_PRP_OUTPUT                            6
#define PRJ_PRP_LPARAMS                           7
#define PRJ_PRP_LPROGRAM                          8
#define PRJ_PRP_BACKUP                            9
#define PRJ_PRP_XHB                               10
#define PRJ_PRP_XPP                               11
#define PRJ_PRP_CLP                               12
//
#define PRJ_PRP_PRP_VRBLS                         12

/* Project Properties array elements */
#define E_qPrjType                                1
#define E_oPrjTtl                                 2
#define E_oPrjLoc                                 3
#define E_oPrjWrk                                 4
#define E_oPrjDst                                 5
#define E_oPrjOut                                 6
#define E_oPrjLau                                 7
#define E_oPrjLEx                                 8
#define E_oPrjInc                                 9
#define E_oPrjSrc                                 10
#define E_oPrjMta                                 11
#define E_oPrjHbp                                 12
#define E_oPrjCmp                                 13

#define SB_PNL_MAIN                               1
#define SB_PNL_READY                              2
#define SB_PNL_LINE                               3
#define SB_PNL_COLUMN                             4
#define SB_PNL_INS                                5
#define SB_PNL_SELECTEDCHARS                      6
#define SB_PNL_MODIFIED                           7
#define SB_PNL_STREAM                             8
#define SB_PNL_EDIT                               9
#define SB_PNL_SEARCH                             10
#define SB_PNL_CODEC                              11
#define SB_PNL_ENVIRON                            12
#define SB_PNL_VIEW                               13
#define SB_PNL_PROJECT                            14
#define SB_PNL_THEME                              15

#define TAB_OTAB                                  1
#define TAB_OEDITOR                               2

#define TRE_OITEM                                 1
#define TRE_TYPE                                  2  // Path | Source File | Project | Opened File
#define TRE_OPARENT                               3
#define TRE_ORIGINAL                              4
#define TRE_DATA                                  5

#define ACT_NAME                                  1
#define ACT_TEXT                                  2
#define ACT_IMAGE                                 3
#define ACT_SHORTCUT                              4
#define ACT_CHECKABLE                             5
#define ACT_VISINMENU                             6

#define IDE_PART_EDITOR                           0
#define IDE_PART_DBU                              1
#define IDE_PART_REPORTSDESIGNER                  2
#define IDE_PART_CUISCREENDESIGNER                3

#endif
