/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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

#ifndef __HBIDE_CH
   #define __HBIDE_CH

#define CRLF                                      chr( 13 )+chr( 10 )
#define _EOL                                      chr( 10 )


#define INI_HBIDE                                 1
#define INI_PROJECTS                              2
#define INI_FILES                                 3
#define INI_FIND                                  4
#define INI_REPLACE                               5
#define INI_RECENTFILES                           6
#define INI_RECENTPROJECTS                        7

#define INI_SECTIONS_COUNT                        7


/* INI_HBIDE */
#define MainWindowGeometry                        1
#define ProjectTreeVisible                        2
#define ProjectTreeGeometry                       3
#define FunctionListVisible                       4
#define FunctionListGeometry                      5
#define RecentTabIndex                            6
#define CurrentProject                            7
#define GotoDialogGeometry                        8
#define PropsDialogGeometry                       9
#define FindDialogGeometry                        10
#define ThemesDialogGeometry                      11
#define CurrentTheme                              12

#define INI_HBIDE_VRBLS                           12

/* .hbi structure constants */
#define PRJ_PRP_PROPERTIES                        1
#define PRJ_PRP_FLAGS                             2
#define PRJ_PRP_SOURCES                           3
#define PRJ_PRP_METADATA                          4

#define PRJ_PRP_SECTIONS                          4

#define PRJ_PRP_TYPE                              1
#define PRJ_PRP_TITLE                             2
#define PRJ_PRP_LOCATION                          3
#define PRJ_PRP_WRKFOLDER                         4
#define PRJ_PRP_DSTFOLDER                         5
#define PRJ_PRP_OUTPUT                            6
#define PRJ_PRP_LPARAMS                           7
#define PRJ_PRP_LPROGRAM                          8

#define PRJ_PRP_PRP_VRBLS                         8

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
#define E_oPrjSrc                                10
#define E_oPrjMta                                11
#define E_oPrjHbp                                12
#define E_oPrjCmp                                13

#define SB_PNL_MAIN                               1
#define SB_PNL_READY                              2
#define SB_PNL_LINE                               3
#define SB_PNL_COLUMN                             4
#define SB_PNL_INS                                5
#define SB_PNL_M_1                                6
#define SB_PNL_MODIFIED                           7
#define SB_PNL_M_2                                8
#define SB_PNL_STREAM                             9
#define SB_PNL_EDIT                              10
#define SB_PNL_SEARCH                            11
#define SB_PNL_CODEC                             12
#define SB_PNL_PROJECT                           13

#define TAB_OTAB                                  1
#define TAB_QEDIT                                 2
#define TAB_QHILIGHTER                            3
#define TAB_QLAYOUT                               4
#define TAB_SOURCEFILE                            5
#define TAB_QDOCUMENT                             6
#define TAB_OEDITOR                               7

#define TRE_OITEM                                 1
#define TRE_TYPE                                  2  // Path | Source File | Project | Opened File
#define TRE_OPARENT                               3
#define TRE_ORIGINAL                              4
#define TRE_DATA                                  5

#endif

