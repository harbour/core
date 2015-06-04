/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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


#ifndef __CACHEMGR_CH
   #define __CACHEMGR_CH

#define VOU_CLR_HEADER                            "W+/RB"
#define VOU_CLR_GETS                              "W+/BG,GR+/G,,,W+/BG"
#define VOU_CLR_SAYS                              "W/B"
#define VOU_CLR_BOX                               "W+/B"
#define VOU_CLR_MENU                              "W+/BG,W+/B,,,W/BG"
#define VOU_CLR_MAIN                              "N/W"
#define VOU_CLR_BROWSE                            "W+/B, N/BG"
#define VOU_CLR_LOGO                              "W+/N"
#define VOU_CLR_TITLE                             "GR+/B"
#define VOU_CLR_MENU_Z                            "W+/B,W+/R,,,W/BG"
#define VOU_CLR_POPUP                             "W+/B,N/W*,,,W/B"
#define VOU_CLR_HBR                               "B/W"

#define VOU_NTOP                                  4
#define VOU_NLFT                                  3
#define VOU_NBTM                                  20
#define VOU_NRGT                                  75

#define VOU_WE                                    1
#define VOU_YOU                                   4

#define LG1                                       REPLICATE( CHR( 223 ), 1 )
#define LG2                                       REPLICATE( CHR( 223 ), 2 )
#define LG3                                       REPLICATE( CHR( 223 ), 3 )
#define LG4                                       REPLICATE( CHR( 223 ), 4 )
#define LG5                                       REPLICATE( CHR( 223 ), 5 )
#define LG6                                       REPLICATE( CHR( 223 ), 6 )
#define LG7                                       REPLICATE( CHR( 223 ), 7 )
#define LG8                                       REPLICATE( CHR( 223 ), 8 )
#define LG9                                       REPLICATE( CHR( 223 ), 9 )
#define LG10                                      REPLICATE( CHR( 223 ),10 )


#define VOU_MSG_INFO                              1
#define VOU_MSG_STOP                              2
#define VOU_MSG_OKCANCEL                          3
#define VOU_MSG_YESNO                             4
#define VOU_MSG_BEEP                              5


#endif                                            // __CACHEMGR_CH
