/*
 * Harbour Project source code:
 * Header file for cross-compatibility between different Harbour flavours
 *
 * Copyright 1999-2007 {list of individual authors and e-mail addresses}
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#ifdef HB_COMPAT_H_
#define HB_COMPAT_H_

#include "hbapi.h"
#include "hbapiitm.h"

/* xHarbour functions mapped to Harbour ones */

#ifdef __XHARBOUR__

   #define hb_retc_buffer( szText )            hb_retcAdopt( ( szText ) )
   #define hb_retclen_buffer( szText, ulLen )  hb_retclenAdopt( ( szText ), ( ulLen ) )
   #define hb_retc_const( szText )             hb_retcStatic( ( szText ) )

   #define hb_storclen_buffer                  hb_storclenAdopt
   #define hb_itemPutCLConst                   hb_itemPutCRawStatic

   #define HB_MAXINT                           HB_LONG
   #define HB_BOOL                             BOOL
   #define HB_TRUE                             TRUE
   #define HB_FALSE                            FALSE
   #define HB_U32                              UINT32
   #define HB_U8                               BYTE
   #define HB_UINT                             UINT
   #define HB_U16                              UINT16
   #define HB_I32                              INT32
   #define HB_ISCHAR                           ISCHAR
   #define HB_ISBLOCK                          ISBLOCK
   #define HB_ISPOINTER                        ISPOINTER
   #define HB_ISOBJECT                         ISOBJECT
   #define HB_ISARRAY                          ISARRAY
   #define HB_ISLOG                            ISLOG
   #define HB_ISNIL                            ISNIL
   #define HB_ISBYREF                          ISBYREF
   #define HB_ISNUM                            ISNUM
   #define hb_parldef( l1, l2 )                ( ISLOG( l1 ) ? hb_parl( l1 )    : l2 )
   #define hb_parnidef( n1, n2 )               ( ISNUM( n1 ) ? hb_parni( n1 )   : n2 )
   #define hb_parnldef( n1, n2 )               ( ISNUM( n1 ) ? hb_parnl( n1 )   : n2 )
   #define hb_parnintdef( n1, n2 )             ( ISNUM( n1 ) ? hb_parnint( n1 ) : n2 )
   #define HB_BYTE                             BYTE
   #define HB_USHORT                           USHORT
   #define HB_UCHAR                            UCHAR
   #define HB_FT_IMAGE                         HB_FT_PICTURE
   #define HB_I64                              INT64
   #define hb_gcMark                           hb_gcItemRef
   #define hb_vmAtQuit                         hb_vmAtExit
   #define HB_FSNAMECONV                       hb_fsNameConv
   #define hb_cdpFindExt                       hb_cdpFind
   #define hb_itemPutTS                        hb_itemPutDTS
   #define HB_IS_TIMESTAMP                     HB_IS_TIMEFLAG
   #define HB_CDP_ISBINSORT( cdp )             0
   #define HB_PFS                              PFLL

#else

   #define hb_retcAdopt( szText )              hb_retc_buffer( ( szText ) )
   #define hb_retclenAdopt( szText, ulLen )    hb_retclen_buffer( ( szText ), ( ulLen ) )
   #define hb_retcStatic( szText )             hb_retc_const( ( szText ) )

   #define hb_storclenAdopt                    hb_storclen_buffer
   #define hb_itemPutCRawStatic                hb_itemPutCLConst

#endif

#endif
