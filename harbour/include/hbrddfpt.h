/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * DBFFPT RDD
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
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

#ifndef HB_RDDFPT_H_
#define HB_RDDFPT_H_

#include "hbsetup.h"
#include "hbdbf.h"
#include "hbdbferr.h"
#include "hbapirdd.h"
#ifndef HB_CDP_SUPPORT_OFF
#include "hbapicdp.h"
#endif
#define HB_EXTERNAL_RDDDBF_USE
#include "hbrdddbf.h"

HB_EXTERN_BEGIN

/* MEMO constants and defaults */
#define FPT_MEMOEXT                          ".fpt"
#define FPT_LOCKPOS                     0x00000000L
#define FPT_LOCKSIZE                    0x7FFFFFFFL
#define FPT_DEFBLOCKSIZE                         64
#define SIX_ITEM_BUFSIZE                         14
#define FLEX_ITEM_BUFSIZE                         8
#define MAX_SIXFREEBLOCKS                        82
#define MAX_FLEXFREEBLOCKS                      126
#define FLEXGCPAGE_SIZE                        1010

/* usMemoType */
#define MEMO_DBT              1
#define MEMO_FPT_HB           2
#define MEMO_FPT_SIX          3
#define MEMO_FPT_SIXHB        4
#define MEMO_FPT_FLEX         5
#define MEMO_FPT_CLIP         6

#define FPTIT_BINARY       0x0000
#define FPTIT_PICT         0x0000      /* Picture */
#define FPTIT_TEXT         0x0001      /* Text    */
#define FPTIT_OBJ          0x0002      /* Object  */

#define FPTIT_SIX_LNUM     0x0002      /* LONG LE */
#define FPTIT_SIX_DNUM     0x0008      /* DOUBLE LE */
#define FPTIT_SIX_LDATE    0x0020      /* DATE (LONG LE) */
#define FPTIT_SIX_LOG      0x0080      /* LOGIC */
#define FPTIT_SIX_CHAR     0x0400      /* CHAR */
#define FPTIT_SIX_ARRAY    0x8000      /* ARRAY */

//#define FPTIT_SIX_BLOCK    0x1000
//#define FPTIT_SIX_VREF     0x2000
//#define FPTIT_SIX_MREF     0x4000

#define FPTIT_FLEX_GC      0x03E8   // 1000
#define FPTIT_FLEX_UNUSED  0x03E9   // 1001
#define FPTIT_FLEX_ARRAY   0x03EA   // 1002
#define FPTIT_FLEX_OBJECT  0x03EB   // 1003 *
#define FPTIT_FLEX_VOARR   0x03EC   // 1004 *
#define FPTIT_FLEX_VOOBJ   0x03ED   // 1005 *
#define FPTIT_FLEX_NIL     0x03EE   // 1006
#define FPTIT_FLEX_TRUE    0x03EF   // 1007
#define FPTIT_FLEX_FALSE   0x03F0   // 1008
#define FPTIT_FLEX_LDATE   0x03F1   // 1009
#define FPTIT_FLEX_CHAR    0x03F2   // 1010
#define FPTIT_FLEX_UCHAR   0x03F3   // 1011 *
#define FPTIT_FLEX_SHORT   0x03F4   // 1012
#define FPTIT_FLEX_USHORT  0x03F5   // 1013 *
#define FPTIT_FLEX_LONG    0x03F6   // 1014
#define FPTIT_FLEX_ULONG   0x03F7   // 1015 *
#define FPTIT_FLEX_DOUBLE  0x03F8   // 1016
#define FPTIT_FLEX_LDOUBLE 0x03F9   // 1017 *
#define FPTIT_FLEX_COMPCH  0x03FA   // 1018 *

#define FPTIT_FLEXAR_NIL    0x00
#define FPTIT_FLEXAR_STR    0x07
#define FPTIT_FLEXAR_ARAY   0x0C
#define FPTIT_FLEXAR_DATE   0x0E
#define FPTIT_FLEXAR_DBL    0x0F
#define FPTIT_FLEXAR_BYTE   0x11
#define FPTIT_FLEXAR_CHAR   0x12
#define FPTIT_FLEXAR_SHORT  0x13
#define FPTIT_FLEXAR_USHORT 0x14
#define FPTIT_FLEXAR_LONG   0x15
#define FPTIT_FLEXAR_NUL    0x18
#define FPTIT_FLEXAR_TRUE   0x19
#define FPTIT_FLEXAR_FALSE  0x1A
#define FPTIT_FLEXAR_BYTE2  0x1D
#define FPTIT_FLEXAR_SHORT2 0x1E
#define FPTIT_FLEXAR_LONG2  0x20
#define FPTIT_FLEXAR_ULONG  0x21

/*
#define HB_IT_NIL       ( ( USHORT ) 0x0000 )
#define HB_IT_POINTER   ( ( USHORT ) 0x0001 )
#define HB_IT_LONGLONG  ( ( USHORT ) 0x0040 )
#define HB_IT_SYMBOL    ( ( USHORT ) 0x0100 )
#define HB_IT_ALIAS     ( ( USHORT ) 0x0200 )
#define HB_IT_BLOCK     ( ( USHORT ) 0x1000 )
#define HB_IT_BYREF     ( ( USHORT ) 0x2000 )
#define HB_IT_MEMVAR    ( ( USHORT ) 0x4000 )
#define HB_IT_ANY       ( ( USHORT ) 0xFFFF )
*/

/* MEMO file strucutres */
typedef struct _FPTHEADER
{
   BYTE  nextBlock[ 4 ];            /* Next free block in the file */
   BYTE  reserved1[ 2 ];            /* */
   BYTE  blockSize[ 2 ];            /* Size of block */
   BYTE  signature1[ 10 ];          /* Signature: "SixMemo", "Harbour", "Made by CLIP"-overwrites next bytes*/
   BYTE  nGCitems[ 2 ];             /* number of GC items in reserved2 (max 82)*/
   BYTE  reserved2[ 492 ];          /* */
   BYTE  signature2[ 12 ];          /* Signature: "FlexFile3\003" */
   BYTE  flexRev[ 4 ];              /* Offset of reversed GC page */
   BYTE  flexDir[ 4 ];              /* Offset of GC page */
   BYTE  counter[ 4 ];              /* cyclic counter to sign changes in network env. */
   BYTE  rootBlock[ 4 ];            /* Clipper 5.3 ROOT data block */
   BYTE  flexSize[ 2 ];             /* FlexFile3 alternative block size */
   BYTE  reserved4[ 482 ];          /* */
} FPTHEADER;
typedef FPTHEADER * LPFPTHEADER;

typedef struct _FPTBLOCK
{
   BYTE  type[ 4 ];                 /* see: FPTIT_ */
   BYTE  size[ 4 ];                 /* length of data in bytes */
} FPTBLOCK;
typedef FPTBLOCK * LPFPTBLOCK;


/* MEMO internal memory structures */
typedef struct _MEMOGCITEM
{
   ULONG ulOffset;                  /* Number of blocks */
   ULONG ulSize;                    /* Block number */
   BOOL  fChanged;                  /* Mark the free page as changed */
} MEMOGCITEM;
typedef MEMOGCITEM * LPMEMOGCITEM;

typedef struct _MEMOGCTABLE
{
   BYTE   bType;                    /* MEMO_FPT_SIX or MEMO_FPT_FLEX */
   BYTE   bChanged;                 /* Should we write GC data to disk */
   ULONG  ulNextBlock;              /* Next free block in the file */
   ULONG  ulRevPage;                /* FLEX Rev GC page offset */
   ULONG  ulDirPage;                /* FLEX Dir GC page offset */
   ULONG  ulCounter;                /* FLEX cyclic counter */
   ULONG  ulSize;                   /* FLEX GC page size in bytes */
   USHORT usMaxItem;                /* max number of items in pGCitems */
   USHORT usItems;                  /* number of items in pGCitems */
   LPMEMOGCITEM pGCitems;           /* free block list */
   FPTHEADER fptHeader;             /* FPT file header */
} MEMOGCTABLE;
typedef MEMOGCTABLE * LPMEMOGCTABLE;


/*
 *  DBFFPT WORKAREA
 *  ------------
 *  The Workarea Structure of DBFFPT RDD
 *
 */

/* we don't have to change DBFAREA to create FPTAREA */
typedef DBFAREA FPTAREA;
typedef FPTAREA * LPFPTAREA;
#ifndef FPTAREAP
#define FPTAREAP LPFPTAREA
#endif

/*
 * -- DBFFPT METHODS --
 */

#define SUPERTABLE                         ( &fptSuper )

#define hb_fptBof                                  NULL
#define hb_fptEof                                  NULL
#define hb_fptFound                                NULL
#define hb_fptGoBottom                             NULL
#define hb_fptGoTo                                 NULL
#define hb_fptGoToId                               NULL
#define hb_fptGoTop                                NULL
#define hb_fptSeek                                 NULL
#define hb_fptSkip                                 NULL
#define hb_fptSkipFilter                           NULL
#define hb_fptSkipRaw                              NULL
#define hb_fptAddField                             NULL
#define hb_fptAppend                               NULL
#define hb_fptCreateFields                         NULL
#define hb_fptDeleteRec                            NULL
#define hb_fptDeleted                              NULL
#define hb_fptFieldCount                           NULL
#define hb_fptFieldDisplay                         NULL
static ERRCODE hb_fptFieldInfo( FPTAREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem );
#define hb_fptFieldName                            NULL
#define hb_fptFlush                                NULL
#define hb_fptGetRec                               NULL
static ERRCODE hb_fptGetValue( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
static ERRCODE hb_fptGetVarLen( FPTAREAP pArea, USHORT uiIndex, ULONG * pLength );
#define hb_fptGoCold                               NULL
#define hb_fptGoHot                                NULL
#define hb_fptPutRec                               NULL
static ERRCODE hb_fptPutValue( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
#define hb_fptRecall                               NULL
#define hb_fptRecCount                             NULL
#define hb_fptRecInfo                              NULL
#define hb_fptRecNo                                NULL
#define hb_fptSetFieldExtent                       NULL
#define hb_fptAlias                                NULL
#define hb_fptClose                                NULL
#define hb_fptCreate                               NULL
static ERRCODE hb_fptInfo( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
#define hb_fptNewArea                              NULL
#define hb_fptOpen                                 NULL
#define hb_fptRelease                              NULL
static ERRCODE hb_fptStructSize( FPTAREAP pArea, USHORT * uiSize );
static ERRCODE hb_fptSysName( FPTAREAP pArea, BYTE * pBuffer );
#define hb_fptEval                                 NULL
#define hb_fptPack                                 NULL
#define hb_fptPackRec                              NULL
#define hb_fptSort                                 NULL
#define hb_fptTrans                                NULL
#define hb_fptTransRec                             NULL
#define hb_fptZap                                  NULL
#define hb_fptChildEnd                             NULL
#define hb_fptChildStart                           NULL
#define hb_fptChildSync                            NULL
#define hb_fptSyncChildren                         NULL
#define hb_fptClearRel                             NULL
#define hb_fptForceRel                             NULL
#define hb_fptRelArea                              NULL
#define hb_fptRelEval                              NULL
#define hb_fptRelText                              NULL
#define hb_fptSetRel                               NULL
#define hb_fptOrderListAdd                         NULL
#define hb_fptOrderListClear                       NULL
#define hb_fptOrderListDelete                      NULL
#define hb_fptOrderListFocus                       NULL
#define hb_fptOrderListRebuild                     NULL
#define hb_fptOrderCondition                       NULL
#define hb_fptOrderCreate                          NULL
#define hb_fptOrderDestroy                         NULL
#define hb_fptOrderInfo                            NULL
#define hb_fptClearFilter                          NULL
#define hb_fptClearLocate                          NULL
#define hb_fptClearScope                           NULL
#define hb_fptCountScope                           NULL
#define hb_fptFilterText                           NULL
#define hb_fptScopeInfo                            NULL
#define hb_fptSetFilter                            NULL
#define hb_fptSetLocate                            NULL
#define hb_fptSetScope                             NULL
#define hb_fptSkipScope                            NULL
#define hb_fptCompile                              NULL
#define hb_fptError                                NULL
#define hb_fptEvalBlock                            NULL
#define hb_fptRawLock                              NULL
#define hb_fptLock                                 NULL
#define hb_fptUnLock                               NULL
#define hb_fptCloseMemFile                         NULL
static ERRCODE hb_fptCreateMemFile( FPTAREAP pArea, LPDBOPENINFO pCreateInfo );
#define hb_fptGetValueFile                         NULL
static ERRCODE hb_fptOpenMemFile( FPTAREAP pArea, LPDBOPENINFO pOpenInfo );
#define hb_fptPutValueFile                         NULL
static ERRCODE hb_fptReadDBHeader( FPTAREAP pArea );
static ERRCODE hb_fptWriteDBHeader( FPTAREAP pArea );
#define hb_fptExit                                 NULL
#define hb_fptDrop                                 NULL
#define hb_fptExists                               NULL
#define hb_fptWhoCares                             NULL

HB_EXTERN_END

#endif /* HB_RDDFPT */
