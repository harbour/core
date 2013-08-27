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

#ifndef HB_RDDFPT_H_
#define HB_RDDFPT_H_

#include "hbrdddbf.h"

HB_EXTERN_BEGIN

/* MEMO constants and defaults */
#define DBT_MEMOEXT                          ".dbt"
#define FPT_MEMOEXT                          ".fpt"
#define SMT_MEMOEXT                          ".smt"
#define DBV_MEMOEXT                          ".dbv"
#define DBT_DEFBLOCKSIZE                        512
#define FPT_DEFBLOCKSIZE                         64
#define SMT_DEFBLOCKSIZE                         32

#define FPT_LOCKPOS                     0x00000000L
#define FPT_LOCKSIZE                    0x00000001L

#define FPT_ROOTBLOCK_OFFSET                  0x218 /* Clipper 5.3 ROOT data block offset */

#define SIX_ITEM_BUFSIZE                         14
#define FLEX_ITEM_BUFSIZE                         8
#define MAX_SIXFREEBLOCKS                        82
#define MAX_FLEXFREEBLOCKS                      126
#define FLEXGCPAGE_SIZE                        1010

/* "V" filed types */
#define HB_VF_CHAR            64000
#define HB_VF_DATE            64001
#define HB_VF_INT             64002
#define HB_VF_LOG             64003
#define HB_VF_DNUM            64004
#define HB_VF_ARRAY           64005
#define HB_VF_BLOB            64006
#define HB_VF_BLOBCOMPRESS    64007
#define HB_VF_BLOBENCRYPT     64008

/* SMT types */
#define SMT_IT_NIL            0
#define SMT_IT_CHAR           1
#define SMT_IT_INT            2
#define SMT_IT_DOUBLE         3
#define SMT_IT_DATE           4
#define SMT_IT_LOGICAL        5
#define SMT_IT_ARRAY          6

#define FPTIT_DUMMY        0xDEADBEAF
#define FPTIT_BINARY       0x0000
#define FPTIT_PICT         0x0000      /* Picture */
#define FPTIT_TEXT         0x0001      /* Text    */
#define FPTIT_OBJ          0x0002      /* Object  */

#define FPTIT_SIX_NIL      0x0000      /* NIL VALUE (USED ONLY IN ARRAYS) */
#define FPTIT_SIX_LNUM     0x0002      /* LONG LE */
#define FPTIT_SIX_DNUM     0x0008      /* DOUBLE LE */
#define FPTIT_SIX_LDATE    0x0020      /* DATE (LONG LE) */
#define FPTIT_SIX_LOG      0x0080      /* LOGIC */
#define FPTIT_SIX_CHAR     0x0400      /* CHAR */
#define FPTIT_SIX_ARRAY    0x8000      /* ARRAY */

/* #define FPTIT_SIX_BLOCK    0x1000 */
/* #define FPTIT_SIX_VREF     0x2000 */
/* #define FPTIT_SIX_MREF     0x4000 */

#define FPTIT_FLEX_GC      0x03E8   /* 1000   */
#define FPTIT_FLEX_UNUSED  0x03E9   /* 1001   */
#define FPTIT_FLEX_ARRAY   0x03EA   /* 1002   */
#define FPTIT_FLEX_OBJECT  0x03EB   /* 1003 ! */
#define FPTIT_FLEX_VOARR   0x03EC   /* 1004 ! */
#define FPTIT_FLEX_VOOBJ   0x03ED   /* 1005 ! */
#define FPTIT_FLEX_NIL     0x03EE   /* 1006   */
#define FPTIT_FLEX_TRUE    0x03EF   /* 1007   */
#define FPTIT_FLEX_FALSE   0x03F0   /* 1008   */
#define FPTIT_FLEX_LDATE   0x03F1   /* 1009   */
#define FPTIT_FLEX_CHAR    0x03F2   /* 1010   */
#define FPTIT_FLEX_UCHAR   0x03F3   /* 1011   */
#define FPTIT_FLEX_SHORT   0x03F4   /* 1012   */
#define FPTIT_FLEX_USHORT  0x03F5   /* 1013   */
#define FPTIT_FLEX_LONG    0x03F6   /* 1014   */
#define FPTIT_FLEX_ULONG   0x03F7   /* 1015   */
#define FPTIT_FLEX_DOUBLE  0x03F8   /* 1016   */
#define FPTIT_FLEX_LDOUBLE 0x03F9   /* 1017 ! */
#define FPTIT_FLEX_COMPRCH 0x03FA   /* 1018 ! */

/* Flex II types */
#define FPTIT_FLEX_DBLITEM 0x2710   /* 10000 14-bytes Clipper double item */
#define FPTIT_FLEX_LOGICAL 0x2711   /* 10001 4-bytes logical value  */
#define FPTIT_FLEX_NULSTR  0x2722   /* 10002 empty string */


#define FPTIT_FLEXAR_NIL      0x00  /* () */
#define FPTIT_FLEXAR_UCHAR    0x01  /* uchar */
#define FPTIT_FLEXAR_CHAR     0x02  /* char */
#define FPTIT_FLEXAR_SHORT    0x03  /* short[2] */
#define FPTIT_FLEXAR_USHORT   0x04  /* ushort[2] */
#define FPTIT_FLEXAR_LONG     0x05  /* long[4] */
                           /* 0x06 - unknown */
#define FPTIT_FLEXAR_STR      0x07  /* len[2], char[n] */
                           /* 0x08 - unknown */
#define FPTIT_FLEXAR_DOUBLE   0x09  /* double[8] */
#define FPTIT_FLEXAR_DATEX    0x0A  /* long[4] */
#define FPTIT_FLEXAR_LOGIC    0x0B  /* val[1] */
#define FPTIT_FLEXAR_ARAY     0x0C  /* len[2], ... */
                           /* 0x0D - unknown */
#define FPTIT_FLEXAR_DATEJ    0x0E  /* long[4] */
#define FPTIT_FLEXAR_DOUBLE2  0x0F  /* len, dec, double[8] */
                           /* 0x10 - unknown */
#define FPTIT_FLEXAR_UCHAR1   0x11  /* byte, dec */
#define FPTIT_FLEXAR_CHAR1    0x12  /* char, dec */
#define FPTIT_FLEXAR_SHORT1   0x13  /* short[2], len */
#define FPTIT_FLEXAR_USHORT1  0x14  /* ushort[2], len */
#define FPTIT_FLEXAR_LONG1    0x15  /* long[4], len */
                           /* 0x16 - unknown */
                           /* 0x17 - unknown */
#define FPTIT_FLEXAR_NUL      0x18  /* () */
#define FPTIT_FLEXAR_TRUE     0x19  /* () */
#define FPTIT_FLEXAR_FALSE    0x1A  /* () */
#define FPTIT_FLEXAR_LDOUBLE  0x1B  /* longdouble[10] */
#define FPTIT_FLEXAR_UCHAR2   0x1C  /* byte[1], len, dec */
#define FPTIT_FLEXAR_CHAR2    0x1D  /* char[1], len, dec */
#define FPTIT_FLEXAR_SHORT2   0x1E  /* short[2], len, dec */
#define FPTIT_FLEXAR_USHORT2  0x1F  /* ushort[2], len, dec */
#define FPTIT_FLEXAR_LONG2    0x20  /* long[4], len, dec */
#define FPTIT_FLEXAR_ULONG2   0x21  /* ulong[4], len, dec */


/* MEMO file strucutres */
typedef struct _FPTHEADER
{
   HB_BYTE  nextBlock[ 4 ];            /* Next free block in the file */
   HB_BYTE  blockSize[ 4 ];            /* Size of block */
   HB_BYTE  signature1[ 10 ];          /* Signature: "SixMemo", "Harbour", "Made by CLIP"-overwrites next bytes*/
   HB_BYTE  nGCitems[ 2 ];             /* number of GC items in reserved2 (max 82)*/
   HB_BYTE  reserved2[ 492 ];          /* */
   HB_BYTE  signature2[ 12 ];          /* Signature: "FlexFile3\003" */
   HB_BYTE  flexRev[ 4 ];              /* Offset of reversed GC page */
   HB_BYTE  flexDir[ 4 ];              /* Offset of GC page */
   HB_BYTE  counter[ 4 ];              /* cyclic counter to sign changes in network env. */
   HB_BYTE  rootBlock[ 4 ];            /* Clipper 5.3 ROOT data block */
   HB_BYTE  flexSize[ 2 ];             /* FlexFile3 alternative block size */
   HB_BYTE  reserved4[ 482 ];          /* */
} FPTHEADER;
typedef FPTHEADER * LPFPTHEADER;

typedef struct _FPTBLOCK
{
   HB_BYTE  type[ 4 ];                 /* see: FPTIT_ */
   HB_BYTE  size[ 4 ];                 /* length of data in bytes */
} FPTBLOCK;
typedef FPTBLOCK * LPFPTBLOCK;


/* MEMO internal memory structures */
typedef struct _MEMOGCITEM
{
   HB_ULONG ulOffset;               /* Number of blocks */
   HB_ULONG ulSize;                 /* Block number */
   HB_BOOL  fChanged;               /* Mark the free page as changed */
} MEMOGCITEM;
typedef MEMOGCITEM * LPMEMOGCITEM;

typedef struct _MEMOGCTABLE
{
   HB_BYTE   bType;                 /* MEMO_FPT_SIX or MEMO_FPT_FLEX */
   HB_BYTE   bChanged;              /* Should we write GC data to disk */
   HB_ULONG  ulNextBlock;           /* Next free block in the file */
   HB_ULONG  ulPrevBlock;           /* Previous next free block in the file */
   HB_ULONG  ulRevPage;             /* FLEX Rev GC page offset */
   HB_ULONG  ulDirPage;             /* FLEX Dir GC page offset */
   HB_ULONG  ulCounter;             /* FLEX cyclic counter */
   HB_ULONG  ulSize;                /* FLEX GC page size in bytes */
   HB_USHORT usMaxItem;             /* max number of items in pGCitems */
   HB_USHORT usItems;               /* number of items in pGCitems */
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

#undef  SUPERTABLE
#define SUPERTABLE                         ( &fptSuper )

HB_EXTERN_END

#endif /* HB_RDDFPT_H_ */
