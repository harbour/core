/*
 * Harbour Project source code:
 * DBFNTX RDD
 *
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
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

#ifndef HB_RDDNTX_H_
#define HB_RDDNTX_H_

#include "hbrdddbf.h"

HB_EXTERN_BEGIN

/* DBFNTX default extensions */
#define NTX_INDEXEXT                             ".ntx"

/* DBFNTX constants declarations */

#define NTX_IGNORE_REC_NUM                        0x0UL
#define NTX_MAX_REC_NUM                    0xFFFFFFFFUL

#define NTX_DUMMYNODE                      0xFFFFFFFFUL

#define NTX_FLAG_DEFALUT         0x0006
#define NTX_FLAG_OLDDEFALUT      0x0003
#define NTX_FLAG_FORITEM         0x0001
#define NTX_FLAG_PARTIAL         0x0008
#define NTX_FLAG_EXTLOCK         0x0010
#define NTX_FLAG_CUSTOM          0x0020
#define NTX_FLAG_CHGONLY         0x0040
#define NTX_FLAG_TEMPLATE        0x0080
#define NTX_FLAG_SORTRECNO       0x0100
#define NTX_FLAG_LARGEFILE       0x0200
#define NTX_FLAG_MULTIKEY        0x0400
#define NTX_FLAG_COMPOUND        0x8000
#define NTX_FLAG_MASK            0x87FF

#define CTX_MAX_TAGS                 63

#define NTX_MAX_KEY                     256     /* Max len of key */
#define NTX_MAX_EXP                     256     /* Max len of KEY/FOR expression */
#define NTXBLOCKBITS                     10     /* Size of NTX block in bits */
#define NTXBLOCKSIZE      (1<<NTXBLOCKBITS)     /* Size of block in NTX file */
#define NTX_MAX_TAGNAME                  10     /* Max len of tag name */
#define NTX_HDR_UNUSED                  473     /* the unused part of header */
#define NTX_PAGES_PER_TAG                 8
#define NTX_STACKSIZE                    32     /* Maximum page stack size */

#define NTX_ROOTHEAD_HEADSIZE            12

/* index file structures - defined as HB_BYTEs to avoid alignment problems */

typedef struct _NTXHEADER     /* Header of NTX file */
{
   HB_BYTE  type[2];
   HB_BYTE  version[2];
   HB_BYTE  root[4];
   HB_BYTE  next_page[4];
   HB_BYTE  item_size[2];
   HB_BYTE  key_size[2];
   HB_BYTE  key_dec[2];
   HB_BYTE  max_item[2];
   HB_BYTE  half_page[2];
   HB_BYTE  key_expr[ NTX_MAX_EXP ];
   HB_BYTE  unique[1];
   HB_BYTE  unknown1[1];
   HB_BYTE  descend[1];
   HB_BYTE  unknown2[1];
   HB_BYTE  for_expr[ NTX_MAX_EXP ];
   HB_BYTE  tag_name[ NTX_MAX_TAGNAME + 2 ];
   HB_BYTE  custom[1];
   HB_BYTE  unused[ NTX_HDR_UNUSED ];
} NTXHEADER;
typedef NTXHEADER * LPNTXHEADER;

typedef struct
{
   HB_BYTE  type[2];
   HB_BYTE  version[2];
   HB_BYTE  root[4];
} NTXHEADERUPDT;

typedef struct _CTXTAGITEM    /* TAG item in compound NTX (CTX) header */
{
   HB_BYTE  tag_name[ NTX_MAX_TAGNAME + 2 ];
   HB_BYTE  tag_header[ 4 ];
} CTXTAGITEM;
typedef CTXTAGITEM * LPCTXTAGITEM;

typedef struct _CTXHEADER     /* Header of Harbour CTX file */
{
   HB_BYTE  type[ 2 ];     /* 0x9591 LE */
   HB_BYTE  ntags[ 2 ];    /* number of tag entries MAX63 */
   HB_BYTE  version[ 4 ];  /* update counter LE */
   HB_BYTE  freepage[ 4 ]; /* first free page in index file */
   HB_BYTE  filesize[ 4 ]; /* size of index file in pages */
   CTXTAGITEM tags[ CTX_MAX_TAGS ];
} CTXHEADER;
typedef CTXHEADER * LPCTXHEADER;

#if 0
/* original CLIP CTX file header - for information only it's binary
   compatible so both RDD can read the same file but it's not safe
   to use CLIP for writing when the file is open by Harbour.
   In spare time I'll update CLIP to respect my extensions and send
   patches to Rust - hope they will be included in CLIP.
*/
typedef struct _CTXHEADER     /* Header of CLIP CTX file */
{
   HB_BYTE  type[ 2 ];     /* 0x9591 in LE */
   HB_BYTE  ntags[ 1 ];    /* number of tag entries */
   HB_BYTE  unused[ 13 ];
   CTX_TAG tags[ 63 ];
} CTXHEADER
#endif


/* forward declarations
 */
struct _RDDFUNCS;
struct _NTXAREA;
struct _TAGINFO;
struct _NTXINDEX;

typedef struct _KEYINFO
{
   HB_ULONG Tag;      /* page number */
   HB_ULONG Xtra;     /* record number */
   char     key[ 1 ]; /* key value */
} KEYINFO;
typedef KEYINFO * LPKEYINFO;

typedef struct _TREE_STACK
{
   HB_ULONG page;
   HB_SHORT ikey;
}  TREE_STACK;
typedef TREE_STACK * LPTREESTACK;

typedef struct _HB_PAGEINFO
{
   HB_ULONG  Page;
   HB_BOOL   Changed;
   int       iUsed;
   HB_USHORT uiKeys;
   struct _HB_PAGEINFO * pNext;
   struct _HB_PAGEINFO * pPrev;
#ifdef HB_NTX_EXTERNAL_PAGEBUFFER
   char *    buffer;
#else
   char      buffer[ NTXBLOCKSIZE ];
#endif
} HB_PAGEINFO;
typedef HB_PAGEINFO * LPPAGEINFO;

typedef struct _HB_NTXSCOPE
{
   PHB_ITEM   scopeItem;
   LPKEYINFO  scopeKey;
   HB_USHORT  scopeKeyLen;
} HB_NTXSCOPE;
typedef HB_NTXSCOPE * PHB_NTXSCOPE;

typedef struct _TAGINFO
{
   char *      TagName;
   char *      KeyExpr;
   char *      ForExpr;
   PHB_ITEM    pKeyItem;
   PHB_ITEM    pForItem;
   HB_NTXSCOPE top;
   HB_NTXSCOPE bottom;

   HB_USHORT   Signature;

   HB_BOOL     fTagName;
   HB_BOOL     fUsrDescend;
   HB_BOOL     AscendKey;
   HB_BOOL     UniqueKey;

   HB_BOOL     Custom;
   HB_BOOL     ChgOnly;
   HB_BOOL     Partial;
   HB_BOOL     Template;
   HB_BOOL     MultiKey;
   HB_BOOL     fSortRec;

   HB_BOOL     HdrChanged;
   HB_BOOL     TagBOF;
   HB_BOOL     TagEOF;
   HB_ULONG    HeadBlock;
   HB_ULONG    RootBlock;
   HB_USHORT   uiNumber;
   HB_BYTE     KeyType;
   HB_USHORT   nField;
   HB_USHORT   KeyLength;
   HB_USHORT   KeyDec;
   HB_USHORT   MaxKeys;
   LPTREESTACK stack;
   HB_USHORT   stackSize;
   HB_USHORT   stackLevel;
   HB_ULONG    keyCount;
   LPKEYINFO   CurKeyInfo;
   LPKEYINFO   HotKeyInfo;
   HB_BOOL     HotFor;

   struct     _NTXINDEX * Owner;
} TAGINFO;
typedef TAGINFO * LPTAGINFO;

typedef struct _NTXINDEX
{
   char *      IndexName;
   char *      RealName;
   HB_ULONG    Version;       /* The index VERSION filed to signal index updates for other stations */
   HB_ULONG    NextAvail;
   HB_ULONG    TagBlock;      /* Index attr, next free page */
   struct     _NTXAREA * Owner;
   PHB_FILE    DiskFile;
   HB_BOOL     fDelete;       /* delete on close flag */
   HB_BOOL     fReadonly;
   HB_BOOL     fShared;
   HB_BOOL     fFlush;
   HB_BOOL     LargeFile;
   HB_BOOL     Changed;
   HB_BOOL     Update;
   HB_BOOL     Compound;
   HB_BOOL     Production;    /* Production index */
   HB_DBFLOCKDATA lockData;   /* index lock data */
   int         lockWrite;     /* number of write lock set */
   int         lockRead;      /* number of read lock set */

   HB_BYTE *   HeaderBuff;    /* TODO: make it member */
   HB_BOOL     fValidHeader;
   int         iTags;
   LPTAGINFO * lpTags;

   HB_ULONG    ulPages;
   HB_ULONG    ulPageLast;
   HB_ULONG    ulPagesDepth;
   LPPAGEINFO *pages;
   LPPAGEINFO  pChanged;
   LPPAGEINFO  pFirst;
   LPPAGEINFO  pLast;

   struct     _NTXINDEX * pNext;   /* The next index in the list */
} NTXINDEX;
typedef NTXINDEX * LPNTXINDEX;

/* for index creation */
typedef struct
{
   HB_FOFFSET  nOffset;    /* offset in temporary file */
   HB_ULONG    ulKeys;     /* number of keys in page */
   HB_ULONG    ulKeyBuf;   /* number of keys in memory buffer */
   HB_ULONG    ulCurKey;   /* current key in memory buffer */
   HB_BYTE *   pKeyPool;   /* memory buffer */
} NTXSWAPPAGE;
typedef NTXSWAPPAGE * LPNTXSWAPPAGE;

typedef struct
{
   LPTAGINFO  pTag;           /* current Tag */
   HB_FHANDLE hTempFile;      /* handle to temporary file */
   char *     szTempFileName; /* temporary file name */
   int        keyLen;         /* key length */
   HB_BOOL    fUnique;        /* HB_TRUE if index is unique */
   HB_BOOL    fReindex;       /* HB_TRUE if reindexing is in process */
   HB_ULONG   ulMaxRec;       /* the highest record number */
   HB_ULONG   ulTotKeys;      /* total number of keys indexed */
   HB_ULONG   ulKeys;         /* keys in curently created page */
   HB_ULONG   ulPages;        /* number of pages */
   HB_ULONG   ulCurPage;      /* current page */
   HB_ULONG   ulPgKeys;       /* maximum number of key in page memory buffer */
   HB_ULONG   ulMaxKey;       /* maximum number of keys in single page */
   HB_BYTE *  pKeyPool;       /* memory buffer for current page then for pages */
   HB_BYTE *  pStartKey;      /* begining of key pool after sorting */
   LPNTXSWAPPAGE pSwapPage;   /* list of pages */
   LPPAGEINFO NodeList[ NTX_STACKSIZE ];   /* Stack of pages */
   HB_ULONG   ulFirst;
   HB_ULONG * pSortedPages;
   HB_BYTE    pLastKey[ NTX_MAX_KEY ]; /* last key val */
   HB_ULONG   ulLastRec;

   HB_BYTE *  pBuffIO;        /* index IO buffer */
   HB_ULONG   ulSizeIO;       /* size of IO buffer in index pages */
   HB_ULONG   ulPagesIO;      /* number of index pages in buffer */
   HB_ULONG   ulFirstIO;      /* first page in buffer */
   HB_ULONG   ulLastIO;       /* last page in buffer */
} NTXSORTINFO;
typedef NTXSORTINFO * LPNTXSORTINFO;

/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBFNTX RDD
 *
 */

typedef struct _NTXAREA
{
   DBFAREA dbfarea;

   /*
   *  NTX's additions to the workarea structure
   *
   *  Warning: The above section MUST match DBFAREA exactly! Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   HB_BOOL        fNtxAppend;       /* HB_TRUE if new record is added */
   HB_BOOL        fSetTagNumbers;   /* Tag number should be recreated */
   LPNTXINDEX     lpIndexes;        /* Pointer to list of indexes */
   LPTAGINFO      lpCurTag;         /* Pointer to current order */
   LPNTXSORTINFO  pSort;            /* Index build structure */

} NTXAREA;
typedef NTXAREA * LPNTXAREA;

#ifndef NTXAREAP
#define NTXAREAP LPNTXAREA
#endif

#undef  SUPERTABLE
#define SUPERTABLE                         ( &ntxSuper )

HB_EXTERN_END

#endif /* HB_RDDNTX_H_ */
