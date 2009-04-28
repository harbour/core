/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBFNTX RDD
 *
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
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
#define NTX_TAGITEMSIZE                  16     /* Size of tag item in CTX header */
#define NTX_HDR_UNUSED                  473     /* the unused part of header */
#define NTX_PAGES_PER_TAG                 8
#define NTX_STACKSIZE                    32     /* Maximum page stack size */

/* index file structures - defined as BYTEs to avoid alignment problems */

typedef struct _NTXHEADER     /* Header of NTX file */
{
   BYTE  type[2];
   BYTE  version[2];
   BYTE  root[4];
   BYTE  next_page[4];
   BYTE  item_size[2];
   BYTE  key_size[2];
   BYTE  key_dec[2];
   BYTE  max_item[2];
   BYTE  half_page[2];
   BYTE  key_expr[ NTX_MAX_EXP ];
   BYTE  unique[1];
   BYTE  unknown1[1];
   BYTE  descend[1];
   BYTE  unknown2[1];
   BYTE  for_expr[ NTX_MAX_EXP ];
   BYTE  tag_name[ NTX_MAX_TAGNAME + 2 ];
   BYTE  custom[1];
   BYTE  unused[ NTX_HDR_UNUSED ];
} NTXHEADER;
typedef NTXHEADER * LPNTXHEADER;

typedef struct _CTXTAGITEM    /* TAG item in compound NTX (CTX) header */
{
   BYTE  tag_name[ NTX_MAX_TAGNAME + 2 ];
   BYTE  tag_header[ 4 ];
} CTXTAGITEM;
typedef CTXTAGITEM * LPCTXTAGITEM;

typedef struct _CTXHEADER     /* Header of xHarbour CTX file */
{
   BYTE  type[ 2 ];     /* 0x9591 LE */
   BYTE  ntags[ 2 ];    /* number of tag entries MAX63 */
   BYTE  version[ 4 ];  /* update counter LE */
   BYTE  freepage[ 4 ]; /* first free page in index file */
   BYTE  filesize[ 4 ]; /* size of index file in pages */
   BYTE  tags[ CTX_MAX_TAGS * NTX_TAGITEMSIZE ];
} CTXHEADER;
typedef CTXHEADER * LPCTXHEADER;

#if 0
/* original CLIP CTX file header - for information only it's binary
   compatible so both RDD can read the same file but it's not safe
   to use CLIP for writing when the file is open by xHarbour.
   In spare time I'll update CLIP to respect my extensions and send
   patches to Rust - hope they will be included in CLIP.
*/
typedef struct _CTXHEADER     /* Header of CLIP CTX file */
{
   BYTE  type[ 2 ];     /* 0x9591 in LE */
   BYTE  ntags[ 1 ];    /* number of tag entries */
   BYTE  unused[ 13 ];
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
   ULONG    Tag;      /* page number */
   ULONG    Xtra;     /* record number */
   char     key[ 1 ]; /* key value */
} KEYINFO;
typedef KEYINFO * LPKEYINFO;

typedef struct _TREE_STACK
{
   ULONG    page;
   SHORT    ikey;
}  TREE_STACK;
typedef TREE_STACK * LPTREESTACK;

typedef struct _HB_PAGEINFO
{
   ULONG    Page;
   BOOL     Changed;
   int      iUsed;
   USHORT   uiKeys;
   struct  _HB_PAGEINFO * pNext;
   struct  _HB_PAGEINFO * pPrev;
#ifdef HB_NTX_EXTERNAL_PAGEBUFFER
   char *   buffer;
#else
   char     buffer[ NTXBLOCKSIZE ];
#endif
} HB_PAGEINFO;
typedef HB_PAGEINFO * LPPAGEINFO;

typedef struct _HB_NTXSCOPE
{
   PHB_ITEM   scopeItem;
   LPKEYINFO  scopeKey;
   USHORT     scopeKeyLen;
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

   USHORT      Signature;

   BOOL        fTagName;
   BOOL        fUsrDescend;
   BOOL        AscendKey;
   BOOL        UniqueKey;

   BOOL        Custom;
   BOOL        ChgOnly;
   BOOL        Partial;
   BOOL        Template;
   BOOL        MultiKey;
   BOOL        fSortRec;

   BOOL        HdrChanged;
   BOOL        TagBOF;
   BOOL        TagEOF;
   ULONG       HeadBlock;
   ULONG       RootBlock;
   USHORT      uiNumber;
   BYTE        KeyType;
   USHORT      nField;
   USHORT      KeyLength;
   USHORT      KeyDec;
   USHORT      MaxKeys;
   LPTREESTACK stack;
   USHORT      stackSize;
   USHORT      stackLevel;
   ULONG       keyCount;
   LPKEYINFO   CurKeyInfo;
   LPKEYINFO   HotKeyInfo;
   BOOL        HotFor;

   struct     _NTXINDEX * Owner;
} TAGINFO;
typedef TAGINFO * LPTAGINFO;

typedef struct _NTXINDEX
{
   char *      IndexName;
   char *      RealName;
   ULONG       Version;       /* The index VERSION filed to signal index updates for other stations */
   ULONG       NextAvail;
   ULONG       TagBlock;      /* Index attr, next free page */
   struct     _NTXAREA * Owner;
   PHB_FILE    DiskFile;
   BOOL        fDelete;       /* delete on close flag */
   BOOL        fReadonly;
   BOOL        fShared;
   BOOL        fFlush;
   BOOL        LargeFile;
   BOOL        Changed;
   BOOL        Update;
   BOOL        Compound;
   BOOL        Production;    /* Production index */
   HB_FOFFSET  ulLockPos;     /* readlock position for CL53 lock scheme */
   int         lockWrite;     /* number of write lock set */
   int         lockRead;      /* number of read lock set */

   BYTE *      HeaderBuff;    /* TODO: make it member */
   BOOL        fValidHeader;
   int         iTags;
   LPTAGINFO * lpTags;

   ULONG       ulPages;
   ULONG       ulPageLast;
   ULONG       ulPagesDepth;
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
   ULONG       ulKeys;     /* number of keys in page */
   ULONG       ulKeyBuf;   /* number of keys in memory buffer */
   ULONG       ulCurKey;   /* current key in memory buffer */
   BYTE *      pKeyPool;   /* memory buffer */
} NTXSWAPPAGE;
typedef NTXSWAPPAGE * LPNTXSWAPPAGE;

typedef struct
{
   LPTAGINFO pTag;            /* current Tag */
   HB_FHANDLE hTempFile;      /* handle to temporary file */
   char *   szTempFileName;   /* temporary file name */
   int      keyLen;           /* key length */
   BOOL     fUnique;          /* TRUE if index is unique */
   BOOL     fReindex;         /* TRUE if reindexing is in process */
   ULONG    ulMaxRec;         /* the highest record number */
   ULONG    ulTotKeys;        /* total number of keys indexed */
   ULONG    ulKeys;           /* keys in curently created page */
   ULONG    ulPages;          /* number of pages */
   ULONG    ulCurPage;        /* current page */
   ULONG    ulPgKeys;         /* maximum number of key in page memory buffer */
   ULONG    ulMaxKey;         /* maximum number of keys in single page */
   BYTE *   pKeyPool;         /* memory buffer for current page then for pages */
   BYTE *   pStartKey;        /* begining of key pool after sorting */
   LPNTXSWAPPAGE pSwapPage;   /* list of pages */
   LPPAGEINFO NodeList[ NTX_STACKSIZE ];   /* Stack of pages */
   ULONG    ulFirst;
   ULONG *  pSortedPages;
   BYTE     pLastKey[ NTX_MAX_KEY ]; /* last key val */
   ULONG    ulLastRec;

   BYTE *   pBuffIO;          /* index IO buffer */
   ULONG    ulSizeIO;         /* size of IO buffer in index pages */
   ULONG    ulPagesIO;        /* number of index pages in buffer */
   ULONG    ulFirstIO;        /* first page in buffer */
   ULONG    ulLastIO;         /* last page in buffer */
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
   struct _RDDFUNCS * lprfsHost; /* Virtual method table for this workarea */
   USHORT uiArea;                /* The number assigned to this workarea */
   void * atomAlias;             /* Pointer to the alias symbol for this workarea */
   USHORT uiFieldExtent;         /* Total number of fields allocated */
   USHORT uiFieldCount;          /* Total number of fields used */
   LPFIELD lpFields;             /* Pointer to an array of fields */
   void * lpFieldExtents;        /* Void ptr for additional field properties */
   PHB_ITEM valResult;           /* All purpose result holder */
   BOOL fTop;                    /* TRUE if "top" */
   BOOL fBottom;                 /* TRUE if "bottom" */
   BOOL fBof;                    /* TRUE if "bof" */
   BOOL fEof;                    /* TRUE if "eof" */
   BOOL fFound;                  /* TRUE if "found" */
   DBSCOPEINFO dbsi;             /* Info regarding last LOCATE */
   DBFILTERINFO dbfi;            /* Filter in effect */
   LPDBORDERCONDINFO lpdbOrdCondInfo;
   LPDBRELINFO lpdbRelations;    /* Parent/Child relationships used */
   USHORT uiParents;             /* Number of parents for this area */
   USHORT heap;
   USHORT heapSize;
   USHORT rddID;
   USHORT uiMaxFieldNameLength;
   PHB_CODEPAGE cdPage;          /* Area's codepage pointer */

   /*
   *  DBFS's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   PHB_FILE pDataFile;              /* Data file handle */
   PHB_FILE pMemoFile;              /* Memo file handle */
   PHB_FILE pMemoTmpFile;           /* Memo temporary file handle */
   char *   szDataFileName;         /* Name of data file */
   char *   szMemoFileName;         /* Name of memo file */
   USHORT   uiHeaderLen;            /* Size of header */
   USHORT   uiRecordLen;            /* Size of record */
   USHORT   uiMemoBlockSize;        /* Size of memo block */
   USHORT   uiNewBlockSize;         /* Size of new memo block */
   USHORT   uiMemoVersion;          /* MEMO file version */
   USHORT   uiDirtyRead;            /* Index dirty read bit filed */
   USHORT   uiNullOffset;           /* Offset to _NullFlags filed */
   USHORT   uiNullCount;            /* Number of null flags */
   BYTE     bTableType;             /* DBF type */
   BYTE     bMemoType;              /* MEMO type used in DBF memo fields */
   BYTE     bLockType;              /* Type of locking shemes */
   BYTE     bCryptType;             /* Type of used encryption */
   DBFHEADER dbfHeader;             /* DBF header buffer */
   USHORT * pFieldOffset;           /* Pointer to field offset array */
   PHB_DBFFIELDBITS pFieldBits;     /* Pointer to extended DBF field info array */
   BYTE *   pRecord;                /* Buffer of record data */
   ULONG    ulRecCount;             /* Total records */
   ULONG    ulRecNo;                /* Current record */
   BOOL     fAutoInc;               /* WorkArea with auto increment fields */
   BOOL     fHasMemo;               /* WorkArea with Memo fields */
   BOOL     fHasTags;               /* WorkArea with MDX or CDX index */
   BOOL     fModStamp;              /* WorkArea with modification autoupdate fields */
   BOOL     fDataFlush;             /* data was written to DBF and not commited */
   BOOL     fMemoFlush;             /* data was written to MEMO and not commited */
   BOOL     fShared;                /* Shared file */
   BOOL     fReadonly;              /* Read only file */
   BOOL     fTemporary;             /* Temporary file */
   BOOL     fValidBuffer;           /* State of buffer */
   BOOL     fPositioned;            /* Positioned record */
   BOOL     fRecordChanged;         /* Record changed */
   BOOL     fAppend;                /* TRUE if new record is added */
   BOOL     fDeleted;               /* TRUE if record is deleted */
   BOOL     fEncrypted;             /* TRUE if record is encrypted */
   BOOL     fTableEncrypted;        /* TRUE if table is encrypted */
   BOOL     fUpdateHeader;          /* Update header of file */
   BOOL     fFLocked;               /* TRUE if file is locked */
   BOOL     fHeaderLocked;          /* TRUE if DBF header is locked */
   BOOL     fPackMemo;              /* Pack memo file in pack operation */
   BOOL     fTrigger;               /* Execute trigger function */
   LPDBOPENINFO lpdbOpenInfo;       /* Pointer to current dbOpenInfo structure in OPEN/CREATE methods */
   LPDBRELINFO lpdbPendingRel;      /* Pointer to parent rel struct */
   ULONG *  pLocksPos;              /* List of records locked */
   ULONG    ulNumLocksPos;          /* Number of records locked */
   BYTE *   pCryptKey;              /* Pointer to encryption key */
   PHB_DYNS pTriggerSym;            /* DynSym pointer to trigger function */

   /*
   *  NTX's additions to the workarea structure
   *
   *  Warning: The above section MUST match DBFAREA exactly! Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   BOOL           fNtxAppend;       /* TRUE if new record is added */
   BOOL           fSetTagNumbers;   /* Tag number should be recreated */
   LPNTXINDEX     lpIndexes;        /* Pointer to list of indexes */
   LPTAGINFO      lpCurTag;         /* Pointer to current order */
   LPNTXSORTINFO  pSort;            /* Index build structure */

} NTXAREA;
typedef NTXAREA * LPNTXAREA;

#ifndef NTXAREAP
#define NTXAREAP LPNTXAREA
#endif


/*
 * -- DBFNTX METHODS --
 */

#define SUPERTABLE                         ( &ntxSuper )

#define ntxBof                   NULL
#define ntxEof                   NULL
#define ntxFound                 NULL
static HB_ERRCODE ntxGoBottom( NTXAREAP pArea );
#define ntxGoTo                  NULL
#define ntxGoToId                NULL
static HB_ERRCODE ntxGoTop( NTXAREAP pArea );
static HB_ERRCODE ntxSeek( NTXAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast );
#define ntxSkip                  NULL
#define ntxSkipFilter            NULL
static HB_ERRCODE ntxSkipRaw( NTXAREAP pArea, LONG lToSkip );
#define ntxAddField              NULL
/* static HB_ERRCODE ntxAppend( NTXAREAP pArea, BOOL bUnLockAll ); */
#define ntxAppend                NULL
#define ntxCreateFields          NULL
#define ntxDeleteRec             NULL
#define ntxDeleted               NULL
#define ntxFieldCount            NULL
#define ntxFieldDisplay          NULL
#define ntxFieldInfo             NULL
#define ntxFieldName             NULL
static HB_ERRCODE ntxFlush( NTXAREAP pArea );
#define ntxGetRec                NULL
#define ntxGetValue              NULL
#define ntxGetVarLen             NULL
static HB_ERRCODE ntxGoCold( NTXAREAP pArea );
static HB_ERRCODE ntxGoHot( NTXAREAP pArea );
#define ntxPutRec                NULL
#define ntxPutValue              NULL
#define ntxRecall                NULL
#define ntxRecCount              NULL
#define ntxRecInfo               NULL
#define ntxRecNo                 NULL
#define ntxRecId                 NULL
#define ntxSetFieldsExtent       NULL
#define ntxAlias                 NULL
static HB_ERRCODE ntxClose( NTXAREAP pArea );
         /* Close workarea - at first we mus close all indexes and than close
            workarea */
#define ntxCreate                NULL
#define ntxInfo                  NULL
#define ntxNewArea               NULL
static HB_ERRCODE ntxOpen( NTXAREAP pArea, LPDBOPENINFO pOpenInfo );
#define ntxRelease               NULL
static HB_ERRCODE ntxStructSize( NTXAREAP pArea, USHORT * uiSize );
#define ntxSysName               NULL
#define ntxEval                  NULL
static HB_ERRCODE ntxPack( NTXAREAP pArea );
#define ntPackRec                NULL
#define ntxSort                  NULL
#define ntxTrans                 NULL
#define ntxTransRec              NULL
static HB_ERRCODE ntxZap( NTXAREAP pArea );
#define ntxchildEnd              NULL
#define ntxchildStart            NULL
#define ntxchildSync             NULL
#define ntxsyncChildren          NULL
#define ntxclearRel              NULL
#define ntxforceRel              NULL
#define ntxrelArea               NULL
#define ntxrelEval               NULL
#define ntxrelText               NULL
#define ntxsetRel                NULL
static HB_ERRCODE ntxOrderListAdd( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
static HB_ERRCODE ntxOrderListClear( NTXAREAP pArea );
static HB_ERRCODE ntxOrderListDelete( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
static HB_ERRCODE ntxOrderListFocus( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
static HB_ERRCODE ntxOrderListRebuild( NTXAREAP pArea );
#define ntxOrderCondition        NULL
static HB_ERRCODE ntxOrderCreate( NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo );
static HB_ERRCODE ntxOrderDestroy( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
static HB_ERRCODE ntxOrderInfo( NTXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pInfo );
#define ntxClearFilter           NULL
#define ntxClearLocate           NULL
#define ntxClearScope            NULL
static HB_ERRCODE ntxCountScope( NTXAREAP pArea, void * pPtr, LONG * plRecNo );
#define ntxFilterText            NULL
#define ntxScopeInfo             NULL
#define ntxSetFilter             NULL
#define ntxSetLocate             NULL
#define ntxSetScope              NULL
#define ntxSkipScope             NULL
#define ntxLocate                NULL
#define ntxCompile               NULL
#define ntxError                 NULL
#define ntxEvalBlock             NULL
#define ntxRawLock               NULL
#define ntxLock                  NULL
#define ntxUnLock                NULL
#define ntxCloseMemFile          NULL
#define ntxCreateMemFile         NULL
#define ntxGetValueFile          NULL
#define ntxOpenMemFile           NULL
#define ntxPutValueFile          NULL
#define ntxReadDBHeader          NULL
#define ntxWriteDBHeader         NULL
static HB_ERRCODE ntxInit( LPRDDNODE pRDD );
#define ntxExit                  NULL
#define ntxDrop                  NULL
#define ntxExists                NULL
static HB_ERRCODE ntxRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem );
#define ntxWhoCares              NULL

HB_EXTERN_END

#endif /* HB_RDDNTX_H_ */
