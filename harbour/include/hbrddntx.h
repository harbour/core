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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifndef HB_RDDNTX_H_
#define HB_RDDNTX_H_

#include "hbapirdd.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif


/* DBFNTX errors */

#define EDBF_OPEN_DBF                              1001
#define EDBF_CREATE_DBF                            1004
#define EDBF_READ                                  1010
#define EDBF_WRITE                                 1011
#define EDBF_CORRUPT                               1012
#define EDBF_DATATYPE                              1020
#define EDBF_DATAWIDTH                             1021
#define EDBF_UNLOCKED                              1022
#define EDBF_SHARED                                1023
#define EDBF_APPENDLOCK                            1024
#define EDBF_READONLY                              1025
#define EDBF_INVALIDKEY                            1026



/* DBFNTX default extensions */
#define NTX_MEMOEXT                               ".dbt"
#define NTX_INDEXEXT                              ".ntx"

/* DBFNTX constants declarations */

#define TOP_RECORD                                                      1
#define BTTM_RECORD                                                     2
#define PREV_RECORD                                                     3
#define NEXT_RECORD                                                     4

#define NTX_MAX_REC_NUM                                       0x7FFFFFFFL
#define NTX_IGNORE_REC_NUM                                             -1

#define NTX_MAX_KEY  250      /* Max len of key */
#define NTXBLOCKSIZE 1024     /* Size of block in NTX file */

/* forward declarations
 */
struct _RDDFUNCS;
struct _NTXAREA;
struct _TAGINFO;
struct _NTXINDEX;

typedef struct _KEYINFO
{
   PHB_ITEM pItem;
   LONG     Tag;
   LONG     Xtra;
   // struct  _KEYINFO * pNext;
} KEYINFO;

typedef KEYINFO * LPKEYINFO;


typedef struct HB_PAGEINFO_STRU
{
   LONG      Page;
   BOOL      Changed;
   BOOL      NewRoot;
   BOOL      lBusy;
   BYTE      PageType;
   LPKEYINFO pKeys;
   USHORT    uiKeys;
   SHORT     CurKey;
   struct   HB_PAGEINFO_STRU * pPrev;
   struct   HB_PAGEINFO_STRU * pNext;
   struct   _TAGINFO * TagParent;
} HB_PAGEINFO;

typedef HB_PAGEINFO * LPPAGEINFO;


typedef struct _TAGINFO
{
   char *     TagName;
   char *     KeyExpr;
   char *     ForExpr;
   PHB_ITEM   pKeyItem;
   PHB_ITEM   pForItem;
   BOOL       AscendKey;
   BOOL       UniqueKey;
   BOOL       TagChanged;
   BOOL       TagBOF;
   BOOL       TagEOF;
   BYTE       KeyType;
   BYTE       OptFlags;
   LONG       TagBlock;
   LONG       RootBlock;
   USHORT     uiPages;
   USHORT     KeyLength;
   USHORT     MaxKeys;
   LPKEYINFO  CurKeyInfo;
   LPPAGEINFO RootPage;
   struct    _NTXINDEX * Owner;
   struct    _TAGINFO * pNext;
} TAGINFO;

typedef TAGINFO * LPTAGINFO;

typedef struct _NTXINDEX
{
   char *    IndexName;
   BOOL      Exact;
   BOOL      Corrupted;
   LONG      TagRoot;
   LONG      NextAvail;
   struct   _NTXAREA * Owner;
   FHANDLE   DiskFile;
   LPTAGINFO CompoundTag;
   struct   _NTXINDEX * pNext;   /* The next index in the list */
} NTXINDEX;

typedef NTXINDEX * LPNTXINDEX;

/* Internal structures used by saving file */

typedef struct _NTXHEADER    /* Header of NTX file */
{
   USHORT   type;
   USHORT   version;
   ULONG    root;
   ULONG    next_page;
   USHORT   item_size;
   USHORT   key_size;
   USHORT   key_dec;
   USHORT   max_item;
   USHORT   half_page;
   char     key_expr[ NTX_MAX_KEY ];
   char     unique;
} NTXHEADER;

typedef NTXHEADER * LPNTXHEADER;

typedef struct _NTXBUFFER    /* Header of each block in NTX file (only block
                                with header has other format */
{
   USHORT   item_count;
   USHORT   item_offset[ 1 ];
} NTXBUFFER;

typedef NTXBUFFER * LPNTXBUFFER;

typedef struct _NTXITEM      /* each item in NTX block has following format */
{
   ULONG    page;     /* subpage (each key in subpage has < value like this key */
   ULONG    rec_no;   /* RecNo of record with this key */
   char     key[ 1 ]; /* value of key */
} NTXITEM;

typedef NTXITEM * LPNTXITEM;

struct _NTXAREA;

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

   /*
   *  DBFS's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   FHANDLE hDataFile;            /* Data file handle */
   FHANDLE hMemoFile;            /* Memo file handle */
   USHORT uiHeaderLen;           /* Size of header */
   USHORT uiRecordLen;           /* Size of record */
   ULONG ulRecCount;             /* Total records */
   char * szDataFileName;        /* Name of data file */
   char * szMemoFileName;        /* Name of memo file */
   BOOL fHasMemo;                /* WorkArea with Memo fields */
   BOOL fHasTags;                /* WorkArea with MDX or CDX index */
   BOOL fShared;                 /* Shared file */
   BOOL fReadonly;               /* Read only file */
   USHORT * pFieldOffset;        /* Pointer to field offset array */
   BYTE * pRecord;               /* Buffer of record data */
   BOOL fValidBuffer;            /* State of buffer */
   BOOL fPositioned;             /* Positioned record */
   ULONG ulRecNo;                /* Current record */
   BOOL fRecordChanged;          /* Record changed */
   BOOL fAppend;                 /* TRUE if new record is added */
   BOOL fDeleted;                /* TRUE if record is deleted */
   BOOL fUpdateHeader;           /* Update header of file */
   BOOL fFLocked;                /* TRUE if file is locked */
   LPDBRELINFO lpdbPendingRel;   /* Pointer to parent rel struct */
   BYTE bYear;                   /* Last update */
   BYTE bMonth;
   BYTE bDay;
   ULONG * pLocksPos;            /* List of records locked */
   ULONG ulNumLocksPos;          /* Number of records locked */

   /*
   *  NTX's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   LPNTXINDEX lpCurIndex;         /* Pointer to current index */
   LPNTXINDEX lpNtxIndex;         /* Pointer to indexes array */


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
static ERRCODE ntxGoBottom( NTXAREAP pArea );
static ERRCODE ntxGoTo( NTXAREAP pArea, ULONG ulRecNo );
#define ntxGoToId                NULL
static ERRCODE ntxGoTop( NTXAREAP pArea );
static ERRCODE ntxSeek( NTXAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast );
#define ntxSkip                  NULL
#define ntxSkipFilter            NULL
static ERRCODE ntxSkipRaw( NTXAREAP pArea, LONG lToSkip );
#define ntxAddField              NULL
static ERRCODE ntxAppend( NTXAREAP pArea, BOOL bUnLockAll );
#define ntxCreateFields          NULL
#define ntxDeleteRec             NULL
#define ntxDeleted               NULL
#define ntxFieldCount            NULL
#define ntxFieldDisplay          NULL
#define ntxFieldInfo             NULL
#define ntxFieldName             NULL
#define ntxFlush                 NULL
#define ntxGetRec                NULL
#define ntxGetValue              NULL
#define ntxGetVarLen             NULL
#define ntxGoCold                NULL
#define ntxGoHot                 NULL
#define ntxPutRec                NULL
static ERRCODE ntxPutValue( NTXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
#define ntxRecall                NULL
#define ntxRecCount              NULL
#define ntxRecInfo               NULL
#define ntxRecNo                 NULL
#define ntxSetFieldsExtent       NULL
#define ntxAlias                 NULL
static ERRCODE ntxClose( NTXAREAP pArea );
         /* Close workarea - at first we mus close all indexes and than close
            workarea */
#define ntxCreate                NULL
#define ntxInfo                  NULL
#define ntxNewArea               NULL
#define ntxOpen                  NULL
#define ntxRelease               NULL
static ERRCODE ntxStructSize( NTXAREAP pArea, USHORT * uiSize );
#define ntxSysName               NULL
#define ntxEval                  NULL
#define ntxPack                  NULL
#define ntPackRec                NULL
#define ntxSort                  NULL
#define ntxTrans                 NULL
#define ntxTransRec              NULL
#define ntxZap                   NULL
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
static ERRCODE ntxOrderListAdd( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
         /* Open next index */
static ERRCODE ntxOrderListClear( NTXAREAP pArea );
         /* Close all indexes */
#define ntxOrderListDelete       NULL
static ERRCODE ntxOrderListFocus( NTXAREAP pArea, LPDBORDERINFO pOrderInfo );
#define ntxOrderListRebuild      NULL
#define ntxOrderCondition        NULL
static ERRCODE ntxOrderCreate( NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo );
         /* Create new Index */
#define ntxOrderDestroy          NULL
static ERRCODE ntxOrderInfo( NTXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pInfo );
         /* Some information about index */
#define ntxClearFilter           NULL
#define ntxClearLocate           NULL
#define ntxClearScope            NULL
#define ntxCountScope            NULL
#define ntxFilterText            NULL
#define ntxScopeInfo             NULL
#define ntxSetFilter             NULL
#define ntxSetLocate             NULL
#define ntxSetScope              NULL
#define ntxSkipScope             NULL
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
#define ntxWhoCares              NULL

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_RDDNTX_H_ */
