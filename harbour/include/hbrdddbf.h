/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBF RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef HB_RDDDBF_H_
#define HB_RDDDBF_H_

#include "hbapirdd.h"
#include "hbdbferr.h"
#include "hbdbf.h"

HB_EXTERN_BEGIN

/* DBF default file extensions */
#define DBF_TABLEEXT                      ".dbf"

/* DBF locking schemes */
#define DBF_LOCKPOS_CLIP                  1000000000UL
#define DBF_LOCKPOS_CL53                  1000000000UL
#define DBF_LOCKPOS_VFP                   0x40000000UL
#define DBF_LOCKPOS_VFPX                  0x7ffffffeUL
#define DBF_LOCKPOS_CL53EXT               4000000000UL
#define DBF_LOCKPOS_XHB64                 HB_LL( 0x7FFFFFFF00000001 )

#define DBF_LOCKDIR_CLIP                  1
#define DBF_LOCKDIR_CL53                  1
#define DBF_LOCKDIR_VFP                   2  /* lock forward at at record offset */
#define DBF_LOCKDIR_VFPX                  -1
#define DBF_LOCKDIR_CL53EXT               1
#define DBF_LOCKDIR_XHB64                 1

#define DBF_FLCKSIZE_CLIP                 1000000000UL
#define DBF_FLCKSIZE_CL53                 1000000000UL
#define DBF_FLCKSIZE_VFP                  0x3ffffffdUL
#define DBF_FLCKSIZE_VFPX                 0x07ffffffUL
#define DBF_FLCKSIZE_CL53EXT              294967295UL
#define DBF_FLCKSIZE_XHB64                0x7ffffffeUL

#define DBF_RLCKSIZE_CLIP                 1UL
#define DBF_RLCKSIZE_CL53                 1UL
#define DBF_RLCKSIZE_VFP                  1UL
#define DBF_RLCKSIZE_VFPX                 1UL
#define DBF_RLCKSIZE_CL53EXT              1UL
#define DBF_RLCKSIZE_XHB64                1UL

#define IDX_LOCKPOS_CLIP                  1000000000UL
#define IDX_LOCKPOS_CL53                  0xfffeffffUL
#define IDX_LOCKPOS_VFP                   0x7ffffffeUL
#define IDX_LOCKPOS_CL53EXT               0xfffeffffUL
#define IDX_LOCKPOS_XHB64                 HB_LL( 0x7FFFFFFF00000001 )

#define IDX_LOCKPOOL_CLIP                 0UL
#define IDX_LOCKPOOL_CL53                 0x00010000UL
#define IDX_LOCKPOOL_VFP                  0UL
#define IDX_LOCKPOOL_CL53EXT              0x00010000UL
#define IDX_LOCKPOOL_XHB64                0UL


/* Index dirty read flags */
#define HB_IDXREAD_CLEAN      0
#define HB_IDXREAD_DEFAULT    1
#define HB_IDXREAD_DIRTY      2

#define HB_IDXREAD_CLEANMASK  HB_IDXREAD_DIRTY
#define HB_IDXREAD_DIRTYMASK  (HB_IDXREAD_DIRTY|HB_IDXREAD_DEFAULT)

#define DBFNODE_DATA( r )     ( ( LPDBFDATA ) hb_stackGetTSD( ( PHB_TSD ) \
                                                      ( r )->lpvCargo ) )
#define DBFAREA_DATA( w )     DBFNODE_DATA( SELF_RDDNODE( w ) )


#define HB_DIRTYREAD( w )     ( ( DBFAREA_DATA( w )->uiDirtyRead & \
                                              ( w )->uiDirtyRead ) != 0 )


/*
 * Private DBF* RDD data kept in RDDNODE
 */
typedef struct _DBFDATA
{
   char     szTableExt[ HB_MAX_FILE_EXT + 1 ];
   char     szIndexExt[ HB_MAX_FILE_EXT + 1 ];
   char     szMemoExt[ HB_MAX_FILE_EXT + 1 ];

   char *   szPasswd;
   char *   szPendingPasswd;
   char *   szTrigger;
   char *   szPendingTrigger;

   BYTE     bLockType;        /* 0 */
   BYTE     bTableType;       /* DB_DBF_STD */
   BYTE     bCryptType;       /* DB_CRYPT_NONE */
   BYTE     bMemoType;        /* DB_MEMO_FPT */
   BYTE     bMemoExtType;     /* DB_MEMOVER_FLEX */
   USHORT   uiDirtyRead;      /* HB_IDXREAD_CLEANMASK */
   USHORT   uiMemoBlockSize;  /* 0 */

   BOOL     fSortRecNo;
   BOOL     fMultiKey;
   BOOL     fStruct;
   BOOL     fStrictStruct;
   BOOL     fMultiTag;
} DBFDATA, * LPDBFDATA;

typedef struct _HB_DBFFIELDBITS
{
   USHORT   uiNullBit;
   USHORT   uiLengthBit;
} HB_DBFFIELDBITS, * PHB_DBFFIELDBITS;


/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBF RDD
 *
 */

typedef struct _DBFAREA
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
   char *   pCryptKey;              /* Pointer to encryption key */
   PHB_DYNS pTriggerSym;            /* DynSym pointer to trigger function */
} DBFAREA;

typedef DBFAREA * LPDBFAREA;

#ifndef DBFAREAP
#define DBFAREAP LPDBFAREA
#endif

#ifdef _HB_RDDDBF_INTERNAL_

/*
 * -- DBF METHODS --
 */

#define SUPERTABLE                         ( &dbfSuper )

static HB_ERRCODE hb_dbfBof( DBFAREAP pArea, BOOL * pBof );
static HB_ERRCODE hb_dbfEof( DBFAREAP pArea, BOOL * pEof );
static HB_ERRCODE hb_dbfFound( DBFAREAP pArea, BOOL * pFound );
static HB_ERRCODE hb_dbfGoBottom( DBFAREAP pArea );
static HB_ERRCODE hb_dbfGoTo( DBFAREAP pArea, ULONG ulRecNo );
static HB_ERRCODE hb_dbfGoToId( DBFAREAP pArea, PHB_ITEM pItem );
static HB_ERRCODE hb_dbfGoTop( DBFAREAP pArea );
#define hb_dbfSeek                                 NULL
static HB_ERRCODE hb_dbfSkip( DBFAREAP pArea, LONG lToSkip );
#define hb_dbfSkipFilter                           NULL
static HB_ERRCODE hb_dbfSkipRaw( DBFAREAP pArea, LONG lToSkip );
static HB_ERRCODE hb_dbfAddField( DBFAREAP pArea, LPDBFIELDINFO pFieldInfo );
static HB_ERRCODE hb_dbfAppend( DBFAREAP pArea, BOOL bUnLockAll );
#define hb_dbfCreateFields                         NULL
static HB_ERRCODE hb_dbfDeleteRec( DBFAREAP pArea );
static HB_ERRCODE hb_dbfDeleted( DBFAREAP pArea, BOOL * pDeleted );
#define hb_dbfFieldCount                           NULL
#define hb_dbfFieldDisplay                         NULL
static HB_ERRCODE hb_dbfFieldInfo( DBFAREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem );
#define hb_dbfFieldName                            NULL
static HB_ERRCODE hb_dbfFlush( DBFAREAP pArea );
static HB_ERRCODE hb_dbfGetRec( DBFAREAP pArea, BYTE ** pBuffer );
static HB_ERRCODE hb_dbfGetValue( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
static HB_ERRCODE hb_dbfGetVarLen( DBFAREAP pArea, USHORT uiIndex, ULONG * pLength );
static HB_ERRCODE hb_dbfGoCold( DBFAREAP pArea );
static HB_ERRCODE hb_dbfGoHot( DBFAREAP pArea );
static HB_ERRCODE hb_dbfPutRec( DBFAREAP pArea, BYTE * pBuffer );
static HB_ERRCODE hb_dbfPutValue( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
static HB_ERRCODE hb_dbfRecall( DBFAREAP pArea );
static HB_ERRCODE hb_dbfRecCount( DBFAREAP pArea, ULONG * pRecCount );
static HB_ERRCODE hb_dbfRecInfo( DBFAREAP pArea, PHB_ITEM pRecID, USHORT uiInfoType, PHB_ITEM pInfo );
static HB_ERRCODE hb_dbfRecNo( DBFAREAP pArea, ULONG * pRecNo );
static HB_ERRCODE hb_dbfRecId( DBFAREAP pArea, PHB_ITEM pRecNo );
static HB_ERRCODE hb_dbfSetFieldExtent( DBFAREAP pArea, USHORT uiFieldExtent );
#define hb_dbfAlias                                NULL
static HB_ERRCODE hb_dbfClose( DBFAREAP pArea );
static HB_ERRCODE hb_dbfCreate( DBFAREAP pArea, LPDBOPENINFO pCreateInfo );
static HB_ERRCODE hb_dbfInfo( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
static HB_ERRCODE hb_dbfNewArea( DBFAREAP pArea );
static HB_ERRCODE hb_dbfOpen( DBFAREAP pArea, LPDBOPENINFO pOpenInfo );
#define hb_dbfRelease                              NULL
static HB_ERRCODE hb_dbfStructSize( DBFAREAP pArea, USHORT * uiSize );
#define hb_dbfSysName                              NULL
#define hb_dbfEval                                 NULL
static HB_ERRCODE hb_dbfPack( DBFAREAP pArea );
static HB_ERRCODE hb_dbfPackRec( DBFAREAP pArea, ULONG ulRecNo, BOOL *fWritten );
static HB_ERRCODE hb_dbfSort( DBFAREAP pArea, LPDBSORTINFO pSortInfo );
static HB_ERRCODE hb_dbfTrans( DBFAREAP pArea, LPDBTRANSINFO pTransInfo );
#define hb_dbfTransRec                             NULL
static HB_ERRCODE hb_dbfZap( DBFAREAP pArea );
static HB_ERRCODE hb_dbfChildEnd( DBFAREAP pArea, LPDBRELINFO pRelInfo );
static HB_ERRCODE hb_dbfChildStart( DBFAREAP pArea, LPDBRELINFO pRelInfo );
static HB_ERRCODE hb_dbfChildSync( DBFAREAP pArea, LPDBRELINFO pRelInfo );
#define hb_dbfSyncChildren                         NULL
#define hb_dbfClearRel                             NULL
static HB_ERRCODE hb_dbfForceRel( DBFAREAP pArea );
#define hb_dbfRelArea                              NULL
#define hb_dbfRelEval                              NULL
#define hb_dbfRelText                              NULL
#define hb_dbfSetRel                               NULL
#define hb_dbfOrderListAdd                         NULL
#define hb_dbfOrderListClear                       NULL
#define hb_dbfOrderListDelete                      NULL
#define hb_dbfOrderListFocus                       NULL
#define hb_dbfOrderListRebuild                     NULL
#define hb_dbfOrderCondition                       NULL
#define hb_dbfOrderCreate                          NULL
#define hb_dbfOrderDestroy                         NULL
#define hb_dbfOrderInfo                            NULL
#define hb_dbfClearFilter                          NULL
#define hb_dbfClearLocate                          NULL
#define hb_dbfClearScope                           NULL
#define hb_dbfCountScope                           NULL
#define hb_dbfFilterText                           NULL
#define hb_dbfScopeInfo                            NULL
static HB_ERRCODE hb_dbfSetFilter( DBFAREAP pArea, LPDBFILTERINFO pFilterInfo );
#define hb_dbfSetLocate                            NULL
#define hb_dbfSetScope                             NULL
#define hb_dbfSkipScope                            NULL
#define hb_dbfLocate                               NULL
#define hb_dbfCompile                              NULL
#define hb_dbfError                                NULL
#define hb_dbfEvalBlock                            NULL
static HB_ERRCODE hb_dbfRawLock( DBFAREAP pArea, USHORT uiAction, ULONG lRecNo );
static HB_ERRCODE hb_dbfLock( DBFAREAP pArea, LPDBLOCKINFO pLockInfo );
static HB_ERRCODE hb_dbfUnLock( DBFAREAP pArea, PHB_ITEM pRecNo );
#define hb_dbfCloseMemFile                         NULL
static HB_ERRCODE hb_dbfCreateMemFile( DBFAREAP pArea, LPDBOPENINFO pCreateInfo );
static HB_ERRCODE hb_dbfGetValueFile( DBFAREAP pArea, USHORT uiIndex, const char * szFile, USHORT uiMode );
static HB_ERRCODE hb_dbfOpenMemFile( DBFAREAP pArea, LPDBOPENINFO pOpenInfo );
static HB_ERRCODE hb_dbfPutValueFile( DBFAREAP pArea, USHORT uiIndex, const char * szFile, USHORT uiMode );

static HB_ERRCODE hb_dbfReadDBHeader( DBFAREAP pArea );
static HB_ERRCODE hb_dbfWriteDBHeader( DBFAREAP pArea );

static HB_ERRCODE hb_dbfInit( LPRDDNODE pRDD );
static HB_ERRCODE hb_dbfExit( LPRDDNODE pRDD );
static HB_ERRCODE hb_dbfDrop( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, ULONG ulConnect );
static HB_ERRCODE hb_dbfExists( LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, ULONG ulConnect );
static HB_ERRCODE hb_dbfRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem );

#define hb_dbfWhoCares                             NULL

#endif /* _HB_RDDDBF_INTERNAL_ */

extern HB_EXPORT ULONG      hb_dbfGetMemoBlock( DBFAREAP pArea, USHORT uiIndex );
extern HB_EXPORT void       hb_dbfPutMemoBlock( DBFAREAP pArea, USHORT uiIndex,
                                                ULONG ulBlock );
extern HB_EXPORT HB_ERRCODE hb_dbfGetMemoData( DBFAREAP pArea, USHORT uiIndex,
                                               ULONG * pulBlock, ULONG * pulSize,
                                               ULONG * pulType );
extern HB_EXPORT HB_ERRCODE hb_dbfSetMemoData( DBFAREAP pArea, USHORT uiIndex,
                                               ULONG ulBlock, ULONG ulSize,
                                               ULONG ulType );
extern HB_EXPORT HB_ERRCODE hb_dbfGetEGcode( HB_ERRCODE errCode );
extern HB_EXPORT BOOL       hb_dbfLockIdxFile( PHB_FILE pFile, BYTE bScheme, USHORT usMode, HB_FOFFSET *pPoolPos );
extern HB_EXPORT BOOL       hb_dbfLockIdxGetData( BYTE bScheme, HB_FOFFSET *ulPos, HB_FOFFSET *ulPool );

#ifndef HB_CDP_SUPPORT_OFF
extern HB_EXPORT void hb_dbfTranslateRec( DBFAREAP pArea, BYTE * pBuffer, PHB_CODEPAGE cdp_src, PHB_CODEPAGE cdp_dest );
#endif

HB_EXTERN_END

#endif /* HB_RDDDBF_H_ */
