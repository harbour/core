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

#ifndef HB_RDDDBF_H_
#define HB_RDDDBF_H_

#include "hbapirdd.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* DBF header */

typedef struct _DBFHEADER
{
   BYTE   bVersion;
   BYTE   bYear;
   BYTE   bMonth;
   BYTE   bDay;
   ULONG  ulRecCount;
   USHORT uiHeaderLen;
   USHORT uiRecordLen;
   BYTE   bReserved1[ 16 ];
   BYTE   bHasTags;
   BYTE   bReserved2[ 3 ];
} DBFHEADER;

typedef DBFHEADER * LPDBFHEADER;



typedef struct _DBFFIELD
{
   BYTE bName[ 11 ];
   BYTE bType;
   BYTE bReserved1[ 4 ];
   BYTE bLen;
   BYTE bDec;
   BYTE bReserved2[ 13 ];
   BYTE bHasTag;
} DBFFIELD;

typedef DBFFIELD * LPDBFFIELD;



/* DBF errors */

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



/* DBF default file extensions */

#define DBF_TABLEEXT                              ".dbf"
#define DBF_MEMOEXT                               ".dbt"



/* DBF lock */

#define DBF_LOCKPOS                          1000000000L


/* DBT's */

#define DBT_BLOCKSIZE                                512


/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBF RDD
 *
 */

typedef struct _DBFAREA
{
   struct _RDDFUNCS * lprfsHost; /* Virtual method table for this workarea */
   USHORT   uiArea;              /* The number assigned to this workarea */
   void * atomAlias;             /* Pointer to the alias symbol for this workarea */
   USHORT   uiFieldExtent;       /* Total number of fields allocated */
   USHORT   uiFieldCount;        /* Total number of fields used */
   LPFIELD  lpFields;            /* Pointer to an array of fields */
   void *   lpFieldExtents;      /* Void ptr for additional field properties */
   PHB_ITEM valResult;           /* All purpose result holder */
   BOOL fTop;                    /* TRUE if "top" */
   BOOL fBottom;                 /* TRUE if "bottom" */
   BOOL fBof;                    /* TRUE if "bof" */
   BOOL fEof;                    /* TRUE if "eof" */
   BOOL fFound;                  /* TRUE if "found" */
   DBSCOPEINFO  dbsi;            /* Info regarding last LOCATE */
   DBFILTERINFO dbfi;            /* Filter in effect */
   LPDBORDERCONDINFO lpdbOrdCondInfo;
   LPDBRELINFO lpdbRelations;    /* Parent/Child relationships used */
   USHORT uiParents;             /* Number of parents for this area */
   USHORT   heap;
   USHORT   heapSize;
   USHORT   rddID;

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
   BOOL fReadOnly;               /* Read only file */
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
} DBFAREA;

typedef DBFAREA * LPDBFAREA;

#ifndef DBFAREAP
#define DBFAREAP LPDBFAREA
#endif



/*
 * -- DBF METHODS --
 */

#define SUPERTABLE                         ( &dbfSuper )

extern ERRCODE hb_dbfBof( DBFAREAP pArea, BOOL * pBof );
extern ERRCODE hb_dbfEof( DBFAREAP pArea, BOOL * pEof );
extern ERRCODE hb_dbfFound( DBFAREAP pArea, BOOL * pFound );
extern ERRCODE hb_dbfGoBottom( DBFAREAP pArea );
extern ERRCODE hb_dbfGoTo( DBFAREAP pArea, ULONG ulRecNo );
extern ERRCODE hb_dbfGoToId( DBFAREAP pArea, PHB_ITEM pItem );
extern ERRCODE hb_dbfGoTop( DBFAREAP pArea );
#define hb_dbfSeek                                 NULL
extern ERRCODE hb_dbfSkip( DBFAREAP pArea, LONG lToSkip );
#define hb_dbfSkipFilter                           NULL
extern ERRCODE hb_dbfSkipRaw( DBFAREAP pArea, LONG lToSkip );
extern ERRCODE hb_dbfAddField( DBFAREAP pArea, LPDBFIELDINFO pFieldInfo );
extern ERRCODE hb_dbfAppend( DBFAREAP pArea, BOOL bUnLockAll );
#define hb_dbfCreateFields                         NULL
extern ERRCODE hb_dbfDeleteRec( DBFAREAP pArea );
extern ERRCODE hb_dbfDeleted( DBFAREAP pArea, BOOL * pDeleted );
#define hb_dbfFieldCount                           NULL
#define hb_dbfFieldDisplay                         NULL
#define hb_dbfFieldInfo                            NULL
#define hb_dbfFieldName                            NULL
extern ERRCODE hb_dbfFlush( DBFAREAP pArea );
#define hb_dbfGetRec                               NULL
extern ERRCODE hb_dbfGetValue( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
extern ERRCODE hb_dbfGetVarLen( DBFAREAP pArea, USHORT uiIndex, ULONG * pLength );
extern ERRCODE hb_dbfGoCold( DBFAREAP pArea );
extern ERRCODE hb_dbfGoHot( DBFAREAP pArea );
extern ERRCODE hb_dbfPutRec( DBFAREAP pArea, BYTE * pBuffer );
extern ERRCODE hb_dbfPutValue( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
extern ERRCODE hb_dbfRecAll( DBFAREAP pArea );
extern ERRCODE hb_dbfRecCount( DBFAREAP pArea, ULONG * pRecCount );
#define hb_dbfRecInfo                              NULL
extern ERRCODE hb_dbfRecNo( DBFAREAP pArea, PHB_ITEM pRecNo );
extern ERRCODE hb_dbfSetFieldExtent( DBFAREAP pArea, USHORT uiFieldExtent );
#define hb_dbfAlias                                NULL
extern ERRCODE hb_dbfClose( DBFAREAP pArea );
extern ERRCODE hb_dbfCreate( DBFAREAP pArea, LPDBOPENINFO pCreateInfo );
extern ERRCODE hb_dbfInfo( DBFAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
extern ERRCODE hb_dbfNewArea( DBFAREAP pArea );
extern ERRCODE hb_dbfOpen( DBFAREAP pArea, LPDBOPENINFO pOpenInfo );
#define hb_dbfRelease                              NULL
extern ERRCODE hb_dbfStructSize( DBFAREAP pArea, USHORT * uiSize );
extern ERRCODE hb_dbfSysName( DBFAREAP pArea, BYTE * pBuffer );
#define hb_dbfEval                                 NULL
extern ERRCODE hb_dbfPack( DBFAREAP pArea );
#define hb_dbfPackRec                              NULL
extern ERRCODE hb_dbfSort( DBFAREAP pArea, LPDBSORTINFO pSortInfo );
extern ERRCODE hb_dbfTrans( DBFAREAP pArea, LPDBTRANSINFO pTransInfo );
extern ERRCODE hb_dbfTransRec( DBFAREAP pArea, LPDBTRANSINFO pTransInfo );
extern ERRCODE hb_dbfZap( DBFAREAP pArea );
extern ERRCODE hb_dbfChildEnd( DBFAREAP pArea, LPDBRELINFO pRelInfo );
extern ERRCODE hb_dbfChildStart( DBFAREAP pArea, LPDBRELINFO pRelInfo );
extern ERRCODE hb_dbfChildSync( DBFAREAP pArea, LPDBRELINFO pRelInfo );
#define hb_dbfSyncChildren                         NULL
#define hb_dbfClearRel                             NULL
extern ERRCODE hb_dbfForceRel( DBFAREAP pArea );
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
extern ERRCODE hb_dbfSetFilter( DBFAREAP pArea, LPDBFILTERINFO pFilterInfo );
#define hb_dbfSetLocate                            NULL
#define hb_dbfSetScope                             NULL
#define hb_dbfSkipScope                            NULL
#define hb_dbfCompile                              NULL
#define hb_dbfError                                NULL
#define hb_dbfEvalBlock                            NULL
#define hb_dbfRawLock                              NULL
extern ERRCODE hb_dbfLock( DBFAREAP pArea, LPDBLOCKINFO pLockInfo );
extern ERRCODE hb_dbfUnLock( DBFAREAP pArea, ULONG ulRecNo );
#define hb_dbfCloseMemFile                         NULL
extern ERRCODE hb_dbfCreateMemFile( DBFAREAP pArea, LPDBOPENINFO pCreateInfo );
#define hb_dbfGetValueFile                         NULL
extern ERRCODE hb_dbfOpenMemFile( DBFAREAP pArea, LPDBOPENINFO pOpenInfo );
#define hb_dbfPutValueFile                         NULL
extern ERRCODE hb_dbfReadDBHeader( DBFAREAP pArea );
extern ERRCODE hb_dbfWriteDBHeader( DBFAREAP pArea );
#define hb_dbfWhoCares                             NULL

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_RDDDBF_H_ */