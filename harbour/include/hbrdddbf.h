/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBF RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef HB_RDDDBF_H_
#define HB_RDDDBF_H_

#include "hbapirdd.h"
#include "hbdbferr.h"
#include "hbdbf.h"

HB_EXTERN_BEGIN

/* DBF default file extensions */
#define DBF_TABLEEXT                      ".dbf"

/* DBF locking schemes */
#define DBF_LOCKPOS_CLIPPER               1000000000UL
#define DBF_LOCKPOS_CLIPPER2              4000000000UL
#define DBF_LOCKPOS_COMIX                 1000000000UL
#define DBF_LOCKPOS_VFP                   0x40000000UL
#define DBF_LOCKPOS_VFPX                  0x7ffffffeUL
#define DBF_LOCKPOS_HB32                  4000000000UL
#define DBF_LOCKPOS_HB64                  HB_LL( 0x7FFFFFFF00000001 )

#define DBF_LOCKDIR_CLIPPER               1
#define DBF_LOCKDIR_CLIPPER2              1
#define DBF_LOCKDIR_COMIX                 1
#define DBF_LOCKDIR_VFP                   2  /* lock forward at at record offset */
#define DBF_LOCKDIR_VFPX                  -1
#define DBF_LOCKDIR_HB32                  1
#define DBF_LOCKDIR_HB64                  1

#define DBF_FLCKSIZE_CLIPPER              1000000000UL
#define DBF_FLCKSIZE_CLIPPER2             294967295UL
#define DBF_FLCKSIZE_COMIX                1000000000UL
#define DBF_FLCKSIZE_VFP                  0x3ffffffdUL
#define DBF_FLCKSIZE_VFPX                 0x07ffffffUL
#define DBF_FLCKSIZE_HB32                 294967295UL
#define DBF_FLCKSIZE_HB64                 0xfffffffeUL

#define DBF_RLCKSIZE_CLIPPER              1UL
#define DBF_RLCKSIZE_CLIPPER2             1UL
#define DBF_RLCKSIZE_COMIX                1UL
#define DBF_RLCKSIZE_VFP                  1UL
#define DBF_RLCKSIZE_VFPX                 1UL
#define DBF_RLCKSIZE_HB32                 1UL
#define DBF_RLCKSIZE_HB64                 1UL

#define IDX_LOCKPOS_CLIPPER               1000000000UL
#define IDX_LOCKPOS_CLIPPER2              1000000000UL
#define IDX_LOCKPOS_COMIX                 0xfffeffffUL
#define IDX_LOCKPOS_VFP                   0x7ffffffeUL
#define IDX_LOCKPOS_HB32                  0xfffeffffUL
#define IDX_LOCKPOS_HB64                  HB_LL( 0x7FFFFFFF00000001 )

#define IDX_LOCKPOOL_CLIPPER              0UL
#define IDX_LOCKPOOL_CLIPPER2             0UL
#define IDX_LOCKPOOL_COMIX                0x00010000UL
#define IDX_LOCKPOOL_VFP                  0UL
#define IDX_LOCKPOOL_HB32                 0x00010000UL
#define IDX_LOCKPOOL_HB64                 0x00010000UL


/* Index dirty read flags */
#define HB_IDXREAD_CLEAN      0
#define HB_IDXREAD_DEFAULT    1
#define HB_IDXREAD_DIRTY      2

#define HB_IDXREAD_CLEANMASK  HB_IDXREAD_DIRTY
#define HB_IDXREAD_DIRTYMASK  (HB_IDXREAD_DIRTY|HB_IDXREAD_DEFAULT)

#define DBFNODE_DATA( r )     ( ( LPDBFDATA ) hb_stackGetTSD( ( PHB_TSD ) \
                                                      ( r )->lpvCargo ) )
#define DBFAREA_DATA( w )     DBFNODE_DATA( SELF_RDDNODE( &( w )->area ) )


#define HB_DIRTYREAD( w )     ( ( DBFAREA_DATA( w )->uiDirtyRead & \
                                              ( w )->uiDirtyRead ) != 0 )


/*
 * Private DBF* RDD data kept in RDDNODE
 */
typedef struct _DBFDATA
{
   char      szTableExt[ HB_MAX_FILE_EXT + 1 ];
   char      szIndexExt[ HB_MAX_FILE_EXT + 1 ];
   char      szMemoExt[ HB_MAX_FILE_EXT + 1 ];

   char *    szPasswd;
   char *    szPendingPasswd;
   char *    szTrigger;
   char *    szPendingTrigger;

   HB_BYTE   bLockType;        /* 0 */
   HB_BYTE   bTableType;       /* DB_DBF_STD */
   HB_BYTE   bCryptType;       /* DB_CRYPT_NONE */
   HB_BYTE   bMemoType;        /* DB_MEMO_FPT */
   HB_BYTE   bMemoExtType;     /* DB_MEMOVER_FLEX */
   HB_USHORT uiDirtyRead;      /* HB_IDXREAD_CLEANMASK */
   HB_ULONG  ulMemoBlockSize;  /* 0 */

   HB_BOOL   fSortRecNo;
   HB_BOOL   fMultiKey;
   HB_BOOL   fStruct;
   HB_BOOL   fStrictStruct;
   HB_BOOL   fMultiTag;
} DBFDATA, * LPDBFDATA;

typedef struct _HB_DBFFIELDBITS
{
   HB_USHORT uiNullBit;
   HB_USHORT uiLengthBit;
} HB_DBFFIELDBITS, * PHB_DBFFIELDBITS;

typedef struct _HB_DBFLOCKDATA
{
   HB_FOFFSET     offset;
   HB_FOFFSET     size;
   HB_FOFFSET     next;
   HB_FOFFSET     tolock;
   int            type;
   int            count;
} HB_DBFLOCKDATA, * PHB_DBFLOCKDATA;


/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBF RDD
 *
 */

typedef struct _DBFAREA
{
   AREA area;

   /*
   *  DBFS's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   PHB_FILE    pDataFile;           /* Data file handle */
   PHB_FILE    pMemoFile;           /* Memo file handle */
   PHB_FILE    pMemoTmpFile;        /* Memo temporary file handle */
   char *      szDataFileName;      /* Name of data file */
   char *      szMemoFileName;      /* Name of memo file */
   HB_USHORT   uiHeaderLen;         /* Size of header */
   HB_USHORT   uiRecordLen;         /* Size of record */
   HB_ULONG    ulMemoBlockSize;     /* Size of memo block */
   HB_ULONG    ulNewBlockSize;      /* Size of new memo block */
   HB_USHORT   uiMemoVersion;       /* MEMO file version */
   HB_USHORT   uiDirtyRead;         /* Index dirty read bit filed */
   HB_USHORT   uiNullOffset;        /* Offset to _NullFlags filed */
   HB_USHORT   uiNullCount;         /* Number of null flags */
   HB_BYTE     bTableType;          /* DBF type */
   HB_BYTE     bMemoType;           /* MEMO type used in DBF memo fields */
   HB_BYTE     bLockType;           /* Type of locking shemes */
   HB_BYTE     bCryptType;          /* Type of used encryption */
   DBFHEADER   dbfHeader;           /* DBF header buffer */
   HB_USHORT * pFieldOffset;        /* Pointer to field offset array */
   PHB_DBFFIELDBITS pFieldBits;     /* Pointer to extended DBF field info array */
   HB_BYTE *   pRecord;             /* Buffer of record data */
   HB_ULONG    ulRecCount;          /* Total records */
   HB_ULONG    ulRecNo;             /* Current record */
   HB_BOOL     fAutoInc;            /* WorkArea with auto increment fields */
   HB_BOOL     fHasMemo;            /* WorkArea with Memo fields */
   HB_BOOL     fHasTags;            /* WorkArea with MDX or CDX index */
   HB_BOOL     fModStamp;           /* WorkArea with modification autoupdate fields */
   HB_BOOL     fDataFlush;          /* data was written to DBF and not commited */
   HB_BOOL     fMemoFlush;          /* data was written to MEMO and not commited */
   HB_BOOL     fShared;             /* Shared file */
   HB_BOOL     fReadonly;           /* Read only file */
   HB_BOOL     fTemporary;          /* Temporary file */
   HB_BOOL     fValidBuffer;        /* State of buffer */
   HB_BOOL     fPositioned;         /* Positioned record */
   HB_BOOL     fRecordChanged;      /* Record changed */
   HB_BOOL     fAppend;             /* HB_TRUE if new record is added */
   HB_BOOL     fDeleted;            /* HB_TRUE if record is deleted */
   HB_BOOL     fEncrypted;          /* HB_TRUE if record is encrypted */
   HB_BOOL     fTableEncrypted;     /* HB_TRUE if table is encrypted */
   HB_BOOL     fUpdateHeader;       /* Update header of file */
   HB_BOOL     fFLocked;            /* HB_TRUE if file is locked */
   HB_BOOL     fHeaderLocked;       /* HB_TRUE if DBF header is locked */
   HB_BOOL     fPackMemo;           /* Pack memo file in pack operation */
   HB_BOOL     fTrigger;            /* Execute trigger function */
   LPDBOPENINFO lpdbOpenInfo;       /* Pointer to current dbOpenInfo structure in OPEN/CREATE methods */
   LPDBRELINFO lpdbPendingRel;      /* Pointer to parent rel struct */
   HB_ULONG *  pLocksPos;           /* List of records locked */
   HB_ULONG    ulNumLocksPos;       /* Number of records locked */
   char *      pCryptKey;           /* Pointer to encryption key */
   PHB_DYNS    pTriggerSym;         /* DynSym pointer to trigger function */
} DBFAREA;

typedef DBFAREA * LPDBFAREA;

#ifndef DBFAREAP
#define DBFAREAP LPDBFAREA
#endif

#define SUPERTABLE                         ( &dbfSuper )

extern HB_EXPORT HB_ULONG   hb_dbfGetMemoBlock( DBFAREAP pArea, HB_USHORT uiIndex );
extern HB_EXPORT void       hb_dbfPutMemoBlock( DBFAREAP pArea, HB_USHORT uiIndex,
                                                HB_ULONG ulBlock );
extern HB_EXPORT HB_ERRCODE hb_dbfGetMemoData( DBFAREAP pArea, HB_USHORT uiIndex,
                                               HB_ULONG * pulBlock, HB_ULONG * pulSize,
                                               HB_ULONG * pulType );
extern HB_EXPORT HB_ERRCODE hb_dbfSetMemoData( DBFAREAP pArea, HB_USHORT uiIndex,
                                               HB_ULONG ulBlock, HB_ULONG ulSize,
                                               HB_ULONG ulType );
extern HB_EXPORT HB_ERRCODE hb_dbfGetEGcode( HB_ERRCODE errCode );
extern HB_EXPORT HB_BOOL    hb_dbfLockIdxGetData( HB_BYTE bScheme, PHB_DBFLOCKDATA pLockData );
extern HB_EXPORT HB_BOOL    hb_dbfLockIdxFile( DBFAREAP pArea, PHB_FILE pFile,
                                               int iType, HB_BOOL fLateWrlck,
                                               PHB_DBFLOCKDATA pLockData );
extern HB_EXPORT HB_BOOL    hb_dbfLockIdxWrite( DBFAREAP pArea, PHB_FILE pFile,
                                                PHB_DBFLOCKDATA pLockData );

extern HB_EXPORT void hb_dbfTranslateRec( DBFAREAP pArea, HB_BYTE * pBuffer, PHB_CODEPAGE cdp_src, PHB_CODEPAGE cdp_dest );

HB_EXTERN_END

#endif /* HB_RDDDBF_H_ */
