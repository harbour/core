/*
 * $Id$
 */

/*
 * DBFCDX RDD (ver.2)
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef HB_RDDCDX_H_
#define HB_RDDCDX_H_

#include "hbrdddbf.h"

HB_EXTERN_BEGIN

/* CDX constants and defaults */
#define CDX_INDEXEXT                              ".cdx"
#define CDX_MAXKEY                                  240
#define CDX_MAXEXP                                  255
#define CDX_MAXTAGNAMELEN                            10
#define CDX_PAGELEN                                 512
#define CDX_HEADERLEN                              1024
#define CDX_HEADEREXPLEN          (CDX_HEADERLEN - 512)
#define CDX_HEADERPAGES   ((CDX_HEADERLEN+CDX_PAGELEN-1)/CDX_PAGELEN)
#define CDX_INT_FREESPACE              (CDX_PAGELEN-12) /* 500 */
#define CDX_EXT_FREESPACE              (CDX_PAGELEN-24) /* 488 */
#define CDX_DUMMYNODE                       0xFFFFFFFFL

/* #define CDX_LOCKOFFSET                      0x7FFFFFFEL */
/* #define CDX_LOCKSIZE                                 1L */
#define CDX_STACKSIZE                                64
#define CDX_PAGECACHESIZE                             8
#define CDX_NODE_BRANCH                               0
#define CDX_NODE_ROOT                                 1
#define CDX_NODE_LEAF                                 2
#define CDX_NODE_UNUSED                            0xFF
#define CDX_IGNORE_REC_NUM                         0x0L
#define CDX_MAX_REC_NUM                     0xFFFFFFFFL
#define CDX_BALANCE_LEAFPAGES                         3
#define CDX_BALANCE_INTPAGES                          3

#define CDX_CURKEY_UNDEF                        (1<< 0)
#define CDX_CURKEY_REC                          (1<< 1)
#define CDX_CURKEY_VAL                          (1<< 2)
#define CDX_CURKEY_INPAGE                       (1<< 3)
#define CDX_CURKEY_INSTACK                      (1<< 4)
#define CDX_CURKEY_NOTEXIST                     (1<< 5)
#define CDX_CURKEY_RAWCNT                       (1<< 6)
#define CDX_CURKEY_RAWPOS                       (1<< 7)
#define CDX_CURKEY_LOGCNT                       (1<< 8)
#define CDX_CURKEY_LOGPOS                       (1<< 9)

#define TOP_RECORD                                    1
#define BTTM_RECORD                                   2
#define PREV_RECORD                                   3
#define NEXT_RECORD                                   4
#define PRVU_RECORD                                   6
#define NXTU_RECORD                                   5

#define NODE_NEWLASTKEY                               1
#define NODE_SPLIT                                    2
#define NODE_JOIN                                     4
#define NODE_BALANCE                                  8
#define NODE_EAT                                     16

#define CURKEY_RAWCNT(pTag)   (((pTag)->curKeyState & CDX_CURKEY_RAWCNT) != 0)
#define CURKEY_LOGCNT(pTag)   (((pTag)->curKeyState & CDX_CURKEY_LOGCNT) != 0)

#define CURKEY_RAWPOS(pTag)   ( ((pTag)->curKeyState & CDX_CURKEY_RAWPOS) != 0 && \
                                 (pTag)->rawKeyRec == (pTag)->CurKey->rec )
#define CURKEY_SETRAWPOS(pTag) do { (pTag)->curKeyState |= CDX_CURKEY_RAWPOS; \
                                    (pTag)->rawKeyRec = (pTag)->CurKey->rec; } while(0)

#define CURKEY_LOGPOS(pTag)   ( ((pTag)->curKeyState & CDX_CURKEY_LOGPOS) != 0 && \
                                 (pTag)->logKeyRec == (pTag)->pIndex->pArea->dbfarea.ulRecNo )
#define CURKEY_SETLOGPOS(pTag) do { (pTag)->curKeyState |= CDX_CURKEY_LOGPOS; \
                                    (pTag)->logKeyRec = (pTag)->pIndex->pArea->dbfarea.ulRecNo; } while(0)

/*
#define CURKEY_UNDEF(pTag)    (((pTag)->curKeyState & CDX_CURKEY_UNDEF) != 0)
#define CURKEY_NOTEXIST(pTag) (((pTag)->curKeyState & CDX_CURKEY_NOTEXIST) != 0)
#define CURKEY_ISSET(pTag)    (((pTag)->curKeyState & (CDX_CURKEY_NOTEXIST | CDX_CURKEY_UNDEF)) == 0)
#define CURKEY_REC(pTag)      ((((pTag)->curKeyState & CDX_CURKEY_REC) != 0) ? (pTag)->curKey->rec : 0)
#define CURKEY_VAL(pTag)      ((((pTag)->curKeyState & CDX_CURKEY_VAL) != 0) ? (pTag)->curKey->val : NULL)
#define CURKEY_REFRESH(pTag)
*/

#define HB_CDXMAXKEY( x )     ( ( HB_USHORT ) ( ( x ) > CDX_MAXKEY ? CDX_MAXKEY : ( x ) ) )
#define HB_CDXBITMASK( x )    ( ( HB_LONG ) ( ( 1L << ( x ) ) - 1 ) )

/* #define FAST_GOCOLD( A )      ((A)->dbfarea.fRecordChanged || (A)->fCdxAppend ? (SELF_GOCOLD((AREAP)(A))) : HB_SUCCESS) */
#define FAST_GOCOLD( A )      SELF_GOCOLD(A)


#define CDX_TYPE_UNIQUE        0x01    /* unique index */
#define CDX_TYPE_TEMPORARY     0x02    /* temporary index */
#define CDX_TYPE_CUSTOM        0x04    /* custom index */
#define CDX_TYPE_FORFILTER     0x08    /* for expression present */
#define CDX_TYPE_BITVECTOR     0x10    /* SoftC? */
#define CDX_TYPE_COMPACT       0x20    /* FoxPro */
#define CDX_TYPE_COMPOUND      0x40    /* FoxPro */
#define CDX_TYPE_STRUCTURE     0x80    /* FoxPro */

#define CDX_CMP_EXACT          0x00    /* exact comparision */
#define CDX_CMP_PREFIX         0x01    /* prefix comparision */
#define CDX_CMP_DATE           0x02    /* date comparision */

/*
 SIx3 order temperature flags:
   switch ( indexOpt & ( CDX_TYPE_TEMPORARY | CDX_TYPE_CUSTOM ) )
      case CDX_TYPE_TEMPORARY:
         PARTIAL_RYO
      case CDX_TYPE_CUSTOM:
         PARTIAL_RYO | CHGONLY_RYO
      case CDX_TYPE_TEMPORARY | CDX_TYPE_CUSTOM:
         PARTIAL_RYO | NOUPDATE_RYO
         if index key begin with:
            'SXCHAR(' or 'SXNUM(' or 'SXDATE(' or 'SXLOG('
         then
            | TEMPLATE_RYO

   sx_chill()  if ( ! NOUPDATE_RYO ) then set ( CHGONLY_RYO | PARTIAL_RYO )
                  if ( indexOpt & ( CDX_TYPE_TEMPORARY | CDX_TYPE_CUSTOM ) !=
                        CDX_TYPE_TEMPORARY | CDX_TYPE_CUSTOM )
                  {
                     indexOpt &= ~CDX_TYPE_CUSTOM;
                     indexOpt |= CDX_TYPE_TEMPORARY
                  }

   sx_warm()   if ( ! NOUPDATE_RYO ) then clear CHGONLY_RYO
                  if ( indexOpt & ( CDX_TYPE_TEMPORARY | CDX_TYPE_CUSTOM ) !=
                        CDX_TYPE_TEMPORARY | CDX_TYPE_CUSTOM )
                  {
                     indexOpt |= CDX_TYPE_CUSTOM;
                     indexOpt &= ~CDX_TYPE_TEMPORARY
                  }

   sx_freeze() set NOUPDATE_RYO
                  indexOpt |= CDX_TYPE_TEMPORARY | CDX_TYPE_CUSTOM;
*/

/* CDX index node strucutres */
/* Compact Index Header Record */
typedef struct _CDXTAGHEADER
{
   HB_BYTE     rootPtr  [ 4 ];   /* offset of the root node */
   HB_BYTE     freePtr  [ 4 ];   /* offset of list of free pages or -1 */
   HB_BYTE     reserved1[ 4 ];   /* Version number ??? */
   HB_BYTE     keySize  [ 2 ];   /* key length */
   HB_BYTE     indexOpt;         /* index options see CDX_TYPE_* */
   HB_BYTE     indexSig;         /* index signature */
   HB_BYTE     reserved2[ 478 ];
   HB_BYTE     codepage[ 5 ];    /* VFP codepage */
   HB_BYTE     ignoreCase;       /* 1 = ignore case, key converted to upper */
   HB_BYTE     reserved3[ 2 ];
   HB_BYTE     ascendFlg[ 2 ];   /* 0 = ascending  1 = descending */
   HB_BYTE     forExpPos[ 2 ];   /* offset of filter expression */
   HB_BYTE     forExpLen[ 2 ];   /* length of filter expression */
   HB_BYTE     keyExpPos[ 2 ];   /* offset of key expression */
   HB_BYTE     keyExpLen[ 2 ];   /* length of key expression */
   HB_BYTE     keyExpPool[ CDX_HEADEREXPLEN ];
} CDXTAGHEADER;
typedef CDXTAGHEADER * LPCDXTAGHEADER;

/* Compact Index Interior Node Record */
typedef struct _CDXINTNODE
{
   HB_BYTE     attr    [ 2 ];    /* node type see CDX_NODE_* */
   HB_BYTE     nKeys   [ 2 ];    /* number of keys */
   HB_BYTE     leftPtr [ 4 ];    /* offset of left node or -1 */
   HB_BYTE     rightPtr[ 4 ];    /* offset of right node or -1 */
   HB_BYTE     keyPool [ CDX_INT_FREESPACE ];
} CDXINTNODE;
typedef CDXINTNODE * LPCDXINTNODE;
typedef CDXINTNODE CDXNODE;
typedef CDXNODE * LPCDXNODE;

/* Compact Index Exterior Node Record */
typedef struct _CDXEXTNODE
{
   HB_BYTE     attr    [ 2 ];    /* node type see CDX_NODE_* */
   HB_BYTE     nKeys   [ 2 ];    /* number of keys */
   HB_BYTE     leftPtr [ 4 ];    /* offset of left node or -1 */
   HB_BYTE     rightPtr[ 4 ];    /* offset of right node or -1 */
   HB_BYTE     freeSpc [ 2 ];    /* free space available in a page */
   HB_BYTE     recMask [ 4 ];    /* record number mask */
   HB_BYTE     dupMask;          /* duplicate bytes count mask */
   HB_BYTE     trlMask;          /* trailing bytes count mask */
   HB_BYTE     recBits;          /* number of bits for record number */
   HB_BYTE     dupBits;          /* number of bits for duplicate count */
   HB_BYTE     trlBits;          /* number of bits for trailing count */
   HB_BYTE     keyBytes;         /* total number of bytes for recnn/dup/trail info */
   HB_BYTE     keyPool [ CDX_EXT_FREESPACE ];      /* rec/dup/trl */
} CDXEXTNODE;
typedef CDXEXTNODE * LPCDXEXTNODE;



/* CDX internal memory structures */

struct _CDXAREA;  /* forward declaration */
struct _CDXINDEX; /* forward declaration */
struct _CDXTAG;   /* forward declaration */

typedef struct _CDXKEY
{
   HB_BYTE * val;
   HB_USHORT len;
   HB_USHORT mode;
   HB_ULONG  rec;
} CDXKEY;
typedef CDXKEY * LPCDXKEY;

typedef struct _CDXPAGE
{
   HB_ULONG  Page;
   HB_ULONG  Left;
   HB_ULONG  Right;

   HB_BYTE   PageType;
   int       iKeys;
   int       iCurKey;

   HB_BOOL   fChanged;
   HB_BYTE   bUsed;

   HB_ULONG  RNMask;
   HB_BYTE   ReqByte;
   HB_BYTE   RNBits;
   HB_BYTE   DCBits;
   HB_BYTE   TCBits;
   HB_BYTE   DCMask;
   HB_BYTE   TCMask;
   HB_BOOL   fBufChanged;
   union
   {
      CDXEXTNODE extNode;
      CDXINTNODE intNode;
   } node;
   HB_BYTE   bufKeyVal[ CDX_MAXKEY ];      /* buffer for leaf key val or added branch key */
   HB_SHORT  bufKeyNum;                    /* do not change these vars' order             */
   HB_SHORT  bufKeyPos;                    /* they have to be just after the node         */
   HB_SHORT  bufKeyLen;                    /* and maybe temporary overwriten when adding  */
   HB_SHORT  iFree;                        /* new key to interior node record.            */
   HB_BYTE * pKeyBuf;                      /* pointer to uncompressed leaf page key pool  */
   /* HB_SHORT iKeyInBuf; */

   struct _CDXPAGE * Owner;
   struct _CDXPAGE * Child;
   struct _CDXTAG  * TagParent;
   struct _CDXPAGE * pPoolPrev;
   struct _CDXPAGE * pPoolNext;
} CDXPAGE;
typedef CDXPAGE * LPCDXPAGE;

typedef struct _CDXSTACK
{
   LPCDXPAGE Page;
   int       iKey;
} CDXSTACK;
typedef CDXSTACK * LPCDXSTACK;

typedef struct _CDXLIST
{
   HB_ULONG ulAddr;
   HB_BOOL  fStat;
   struct _CDXLIST * pNext;
} CDXLIST;
typedef CDXLIST * LPCDXLIST;

typedef struct _CDXTAG
{
   char *    szName;          /* Name of tag */
   char *    KeyExpr;         /* a tag key expression as text */
   char *    ForExpr;         /* a tag for expression as text */
   PHB_ITEM  pKeyItem;        /* item with a macro pcode for a tag key expression */
   PHB_ITEM  pForItem;        /* item with a macro pcode for a tag for expression */
   HB_USHORT uiType;          /* a type of key expression value */
   HB_USHORT uiLen;           /* length of the key expression value */
   HB_USHORT nField;          /* Field number for simple (one field) key expersion */
   HB_BYTE   bTrail;          /* trailing character for shorter key value */
   HB_BYTE   OptFlags;        /* index options flag */
   HB_BOOL   AscendKey;       /* ascending/descending order flag */
   HB_BOOL   UniqueKey;       /* unique order flag */
   HB_BOOL   Temporary;       /* temporary order flag */
   HB_BOOL   Custom;          /* custom order flag */
   HB_BOOL   Template;        /* user keyadata in ordKeyAdd()/ordKeyDel() accepted */
   HB_BOOL   MultiKey;        /* repeated key values in custom indexes accepted */
   HB_BOOL   Partial;         /* order is updated only partially - missing keys possible */
   HB_BOOL   ChgOnly;         /* only existing key modifications are updated, no new key added */
   HB_BOOL   UsrAscend;       /* user settable ascending/descending order flag */
   HB_BOOL   UsrUnique;       /* user settable unique order flag */
   HB_BOOL   IgnoreCase;      /* ignore case (upper keys) */

   HB_BOOL   TagChanged;
   HB_BOOL   TagBOF;
   HB_BOOL   TagEOF;

   HB_BOOL   fRePos;
   int       curKeyState;     /* see: CDX_CURKEY_* */
   HB_ULONG  rawKeyCount;
   HB_ULONG  rawKeyPos;
   HB_ULONG  rawKeyRec;
   HB_ULONG  logKeyCount;
   HB_ULONG  logKeyPos;
   HB_ULONG  logKeyRec;

   HB_ULONG  TagBlock;        /* a page offset where a tag header is stored */
   HB_ULONG  RootBlock;       /* a page offset with the root of keys tree */
   HB_USHORT MaxKeys;         /* maximum number of keys in Interior node */

   struct _CDXINDEX * pIndex; /* a parent index info */
   struct _CDXTAG   * pNext;  /* pointer to next tag in index */

   /* CDXSTACK  PageStack[ CDX_STACKSIZE ]; */  /* stack with page path to current key */
   LPCDXPAGE  RootPage;       /* pointer to root of keys tree in memory */
   LPCDXKEY   CurKey;         /* current value of key expression */
   LPCDXKEY   HotKey;         /* value of hot key expression */
   HB_BOOL    HotFor;         /* index FOR condition for HotKey */

   PHB_ITEM   topScope;       /* Top scope HB_ITEM */
   LPCDXKEY   topScopeKey;    /* Top scope index key */
   PHB_ITEM   bottomScope;    /* Bottom scope HB_ITEM */
   LPCDXKEY   bottomScopeKey; /* Bottom index key */

   LPCDXPAGE  pagePool;       /* page buffer in memory */
} CDXTAG;
typedef CDXTAG * LPCDXTAG;

typedef struct _CDXINDEX
{
   char *     szFileName;     /* Name of index file */
   char *     szRealName;     /* Real name of index file */
   PHB_FILE   pFile;          /* Index file handle */
   struct _CDXAREA  * pArea;  /* Parent WorkArea */
   struct _CDXINDEX * pNext;  /* The next index in the list */
   LPCDXTAG   pCompound;      /* Compound tag */
   LPCDXTAG   TagList;        /* List of tags in index file */
   HB_BOOL    fShared;        /* Shared file */
   HB_BOOL    fReadonly;      /* Read only file */
   HB_BOOL    fDelete;        /* delete on close flag */
   HB_ULONG   nextAvail;      /* offset to next free page in the end of index file */
   HB_ULONG   freePage;       /* offset to next free page inside index file */
   LPCDXLIST  freeLst;        /* list of free pages in index file */
   int        lockWrite;      /* number of write lock set */
   int        lockRead;       /* number of read lock set */
   HB_DBFLOCKDATA lockData;   /* index lock data */
#ifdef HB_CDX_DBGCODE
   HB_BOOL    RdLck;
   HB_BOOL    WrLck;
#endif
   HB_BOOL    fChanged;       /* changes written to index, need upadte ulVersion */
   HB_ULONG   ulVersion;      /* network version/update flag */
   HB_BOOL    fFlush;         /* changes written to index, need upadte ulVersion */
} CDXINDEX;
typedef CDXINDEX * LPCDXINDEX;

/* for index creation */
typedef struct
{
   HB_FOFFSET nOffset;         /* offset in temporary file */
   HB_ULONG   ulKeys;          /* number of keys in page */
   HB_ULONG   ulKeyBuf;        /* number of keys in memory buffer */
   HB_ULONG   ulCurKey;        /* current key in memory buffer */
   HB_BYTE *  pKeyPool;        /* memory buffer */
} CDXSWAPPAGE;
typedef CDXSWAPPAGE * LPCDXSWAPPAGE;

typedef struct
{
   LPCDXTAG   pTag;             /* current Tag */
   HB_FHANDLE hTempFile;        /* handle to temporary file */
   char *     szTempFileName;   /* temporary file name */
   int        keyLen;           /* key length */
   HB_BYTE    bTrl;             /* filler char for shorter keys */
   HB_BOOL    fUnique;          /* HB_TRUE if index is unique */
   HB_BOOL    fReindex;         /* HB_TRUE if reindexing is in process */
   HB_ULONG   ulMaxRec;         /* the highest record number */
   HB_ULONG   ulTotKeys;        /* total number of keys indexed */
   HB_ULONG   ulKeys;           /* keys in curently created page */
   HB_ULONG   ulPages;          /* number of pages */
   HB_ULONG   ulCurPage;        /* current page */
   HB_ULONG   ulPgKeys;         /* maximum number of key in page memory buffer */
   HB_ULONG   ulMaxKey;         /* maximum number of keys in single page */
   HB_BYTE *  pKeyPool;         /* memory buffer for current page then for pages */
   LPCDXSWAPPAGE pSwapPage;     /* list of pages */
   LPCDXPAGE  NodeList[ CDX_STACKSIZE ];   /* Stack of pages */
   HB_ULONG   ulFirst;
   HB_ULONG * pSortedPages;
   HB_BYTE    pLastKey[ CDX_MAXKEY ]; /* last key val */
   HB_ULONG   ulLastRec;
   HB_BYTE *  pRecBuff;
#ifndef HB_CDX_PACKTRAIL
   int        iLastTrl;         /* last key trailing spaces */
#endif
} CDXSORTINFO;
typedef CDXSORTINFO * LPCDXSORTINFO;



/*
 *  DBF WORKAREA
 *  ------------
 *  The Workarea Structure of DBFCDX RDD
 *
 */

typedef struct _CDXAREA
{
   DBFAREA dbfarea;

   /*
   *  CDX's additions to the workarea structure
   *
   *  Warning: The above section MUST match DBFAREA exactly! Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   LPCDXSORTINFO  pSort;         /* Index build structure */
   LPCDXINDEX     lpIndexes;     /* Pointer to indexes array  */
   const HB_UCHAR * sortTab;     /* Table with sorted characters */
   HB_BOOL        fCdxAppend;    /* Appended record changed */
   HB_BOOL        fSortCDP;      /* Use CDP functions for sorting */
   HB_USHORT      uiTag;         /* current tag focus */

} CDXAREA;

typedef CDXAREA * LPCDXAREA;

#ifndef CDXAREAP
#define CDXAREAP LPCDXAREA
#endif

#undef  SUPERTABLE
#define SUPERTABLE                         ( &cdxSuper )

HB_EXTERN_END

#endif /* HB_RDDCDX_H_ */
