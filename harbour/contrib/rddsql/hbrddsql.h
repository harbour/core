/*
 * $Id$
 */

/*
 * SQL Database Driver include file
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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


#ifndef HBSQLDD_H_
#define HBSQLDD_H_

#include "hbapirdd.h"
#include "hbdbferr.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbapi.h"


/* New ...INFO_ constants */
#define DBI_QUERY             1001

#define RDDI_CONNECT          1001
#define RDDI_DISCONNECT       1002
#define RDDI_EXECUTE          1003
#define RDDI_ERROR            1004
#define RDDI_ERRORNO          1005
#define RDDI_NEWID            1006
#define RDDI_AFFECTEDROWS     1007
#define RDDI_QUERY            1008


/*
====================================================================
  SQLBASE
====================================================================
*/

#define MAX_FIELD_NAME          64

#define SQLDD_ROWSET_INIT      256
#define SQLDD_ROWSET_RESIZE     64

#define SQLDD_FLAG_DELETED       1
#define SQLDD_FLAG_CACHED        2


typedef struct _SQLBASEAREA
{
   AREA area;

   /*
    *  SQLBASE additions to the workarea structure
    */

   LPDBRELINFO                lpdbPendingRel;

   ULONG                      ulConnection;
   struct _SQLDDCONNECTION*   pConnection;
   struct _SDDNODE*           pSDD;

   char*                      szQuery;       /* SQL query */

   ULONG                      ulRecNo;       /* Current record number */
   ULONG                      ulRecCount;    /* Total records */
   ULONG                      ulRecMax;      /* Size of pRow, pRowFlags buffer */

   void**                     pRow;          /* array of native pointers or cached PHB_ITEM */
   BYTE*                      pRowFlags;

   void*                      pRecord;
   BYTE                       bRecordFlags;

   void*                      pResult;          /* SQL result */
   void*                      pStmt;            /* SQL statement */
   void*                      pTrans;           /* SQL transaction */

   void*                      pNatRecord;
   void*                      pNatLength;

   BOOL                       fFetched;
   BOOL                       fPositioned;
   BOOL                       fAppend;
   BOOL                       fRecordChanged;
} SQLBASEAREA, *SQLBASEAREAP;


typedef struct _SQLDDCONNECTION
{
   struct _SDDNODE *    pSDD;
   void*                hConnection;
   void*                hCargo;
   int                  iError;
   char*                szError;
   char*                szQuery;
   PHB_ITEM             pNewID;
   ULONG                ulAffectedRows;
} SQLDDCONNECTION;


/*
====================================================================
  SQLMIX
====================================================================
*/

#define MIX_MAXKEYLEN        1024
#define MIX_MAXTAGNAMELEN      16


#define MIX_NODE_ORDER         2    /* >=2 */


typedef struct _MIXKEY
{
   ULONG     rec;
   BYTE      notnul;
   BYTE      val[ 1 ];
} MIXKEY, *PMIXKEY;


typedef struct _MIXNODE
{
   unsigned int          Leaf;
   unsigned int          KeyCount;
   struct _MIXNODE*      Parent;
   struct _MIXNODE*      Child[ MIX_NODE_ORDER + 1 ];
} MIXNODE, *PMIXNODE;


typedef struct _MIXNODELEAF
{
   unsigned int          Leaf;
   unsigned int          KeyCount;
   struct _MIXNODE*      Parent;
} MIXNODELEAF, *PMIXNODELEAF;


typedef struct _MIXTAG
{
   struct _MIXTAG*      pNext;
   struct _SQLMIXAREA*  pArea;
   char*                szName;
   char*                szKeyExpr;
   char*                szForExpr;
   PHB_ITEM             pKeyItem;
   PHB_ITEM             pForItem;

   BYTE                 bType;
   unsigned int         uiKeyLen;            /* Length of key */
   unsigned int         uiTotalLen;          /* Total length of key structure */

   BOOL                 fEof;
   BOOL                 fBof;
   BOOL                 fCustom;

   PMIXNODE             Root;

   PMIXKEY              CurKey;
   PMIXNODE             CurNode;
   unsigned int         CurPos;

   PMIXKEY              HotKey;
   BOOL                 HotFor;

   BYTE*                pSortTable;  /* National sorttable for character key tags, NULL otherwise */
} MIXTAG, *PMIXTAG;


typedef struct _SQLMIXAREA
{
   SQLBASEAREA sqlarea;

   /*
    *  SQLMIX additions to the sqlbase workarea structure
    */

   PMIXTAG      pTagList;
   PMIXTAG      pTag;
   BYTE*        pSortTable;

} SQLMIXAREA, *SQLMIXAREAP;



/*
====================================================================
  SQLDD
====================================================================
*/

typedef   HB_ERRCODE   (* SDDFUNC_CONNECT    )( SQLDDCONNECTION* pConnection, PHB_ITEM pItem );
typedef   HB_ERRCODE   (* SDDFUNC_DISCONNECT )( SQLDDCONNECTION* pConnection );
typedef   HB_ERRCODE   (* SDDFUNC_EXECUTE    )( SQLDDCONNECTION* pConnection, PHB_ITEM pItem );
typedef   HB_ERRCODE   (* SDDFUNC_OPEN       )( SQLBASEAREAP pArea );
typedef   HB_ERRCODE   (* SDDFUNC_CLOSE      )( SQLBASEAREAP pArea );
typedef   HB_ERRCODE   (* SDDFUNC_GOTO       )( SQLBASEAREAP pArea, ULONG ulRecNo );
typedef   HB_ERRCODE   (* SDDFUNC_GETVALUE   )( SQLBASEAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
typedef   HB_ERRCODE   (* SDDFUNC_GETVARLEN  )( SQLBASEAREAP pArea, USHORT uiIndex, ULONG * pLength );


typedef struct _SDDNODE
{
   struct _SDDNODE *    pNext;

   const char*          Name;
   SDDFUNC_CONNECT      Connect;
   SDDFUNC_DISCONNECT   Disconnect;
   SDDFUNC_EXECUTE      Execute;
   SDDFUNC_OPEN         Open;
   SDDFUNC_CLOSE        Close;
   SDDFUNC_GOTO         GoTo;
   SDDFUNC_GETVALUE     GetValue;
   SDDFUNC_GETVARLEN    GetVarLen;
} SDDNODE, * PSDDNODE;


int hb_sddRegister( PSDDNODE pSdd );



/*
====================================================================
  Misc
====================================================================
*/

/* Error subcodes */
#define ESQLDD_NOTCONNECTED        1901
#define ESQLDD_INVALIDFIELD        1902
#define ESQLDD_INVALIDQUERY        1903
#define ESQLDD_START               1904
#define ESQLDD_COMMIT              1905
#define ESQLDD_STMTALLOC           1906
#define ESQLDD_STMTDESCR           1907
#define ESQLDD_STMTFREE            1908
#define ESQLDD_FETCH               1909
#define ESQLDD_LOWMEMORY           1910
#define ESQLDD_NULLSDD             1911

#endif
