/*
 * $Id$

   Copyright(C) 1999 by Bruno Cantero.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: bruno@issnet.net
 */

#include <extend.h>
#include <init.h>
#include <itemapi.h>
#include <errorapi.h>
#include <set.h>
#include <rdd.api>
#include <ctoharb.h>
#include <set.ch>

#define HARBOUR_MAX_RDD_DRIVERNAME_LENGTH       32
#define MAX_WORKAREAS                          250

#define RDD_BOF                                  0
#define RDD_EOF                                  1
#define RDD_FOUND                                2
#define RDD_GOBOTTOM                             3
#define RDD_GO                                   4
#define RDD_GOTOP                                5
#define RDD_CLOSE                                6
#define RDD_OPEN                                 7

typedef struct
{
   char   szName[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];
   USHORT uiType;
   RDDFUNCS pTable;
} RDDNODE, * PRDDNODE;

HARBOUR HB_BOF( void );
HARBOUR HB_DBCLOSEAREA( void );
HARBOUR HB_DBGOBOTTOM( void );
HARBOUR HB_DBGOTO( void );
HARBOUR HB_DBGOTOP( void );
HARBOUR HB_DBSELECTAREA( void );
HARBOUR HB_DBSETDRIVER( void );
HARBOUR HB_DBUSEAREA( void );
HARBOUR HB_EOF( void );
HARBOUR HB_FOUND( void );
HARBOUR HB_RDDLIST( void );
HARBOUR HB_RDDREGISTER( void );
HARBOUR HB_RDDSETDEFAULT( void );

HB_INIT_SYMBOLS_BEGIN( dbCmd__InitSymbols )
{ "BOF",           FS_PUBLIC, HB_BOF,           0 },
{ "DBCLOSEAREA",   FS_PUBLIC, HB_DBCLOSEAREA,   0 },
{ "DBGOBOTTOM",    FS_PUBLIC, HB_DBGOBOTTOM,    0 },
{ "DBGOTO",        FS_PUBLIC, HB_DBGOTO,        0 },
{ "DBGOTOP",       FS_PUBLIC, HB_DBGOTOP,       0 },
{ "DBSELECTAREA",  FS_PUBLIC, HB_DBSELECTAREA,  0 },
{ "DBSETDRIVER",   FS_PUBLIC, HB_DBSETDRIVER,   0 },
{ "DBUSEAREA",     FS_PUBLIC, HB_DBUSEAREA,     0 },
{ "EOF",           FS_PUBLIC, HB_EOF,           0 },
{ "FOUND",         FS_PUBLIC, HB_FOUND,         0 },
{ "RDDLIST",       FS_PUBLIC, HB_RDDLIST,       0 },
{ "RDDREGISTER",   FS_PUBLIC, HB_RDDREGISTER,   0 },
{ "RDDSETDEFAULT", FS_PUBLIC, HB_RDDSETDEFAULT, 0 }
HB_INIT_SYMBOLS_END( dbCmd__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup dbCmd__InitSymbols
#endif

/* From strings.c */
char * hb_strUpper( char * szText, long lLen );

extern STACK stack;
extern HB_set_struct hb_set;

static char * szDefDriver = 0;  /* Default RDD name */
static USHORT uiCurrArea = 1;   /* Selectd area */
static PRDDNODE pRDDList = 0;   /* Registered RDD's */
static USHORT uiRDDCount = 0;
static USHORT uiNetError = 0;

static AREA pWorkAreas[ MAX_WORKAREAS - 1 ];

void hb_rddInitialize( void )
{
   memset( pWorkAreas, 0, sizeof( AREA ) );
   szDefDriver = ( char * ) hb_xgrab( 1 );
   szDefDriver[ 0 ] = '\0';
}

void hb_rddRelease( void )
{
   hb_xfree( szDefDriver );
   if( pRDDList )
      hb_xfree( pRDDList );
}

static USHORT hb_GetRDDId( char * szDriver )
{
   PRDDNODE pList;
   USHORT uiCount;
   PHB_ITEM pError;

   for( uiCount = 0; uiCount < uiRDDCount; uiCount++ )
   {
      pList = pRDDList + ( sizeof( RDDNODE ) * uiCount );
      if( strcmp( pList->szName, szDriver ) == 0 )
	 return uiCount + 1;
   }
   /* TODO: hb_errorRT_INTERNAL() */
   pError = hb_errNew();
   hb_errPutDescription( pError, "Internal error 9001" );
   hb_errLaunch( pError );
   hb_errRelease( pError );
   return 0;
}

static BOOL hb_rddRegister( char * szDriver, USHORT uiType )
{
   PRDDNODE pList;
   USHORT uiCount;
   PDYNSYM pGetFuncTable;
   char * szGetFuncTable;

   for( uiCount = 0; uiCount < uiRDDCount; uiCount++ )
   {
      pList = pRDDList + ( sizeof( RDDNODE ) * uiCount );
      if( strcmp( pList->szName, szDriver ) == 0 )
	 return 0;
   }

   szGetFuncTable = (char *)hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_FindDynSym( szGetFuncTable );
   hb_xfree( szGetFuncTable );

   if( !pGetFuncTable )
      return 0;
   if( !pRDDList )
      pRDDList = (PRDDNODE)hb_xgrab( sizeof( RDDNODE ) );
   else
      pRDDList = (PRDDNODE)hb_xrealloc( pRDDList, sizeof( RDDNODE ) * ( uiRDDCount + 1 ) );

   pList = pRDDList + ( sizeof( RDDNODE ) * uiRDDCount );
   strcpy( pList->szName, szDriver );
   pList->uiType = uiType;
   uiRDDCount++;

   PushSymbol( pGetFuncTable->pSymbol );
   PushNil();
   PushLong( ( long ) &uiCount );
   PushLong( ( long ) &pList->pTable );
   Do( 2 );

   return 1;
}

static USHORT hb_FindAlias( char * szAlias )
{
   return 1; /* Not implemented yet */
}

static void hb_SelectFirstAvailable( void )
{
   USHORT uiCount;

   for( uiCount = 0; uiCount < MAX_WORKAREAS; uiCount++ )
   {
      if( pWorkAreas[ uiCount ].rddID == 0 )
      {
	 uiCurrArea = uiCount + 1;
	 return;
      }
   }
}

ERRCODE hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, PBYTE szDrvName )
{
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   if( !pTable )
      return FAILURE;

   /* Copy the pSuperTable into pTable */
   memcpy( ( void * ) pTable, ( void * ) pSuperTable, sizeof( RDDFUNCS ) );

   /* Copy the non NULL entries from pSubTable into pTable */
   pFunction = ( DBENTRYP_V * ) pTable;
   pSubFunction = ( DBENTRYP_V * ) pSubTable;
   for( uiCount = 0; uiCount < RDDFUNCSCOUNT; uiCount++ )
   {
      if( * pSubFunction )
	 * pFunction = * pSubFunction;
      pFunction += 1;
      pSubFunction += 1;
   }
   return SUCCESS;
}

static void hb_dbSelectArea( USHORT uiNewArea )
{
   if( uiNewArea == 0)
      hb_SelectFirstAvailable();
   else if( uiNewArea <= MAX_WORKAREAS )
      uiCurrArea = uiNewArea;
}

HARBOUR HB_BOF( void )
{
   BOOL bBof = TRUE;
   DBENTRYP_BP * pFunction;

   pFunction = ( ( DBENTRYP_BP * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_BOF;
   if( * pFunction )
     ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ], &bBof );
   hb_retl( bBof );
}

HARBOUR HB_DBCLOSEAREA( void )
{
   DBENTRYP_V * pFunction;
   PHB_ITEM pError;

   if( pWorkAreas[ uiCurrArea - 1 ].rddID == 0 )
      return;

   pFunction = ( ( DBENTRYP_V * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_CLOSE;
   if( ! * pFunction )
   {
      pError = hb_errNew();
      hb_errPutDescription( pError, "Internal error 9xxx" );
      hb_errLaunch( pError );
      hb_errRelease( pError );
      pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
   }
   else
   {
      ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ] );
      pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
   }
}

HARBOUR HB_DBGOBOTTOM( void )
{
   DBENTRYP_V * pFunction;
   PHB_ITEM pError;

   pFunction = ( ( DBENTRYP_V * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_GOBOTTOM;
   if( ! * pFunction )
   {
      pError = hb_errNew();
      hb_errPutDescription( pError, "Internal error 9xxx" );
      hb_errLaunch( pError );
      hb_errRelease( pError );
      pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
   }
   else
      ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ] );
}

HARBOUR HB_DBGOTO( void )
{
   DBENTRYP_L * pFunction;
   PHB_ITEM pError;

   pFunction = ( ( DBENTRYP_L * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_GO;
   if( ! * pFunction )
   {
      pError = hb_errNew();
      hb_errPutDescription( pError, "Internal error 9xxx" );
      hb_errLaunch( pError );
      hb_errRelease( pError );
      pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
   }
   else if( ISNUM( 1 ) )
      ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ], hb_parnl( 1 ) );
   else
      hb_errorRT_BASE( EG_ARG, 1068, "Argument error", "DBGOTO" );
}

HARBOUR HB_DBGOTOP( void )
{
   DBENTRYP_V * pFunction;
   PHB_ITEM pError;

   pFunction = ( ( DBENTRYP_V * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_GOTOP;
   if( ! * pFunction )
   {
      pError = hb_errNew();
      hb_errPutDescription( pError, "Internal error 9xxx" );
      hb_errLaunch( pError );
      hb_errRelease( pError );
      pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
   }
   else
      ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ] );
}

HARBOUR HB_DBUSEAREA( void )
{
   USHORT uiRDDId;
   char * szDriver, * szFileName, * szAlias;
   WORD wLen;
   PRDDNODE pList;
   DBENTRYP_VP * pFunction;
   DBOPENINFO pInfo;
   PHB_ITEM pError;

   uiNetError = 0;

   if( ISLOG( 1 ) )
      hb_SelectFirstAvailable();
   else if( pWorkAreas[ uiCurrArea - 1 ].rddID != 0 )
   {
      // CloseArea()
   }

   szDriver = szDefDriver;
   uiRDDId = 1;
   if( ISCHAR( 2 ) )
   {
      szDriver = hb_parc( 2 );
      if( ( wLen = strlen( szDriver ) ) > 0 )
      {
	 szDriver = hb_strUpper( szDriver, wLen );
	 if( ( uiRDDId = hb_GetRDDId( szDriver ) ) == 0 )
	 {
	    pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
	    return;
	 }
      }
   }
   pList = pRDDList + ( sizeof( RDDNODE ) * ( uiRDDId - 1 ) );
   pWorkAreas[ uiCurrArea - 1 ].rddID = uiRDDId;
   pWorkAreas[ uiCurrArea - 1 ].lprfsHost = &pList->pTable;

   pFunction = ( ( DBENTRYP_VP * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_OPEN;
   if( ! * pFunction )
   {
      pError = hb_errNew();
      hb_errPutDescription( pError, "Internal error 9xxx" );
      hb_errLaunch( pError );
      hb_errRelease( pError );
      pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
      return;
   }

   if( !ISCHAR( 3 ) )
   {
      hb_errorRT_BASE(EG_ARG, 1068, "Argument error", "DBUSEAREA" );
      pWorkAreas[ uiCurrArea - 1 ].rddID = 0;
      return;
   }
   szFileName = hb_parc( 3 );

   /* TODO: Implement alias from szFilename */
   szAlias = ISCHAR( 4 ) ? hb_parc( 4 ) : szFileName;

   pInfo.uiArea = uiCurrArea;
   pInfo.abName = (PBYTE)szFileName;
   pInfo.atomAlias = (PBYTE)szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;
   ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ], &pInfo );
}

HARBOUR HB_DBSELECTAREA( void )
{
   USHORT uiNewArea;
   char * szAlias;

   if( ISCHAR( 1 ) )
   {
      szAlias = hb_parc( 1 );
      if( ( uiNewArea = hb_FindAlias( szAlias ) ) == 0 )
      {
	 hb_errorRT_BASE( EG_ARG, 1002, "Alias not found", szAlias );
	 return;
      }
      hb_dbSelectArea( uiNewArea );
   }
   else if( ISNUM( 1 ) )
      hb_dbSelectArea( hb_parni( 1 ) );
}

HARBOUR HB_DBSETDRIVER( void )
{
   HB_RDDSETDEFAULT();
}

HARBOUR HB_EOF( void )
{
   BOOL bEof = TRUE;
   DBENTRYP_BP * pFunction;

   pFunction = ( ( DBENTRYP_BP * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_EOF;
   if( * pFunction )
     ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ], &bEof );
   hb_retl( bEof );
}

HARBOUR HB_FOUND( void )
{
   BOOL bFound = FALSE;
   DBENTRYP_BP * pFunction;

   pFunction = ( ( DBENTRYP_BP * ) pWorkAreas[ uiCurrArea - 1 ].lprfsHost ) + RDD_FOUND;
   if( * pFunction )
     ( * pFunction )( &pWorkAreas[ uiCurrArea - 1 ], &bFound );
   hb_retl( bFound );
}

HARBOUR HB_RDDLIST( void )
{
   USHORT   uiCount, uiType;
   PHB_ITEM pSubArray, pName, pType;
   PRDDNODE pList;

   uiType = hb_parni( 1 );       /* 0 all types of RDD's */
   hb_arrayNew( &stack.Return, 0 );
   pSubArray = hb_itemArrayNew( 2 );
   pName = hb_itemNew( 0 );
   pType = hb_itemNew( 0 );
   for( uiCount = 0; uiCount < uiRDDCount; uiCount++ )
   {
      pList = pRDDList + ( sizeof( RDDNODE ) * uiCount );
      if( ( uiType == 0 ) || ( uiType == pList->uiType ) )
      {
	 hb_arraySet( pSubArray, 1, hb_itemPutC( pName, pList->szName ) );
	 hb_arraySet( pSubArray, 2, hb_itemPutNL( pType, pList->uiType ) );
	 hb_arrayAdd( &stack.Return, pSubArray );
      }
   }
   hb_itemRelease( pName );
   hb_itemRelease( pType );
   hb_itemRelease( pSubArray );
}

HARBOUR HB_RDDREGISTER( void )
{
   char * szDriver;
   WORD wLen;

   if( ISCHAR( 1 ) )
   {
      szDriver = hb_parc( 1 );
      if( ( wLen = strlen( szDriver ) ) > 0 )
      {
	 szDriver = hb_strUpper( szDriver, wLen );
	 if( !hb_rddRegister( szDriver, hb_parni( 2 ) ) )
	 {
	    /* TODO: hb_errorRT_INTERNAL() */
	    PHB_ITEM pError = hb_errNew();
	    hb_errPutDescription( pError, "Internal error 9001" );
	    hb_errLaunch( pError );
	    hb_errRelease( pError );
	 }
      }
   }
}

HARBOUR HB_RDDSETDEFAULT( void )
{
   char * szNewDriver;
   WORD wLen;

   hb_retc( szDefDriver );
   if( ISCHAR( 1 ) )
   {
      szNewDriver = hb_parc( 1 );
      if( ( wLen = strlen( szNewDriver ) ) > 0 )
      {
	 szNewDriver = hb_strUpper( szNewDriver, wLen );
	 szDefDriver = ( char * ) hb_xrealloc( szDefDriver, wLen + 1 );
	 strcpy( szDefDriver, szNewDriver );
      }
      else hb_errorRT_DBCMD( EG_ARG, 1015, "Argument error", "RDDSETDEFAULT" );
   }
}
