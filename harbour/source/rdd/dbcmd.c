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

#include "extend.h"
#include "init.h"
#include "itemapi.h"
#include "errorapi.h"
#include "set.h"
#include "rdd.api"
#include "rddsys.ch"
#include "ctoharb.h"
#include "set.ch"

#define HARBOUR_MAX_RDD_DRIVERNAME_LENGTH       32

typedef struct _RDDNODE
{
   char     szName[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   USHORT   uiType;            /* Type of RDD */
   RDDFUNCS pTable;            /* Table of functions */
   USHORT   uiFunctions;       /* Number of functions in the table */
   USHORT   uiAreaSize;        /* Size of the WorkArea */
   struct _RDDNODE * pNext;    /* Next RDD in the list */
} RDDNODE, * PRDDNODE;

typedef struct _AREANODE
{
   void *             pArea;   /* WorkAreas with different sizes */
   struct _AREANODE * pPrev;   /* Prev WorkArea in the list */
   struct _AREANODE * pNext;   /* Next WorkArea in the list */
} AREANODE, * PAREANODE;

/* TODO: must be changed to a hb_errorRT... */
static void MyError( char * szError, char * szParam )
{
   printf( "\n%s %s\n\7", szError, szParam );
}

HARBOUR HB_BOF( void );
HARBOUR HB_DBCLOSEALL( void );
HARBOUR HB_DBCLOSEAREA( void );
HARBOUR HB_DBCREATE( void );
HARBOUR HB_DBGOBOTTOM( void );
HARBOUR HB_DBGOTO( void );
HARBOUR HB_DBGOTOP( void );
HARBOUR HB_DBSELECTAREA( void );
HARBOUR HB_DBSETDRIVER( void );
HARBOUR HB_DBSKIP( void );
HARBOUR HB_DBUSEAREA( void );
HARBOUR HB_EOF( void );
HARBOUR HB_FOUND( void );
HARBOUR HB_RDDLIST( void );
HARBOUR HB_RDDREGISTER( void );
HARBOUR HB_RDDSETDEFAULT( void );
HARBOUR HB_RDDSHUTDOWN( void );
HARBOUR HB_RDDSYS( void );

HB_INIT_SYMBOLS_BEGIN( dbCmd__InitSymbols )
{ "BOF",           FS_PUBLIC, HB_BOF,           0 },
{ "DBCLOSEALL",    FS_PUBLIC, HB_DBCLOSEALL,    0 },
{ "DBCLOSEAREA",   FS_PUBLIC, HB_DBCLOSEAREA,   0 },
{ "DBCREATE",      FS_PUBLIC, HB_DBCREATE,      0 },
{ "DBGOBOTTOM",    FS_PUBLIC, HB_DBGOBOTTOM,    0 },
{ "DBGOTO",        FS_PUBLIC, HB_DBGOTO,        0 },
{ "DBGOTOP",       FS_PUBLIC, HB_DBGOTOP,       0 },
{ "DBSELECTAREA",  FS_PUBLIC, HB_DBSELECTAREA,  0 },
{ "DBSETDRIVER",   FS_PUBLIC, HB_DBSETDRIVER,   0 },
{ "DBSKIP",        FS_PUBLIC, HB_DBSKIP,        0 },
{ "DBUSEAREA",     FS_PUBLIC, HB_DBUSEAREA,     0 },
{ "EOF",           FS_PUBLIC, HB_EOF,           0 },
{ "FOUND",         FS_PUBLIC, HB_FOUND,         0 },
{ "RDDLIST",       FS_PUBLIC, HB_RDDLIST,       0 },
{ "RDDREGISTER",   FS_PUBLIC, HB_RDDREGISTER,   0 },
{ "RDDSETDEFAULT", FS_PUBLIC, HB_RDDSETDEFAULT, 0 },
{ "RDDSHUTDOWN",   FS_PUBLIC, HB_RDDSHUTDOWN,   0 }
HB_INIT_SYMBOLS_END( dbCmd__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup dbCmd__InitSymbols
#endif

static char * szDefDriver = 0;      /* Default RDD name */
static USHORT uiCurrArea = 1;       /* Selectd area */
static PRDDNODE pRddList = 0;       /* Registered RDD's */
static USHORT uiNetError = 0;       /* Error on Networked environments */

static PAREANODE pWorkAreas = 0;    /* WorkAreas */
static PAREANODE pCurrArea = 0;     /* Pointer to a selectd and valid area */

static void LinkRddSys( void )    /* Never must be called */
{
   HB_RDDSYS();
   LinkRddSys();       /* just to keep compiler silent */
}

static void hb_CheckRdd( void )
{
   if( !szDefDriver )
   {
      szDefDriver = ( char * ) hb_xgrab( 1 );
      szDefDriver[ 0 ] = '\0';
   }
}

static void hb_CloseAll( void )
{
   DBENTRYP_V pFunction;

   pCurrArea = pWorkAreas;
   while( pWorkAreas )
   {
      pCurrArea = pWorkAreas;
      pWorkAreas = pWorkAreas->pNext;
      if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
	 MyError( "No table error ", "9xxxx" );
      else
      {
	 pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->close;
	 if( !pFunction || ( ( * pFunction )( ( AREAP ) pCurrArea->pArea ) != SUCCESS ) )
	    MyError( "Internal error ", "9xxxx" );
      }
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
   }
   uiCurrArea = 1;
   pCurrArea = 0;
   pWorkAreas = 0;
}

static PRDDNODE hb_FindRddNode( char * szDriver )
{
   PRDDNODE pRddNode;

   pRddNode = pRddList;
   while( pRddNode )
   {
      if( strcmp( pRddNode->szName, szDriver ) == 0 ) /* Matched RDD */
	 return pRddNode;
      pRddNode = pRddNode->pNext;
   }
   return 0;
}

static BOOL hb_rddRegister( char * szDriver, USHORT uiType )
{
   PRDDNODE pRddNode, pRddNewNode;
   PDYNSYM pGetFuncTable;
   char * szGetFuncTable;

   if( hb_FindRddNode( szDriver ) )    /* Duplicated RDD */
      return FALSE;

   szGetFuncTable = ( char * ) hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_FindDynSym( szGetFuncTable );
   hb_xfree( szGetFuncTable );
   if( !pGetFuncTable )
      return FALSE;              /* Not valid RDD */

   /* Create a new RDD node */
   pRddNewNode = ( PRDDNODE ) hb_xgrab( sizeof( RDDNODE ) );
   memset( pRddNewNode, 0, sizeof( RDDNODE ) );

   /* Fill the new RDD node */
   strncpy( pRddNewNode->szName, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   pRddNewNode->uiType = uiType;

   /* Call <szDriver>_GETFUNCTABLE() */
   PushSymbol( pGetFuncTable->pSymbol );
   PushNil();
   PushLong( ( long ) &pRddNewNode->uiFunctions );
   PushLong( ( long ) &pRddNewNode->pTable );
   Do( 2 );
   if ( hb_parni( -1 ) != SUCCESS )
   {
      hb_xfree( pRddNewNode );         /* Delete de new RDD node */
      return FALSE;
   }

   if( !pRddList )                  /* First RDD node */
      pRddList = pRddNewNode;
   else
   {
      pRddNode = pRddList;
      while( pRddNode->pNext )
	 pRddNode = pRddNode->pNext;   /* Locate the last RDD node */
      pRddNode->pNext = pRddNewNode;   /* Add the new RDD node */
   }
   return TRUE;
}

static USHORT hb_FindAlias( char * szAlias )
{
   return 1; /* Not implemented yet */
}

static void hb_SelectFirstAvailable( void )
{
   PAREANODE pAreaNode;

   uiCurrArea = 1;
   pAreaNode = pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea > uiCurrArea )
	 break;
      else if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiCurrArea )
	 uiCurrArea++;
      pAreaNode = pAreaNode->pNext;
   }
   pCurrArea = 0;   /* Selected WorkArea must be created */
}

ERRCODE defUnSupported_V( AREAP pArea )
{
   printf( "Calling defUnSupported()\n" );
   return SUCCESS;
}

ERRCODE defUnSupported_L( AREAP pArea, LONG lLong )
{
   printf( "Calling defUnSupported()\n" );
   return SUCCESS;
}

ERRCODE defBof( AREAP pArea, BOOL * pBof )
{
   printf( "Calling defBof()\n" );
   return SUCCESS;
}

ERRCODE defEof( AREAP pArea, BOOL * pEof )
{
   printf( "Calling defEof()\n" );
   return SUCCESS;
}

ERRCODE defFound( AREAP pArea, BOOL * pFound )
{
   printf( "Calling defFound()\n" );
   return SUCCESS;
}

ERRCODE defGoBottom( AREAP pArea )
{
   printf( "Calling defGoBottom()\n" );
   return SUCCESS;
}

ERRCODE defGoTo( AREAP pArea, LONG lRecNo )
{
   printf( "Calling defGoTo()\n" );
   return SUCCESS;
}

ERRCODE defGoTop( AREAP pArea )
{
   printf( "Calling defGoTop()\n" );
   return SUCCESS;
}

ERRCODE defSkip( AREAP pArea, LONG lToSkip )
{
   printf( "Calling defSkip()\n" );
   return SUCCESS;
}

ERRCODE defClose( AREAP pArea )
{
   printf( "Calling defClose()\n" );
   return SUCCESS;
}

ERRCODE defOpen( AREAP pArea, DBOPENINFOP pOpenInfo )
{
   printf( "Calling defOpen()\n" );
   return SUCCESS;
}

ERRCODE defStructSize( AREAP pArea, USHORT * uiSize )
{
   printf( "Calling defStructSize()\n" );
   return SUCCESS;
}

static RDDFUNCS defTable = { defBof,
			     defEof,
			     defFound,
			     defUnSupported_V,
			     defUnSupported_L,
			     defUnSupported_V,
			     defSkip,
			     defClose,
			     defOpen, /* Not defCreate */
			     defOpen,
			     defStructSize
			   };

ERRCODE hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, PBYTE szDrvName )
{
   PRDDNODE pRddNode;
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   if( !pTable )
      return FAILURE;

   /* Copy the pSuperTable into pTable */
   if( !szDrvName )
      memcpy( pTable, &defTable, sizeof( RDDFUNCS ) );
   else
   {
      szDrvName = hb_strUpper( szDrvName, strlen( szDrvName ) );
      if( !( pRddNode = hb_FindRddNode( szDrvName ) ) )
	 return FAILURE;
      memcpy( pTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
   }

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

HARBOUR HB_BOF( void )
{
   BOOL bBof = TRUE;
   DBENTRYP_BP pFunction;

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
   {
      pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->bof;
      if( pFunction )
	 ( * pFunction )( ( AREAP ) pCurrArea->pArea, &bBof );
   }
   hb_retl( bBof );
}

HARBOUR HB_DBCLOSEALL( void )
{
   hb_CloseAll();
}

HARBOUR HB_DBCLOSEAREA( void )
{
   DBENTRYP_V pFunction;

   if( !pCurrArea )
      return;

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
   {
      pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->close;
      if( !pFunction || ( ( * pFunction )( ( AREAP ) pCurrArea->pArea ) != SUCCESS ) )
	 MyError( "Internal error ", "9xxxx" );
   }

   if( pWorkAreas == pCurrArea )  /* Empty list */
      pWorkAreas = 0;
   else
   {
      if( pCurrArea->pPrev )
	 pCurrArea->pPrev->pNext = pCurrArea->pNext;
      if( pCurrArea->pNext )
	 pCurrArea->pNext->pPrev = pCurrArea->pPrev;
   }

   hb_xfree( pCurrArea->pArea );
   hb_xfree( pCurrArea );
   pCurrArea = 0;
}

HARBOUR HB_DBCREATE( void )
{
   char * szFileName, * szDriver;
   PHB_ITEM pStruct;
   WORD wLen;
   PRDDNODE pRddNode;
   DBENTRYP_SP pFunction1;
   DBENTRYP_VP pFunction2;
   AREAP pTempArea;
   USHORT uiSize;
   DBOPENINFO pInfo;

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2 , IT_ARRAY );
   if( ( strlen( szFileName) == 0 ) || !pStruct )
   {
      MyError( "DBCMD/1014 Argument error", "DBCREATE" );
      return;
   }

   hb_CheckRdd();
   szDriver = hb_parc( 3 );
   if( ( wLen = strlen( szDriver ) ) > 0 )
      szDriver = hb_strUpper( szDriver, strlen( szDriver ) );
   else
      szDriver = szDefDriver;

   if( !( pRddNode = hb_FindRddNode( szDriver ) ) )
   {
      MyError( "DBCMD/1015 Argument error", "DBCREATE" );
      return;
   }

   pFunction1 = pRddNode->pTable.structSize;
   pFunction2 = pRddNode->pTable.create;
   if( ! pFunction1 || ! pFunction2 )
   {
      MyError( "Internal error ", "9xxxx" );
      return;
   }
   uiSize = sizeof( AREA );    /* Default Size Area */
   pTempArea = ( AREAP ) hb_xgrab( uiSize );

   /* Need more space? */
   if( ( * pFunction1 )( pTempArea, &uiSize ) != SUCCESS )
   {
      hb_xfree( pTempArea );
      MyError( "Internal error ", "9xxxx" );
      return;
   }
   if( uiSize != sizeof( AREA ) )   /* Size of Area changed */
      pTempArea = ( AREAP ) hb_xrealloc( pTempArea, uiSize );

   pRddNode->uiAreaSize = uiSize; /* Update the size of WorkArea */

   pInfo.abName = ( PBYTE ) szFileName;
   if( ( * pFunction2 )( pTempArea, &pInfo ) != SUCCESS )
      MyError( "Internal error ", "9xxxx" );
   hb_xfree( pTempArea );
}

HARBOUR HB_DBGOBOTTOM( void )
{
   DBENTRYP_V pFunction;

   if( !pCurrArea )
   {
      MyError( "Alias not in use ", "1xxxx" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
   {
      pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->goBottom;
      if( ! pFunction )
	 MyError( "Internal error ", "9xxxx" );
      else
	 ( * pFunction )( ( AREAP ) pCurrArea->pArea );
   }
}

HARBOUR HB_DBGOTO( void )
{
   DBENTRYP_L pFunction;

   if( !pCurrArea )
   {
      MyError( "Alias not in use ", "1xxxx" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
   {
      pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->go;
      if( ! pFunction )
	 MyError( "Internal error ", "9xxxx" );
      else if( ISNUM( 1 ) )
	 ( * pFunction )( ( AREAP ) pCurrArea->pArea, hb_parnl( 1 ) );
      else
	 MyError( "DBCMD/1068 Argument error", "DBGOTO" );
   }
}

HARBOUR HB_DBGOTOP( void )
{
   DBENTRYP_V pFunction;

   if( !pCurrArea )
   {
      MyError( "Alias not in use ", "1xxxx" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
   {
      pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->goTop;
      if( ! pFunction )
	 MyError( "Internal error ", "9xxxx" );
      else
	 ( * pFunction )( ( AREAP ) pCurrArea->pArea );
   }
}

HARBOUR HB_DBSELECTAREA( void )
{
   USHORT uiNewArea;
   char * szAlias;
   PAREANODE pAreaNode;

   if( ISCHAR( 1 ) )
   {
      szAlias = hb_parc( 1 );
      if( ( uiNewArea = hb_FindAlias( szAlias ) ) == 0 )
      {
	 MyError( "DBCMD/1002 Alias not found", szAlias );
	 return;
      }
   }
   else if( !ISNUM( 1 ) )
   {
      MyError( "DBCMD/1068 Argument error", "DBSELECTAREA" );
      return;
   }
   else
      uiNewArea = hb_parni( 1 );

   if( uiNewArea == 0 )
      hb_SelectFirstAvailable();
   else
      uiCurrArea = uiNewArea;

   pAreaNode = pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiCurrArea )
      {
	 pCurrArea = pAreaNode; /* Select a valid WorkArea */
	 return;
      }
      pAreaNode = pAreaNode->pNext;
   }
   pCurrArea = 0; /* Selected WorkArea is closed */
}

HARBOUR HB_DBSETDRIVER( void )
{
   HB_RDDSETDEFAULT();
}

HARBOUR HB_DBSKIP( void )
{
   DBENTRYP_L pFunction;
   PHB_ITEM pItem;
   LONG lToSkip = 1;

   if( !pCurrArea )
   {
      MyError( "Alias not in use ", "1xxxx" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
   {
      pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->skip;
      if( ! pFunction )
	 MyError( "Internal error ", "9xxxx" );
      else
      {
	 pItem = hb_param( 1, IT_NUMERIC );
	 if( pItem )
	 {
	    if( pItem->type == IT_INTEGER )
	       lToSkip = pItem->item.asInteger.value;
	    else if( pItem->type == IT_LONG )
	       lToSkip = pItem->item.asLong.value;
	 }
	 ( * pFunction )( ( AREAP ) pCurrArea->pArea, lToSkip );
      }
   }
}

HARBOUR HB_DBUSEAREA( void )
{
   char * szDriver, * szFileName, * szAlias;
   WORD wLen;
   PRDDNODE pRddNode;
   PAREANODE pAreaNode;
   USHORT uiSize;
   DBENTRYP_V pFunction1;
   DBENTRYP_SP pFunction2;
   DBENTRYP_VP pFunction3;
   DBOPENINFO pInfo;

   uiNetError = 0;

   if( hb_parl( 1 ) )
      hb_SelectFirstAvailable();
   else if( pCurrArea )  /* If current WorkArea is in use then close it */
   {
      pFunction1 = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->close;
      if( !pFunction1 || ( ( * pFunction1 )( ( AREAP ) pCurrArea->pArea ) != SUCCESS ) )
      {
	 MyError( "Internal error ", "9xxxx" );
	 return;
      }

      if( pWorkAreas == pCurrArea )  /* Empty list */
	 pWorkAreas = 0;
      else
      {
	 if( pCurrArea->pPrev )
	    pCurrArea->pPrev->pNext = pCurrArea->pNext;
	 if( pCurrArea->pNext )
	    pCurrArea->pNext->pPrev = pCurrArea->pPrev;
      }

      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
      pCurrArea = 0;
   }

   szDriver = hb_parc( 2 );
   if( ( wLen = strlen( szDriver ) ) > 0 )
      szDriver = hb_strUpper( szDriver, strlen( szDriver ) );
   else
      szDriver = szDefDriver;

   if( !( pRddNode = hb_FindRddNode( szDriver ) ) )
   {
      MyError( "DBCMD/1015 Argument error", "DBCREATE" );
      return;
   }

   szFileName = hb_parc( 3 );
   if( strlen( szFileName ) == 0 )
   {
      MyError( "DBCMD/1005 Argument error", "DBUSEAREA" );
      return;
   }

   /* TODO: Implement szAlias from szFilename */
   szAlias = hb_parc( 4 );
   if( strlen( szAlias ) == 0 )
      szAlias = szFileName;

   /* Create a new WorkArea node */

   pCurrArea = ( PAREANODE ) hb_xgrab( sizeof( AREANODE ) );

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      uiSize = sizeof( AREA );    /* Default Size Area */
      pCurrArea->pArea = hb_xgrab( uiSize );
      pFunction2 = pRddNode->pTable.structSize;

      /* Need more space? */
      if( !pFunction2 || ( * pFunction2 )( ( AREAP ) pCurrArea->pArea, &uiSize ) != SUCCESS )
      {
	 hb_xfree( pCurrArea->pArea );
	 hb_xfree( pCurrArea );
	 pCurrArea = 0;
	 MyError( "Internal error ", "9xxxx" );
	 return;
      }

      if( uiSize != sizeof( AREA ) )   /* Size of Area changed */
	 pCurrArea->pArea = hb_xrealloc( pCurrArea->pArea, uiSize );

      pRddNode->uiAreaSize = uiSize; /* Update the size of WorkArea */
   }
   else
      pCurrArea->pArea = hb_xgrab( pRddNode->uiAreaSize );

   ( ( AREAP ) pCurrArea->pArea )->lprfsHost = &pRddNode->pTable;

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
   {
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
      pCurrArea = 0;
      MyError( "No table error ", "9xxxx" );
      return;
   }

   pInfo.uiArea = uiCurrArea;
   pInfo.abName = ( PBYTE ) szFileName;
   pInfo.atomAlias = ( PBYTE ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;

   pFunction3 = pRddNode->pTable.open;
   if( !pFunction3 || ( * pFunction3 )( ( AREAP ) pCurrArea->pArea, &pInfo ) != SUCCESS )
   {
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
      pCurrArea = 0;
      MyError( "No table error ", "9xxxx" );
      return;
   }

   /* Insert the new WorkArea node */

   if( !pWorkAreas )
   {
      pWorkAreas = pCurrArea;  /* The new WorkArea node is the first */
      return;
   }

   pAreaNode = pWorkAreas;
   while( pAreaNode->pNext )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea > uiCurrArea )
      {
	 /* Insert the new WorkArea node */
	 pCurrArea->pPrev = pAreaNode->pPrev;
	 pCurrArea->pNext = pAreaNode;
	 pAreaNode->pPrev = pCurrArea;
	 if( pCurrArea->pPrev )
	    pCurrArea->pPrev->pNext = pCurrArea;
      }
      pAreaNode = pAreaNode->pNext;
   }
   pAreaNode->pNext = pCurrArea; /* Append the new WorkArea node */
   pCurrArea->pPrev = pAreaNode;
}

HARBOUR HB_EOF( void )
{
   BOOL bEof = TRUE;
   DBENTRYP_BP pFunction;

   if( pCurrArea )
   {
      if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
	 MyError( "No table error ", "9xxxx" );
      else
      {
	 pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->eof;
	 if( pFunction )
	    ( * pFunction )( ( AREAP ) pCurrArea->pArea, &bEof );
      }
   }
   hb_retl( bEof );
}

HARBOUR HB_FOUND( void )
{
   BOOL bFound = FALSE;
   DBENTRYP_BP pFunction;

   if( pCurrArea )
   {
      if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
	 MyError( "No table error ", "9xxxx" );
      else
      {
	 pFunction = ( ( AREAP ) pCurrArea->pArea )->lprfsHost->found;
	 if( pFunction )
	    ( * pFunction )( ( AREAP ) pCurrArea->pArea, &bFound );
      }
   }
   hb_retl( bFound );
}

HARBOUR HB_RDDLIST( void )
{
   USHORT uiType;
   PHB_ITEM pName;
   PRDDNODE pRddNode;

   hb_CheckRdd();
   hb_arrayNew( &stack.Return, 0 );
   pName = hb_itemNew( 0 );
   pRddNode = pRddList;
   uiType = hb_parni( 1 );       /* 0 all types of RDD's */
   while( pRddNode )
   {
      if( ( uiType == 0 ) || ( pRddNode->uiType == uiType ) )
	 hb_arrayAdd( &stack.Return, hb_itemPutC( pName, pRddNode->szName ) );
      pRddNode = pRddNode->pNext;
   }
   hb_itemRelease( pName );
}

HARBOUR HB_RDDREGISTER( void )
{
   char * szDriver;
   WORD wLen;

   szDriver = hb_parc( 1 );
   if( ( wLen = strlen( szDriver ) ) > 0 )
   {
      hb_CheckRdd();
      szDriver = hb_strUpper( szDriver, wLen );
      if( hb_rddRegister( szDriver, hb_parni( 2 ) ) )
	 return;
   }
   MyError( "Internal error", "RDDREGISTER" );
}

HARBOUR HB_RDDSETDEFAULT( void )
{
   char * szNewDriver;
   WORD wLen;

   hb_CheckRdd();
   hb_retc( szDefDriver );
   szNewDriver = hb_parc( 1 );
   if( ( wLen = strlen( szNewDriver ) ) > 0 )
   {
      szNewDriver = hb_strUpper( szNewDriver, wLen );
      szDefDriver = ( char * ) hb_xrealloc( szDefDriver, wLen + 1 );
      strcpy( szDefDriver, szNewDriver );
   }
}

HARBOUR HB_RDDSHUTDOWN( void )
{
   PRDDNODE pRddNode;

   hb_CloseAll();
   hb_xfree( szDefDriver );
   while( pRddList )
   {
      pRddNode = pRddList;
      pRddList = pRddList->pNext;
      hb_xfree( pRddNode );
   }
}
