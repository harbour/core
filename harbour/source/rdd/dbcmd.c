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

#include <ctype.h>
#include "extend.h"
#include "init.h"
#include "itemapi.h"
#include "errorapi.h"
#include "rddapi.h"
#include "set.h"
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
} RDDNODE;

typedef RDDNODE * LPRDDNODE;

typedef struct _AREANODE
{
   void *             pArea;   /* WorkAreas with different sizes */
   struct _AREANODE * pPrev;   /* Prev WorkArea in the list */
   struct _AREANODE * pNext;   /* Next WorkArea in the list */
} AREANODE;

typedef AREANODE * LPAREANODE;

/* TODO: must be changed to a hb_errorRT... */
static void MyError( char * szError, char * szParam )
{
   printf( "\n%s %s\n", szError, szParam );
}

HARBOUR HB_BOF( void );
HARBOUR HB_DBCLOSEALL( void );
HARBOUR HB_DBCLOSEAREA( void );
HARBOUR HB_DBCREATE( void );
HARBOUR HB_DBF( void );
HARBOUR HB_DBGOBOTTOM( void );
HARBOUR HB_DBGOTO( void );
HARBOUR HB_DBGOTOP( void );
HARBOUR HB_DBSELECTAREA( void );
HARBOUR HB_DBSETDRIVER( void );
HARBOUR HB_DBSKIP( void );
HARBOUR HB_DBUSEAREA( void );
HARBOUR HB_DELIM( void );
HARBOUR HB_EOF( void );
HARBOUR HB_FOUND( void );
HARBOUR HB_RDDLIST( void );
HARBOUR HB_RDDREGISTER( void );
HARBOUR HB_RDDSETDEFAULT( void );
HARBOUR HB_RDDSHUTDOWN( void );
HARBOUR HB_RDDSYS( void );
HARBOUR HB_SDF( void );

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
static LPRDDNODE pRddList = 0;      /* Registered RDD's */
static USHORT uiNetError = 0;       /* Error on Networked environments */

static LPAREANODE pWorkAreas = 0;   /* WorkAreas */
static LPAREANODE pCurrArea = 0;    /* Pointer to a selectd and valid area */

static void hb_CheckRdd( void )
{
   if( !szDefDriver )
   {
      szDefDriver = ( char * ) hb_xgrab( 1 );
      szDefDriver[ 0 ] = '\0';

      /* Force link the built-in RDD's */
      HB_DBF();
      HB_SDF();
      HB_DELIM();
      HB_RDDSYS();
   }
}

static void hb_CloseAll( void )
{
   pCurrArea = pWorkAreas;
   while( pWorkAreas )
   {
      pCurrArea = pWorkAreas;
      pWorkAreas = pWorkAreas->pNext;
      if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
         MyError( "No table error ", "9xxxx" );
      else
         SELF_CLOSE( ( AREAP ) pCurrArea->pArea );

      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
   }
   uiCurrArea = 1;
   pCurrArea = 0;
   pWorkAreas = 0;
}

static LPRDDNODE hb_FindRddNode( char * szDriver )
{
   LPRDDNODE pRddNode;

   pRddNode = pRddList;
   while( pRddNode )
   {
      if( strcmp( pRddNode->szName, szDriver ) == 0 ) /* Matched RDD */
         return pRddNode;
      pRddNode = pRddNode->pNext;
   }
   return 0;
}

static int hb_rddRegister( char * szDriver, USHORT uiType )
{
   LPRDDNODE pRddNode, pRddNewNode;
   PHB_DYNS pGetFuncTable;
   char * szGetFuncTable;

   if( hb_FindRddNode( szDriver ) )    /* Duplicated RDD */
      return 1;

   szGetFuncTable = ( char * ) hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_FindDynSym( szGetFuncTable );
   hb_xfree( szGetFuncTable );
   if( !pGetFuncTable )
      return 2;              /* Not valid RDD */

   /* Create a new RDD node */
   pRddNewNode = ( LPRDDNODE ) hb_xgrab( sizeof( RDDNODE ) );
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
      return 3;                     /* Invalid FUNCTABLE */
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
   return 0;  /* Ok */
}

static USHORT hb_FindAlias( char * szAlias )
{
   return 1; /* Not implemented yet */
}

static void hb_SelectFirstAvailable( void )
{
   LPAREANODE pAreaNode;

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

static ERRCODE UnSupported_V( AREAP pArea )
{
   printf( "Calling default: UnSupported()\n" );
   return SUCCESS;
}

static ERRCODE UnSupported_L( AREAP pArea, LONG lLong )
{
   printf( "Calling default: UnSupported()\n" );
   return SUCCESS;
}

static ERRCODE Bof( AREAP pArea, BOOL * pBof )
{
   printf( "Calling default: Bof()\n" );
   return SUCCESS;
}

static ERRCODE Eof( AREAP pArea, BOOL * pEof )
{
   printf( "Calling default: Eof()\n" );
   return SUCCESS;
}

static ERRCODE Found( AREAP pArea, BOOL * pFound )
{
   printf( "Calling default: Found()\n" );
   return SUCCESS;
}

static ERRCODE Skip( AREAP pArea, LONG lToSkip )
{
   printf( "Calling default: Skip()\n" );
   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   printf( "Calling default: Close()\n" );
   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   printf( "Calling default: Open()\n" );
   return SUCCESS;
}

static ERRCODE Release( AREAP pArea )
{
   printf( "Calling default: Release()\n" );
   return SUCCESS;
}

static ERRCODE StructSize( AREAP pArea, USHORT * uiSize )
{
   pArea = pArea;     /* Just to keep compiler silent */
   uiSize = uiSize;   /* Just to keep compiler silent */
   return SUCCESS;
}

static RDDFUNCS defTable = { Bof,
                             Eof,
                             Found,
                             UnSupported_V,
                             UnSupported_L,
                             UnSupported_V,
                             Skip,
                             Close,
                             Open, /* Not defCreate */
                             Open,
                             Release,
                             StructSize,
                             UnSupported_V
                           };

ERRCODE hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, BYTE * szDrvName )
{
   char * szSuperName;
   LPRDDNODE pRddNode;
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   if( !pTable )
      return FAILURE;

   /* Copy the pSuperTable into pTable */
   if( !szDrvName || !( uiCount = strlen( ( const char * ) szDrvName ) ) )
      memcpy( pTable, &defTable, sizeof( RDDFUNCS ) );
   else
   {
      szSuperName = ( char * ) hb_xgrab( uiCount + 1 );
      strcpy( szSuperName, ( char * ) szDrvName );
      szSuperName = hb_strUpper( szSuperName, uiCount );
      pRddNode = hb_FindRddNode( szSuperName );
      hb_xfree( szSuperName );
      if( !pRddNode )
        {
          return FAILURE;
        }
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

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_BOF( ( AREAP ) pCurrArea->pArea, &bBof );
   hb_retl( bBof );
}

HARBOUR HB_DBCLOSEALL( void )
{
   hb_CloseAll();
}

HARBOUR HB_DBCLOSEAREA( void )
{
   if( !pCurrArea )
      return;

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
      SELF_CLOSE( ( AREAP ) pCurrArea->pArea );

   SELF_RELEASE( ( AREAP ) pCurrArea->pArea );

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
   PHB_ITEM pStruct, pFieldDesc;
   WORD wLen;
   LPRDDNODE pRddNode;
   AREAP pTempArea;
   USHORT uiSize, uiFieldCount;
   DBOPENINFO pInfo;
   LPFIELD pFields, pField, pPrevField;

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2 , IT_ARRAY );
   if( ( strlen( szFileName) == 0 ) || !pStruct || !pStruct->item.asArray.value->ulLen )
   {
      MyError( "DBCMD/1014 Argument error", "DBCREATE" );
      return;
   }

   /* TODO: append default extension to szFileName if necessary */

   pFields = ( LPFIELD ) hb_xgrab( pStruct->item.asArray.value->ulLen * sizeof( FIELD ) );
   pField = pFields;
   pPrevField = 0;

   for( uiSize = 0; uiSize < pStruct->item.asArray.value->ulLen; uiSize++ )
   {
      pFieldDesc = pStruct->item.asArray.value->pItems + uiSize;
      if( pFieldDesc->item.asArray.value->ulLen != 4 )
      {
         hb_xfree( pFields );
         MyError( "DBCMD/1014 Argument error", "DBCREATE" );
         return;
      }

      /* TODO: change to a symbol */
      pField->sym = ( void * ) hb_arrayGetString( pFieldDesc, 1 );

      if( strlen( ( char * ) pField->sym ) == 0 )
      {
         hb_xfree( pFields );
         MyError( "DBCMD/1014 Argument error", "DBCREATE" );
         return;
      }

      pField->uiLen = 0;
      pField->uiType = toupper( hb_arrayGetString( pFieldDesc, 2 )[ 0 ] );
      if( pField->uiType == 'N' )
      {
         pField->uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 );
         pField->uiDec = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 4 );
      }
      else
      {
         if( pField->uiType == 'L' || pField->uiType == 'D' ||
             pField->uiType == 'M' )
            pField->uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 );
         else if( pField->uiType == 'C' )
         {
            pField->uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 ) +
                            ( ( USHORT ) hb_arrayGetDouble( pFieldDesc, 4 ) << 8 );
         }
         pField->uiDec = 0;
      }
      if( pField->uiLen == 0 )
      {
         MyError( "DBCMD/1014 Argument error", "DBCREATE" );
         hb_xfree( pFields );
         return;
      }

      pField->uiArea = 0;
      pField->uiTypeExtended = 0;

      pPrevField = pField;  /* Link the field */
      pField++;
      if( uiSize < ( pStruct->item.asArray.value->ulLen - 1 ) )
         pPrevField->lpfNext = pField;
      else
         pPrevField->lpfNext = 0;
   }
   uiFieldCount = uiSize;

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

   uiSize = sizeof( AREA );    /* Default Size Area */
   pTempArea = ( AREAP ) hb_xgrab( uiSize );

   pTempArea->lprfsHost = &pRddNode->pTable;

   /* Need more space? */
   SELF_STRUCTSIZE( ( AREAP ) pTempArea, &uiSize );
   if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      pTempArea = ( AREAP ) hb_xrealloc( pTempArea, uiSize );

   pRddNode->uiAreaSize = uiSize; /* Update the size of WorkArea */

   /* Fill the WorkArea */
   pTempArea->uiFieldCount = uiFieldCount;
   pTempArea->lpFields = pFields;
   pTempArea->lpFileInfo = 0;

   pInfo.abName = ( BYTE * ) szFileName;
   if( SELF_CREATE( ( AREAP ) pTempArea, &pInfo ) == FAILURE )
      MyError( "DBCMD/1015 Create error", "DBCREATE" );

   hb_xfree( pTempArea->lpFields );
   hb_xfree( pTempArea );
}

HARBOUR HB_DBGOBOTTOM( void )
{
   if( !pCurrArea )
   {
      MyError( "Alias not in use ", "1xxxx" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
      SELF_GOBOTTOM( ( AREAP ) pCurrArea->pArea );
}

HARBOUR HB_DBGOTO( void )
{
   if( !pCurrArea )
   {
      MyError( "Alias not in use ", "1xxxx" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else if( ISNUM( 1 ) )
      SELF_GOTO( ( AREAP ) pCurrArea->pArea, hb_parnl( 1 ) );
   else
      MyError( "DBCMD/1068 Argument error", "DBGOTO" );
}

HARBOUR HB_DBGOTOP( void )
{
   if( !pCurrArea )
   {
      MyError( "Alias not in use ", "1xxxx" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      MyError( "No table error ", "9xxxx" );
   else
      SELF_GOTOP( ( AREAP ) pCurrArea->pArea );
}

HARBOUR HB_DBSELECTAREA( void )
{
   USHORT uiNewArea;
   char * szAlias;
   LPAREANODE pAreaNode;

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
      pItem = hb_param( 1, IT_NUMERIC );
      if( pItem )
      {
         if( pItem->type == IT_INTEGER )
            lToSkip = pItem->item.asInteger.value;
         else if( pItem->type == IT_LONG )
            lToSkip = pItem->item.asLong.value;
      }
      SELF_SKIP( ( AREAP ) pCurrArea->pArea, lToSkip );
   }
}

HARBOUR HB_DBUSEAREA( void )
{
   char * szDriver, * szFileName, * szAlias;
   WORD wLen;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;
   USHORT uiSize;
   DBOPENINFO pInfo;

   uiNetError = 0;

   if( hb_parl( 1 ) )
      hb_SelectFirstAvailable();
   else if( pCurrArea )  /* If current WorkArea is in use then close it */
   {
      SELF_CLOSE( ( AREAP ) pCurrArea->pArea );
      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );

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

   pCurrArea = ( LPAREANODE ) hb_xgrab( sizeof( AREANODE ) );

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      uiSize = sizeof( AREA );    /* Default Size Area */
      pCurrArea->pArea = ( AREAP ) hb_xgrab( uiSize );
      memset( pCurrArea->pArea, 0, uiSize );
      ( ( AREAP ) pCurrArea->pArea )->lprfsHost = &pRddNode->pTable;

      /* Need more space? */
      SELF_STRUCTSIZE( ( AREAP ) pCurrArea->pArea, &uiSize );
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
         pCurrArea->pArea = ( AREAP ) hb_xrealloc( pCurrArea->pArea, uiSize );

      pRddNode->uiAreaSize = uiSize; /* Update the size of WorkArea */
   }
   else
   {
      pCurrArea->pArea = ( AREAP ) hb_xgrab( pRddNode->uiAreaSize );
      memset( pCurrArea->pArea, 0, pRddNode->uiAreaSize );
      ( ( AREAP ) pCurrArea->pArea )->lprfsHost = &pRddNode->pTable;
   }

   pCurrArea->pPrev = 0;
   pCurrArea->pNext = 0;

   if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
   {
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
      pCurrArea = 0;
      MyError( "No table error ", "9xxxx" );
      return;
   }

   pInfo.uiArea = uiCurrArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;

   SELF_OPEN( ( AREAP ) pCurrArea->pArea, &pInfo );

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

   if( pCurrArea )
   {
      if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
         MyError( "No table error ", "9xxxx" );
      else
         SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
   }
   hb_retl( bEof );
}

HARBOUR HB_FOUND( void )
{
   BOOL bFound = FALSE;

   if( pCurrArea )
   {
      if( !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
         MyError( "No table error ", "9xxxx" );
      else
         SELF_FOUND( ( AREAP ) pCurrArea->pArea, &bFound );
   }
   hb_retl( bFound );
}

HARBOUR HB_RDDLIST( void )
{
   USHORT uiType;
   PHB_ITEM pName;
   LPRDDNODE pRddNode;

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

   hb_CheckRdd();
   szDriver = hb_parc( 1 );
   if( ( wLen = strlen( szDriver ) ) > 0 )
   {
      szDriver = hb_strUpper( szDriver, wLen );
      /*
       * hb_rddRegister returns:
       *
       * 0: Ok, RDD registered
       * 1: RDD already registerd
       * > 1: error
       */
      if( hb_rddRegister( szDriver, hb_parni( 2 ) ) <= 1 )
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
   LPRDDNODE pRddNode;

   hb_CloseAll();
   hb_xfree( szDefDriver );
   while( pRddList )
   {
      pRddNode = pRddList;
      pRddList = pRddList->pNext;
      hb_xfree( pRddNode );
   }
}
