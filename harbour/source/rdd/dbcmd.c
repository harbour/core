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
#include "itemapi.h"
#include "init.h"
#include "errorapi.h"
#include "rddapi.h"
#include "set.h"
#include "ctoharb.h"
#include "rddsys.ch"
#include "set.ch"

#define HARBOUR_MAX_RDD_DRIVERNAME_LENGTH       32
#define HARBOUR_MAX_RDD_FIELDNAME_LENGTH        32

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

extern HARBOUR HB_DBF( void );
extern HARBOUR HB_SDF( void );
extern HARBOUR HB_DELIM( void );
extern HARBOUR HB_RDDSYS( void );

HARBOUR HB_ALIAS( void );
HARBOUR HB_BOF( void );
HARBOUR HB_DBCLOSEALL( void );
HARBOUR HB_DBCLOSEAREA( void );
HARBOUR HB_DBCOMMIT( void );
HARBOUR HB_DBCREATE( void );
HARBOUR HB_DBDELETE( void );
HARBOUR HB_DBGOBOTTOM( void );
HARBOUR HB_DBGOTO( void );
HARBOUR HB_DBGOTOP( void );
HARBOUR HB_DBRECALL( void );
HARBOUR HB_DBSELECTAREA( void );
HARBOUR HB_DBSETDRIVER( void );
HARBOUR HB_DBSKIP( void );
HARBOUR HB_DBTABLEEXT( void );
HARBOUR HB_DBUSEAREA( void );
HARBOUR HB_DELETED( void );
HARBOUR HB_EOF( void );
HARBOUR HB_FCOUNT( void );
HARBOUR HB_FIELDGET( void );
HARBOUR HB_FIELDNAME( void );
HARBOUR HB_FIELDPOS( void );
HARBOUR HB_FIELDPUT( void );
HARBOUR HB_FOUND( void );
HARBOUR HB_LASTREC( void );
HARBOUR HB_RDDLIST( void );
HARBOUR HB_RDDNAME( void );
HARBOUR HB_RDDREGISTER( void );
HARBOUR HB_RDDSETDEFAULT( void );
HARBOUR HB_RDDSHUTDOWN( void );
HARBOUR HB_RECCOUNT( void );
HARBOUR HB_RECNO( void );
HARBOUR HB_SELECT( void );
HARBOUR HB_USED( void );

HB_INIT_SYMBOLS_BEGIN( dbCmd__InitSymbols )
{ "ALIAS",         FS_PUBLIC, HB_ALIAS,         0 },
{ "BOF",           FS_PUBLIC, HB_BOF,           0 },
{ "DBCLOSEALL",    FS_PUBLIC, HB_DBCLOSEALL,    0 },
{ "DBCLOSEAREA",   FS_PUBLIC, HB_DBCLOSEAREA,   0 },
{ "DBCOMMIT",      FS_PUBLIC, HB_DBCOMMIT,      0 },
{ "DBCREATE",      FS_PUBLIC, HB_DBCREATE,      0 },
{ "DBDELETE",      FS_PUBLIC, HB_DBDELETE,      0 },
{ "DBGOBOTTOM",    FS_PUBLIC, HB_DBGOBOTTOM,    0 },
{ "DBGOTO",        FS_PUBLIC, HB_DBGOTO,        0 },
{ "DBGOTOP",       FS_PUBLIC, HB_DBGOTOP,       0 },
{ "DBRECALL",      FS_PUBLIC, HB_DBRECALL,      0 },
{ "DBSELECTAREA",  FS_PUBLIC, HB_DBSELECTAREA,  0 },
{ "DBSETDRIVER",   FS_PUBLIC, HB_DBSETDRIVER,   0 },
{ "DBSKIP",        FS_PUBLIC, HB_DBSKIP,        0 },
{ "DBTABLEEXT",    FS_PUBLIC, HB_DBTABLEEXT,    0 },
{ "DBUSEAREA",     FS_PUBLIC, HB_DBUSEAREA,     0 },
{ "DELETED",       FS_PUBLIC, HB_DELETED,       0 },
{ "EOF",           FS_PUBLIC, HB_EOF,           0 },
{ "FCOUNT",        FS_PUBLIC, HB_FCOUNT,        0 },
{ "FIELDGET",      FS_PUBLIC, HB_FIELDGET,      0 },
{ "FIELDNAME",     FS_PUBLIC, HB_FIELDNAME,     0 },
{ "FIELDPOS",      FS_PUBLIC, HB_FIELDPOS,      0 },
{ "FIELDPUT",      FS_PUBLIC, HB_FIELDPUT,      0 },
{ "FOUND",         FS_PUBLIC, HB_FOUND,         0 },
{ "LASTREC",       FS_PUBLIC, HB_LASTREC,       0 },
{ "RDDLIST",       FS_PUBLIC, HB_RDDLIST,       0 },
{ "RDDNAME",       FS_PUBLIC, HB_RDDNAME,       0 },
{ "RDDREGISTER",   FS_PUBLIC, HB_RDDREGISTER,   0 },
{ "RDDSETDEFAULT", FS_PUBLIC, HB_RDDSETDEFAULT, 0 },
{ "RDDSHUTDOWN",   FS_PUBLIC, HB_RDDSHUTDOWN,   0 },
{ "RECCOUNT",      FS_PUBLIC, HB_RECCOUNT,      0 },
{ "RECNO",         FS_PUBLIC, HB_RECNO,         0 },
{ "SELECT",        FS_PUBLIC, HB_SELECT,        0 },
{ "USED",          FS_PUBLIC, HB_USED,          0 }
HB_INIT_SYMBOLS_END( dbCmd__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup dbCmd__InitSymbols
#endif

static char * szDefDriver = 0;      /* Default RDD name */
static USHORT uiCurrArea = 1;       /* Selectd area */
static LPRDDNODE pRddList = 0;      /* Registered RDD's */
static USHORT uiNetError = 0;       /* Error on Networked environments */

static LPAREANODE pWorkAreas = 0;   /* WorkAreas */
static LPAREANODE pCurrArea = 0;    /* Pointer to a selectd and valid area */


/*
 * -- BASIC RDD METHODS --
 */

static ERRCODE AddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;

   pField = pArea->lpFields + pArea->uiFieldCount;
   if( pArea->uiFieldCount > 0 )
      ( ( LPFIELD ) ( pField - 1 ) )->lpfNext = pField;
   pField->sym = ( void * ) hb_xgrab( HARBOUR_MAX_RDD_FIELDNAME_LENGTH + 1 );
   memset( pField->sym, 0, HARBOUR_MAX_RDD_FIELDNAME_LENGTH + 1 );
   strncpy( ( char * ) pField->sym, ( char * ) pFieldInfo->atomName,
            HARBOUR_MAX_RDD_FIELDNAME_LENGTH );
   pField->uiType = pFieldInfo->uiType;
   pField->uiTypeExtended = pFieldInfo->typeExtended;
   pField->uiLen = pFieldInfo->uiLen;
   pField->uiDec = pFieldInfo->uiDec;
   pField->uiArea = pArea->uiArea;
   pArea->uiFieldCount++;
   return SUCCESS;
}

static ERRCODE Bof( AREAP pArea, BOOL * pBof )
{
   * pBof = pArea->fBof;
   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   if( pArea->lpFileInfo->hFile != FS_ERROR )
      hb_fsClose( pArea->lpFileInfo->hFile );
   pArea->lpFileInfo->hFile = FS_ERROR;
   ( ( PHB_DYNS ) pArea->atomAlias )->hArea = 0;
   return SUCCESS;
}

static ERRCODE Create( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   pArea->lpFileInfo->hFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
   if( pArea->lpFileInfo->hFile == FS_ERROR )
      return FAILURE;

   if( SELF_WRITEDBHEADER( pArea ) == FAILURE )
   {
      hb_fsClose( pArea->lpFileInfo->hFile );
      pArea->lpFileInfo->hFile = FS_ERROR;
      return FAILURE;
   }

   hb_fsClose( pArea->lpFileInfo->hFile );
   pArea->lpFileInfo->hFile = FS_ERROR;
   return SUCCESS;
}

static ERRCODE CreateFields( AREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiCount;
   PHB_ITEM pFieldDesc;
   DBFIELDINFO pFieldInfo;

   SELF_SETFIELDEXTENT( pArea, pStruct->item.asArray.value->ulLen );
   pFieldInfo.typeExtended = 0;
   for( uiCount = 0; uiCount < pStruct->item.asArray.value->ulLen; uiCount++ )
   {
      pFieldDesc = pStruct->item.asArray.value->pItems + uiCount;
      pFieldInfo.uiType = toupper( hb_arrayGetString( pFieldDesc, 2 )[ 0 ] );
      pFieldInfo.atomName = ( BYTE * ) hb_arrayGetString( pFieldDesc, 1 );
      pFieldInfo.uiLen = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 3 );
      pFieldInfo.uiDec = ( USHORT ) hb_arrayGetDouble( pFieldDesc, 4 );

      SELF_ADDFIELD( pArea, &pFieldInfo );
   }
   return SUCCESS;
}

static ERRCODE Eof( AREAP pArea, BOOL * pEof )
{
   * pEof = pArea->fEof;
   return SUCCESS;
}

static ERRCODE Error( AREAP pArea, PHB_ITEM pError )
{
   char * szRddName;

   szRddName = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
   SELF_SYSNAME( pArea, ( BYTE * ) szRddName );
   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, szRddName );
   hb_xfree( szRddName );
   return hb_errLaunch( pError );
}

static ERRCODE FieldCount( AREAP pArea, USHORT * uiFields )
{
   * uiFields = pArea->uiFieldCount;
   return SUCCESS;
}

static ERRCODE FieldName( AREAP pArea, USHORT uiIndex, void * szName )
{
   LPFIELD pField;

   pField = pArea->lpFields;
   while( pField && uiIndex > 1 )
   {
      pField = pField->lpfNext;
      uiIndex--;
   }
   if( pField )
   {
      strncpy( ( char * ) szName, ( char * ) pField->sym,
               HARBOUR_MAX_RDD_FIELDNAME_LENGTH );
      return SUCCESS;
   }
   else
      return FAILURE;
}

static ERRCODE Flush( AREAP pArea )
{
   HB_SYMBOL_UNUSED( pArea );

   printf( "Calling default: Flush()\n" );
   return SUCCESS;
}

static ERRCODE Found( AREAP pArea, BOOL * pFound )
{
   * pFound = pArea->fFound;
   return SUCCESS;
}

static ERRCODE Info( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( uiIndex );
   HB_SYMBOL_UNUSED( pItem );

   printf( "Calling default: Info()\n" );
   return SUCCESS;
}

static ERRCODE NewArea( AREAP pArea )
{
   pArea->lpFileInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
   pArea->lpFileInfo->hFile = FS_ERROR;
   pArea->lpFileInfo->pNext = 0;
   pArea->lpExtendInfo = ( LPDBEXTENDINFO ) hb_xgrab( sizeof( DBEXTENDINFO ) );
   memset( pArea->lpExtendInfo, 0, sizeof( DBEXTENDINFO ) );
   pArea->lpExtendInfo->pRecNo = hb_itemNew( NULL );
   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   USHORT uiFlags;

   pArea->atomAlias = hb_dynsymFind( ( char * ) pOpenInfo->atomAlias );
   if( pArea->atomAlias && ( ( PHB_DYNS ) pArea->atomAlias )->hArea )
   {
      hb_errRT_DBCMD( EG_DUPALIAS, 1011, 0, ( char * ) pOpenInfo->atomAlias );
      return FAILURE;
   }
   pArea->atomAlias = hb_dynsymGet( ( char * ) pOpenInfo->atomAlias );
   ( ( PHB_DYNS ) pArea->atomAlias )->hArea = pOpenInfo->uiArea;
   
   uiFlags = pOpenInfo->fReadonly ? FO_READ : FO_READWRITE;
   uiFlags |= pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
   pArea->lpFileInfo->hFile = hb_fsOpen( pOpenInfo->abName, uiFlags );
   
   if( pArea->lpFileInfo->hFile == FS_ERROR )
   {
      ( ( PHB_DYNS ) pArea->atomAlias )->hArea = 0;
      return FAILURE;
   }

   if( SELF_READDBHEADER( pArea ) == FAILURE )
   {
      hb_fsClose( pArea->lpFileInfo->hFile );
      pArea->lpFileInfo->hFile = FS_ERROR;
      ( ( PHB_DYNS ) pArea->atomAlias )->hArea = 0;
      return FAILURE;
   }
   pArea->lpExtendInfo->fExclusive = !pOpenInfo->fShared;

   return SELF_GOTOP( pArea );
}

static ERRCODE Release( AREAP pArea )
{
   LPFILEINFO pFileInfo;
   LPFIELD pField;

   if( pArea->lpFields )
   {
      pField = pArea->lpFields;
      while( pField )
      {
         if( pField->sym )
            hb_xfree( pField->sym );
         pField = pField->lpfNext;
      }
      hb_xfree( pArea->lpFields );
   }

   while( pArea->lpFileInfo )
   {
      pFileInfo = pArea->lpFileInfo;
      pArea->lpFileInfo = pArea->lpFileInfo->pNext;
      hb_xfree( pFileInfo );
   }

   if( pArea->lpExtendInfo )
   {
      if( pArea->lpExtendInfo->bRecord )
         hb_xfree( pArea->lpExtendInfo->bRecord );
      hb_itemRelease( pArea->lpExtendInfo->pRecNo );
      hb_xfree( pArea->lpExtendInfo );
   }

   return SUCCESS;
}

static ERRCODE SetFieldExtent( AREAP pArea, USHORT uiFieldExtent )
{
   pArea->uiFieldExtent = uiFieldExtent;
   pArea->lpFields = ( LPFIELD ) hb_xgrab( uiFieldExtent * sizeof( FIELD ) );
   memset( pArea->lpFields, 0, uiFieldExtent * sizeof( FIELD ) );
   return SUCCESS;
}

static ERRCODE Skip( AREAP pArea, LONG lToSkip )
{
   if( pArea->dbfi.fFilter )
      return SELF_SKIPFILTER( pArea, lToSkip );
   else
      return SELF_SKIPRAW( pArea, lToSkip );
}

static ERRCODE SkipFilter( AREAP pArea, LONG lToSkip )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( lToSkip );

   printf( "Calling default: SkipFilter()\n" );
   return SUCCESS;
}

static ERRCODE StructSize( AREAP pArea, USHORT * uiSize )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( uiSize );

   return SUCCESS;
}

static ERRCODE SysName( AREAP pArea, BYTE * pBuffer )
{
   USHORT uiCount;
   LPRDDNODE pRddNode;

   pRddNode = pRddList;
   for( uiCount = 0; uiCount < pArea->rddID; uiCount++ )
      pRddNode = pRddNode->pNext;
   strncpy( ( char * ) pBuffer, pRddNode->szName, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   return SUCCESS;
}

static ERRCODE UnSupported( AREAP pArea )
{
   HB_SYMBOL_UNUSED( pArea );

   printf( "Calling default: UnSupported()\n" );
   return SUCCESS;
}

static RDDFUNCS defTable = { Bof,
                             Eof,
                             Found,
                             UnSupported,
                             ( DBENTRYP_L ) UnSupported,
                             ( DBENTRYP_I ) UnSupported,
                             UnSupported,
                             Skip,
                             SkipFilter,
                             ( DBENTRYP_L ) UnSupported,
                             AddField,
                             CreateFields,
                             UnSupported,
                             ( DBENTRYP_BP ) UnSupported,
                             FieldCount,
                             FieldName,
                             Flush,
                             ( DBENTRYP_SI ) UnSupported,
                             ( DBENTRYP_SI ) UnSupported,
                             UnSupported,
                             ( DBENTRYP_LP ) UnSupported,
                             ( DBENTRYP_I ) UnSupported,
                             SetFieldExtent,
                             Close,
                             Create,
                             Info,
                             NewArea,
                             Open,
                             Release,
                             StructSize,
                             SysName,
                             Error,
                             ( DBENTRYP_VSP ) UnSupported,
                             ( DBENTRYP_VL ) UnSupported,
                             ( DBENTRYP_L ) UnSupported,
                             UnSupported,
                             UnSupported
                           };


static void hb_rddCheck( void )
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

static void hb_rddCloseAll( void )
{
   pCurrArea = pWorkAreas;
   while( pWorkAreas )
   {
      pCurrArea = pWorkAreas;
      pWorkAreas = pWorkAreas->pNext;
      SELF_CLOSE( ( AREAP ) pCurrArea->pArea );
      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
   }
   uiCurrArea = 1;
   pCurrArea = 0;
   pWorkAreas = 0;
}

static LPRDDNODE hb_rddFindNode( char * szDriver, USHORT * uiIndex )
{
   LPRDDNODE pRddNode;
   USHORT uiCount;

   uiCount = 0;
   pRddNode = pRddList;
   while( pRddNode )
   {
      if( strcmp( pRddNode->szName, szDriver ) == 0 ) /* Matched RDD */
      {
         if( uiIndex )
            * uiIndex = uiCount;
         return pRddNode;
      }
      pRddNode = pRddNode->pNext;
      uiCount++;
   }
   if( uiIndex )
      * uiIndex = 0;
   return 0;
}

static int hb_rddRegister( char * szDriver, USHORT uiType )
{
   LPRDDNODE pRddNode, pRddNewNode;
   PHB_DYNS pGetFuncTable;
   char * szGetFuncTable;

   if( hb_rddFindNode( szDriver, 0 ) )    /* Duplicated RDD */
      return 1;

   szGetFuncTable = ( char * ) hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_dynsymFind( szGetFuncTable );
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
   hb_vmPushSymbol( pGetFuncTable->pSymbol );
   hb_vmPushNil();
   hb_vmPushLong( ( long ) &pRddNewNode->uiFunctions );
   hb_vmPushLong( ( long ) &pRddNewNode->pTable );
   hb_vmDo( 2 );
   if ( hb_parni( -1 ) != SUCCESS )
   {
      hb_xfree( pRddNewNode );         /* Delete de new RDD node */
      return 3;                        /* Invalid FUNCTABLE */
   }

   if( !pRddList )                     /* First RDD node */
      pRddList = pRddNewNode;
   else
   {
      pRddNode = pRddList;
      while( pRddNode->pNext )
         pRddNode = pRddNode->pNext;   /* Locate the last RDD node */
      pRddNode->pNext = pRddNewNode;   /* Add the new RDD node */
   }
   return 0;                           /* Ok */
}

static USHORT hb_rddSelect( char * szAlias )
{
   PHB_DYNS pSymAlias;
   
   pSymAlias = hb_dynsymFind( szAlias );
   if( pSymAlias && pSymAlias->hArea )
      return pSymAlias->hArea;
   else
      return 0;
}

static void hb_rddSelectFirstAvailable( void )
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

ERRCODE hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, BYTE * szDrvName )
{
   char * szSuperName;
   LPRDDNODE pRddNode;
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   HB_SYMBOL_UNUSED( pSuperTable );

   if( !pTable )
      return FAILURE;

   /* Copy the pSuperTable into pTable */
   if( !szDrvName || !( uiCount = strlen( ( const char * ) szDrvName ) ) )
      memcpy( pTable, &defTable, sizeof( RDDFUNCS ) );
   else
   {
      szSuperName = ( char * ) hb_xgrab( uiCount + 1 );
      strcpy( szSuperName, ( char * ) szDrvName );
      hb_strUpper( szSuperName, uiCount );
      pRddNode = hb_rddFindNode( szSuperName, 0 );
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

int  hb_rddGetCurrentWorkAreaNumber( void )
{
   return uiCurrArea;
}

void hb_rddSelectWorkAreaNumber( int iArea )
{
   LPAREANODE pAreaNode;

   uiCurrArea = iArea;

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
   pCurrArea = 0;               /* Selected WorkArea is closed */
}

void hb_rddSelectWorkAreaAlias( char * szName )
{
   PHB_DYNS pSymArea;
   char * szAlias;
   WORD wLen;
   
   wLen = strlen( szName );
   szAlias = ( char * ) hb_xgrab( wLen + 1 );
   strcpy( szAlias, szName );
   hb_strUpper( szAlias, wLen );
   pSymArea = hb_dynsymFind( szAlias );
   if( pSymArea && pSymArea->hArea )
      hb_rddSelectWorkAreaNumber( pSymArea->hArea );
   else
      hb_errRT_BASE( EG_NOALIAS, 1002, 0, szAlias );

   hb_xfree( szAlias );
}

/*
 * -- HARBOUR FUNCTIONS --
 */

HARBOUR HB_ALIAS( void )
{
   USHORT uiArea;
   LPAREANODE pAreaNode;
   
   uiArea = hb_parni( 1 );
   uiArea = uiArea ? uiArea : uiCurrArea;
   pAreaNode = pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiArea )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->atomAlias &&
             ( ( PHB_DYNS ) ( ( AREAP ) pAreaNode->pArea )->atomAlias )->hArea )
         {
            hb_retc( ( ( PHB_DYNS ) ( ( AREAP ) pAreaNode->pArea )->atomAlias )->pSymbol->szName );
            return;
         }
         break;
      }
      pAreaNode = pAreaNode->pNext;
   }
   hb_retc( "" );
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
   hb_rddCloseAll();
}

HARBOUR HB_DBCLOSEAREA( void )
{
   if( !pCurrArea )
      return;

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

HARBOUR HB_DBCOMMIT( void )
{
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_FLUSH( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "DBCOMMIT" );
}

HARBOUR HB_DBCREATE( void )
{
   char * szFileName, * szDriver;
   PHB_ITEM pStruct, pFieldDesc;
   LPRDDNODE pRddNode;
   AREAP pTempArea;
   USHORT uiSize, uiRddID;
   DBOPENINFO pInfo;
   WORD wLen;
   PHB_FNAME pFileName;

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2 , IT_ARRAY );
   if( ( strlen( szFileName) == 0 ) || !pStruct || !pStruct->item.asArray.value->ulLen )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, 0, "DBCREATE" );
      return;
   }

   for( uiSize = 0; uiSize < pStruct->item.asArray.value->ulLen; uiSize++ )
   {
      pFieldDesc = pStruct->item.asArray.value->pItems + uiSize;
      if( pFieldDesc->item.asArray.value->ulLen != 4 )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, 0, "DBCREATE" );
         return;
      }

      if( strlen( hb_arrayGetString( pFieldDesc, 1 ) ) == 0 )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, 0, "DBCREATE" );
         return;
      }
   }

   hb_rddCheck();
   szDriver = hb_parc( 3 );
   if( ( wLen = strlen( szDriver ) ) > 0 )
      hb_strUpper( szDriver, wLen );
   else
      szDriver = szDefDriver;

   uiRddID = 0;
   pRddNode = hb_rddFindNode( szDriver, &uiRddID ) ;
   if( !pRddNode )
   {
      hb_errRT_DBCMD( EG_ARG, 1015, 0, "DBCREATE" );
      return;
   }

   uiSize = sizeof( AREA );    /* Default Size Area */
   pTempArea = ( AREAP ) hb_xgrab( uiSize );
   memset( pTempArea, 0, uiSize );

   pTempArea->lprfsHost = &pRddNode->pTable;

   /* Need more space? */
   SELF_STRUCTSIZE( ( AREAP ) pTempArea, &uiSize );
   if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      pTempArea = ( AREAP ) hb_xrealloc( pTempArea, uiSize );

   pRddNode->uiAreaSize = uiSize; /* Update the size of WorkArea */
   pTempArea->rddID = uiRddID;

   SELF_NEW( ( AREAP ) pTempArea );
   SELF_CREATEFIELDS( ( AREAP ) pTempArea, pStruct );

   pFileName = hb_fsFNameSplit( szFileName );

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   strcpy( szFileName, hb_parc( 1 ) );
   if( !pFileName->szExtension )
   {
      HB_DBTABLEEXT();
      strcat( szFileName, hb_parc( -1 ) );
   }
   pInfo.abName = ( BYTE * ) szFileName;

   SELF_CREATE( ( AREAP ) pTempArea, &pInfo );
   SELF_RELEASE( ( AREAP ) pTempArea );
   hb_xfree( szFileName );
   hb_xfree( pFileName );
   hb_xfree( pTempArea );
}

HARBOUR HB_DBDELETE( void )
{
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_DELETE( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "DBDELETE" );
}

HARBOUR HB_DBGOBOTTOM( void )
{
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_GOBOTTOM( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "DBGOBOTTOM" );
}

HARBOUR HB_DBGOTO( void )
{
   PHB_ITEM pItem;
   
   if( !pCurrArea || !( ( AREAP ) pCurrArea->pArea )->lprfsHost )
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "DBGOTOP" );
      return;
   }

   pItem = hb_param( 1, IT_ANY );
   if( !pItem )
      hb_errRT_DBCMD( EG_ARG, 1003, 0, "DBGOTO" );
   else
      SELF_GOTOID( ( AREAP ) pCurrArea->pArea, pItem );
}

HARBOUR HB_DBGOTOP( void )
{
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_GOTOP( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "DBGOTOP" );
}

HARBOUR HB_DBRECALL( void )
{
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_RECALL( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "DBRECALL" );
}

HARBOUR HB_DBSELECTAREA( void )
{
   USHORT uiNewArea;
   char * szAlias;
   LPAREANODE pAreaNode;

   if( ISCHAR( 1 ) )
   {
      szAlias = hb_parc( 1 );
      hb_strUpper( szAlias, strlen( szAlias ) );
     
      if( ( uiNewArea = hb_rddSelect( szAlias ) ) == 0 )
      {
         hb_errRT_BASE( EG_NOALIAS, 1002, 0, szAlias );
         return;
      }
   }
   else
      uiNewArea = hb_parni( 1 );

   if( uiNewArea == 0 )
      hb_rddSelectFirstAvailable();
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

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
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
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "DBSKIP" );
}

HARBOUR HB_DBTABLEEXT( void )
{
   LPRDDNODE pRddNode;
   AREAP pTempArea;
   USHORT uiSize, uiRddID;
   PHB_ITEM pItem;

   if( !pCurrArea )
   {
      hb_rddCheck();
      uiRddID = 0;
      pRddNode = hb_rddFindNode( szDefDriver, &uiRddID );
      if( !pRddNode )
      {
         hb_retc( "" );
         return;
      }
      uiSize = sizeof( AREA );    /* Default Size Area */
      pTempArea = ( AREAP ) hb_xgrab( uiSize );
      memset( pTempArea, 0, uiSize );
      pTempArea->lprfsHost = &pRddNode->pTable;

      /* Need more space? */
      SELF_STRUCTSIZE( ( AREAP ) pTempArea, &uiSize );
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
         pTempArea = ( AREAP ) hb_xrealloc( pTempArea, uiSize );

      pRddNode->uiAreaSize = uiSize; /* Update the size of WorkArea */
      pTempArea->rddID = uiRddID;

      if( SELF_NEW( ( AREAP ) pTempArea ) == FAILURE )
         hb_retc( "" );
      else
      {
         pItem = hb_itemPutC( 0, "" );
         SELF_INFO( ( AREAP ) pTempArea, DBI_TABLEEXT, pItem );
         hb_retc( pItem->item.asString.value );
         hb_itemRelease( pItem );
         SELF_RELEASE( ( AREAP ) pTempArea );
      }
      hb_xfree( pTempArea );
   }
   else
   {
      pItem = hb_itemPutC( 0, "" );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_TABLEEXT, pItem );
      hb_retc( pItem->item.asString.value );
      hb_itemRelease( pItem );
   }
}

HARBOUR HB_DBUSEAREA( void )
{
   char * szDriver, * szFileName, * szAlias;
   WORD wLen;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;
   USHORT uiSize, uiRddID;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;

   uiNetError = 0;

   if( hb_parl( 1 ) )
      hb_rddSelectFirstAvailable();
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

   hb_rddCheck();
   szDriver = hb_parc( 2 );
   if( ( wLen = strlen( szDriver ) ) > 0 )
      hb_strUpper( szDriver, wLen );
   else
      szDriver = szDefDriver;

   uiRddID = 0;
   pRddNode = hb_rddFindNode( szDriver, &uiRddID );
   if( !pRddNode )
   {
      hb_errRT_DBCMD( EG_ARG, 1015, 0, "DBUSEAREA" );
      return;
   }

   szFileName = hb_parc( 3 );
   if( strlen( szFileName ) == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, 1005, 0, "DBUSEAREA" );
      return;
   }

   pFileName = hb_fsFNameSplit( szFileName );
   szAlias = hb_parc( 4 );
   if( strlen( szAlias ) == 0 )
      szAlias = pFileName->szName;

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

   ( ( AREAP ) pCurrArea->pArea )->rddID = uiRddID;

   pCurrArea->pPrev = 0;
   pCurrArea->pNext = 0;

   SELF_NEW( ( AREAP ) pCurrArea->pArea );

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   strcpy( szFileName, hb_parc( 3 ) );
   if( !pFileName->szExtension )
   {
      HB_DBTABLEEXT();
      strcat( szFileName, hb_parc( -1 ) );
   }
   pInfo.uiArea = uiCurrArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;
  
   if( SELF_OPEN( ( AREAP ) pCurrArea->pArea, &pInfo ) == FAILURE )
   {
      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
      hb_xfree( szFileName );
      hb_xfree( pFileName );
      pCurrArea = 0;
      return;
   }

   hb_xfree( szFileName );
   hb_xfree( pFileName );
   ( ( AREAP ) pCurrArea->pArea )->uiArea = uiCurrArea;

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

HARBOUR HB_DELETED( void )
{
   BOOL bDeleted = FALSE;
   
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_DELETED( ( AREAP ) pCurrArea->pArea, &bDeleted );
   hb_retl( bDeleted );
}

HARBOUR HB_EOF( void )
{
   BOOL bEof = TRUE;

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
   hb_retl( bEof );
}

HARBOUR HB_FCOUNT( void )
{
   USHORT uiFields = 0;
   
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_FIELDCOUNT( ( AREAP ) pCurrArea->pArea, &uiFields );
   hb_retni( uiFields );
}

HARBOUR HB_FIELDGET( void )
{
   PHB_ITEM pItem;
   USHORT uiField;
   
   pItem = hb_itemNew( 0 );
   uiField = hb_parni( 1 );

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost && uiField )
      SELF_GETVALUE( ( AREAP ) pCurrArea->pArea, uiField, pItem );

   hb_itemReturn( pItem );
   hb_itemRelease( pItem );
}

HARBOUR HB_FIELDNAME( void )
{
   USHORT uiFields, uiIndex;
   char * szName;
   
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
   {
      uiIndex = hb_parni( 1 );
      if( SELF_FIELDCOUNT( ( AREAP ) pCurrArea->pArea, &uiFields ) == SUCCESS )
      {
         if( uiIndex > 0 && uiIndex <= uiFields )
         {
            szName = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_FIELDNAME_LENGTH + 1 );
            SELF_FIELDNAME( ( AREAP ) pCurrArea->pArea, hb_parni( 1 ), szName );
            hb_retc( szName );
            hb_xfree( szName );
            return;
         }
         hb_errRT_DBCMD( EG_ARG, 1009, 0, "FIELDNAME" );
      }
   }
   hb_retc( "" );
}

HARBOUR HB_FIELDPOS( void )
{
   USHORT uiCount;
   char * szName;
   LPFIELD pField;
   
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
   {
      szName = hb_parc( 1 );
      hb_strUpper( szName, strlen( szName ) );
      uiCount = 0;
      pField = ( ( AREAP ) pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( strcmp( szName, ( char * ) pField->sym ) == 0 )
         {
            hb_retni( uiCount + 1 );
            return;
         }
         pField = pField->lpfNext;
         uiCount++;
      }
   }
   hb_retni( 0 );
}

HARBOUR HB_FIELDPUT( void )
{
   USHORT uiIndex;
   PHB_ITEM pItem;
   
   uiIndex = hb_parni( 1 );
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost && uiIndex )
   {
      pItem = hb_param( 2, IT_ANY );
      if( SELF_PUTVALUE( ( AREAP ) pCurrArea->pArea, uiIndex, pItem ) == SUCCESS )
      {
         hb_itemReturn( pItem );
         return;
      }
   }
   hb_ret();
}

HARBOUR HB_FOUND( void )
{
   BOOL bFound = FALSE;

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_FOUND( ( AREAP ) pCurrArea->pArea, &bFound );
   hb_retl( bFound );
}

HARBOUR HB_LASTREC( void )
{
   HB_RECCOUNT();
}

HARBOUR HB_RDDLIST( void )
{
   USHORT uiType;
   PHB_ITEM pName;
   LPRDDNODE pRddNode;

   hb_rddCheck();
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

HARBOUR HB_RDDNAME( void )
{
   char * pBuffer;

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
   {
      pBuffer = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
      pBuffer[ 0 ] = '\0';
      SELF_SYSNAME( ( AREAP ) pCurrArea->pArea, ( BYTE * ) pBuffer );
      hb_retc( pBuffer );
      hb_xfree( pBuffer );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, 0, "RDDNAME" );
      hb_retc( "" );
   }
}

HARBOUR HB_RDDREGISTER( void )
{
   char * szDriver;
   WORD wLen;

   hb_rddCheck();
   szDriver = hb_parc( 1 );
   if( ( wLen = strlen( szDriver ) ) > 0 )
   {
      hb_strUpper( szDriver, wLen );
      /*
       * hb_rddRegister returns:
       *
       * 0: Ok, RDD registered
       * 1: RDD already registerd
       * > 1: error
       */
      if( hb_rddRegister( szDriver, hb_parni( 2 ) ) <= 1 )
         return;
      hb_errInternal( 9002, "", "", "" );
   }
}

HARBOUR HB_RDDSETDEFAULT( void )
{
   char * szNewDriver;
   WORD wLen;

   hb_rddCheck();
   hb_retc( szDefDriver );
   szNewDriver = hb_parc( 1 );
   if( ( wLen = strlen( szNewDriver ) ) > 0 )
   {
      hb_strUpper( szNewDriver, wLen );
      szDefDriver = ( char * ) hb_xrealloc( szDefDriver, wLen + 1 );
      strcpy( szDefDriver, szNewDriver );
   }
}

HARBOUR HB_RDDSHUTDOWN( void )
{
   LPRDDNODE pRddNode;

   hb_rddCloseAll();
   hb_xfree( szDefDriver );
   while( pRddList )
   {
      pRddNode = pRddList;
      pRddList = pRddList->pNext;
      hb_xfree( pRddNode );
   }
}

HARBOUR HB_RECCOUNT( void )
{
   LONG ulRecCount = 0;

   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_RECCOUNT( ( AREAP ) pCurrArea->pArea, &ulRecCount );
   hb_retnl( ulRecCount );
}

HARBOUR HB_RECNO( void )
{
   PHB_ITEM pRecNo;
   
   pRecNo = hb_itemPutNL( NULL, 0 );
   if( pCurrArea && ( ( AREAP ) pCurrArea->pArea )->lprfsHost )
      SELF_RECNO( ( AREAP ) pCurrArea->pArea, pRecNo );
   hb_itemReturn( pRecNo );
}

HARBOUR HB_SELECT( void )
{
   char * szAlias;
   
   szAlias = hb_parc( 1 );
   if( strlen( szAlias ) > 0 )
      hb_retni( hb_rddSelect( szAlias ) );
   else
      hb_retni( uiCurrArea );
}

HARBOUR HB_USED( void )
{
   if( pCurrArea )
      hb_retl( 1 );
   else
      hb_retl( 0 );
}

