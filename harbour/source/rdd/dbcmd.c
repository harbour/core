/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base RDD module
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

#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "rddapi.h"
#include "set.h"
#include "ctoharb.h"
#include "rddsys.ch"
#include "set.ch"
#include "langapi.h"

#define HARBOUR_MAX_RDD_DRIVERNAME_LENGTH       32
#define HARBOUR_MAX_RDD_ALIAS_LENGTH            32
#define HARBOUR_MAX_RDD_FIELDNAME_LENGTH        32

typedef struct _RDDNODE
{
   char     szName[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   USHORT   uiType;            /* Type of RDD */
   RDDFUNCS pTable;            /* Table of functions */
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

extern HARBOUR HB__DBF( void );
extern HARBOUR HB__SDF( void );
extern HARBOUR HB__DELIM( void );
extern HARBOUR HB_RDDSYS( void );

HARBOUR HB_AFIELDS( void );
HARBOUR HB_ALIAS( void );
HARBOUR HB_BOF( void );
HARBOUR HB_DBAPPEND( void );
HARBOUR HB_DBCLOSEALL( void );
HARBOUR HB_DBCLOSEAREA( void );
HARBOUR HB_DBCOMMIT( void );
HARBOUR HB_DBCOMMITALL( void );
HARBOUR HB_DBCREATE( void );
HARBOUR HB_DBDELETE( void );
HARBOUR HB_DBF( void );
HARBOUR HB_DBGOBOTTOM( void );
HARBOUR HB_DBGOTO( void );
HARBOUR HB_DBGOTOP( void );
HARBOUR HB_DBRECALL( void );
HARBOUR HB_DBRLOCK( void );
HARBOUR HB_DBRLOCKLIST( void );
HARBOUR HB_DBRUNLOCK( void );
HARBOUR HB_DBSELECTAREA( void );
HARBOUR HB_DBSETDRIVER( void );
HARBOUR HB_DBSKIP( void );
HARBOUR HB_DBSTRUCT( void );
HARBOUR HB_DBTABLEEXT( void );
HARBOUR HB_DBUNLOCK( void );
HARBOUR HB_DBUNLOCKALL( void );
HARBOUR HB_DBUSEAREA( void );
HARBOUR HB_DELETED( void );
HARBOUR HB_EOF( void );
HARBOUR HB_FCOUNT( void );
HARBOUR HB_FIELDGET( void );
HARBOUR HB_FIELDNAME( void );
HARBOUR HB_FIELDPOS( void );
HARBOUR HB_FIELDPUT( void );
HARBOUR HB_FLOCK( void );
HARBOUR HB_FOUND( void );
HARBOUR HB_HEADER( void );
HARBOUR HB_LASTREC( void );
HARBOUR HB_LOCK( void );
HARBOUR HB_LUPDATE( void );
HARBOUR HB_NETERR( void );
HARBOUR HB_RDDLIST( void );
HARBOUR HB_RDDNAME( void );
HARBOUR HB_RDDREGISTER( void );
HARBOUR HB_RDDSETDEFAULT( void );
HARBOUR HB_RECCOUNT( void );
HARBOUR HB_RECNO( void );
HARBOUR HB_RECSIZE( void );
HARBOUR HB_RLOCK( void );
HARBOUR HB_SELECT( void );
HARBOUR HB_USED( void );
HARBOUR HB___RDDSETDEFAULT( void );

static char * szDefDriver = NULL;    /* Default RDD name */
static USHORT uiCurrArea = 1;        /* Selectd area */
static LPRDDNODE pRddList = NULL;    /* Registered RDD's */
static BOOL bNetError = FALSE;       /* Error on Networked environments */

static LPAREANODE pWorkAreas = NULL; /* WorkAreas */
static LPAREANODE pCurrArea = NULL;  /* Pointer to a selectd and valid area */


/*
 * -- BASIC RDD METHODS --
 */

static ERRCODE AddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;

   pField = pArea->lpFields + pArea->uiFieldCount;
   if( pArea->uiFieldCount > 0 )
      ( ( LPFIELD ) ( pField - 1 ) )->lpfNext = pField;
   pField->sym = ( void * ) hb_dynsymGet( ( char * ) pFieldInfo->atomName );
   pField->uiType = pFieldInfo->uiType;
   pField->uiTypeExtended = pFieldInfo->typeExtended;
   pField->uiLen = pFieldInfo->uiLen;
   pField->uiDec = pFieldInfo->uiDec;
   pField->uiArea = pArea->uiArea;
   pArea->uiFieldCount++;
   return SUCCESS;
}

static ERRCODE Alias( AREAP pArea, BYTE * szAlias )
{
   strncpy( ( char * ) szAlias,
            ( ( PHB_DYNS ) pArea->atomAlias )->pSymbol->szName,
            HARBOUR_MAX_RDD_ALIAS_LENGTH );
   return SUCCESS;
}

static ERRCODE Bof( AREAP pArea, BOOL * pBof )
{
   * pBof = pArea->fBof;
   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   ( ( PHB_DYNS ) pArea->atomAlias )->hArea = 0;

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
      pFieldInfo.uiType = toupper( hb_arrayGetCPtr( pFieldDesc, 2 )[ 0 ] );
      pFieldInfo.atomName = ( BYTE * ) hb_arrayGetCPtr( pFieldDesc, 1 );
      pFieldInfo.uiLen = ( USHORT ) hb_arrayGetND( pFieldDesc, 3 );
      pFieldInfo.uiDec = ( USHORT ) hb_arrayGetND( pFieldDesc, 4 );

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

static ERRCODE FieldInfo( AREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem )
{
   LPFIELD pField;
   char szType[ 2 ];

   pField = pArea->lpFields;
   while( pField && uiIndex > 1 )
   {
      pField = pField->lpfNext;
      uiIndex--;
   }
   if( pField )
   {
      switch( uiType )
      {
         case DBS_NAME:
            hb_itemPutC( pItem, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName );
            return SUCCESS;

         case DBS_TYPE:
            szType[ 0 ] = pField->uiType;
            szType[ 1 ] = '\0';
            hb_itemPutC( pItem, szType );
            return SUCCESS;

         case DBS_LEN:
            hb_itemPutNL( pItem, pField->uiLen );
            return SUCCESS;

         case DBS_DEC:
            hb_itemPutNL( pItem, pField->uiDec );
            return SUCCESS;
      }
   }
   return FAILURE;
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
      strncpy( ( char * ) szName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName,
               HARBOUR_MAX_RDD_FIELDNAME_LENGTH );
      return SUCCESS;
   }
   return FAILURE;
}

static ERRCODE Found( AREAP pArea, BOOL * pFound )
{
   * pFound = pArea->fFound;
   return SUCCESS;
}

static ERRCODE GetRec( AREAP pArea, BYTE ** pBuffer )
{
   * pBuffer = pArea->lpExtendInfo->bRecord;
   return SUCCESS;
}

static ERRCODE GoCold( AREAP pArea )
{
   PHB_ITEM pError;

   if( pArea->lpExtendInfo->fReadOnly )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_READONLY );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_READONLY ) );
      hb_errPutSubCode( pError, 1025 );
      SELF_ERROR( ( AREAP ) pCurrArea->pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }
   else
      return SUCCESS;
}

static ERRCODE GoHot( AREAP pArea )
{
   pArea->lpExtendInfo->fRecordChanged = FALSE;
   pArea->lpFileInfo->fAppend = FALSE;
   return SUCCESS;
}

static ERRCODE NewArea( AREAP pArea )
{
   pArea->lpFileInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
   memset( pArea->lpFileInfo, 0, sizeof( FILEINFO ) );
   pArea->lpFileInfo->hFile = FS_ERROR;
   pArea->lpExtendInfo = ( LPDBEXTENDINFO ) hb_xgrab( sizeof( DBEXTENDINFO ) );
   memset( pArea->lpExtendInfo, 0, sizeof( DBEXTENDINFO ) );
   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   pArea->atomAlias = hb_dynsymGet( ( char * ) pOpenInfo->atomAlias );
   if( ( ( PHB_DYNS ) pArea->atomAlias )->hArea )
   {
      hb_errRT_DBCMD( EG_DUPALIAS, 1011, NULL, ( char * ) pOpenInfo->atomAlias );
      return FAILURE;
   }

   ( ( PHB_DYNS ) pArea->atomAlias )->hArea = pOpenInfo->uiArea;
   pArea->lpExtendInfo->fExclusive = !pOpenInfo->fShared;
   pArea->lpExtendInfo->fReadOnly = pOpenInfo->fReadonly;

   return SUCCESS;
}

static ERRCODE Release( AREAP pArea )
{
   LPFILEINFO pFileInfo;

   if( pArea->lpFields )
      hb_xfree( pArea->lpFields );

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
   PHB_ITEM pError;

   HB_SYMBOL_UNUSED( pArea );

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );
   SELF_ERROR( pArea, pError );
   hb_errRelease( pError );
   return FAILURE;
}

static RDDFUNCS defTable = { Bof,
                             Eof,
                             Found,
                             UnSupported,
                             ( DBENTRYP_UL ) UnSupported,
                             ( DBENTRYP_I ) UnSupported,
                             UnSupported,
                             Skip,
                             ( DBENTRYP_L ) UnSupported,
                             ( DBENTRYP_L ) UnSupported,
                             AddField,
                             ( DBENTRYP_B ) UnSupported,
                             CreateFields,
                             UnSupported,
                             ( DBENTRYP_BP ) UnSupported,
                             FieldCount,
                             ( DBENTRYP_VF ) UnSupported,
                             FieldInfo,
                             FieldName,
                             UnSupported,
                             GetRec,
                             ( DBENTRYP_SI ) UnSupported,
                             ( DBENTRYP_SVL ) UnSupported,
                             GoCold,
                             GoHot,
                             ( DBENTRYP_P ) UnSupported,
                             ( DBENTRYP_SI ) UnSupported,
                             UnSupported,
                             ( DBENTRYP_ULP ) UnSupported,
                             ( DBENTRYP_ISI ) UnSupported,
                             ( DBENTRYP_I ) UnSupported,
                             SetFieldExtent,
                             Alias,
                             Close,
                             ( DBENTRYP_VP ) UnSupported,
                             ( DBENTRYP_SI ) UnSupported,
                             NewArea,
                             Open,
                             Release,
                             StructSize,
                             SysName,
                             Error,
                             ( DBENTRYP_VSP ) UnSupported,
                             ( DBENTRYP_VL ) UnSupported,
                             ( DBENTRYP_UL ) UnSupported,
                             UnSupported,
                             ( DBENTRYP_VP ) UnSupported,
                             ( DBENTRYP_VP ) UnSupported,
                             UnSupported,
                             UnSupported,
                             ( DBENTRYP_SVP ) UnSupported
                           };


static void hb_rddCheck( void )
{
   if( !szDefDriver )
   {
      szDefDriver = ( char * ) hb_xgrab( 1 );
      szDefDriver[ 0 ] = '\0';

      /* Force link the built-in RDD's */
      HB__DBF();
      HB__SDF();
      HB__DELIM();
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
   pCurrArea = NULL;
   pWorkAreas = NULL;
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
   return NULL;
}

static int hb_rddRegister( char * szDriver, USHORT uiType )
{
   LPRDDNODE pRddNode, pRddNewNode;
   PHB_DYNS pGetFuncTable;
   char * szGetFuncTable;
   USHORT uiFunctions;

   if( hb_rddFindNode( szDriver, 0 ) )    /* Duplicated RDD */
      return 1;

   szGetFuncTable = ( char * ) hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_dynsymFindName( szGetFuncTable );
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
   hb_vmPushLong( ( long ) &uiFunctions );
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

   pSymAlias = hb_dynsymFindName( szAlias );
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
   pCurrArea = NULL;   /* Selected WorkArea must be created */
}

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
   {
      memcpy( pTable, &defTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &defTable, sizeof( RDDFUNCS ) );
   }
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
      memcpy( pSuperTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
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

/*
 * -- FUNCTIONS ACCESSED FROM VIRTUAL MACHINE --
 */

int  hb_rddGetCurrentWorkAreaNumber( void )
{
   return uiCurrArea;
}

ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
   LPAREANODE pAreaNode;

   uiCurrArea = iArea;

   pAreaNode = pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiCurrArea )
      {
         pCurrArea = pAreaNode; /* Select a valid WorkArea */
         return SUCCESS;
      }
      pAreaNode = pAreaNode->pNext;
   }
   pCurrArea = NULL;               /* Selected WorkArea is closed */
   return FAILURE;
}

ERRCODE hb_rddSelectWorkAreaSymbol( PHB_SYMB pSymAlias )
{
   ERRCODE bResult;

   if( pSymAlias->pDynSym->hArea )
      bResult = hb_rddSelectWorkAreaNumber( pSymAlias->pDynSym->hArea );
   else
   {
      /* generate an error with retry possibility
       * (user created error handler can open a missing database)
       */
      USHORT uiAction = E_RETRY;
      HB_ITEM_PTR pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, 1002,
                              NULL, pSymAlias->szName, 0, EF_CANRETRY );

      bResult = FAILURE;
      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
            if( pSymAlias->pDynSym->hArea )
            {
               bResult = hb_rddSelectWorkAreaNumber( pSymAlias->pDynSym->hArea );
               uiAction = E_DEFAULT;
            }
      }
      hb_errRelease( pError );
   }
   return bResult;
}

ERRCODE hb_rddSelectWorkAreaAlias( char * szName )
{
   PHB_DYNS pSymArea;
   ERRCODE bResult;

   pSymArea = hb_dynsymFindName( szName );
   if( pSymArea && pSymArea->hArea )
      bResult = hb_rddSelectWorkAreaNumber( pSymArea->hArea );
   else
   {
      /* generate an error with retry possibility
       * (user created error handler can open a missing database)
       */
      USHORT uiAction = E_RETRY;
      HB_ITEM_PTR pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, 1002,
                              NULL, szName, 0, EF_CANRETRY );

      bResult = FAILURE;
      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            pSymArea = hb_dynsymFindName( szName );
            if( pSymArea && pSymArea->hArea )
            {
               bResult = hb_rddSelectWorkAreaNumber( pSymArea->hArea );
               uiAction = E_DEFAULT;
            }
         }
      }
      hb_errRelease( pError );
   }

   return bResult;
}

ERRCODE hb_rddGetFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );

   if( bSuccess == FAILURE )
   {
      /* generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      USHORT uiAction = E_RETRY;
      HB_ITEM_PTR pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                              NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );
            if( bSuccess == SUCCESS )
               uiAction = E_DEFAULT;
         }
      }
      hb_errRelease( pError );
   }
   return bSuccess;
}

ERRCODE hb_rddPutFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE bSuccess = hb_rddFieldPut( pItem, pFieldSymbol );

   if( bSuccess == FAILURE )
   {
      /* generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      USHORT uiAction = E_RETRY;
      HB_ITEM_PTR pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                              NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            bSuccess = hb_rddFieldPut( pItem, pFieldSymbol );
            if( bSuccess == SUCCESS )
               uiAction = E_DEFAULT;
         }
      }
      hb_errRelease( pError );
   }
   return bSuccess;
}

ERRCODE hb_rddFieldPut( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   LPFIELD pField;
   USHORT uiField;

   if( pCurrArea )
   {
      uiField = 1;
      pField = ( ( AREAP ) pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            SELF_PUTVALUE( ( AREAP ) pCurrArea->pArea, uiField, pItem );
            return SUCCESS;
         }
         pField = pField->lpfNext;
         uiField++;
      }
   }
   return FAILURE;
}

ERRCODE hb_rddFieldGet( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   LPFIELD pField;
   USHORT uiField;

   if( pCurrArea )
   {
      uiField = 1;
      pField = ( ( AREAP ) pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            SELF_GETVALUE( ( AREAP ) pCurrArea->pArea, uiField, pItem );
            return SUCCESS;
         }
         pField = pField->lpfNext;
         uiField++;
      }
   }
   return FAILURE;
}

/*
 * -- HARBOUR FUNCTIONS --
 */

HARBOUR HB_AFIELDS( void )
{
   PHB_ITEM pName, pType, pLen, pDec, pItem;
   USHORT uiFields, uiArrayLen, uiCount;

   if( !pCurrArea )
   {
      hb_retni( 0 );
      return;
   }

   pName = hb_param( 1, IT_ARRAY );
   pType = hb_param( 2, IT_ARRAY );
   pLen = hb_param( 3, IT_ARRAY );
   pDec = hb_param( 4, IT_ARRAY );
   if( !pName && !pType && !pLen && !pDec )
   {
      hb_retni( 0 );
      return;
   }

   pItem = hb_itemNew( NULL );
   SELF_FIELDCOUNT( ( AREAP ) pCurrArea->pArea, &uiFields );
   if( pName )
   {
      uiArrayLen = hb_arrayLen( pName );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_NAME, pItem );
         hb_arraySet( pName, uiCount, pItem );
      }
   }
   if( pType )
   {
      uiArrayLen = hb_arrayLen( pType );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_TYPE, pItem );
         hb_arraySet( pType, uiCount, pItem );
      }
   }
   if( pLen )
   {
      uiArrayLen = hb_arrayLen( pLen );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_LEN, pItem );
         hb_arraySet( pLen, uiCount, pItem );
      }
   }
   if( pDec )
   {
      uiArrayLen = hb_arrayLen( pDec );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_DEC, pItem );
         hb_arraySet( pDec, uiCount, pItem );
      }
   }

   hb_itemRelease( pItem );
   hb_retni( uiArrayLen );
}

HARBOUR HB_ALIAS( void )
{
   USHORT uiArea;
   LPAREANODE pAreaNode;
   char * szAlias;

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
            szAlias = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 );
            SELF_ALIAS( ( AREAP ) pAreaNode->pArea, ( BYTE * ) szAlias );
            hb_retc( szAlias );
            hb_xfree( szAlias );
            return;
         }
         break;
      }
      pAreaNode = pAreaNode->pNext;
   }
   hb_retc( "" );
}

HARBOUR HB_DBF( void )
{
   LPAREANODE pAreaNode = pWorkAreas;

   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiCurrArea )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->atomAlias &&
             ( ( PHB_DYNS ) ( ( AREAP ) pAreaNode->pArea )->atomAlias )->hArea )
         {
            char * szAlias = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 );
            SELF_ALIAS( ( AREAP ) pAreaNode->pArea, ( BYTE * ) szAlias );
            hb_retc( szAlias );
            hb_xfree( szAlias );
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

   if( pCurrArea )
      SELF_BOF( ( AREAP ) pCurrArea->pArea, &bBof );
   hb_retl( bBof );
}

HARBOUR HB_DBAPPEND( void )
{
   BOOL bUnLockAll = TRUE;

   if( pCurrArea )
   {
      bNetError = FALSE;
      if( SELF_GOCOLD( ( AREAP ) pCurrArea->pArea ) == FAILURE )
         return;
      if( ISLOG( 1 ) )
         bUnLockAll = hb_parl( 1 );
      bNetError = ( SELF_APPEND( ( AREAP ) pCurrArea->pArea, bUnLockAll ) == FAILURE );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBAPPEND" );
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
      pWorkAreas = NULL;
   else
   {
      if( pCurrArea->pPrev )
         pCurrArea->pPrev->pNext = pCurrArea->pNext;
      if( pCurrArea->pNext )
         pCurrArea->pNext->pPrev = pCurrArea->pPrev;
   }

   hb_xfree( pCurrArea->pArea );
   hb_xfree( pCurrArea );
   pCurrArea = NULL;
}

HARBOUR HB_DBCOMMIT( void )
{
   if( pCurrArea )
      SELF_FLUSH( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCOMMIT" );
}

HARBOUR HB_DBCOMMITALL( void )
{
   LPAREANODE pAreaNode;

   pAreaNode = pWorkAreas;
   while( pAreaNode )
   {
      SELF_FLUSH( ( AREAP ) pAreaNode->pArea );
      pAreaNode = pAreaNode->pNext;
   }
}

HARBOUR HB_DBCREATE( void )
{
   char * szFileName, * szDriver;
   PHB_ITEM pStruct, pFieldDesc, pFileExt;
   LPRDDNODE pRddNode;
   AREAP pTempArea;
   USHORT uiSize, uiRddID;
   DBOPENINFO pInfo;
   USHORT uiLen;
   PHB_FNAME pFileName;

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2 , IT_ARRAY );
   if( ( strlen( szFileName) == 0 ) || !pStruct || !pStruct->item.asArray.value->ulLen )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
      return;
   }

   for( uiSize = 0; uiSize < pStruct->item.asArray.value->ulLen; uiSize++ )
   {
      pFieldDesc = pStruct->item.asArray.value->pItems + uiSize;
      if( pFieldDesc->item.asArray.value->ulLen != 4 )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
         return;
      }

      if( strlen( hb_arrayGetCPtr( pFieldDesc, 1 ) ) == 0 )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
         return;
      }
   }

   hb_rddCheck();
   szDriver = hb_parc( 3 );
   if( ( uiLen = strlen( szDriver ) ) > 0 )
      hb_strUpper( szDriver, uiLen ); /* TOFIX: Direct access to hb_parc() buffer ! */
   else
      szDriver = szDefDriver;

   uiRddID = 0;
   pRddNode = hb_rddFindNode( szDriver, &uiRddID ) ;
   if( !pRddNode )
   {
      hb_errRT_DBCMD( EG_ARG, 1015, NULL, "DBCREATE" );
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
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pTempArea, DBI_TABLEEXT, pFileExt );
      strcat( szFileName, pFileExt->item.asString.value );
      hb_itemRelease( pFileExt );
   }
   pInfo.abName = ( BYTE * ) szFileName;

   if( SELF_CREATE( ( AREAP ) pTempArea, &pInfo ) == SUCCESS &&
       pTempArea->lpExtendInfo->fHasMemo )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pTempArea, DBI_MEMOEXT, pFileExt );
      szFileName[ 0 ] = '\0';
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      strcat( szFileName, pFileExt->item.asString.value );
      pInfo.abName = ( BYTE * ) szFileName;
      SELF_CREATEMEMFILE( ( AREAP ) pTempArea, &pInfo );
      hb_itemRelease( pFileExt );
   }

   SELF_RELEASE( ( AREAP ) pTempArea );
   hb_xfree( szFileName );
   hb_xfree( pFileName );
   hb_xfree( pTempArea );
}

HARBOUR HB_DBDELETE( void )
{
   if( pCurrArea )
      SELF_DELETE( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBDELETE" );
}

HARBOUR HB_DBGOBOTTOM( void )
{
   if( pCurrArea )
      SELF_GOBOTTOM( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOBOTTOM" );
}

HARBOUR HB_DBGOTO( void )
{
   PHB_ITEM pItem;

   if( !pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOTOP" );
      return;
   }

   pItem = hb_param( 1, IT_ANY );
   if( !pItem )
      hb_errRT_DBCMD( EG_ARG, 1003, NULL, "DBGOTO" );
   else
      SELF_GOTOID( ( AREAP ) pCurrArea->pArea, pItem );
}

HARBOUR HB_DBGOTOP( void )
{
   if( pCurrArea )
      SELF_GOTOP( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOTOP" );
}

HARBOUR HB_DBRECALL( void )
{
   if( pCurrArea )
      SELF_RECALL( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRECALL" );
}

HARBOUR HB_DBRLOCK( void )
{
   DBLOCKINFO pLockInfo;

   pLockInfo.fResult = FALSE;
   if( pCurrArea )
   {
      pLockInfo.itmRecID = hb_parnl( 1 );
      pLockInfo.uiMethod = REC_LOCK;
      SELF_LOCK( ( AREAP ) pCurrArea->pArea, &pLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRLOCK" );

   hb_retl( pLockInfo.fResult );
}

HARBOUR HB_DBRLOCKLIST( void )
{
   PHB_ITEM pList;

   pList = hb_itemArrayNew( 0 );
   if( pCurrArea )
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_GETLOCKARRAY, pList );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRLOCKLIST" );

   hb_itemReturn( pList );
   hb_itemRelease( pList );
}

HARBOUR HB_DBRUNLOCK( void )
{
   if( pCurrArea )
      SELF_UNLOCK( ( AREAP ) pCurrArea->pArea, hb_parnl( 1 ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRUNLOCK" );
}

HARBOUR HB_DBSELECTAREA( void )
{
   USHORT uiNewArea;
   char * szAlias;
   LPAREANODE pAreaNode;

   if( ISCHAR( 1 ) )
   {
      szAlias = hb_parc( 1 );
      if( ( uiNewArea = hb_rddSelect( szAlias ) ) == 0 )
      {
         hb_errRT_BASE( EG_NOALIAS, 1002, NULL, szAlias );
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
   pCurrArea = NULL; /* Selected WorkArea is closed */
}

HARBOUR HB_DBSETDRIVER( void )
{
   char * szNewDriver;
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( szDefDriver );
   szNewDriver = hb_parc( 1 );
   if( ( uiLen = strlen( szNewDriver ) ) > 0 )
   {
      hb_strUpper( szNewDriver, uiLen ); /* TOFIX: Direct access to hb_parc() buffer ! */

      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1015, NULL, "DBSETDRIVER" );
         return;
      }

      szDefDriver = ( char * ) hb_xrealloc( szDefDriver, uiLen + 1 );
      strcpy( szDefDriver, szNewDriver );
   }
}

HARBOUR HB_DBSKIP( void )
{
   LONG lToSkip = 1;

   if( pCurrArea )
   {
      if( ISNUM( 1 ) )
         lToSkip = hb_parnl( 1 );

      SELF_SKIP( ( AREAP ) pCurrArea->pArea, lToSkip );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSKIP" );
}

HARBOUR HB_DBSTRUCT( void )
{
   PHB_ITEM pItem, pData;
   USHORT uiFields, uiCount;

   hb_arrayNew( &hb_stack.Return, 0 );

   if( pCurrArea )
   {
      SELF_FIELDCOUNT( ( AREAP ) pCurrArea->pArea, &uiFields );
      pData = hb_itemNew( NULL );
      pItem = hb_itemNew( NULL );
      for( uiCount = 1; uiCount <= uiFields; uiCount++ )
      {
         hb_arrayNew( pItem, 4 );
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_NAME, pData );
         hb_arraySet( pItem, 1, pData );
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_TYPE, pData );
         hb_arraySet( pItem, 2, pData );
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_LEN, pData );
         hb_arraySet( pItem, 3, pData );
         SELF_FIELDINFO( ( AREAP ) pCurrArea->pArea, uiCount, DBS_DEC, pData );
         hb_arraySet( pItem, 4, pData );
         hb_arrayAdd( &hb_stack.Return, pItem );
      }
      hb_itemRelease( pItem );
      hb_itemRelease( pData );
   }
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
         pItem = hb_itemPutC( NULL, "" );
         SELF_INFO( ( AREAP ) pTempArea, DBI_TABLEEXT, pItem );
         hb_retc( pItem->item.asString.value );
         hb_itemRelease( pItem );
         SELF_RELEASE( ( AREAP ) pTempArea );
      }
      hb_xfree( pTempArea );
   }
   else
   {
      pItem = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_TABLEEXT, pItem );
      hb_retc( pItem->item.asString.value );
      hb_itemRelease( pItem );
   }
}

HARBOUR HB_DBUNLOCK( void )
{
   if( pCurrArea )
      SELF_RAWLOCK( ( AREAP ) pCurrArea->pArea, FILE_UNLOCK, 0 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBUNLOCK" );
}

HARBOUR HB_DBUNLOCKALL( void )
{
   LPAREANODE pTempArea;

   pTempArea = pWorkAreas;
   while( pTempArea )
   {
      SELF_RAWLOCK( ( AREAP ) pTempArea->pArea, FILE_UNLOCK, 0 );
      pTempArea = pTempArea->pNext;
   }
}

HARBOUR HB_DBUSEAREA( void )
{
   char * szDriver, * szFileName, * szAlias;
   USHORT uiLen;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;
   USHORT uiSize, uiRddID;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pFileExt;

   bNetError = FALSE;

   if( hb_parl( 1 ) )
      hb_rddSelectFirstAvailable();
   else if( pCurrArea )  /* If current WorkArea is in use then close it */
   {
      SELF_CLOSE( ( AREAP ) pCurrArea->pArea );
      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );

      if( pWorkAreas == pCurrArea )  /* Empty list */
         pWorkAreas = NULL;
      else
      {
         if( pCurrArea->pPrev )
            pCurrArea->pPrev->pNext = pCurrArea->pNext;
         if( pCurrArea->pNext )
            pCurrArea->pNext->pPrev = pCurrArea->pPrev;
      }

      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
      pCurrArea = NULL;
   }

   hb_rddCheck();
   szDriver = hb_parc( 2 );
   if( ( uiLen = strlen( szDriver ) ) > 0 )
      hb_strUpper( szDriver, uiLen ); /* TOFIX: Direct access to hb_parc() buffer ! */
   else
      szDriver = szDefDriver;

   uiRddID = 0;
   pRddNode = hb_rddFindNode( szDriver, &uiRddID );
   if( !pRddNode )
   {
      hb_errRT_DBCMD( EG_ARG, 1015, NULL, "DBUSEAREA" );
      return;
   }

   szFileName = hb_parc( 3 );
   if( strlen( szFileName ) == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, 1005, NULL, "DBUSEAREA" );
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

      pRddNode->uiAreaSize = uiSize;  /* Update the size of WorkArea */
   }
   else
   {
      pCurrArea->pArea = ( AREAP ) hb_xgrab( pRddNode->uiAreaSize );
      memset( pCurrArea->pArea, 0, pRddNode->uiAreaSize );
      ( ( AREAP ) pCurrArea->pArea )->lprfsHost = &pRddNode->pTable;
   }

   ( ( AREAP ) pCurrArea->pArea )->rddID = uiRddID;

   pCurrArea->pPrev = NULL;
   pCurrArea->pNext = NULL;

   SELF_NEW( ( AREAP ) pCurrArea->pArea );

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   strcpy( szFileName, hb_parc( 3 ) );
   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
      strcat( szFileName, pFileExt->item.asString.value );
      hb_itemRelease( pFileExt );
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
      pCurrArea = NULL;
      return;
   }

   if( ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->fHasMemo )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_MEMOEXT, pFileExt );
      szFileName[ 0 ] = '\0';
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      strcat( szFileName, pFileExt->item.asString.value );
      pInfo.abName = ( BYTE * ) szFileName;
      hb_itemRelease( pFileExt );
      if( SELF_OPENMEMFILE( ( AREAP ) pCurrArea->pArea, &pInfo ) == FAILURE )
      {
         SELF_CLOSE( ( AREAP ) pCurrArea->pArea );
         SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
         hb_xfree( pCurrArea->pArea );
         hb_xfree( pCurrArea );
         hb_xfree( szFileName );
         hb_xfree( pFileName );
         pCurrArea = NULL;
         return;
      }
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

   if( pCurrArea )
      SELF_DELETED( ( AREAP ) pCurrArea->pArea, &bDeleted );
   hb_retl( bDeleted );
}

HARBOUR HB_EOF( void )
{
   BOOL bEof = TRUE;

   if( pCurrArea )
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
   hb_retl( bEof );
}

HARBOUR HB_FCOUNT( void )
{
   USHORT uiFields = 0;

   if( pCurrArea )
      SELF_FIELDCOUNT( ( AREAP ) pCurrArea->pArea, &uiFields );
   hb_retni( uiFields );
}

HARBOUR HB_FIELDGET( void )
{
   PHB_ITEM pItem;
   USHORT uiField;

   pItem = hb_itemNew( NULL );
   uiField = hb_parni( 1 );

   if( pCurrArea && uiField )
      SELF_GETVALUE( ( AREAP ) pCurrArea->pArea, uiField, pItem );

   hb_itemReturn( pItem );
   hb_itemRelease( pItem );
}

HARBOUR HB_FIELDNAME( void )
{
   USHORT uiFields, uiIndex;
   char * szName;

   if( pCurrArea )
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
         hb_errRT_DBCMD( EG_ARG, 1009, NULL, "FIELDNAME" );
      }
   }
   hb_retc( "" );
}

HARBOUR HB_FIELDPOS( void )
{
   USHORT uiCount;
   char * szName;
   LPFIELD pField;

   if( pCurrArea )
   {
      szName = hb_parc( 1 );
      hb_strUpper( szName, strlen( szName ) ); /* TOFIX: Direct access to hb_parc() buffer ! */
      uiCount = 0;
      pField = ( ( AREAP ) pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( strcmp( szName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName ) == 0 )
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
   if( pCurrArea && uiIndex )
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

HARBOUR HB_FLOCK( void )
{
   DBLOCKINFO pLockInfo;

   pLockInfo.fResult = FALSE;
   if( pCurrArea )
   {
      pLockInfo.itmRecID = 0;
      pLockInfo.uiMethod = FILE_LOCK;
      SELF_LOCK( ( AREAP ) pCurrArea->pArea, &pLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "FLOCK" );

   hb_retl( pLockInfo.fResult );
}

HARBOUR HB_FOUND( void )
{
   BOOL bFound = FALSE;

   if( pCurrArea )
      SELF_FOUND( ( AREAP ) pCurrArea->pArea, &bFound );
   hb_retl( bFound );
}

HARBOUR HB_HEADER( void )
{
   PHB_ITEM pRecSize;

   if( !pCurrArea )
      hb_retni( 0 );
   else
   {
      pRecSize = hb_itemNew( NULL );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_GETHEADERSIZE, pRecSize );
      hb_itemReturn( pRecSize );
      hb_itemRelease( pRecSize );
   }
}

HARBOUR HB_LASTREC( void )
{
   HB_RECCOUNT();
}

HARBOUR HB_LOCK( void )
{
   DBLOCKINFO pLockInfo;

   pLockInfo.fResult = FALSE;
   if( pCurrArea )
   {
      pLockInfo.itmRecID = 0;
      pLockInfo.uiMethod = FILE_LOCK;
      SELF_LOCK( ( AREAP ) pCurrArea->pArea, &pLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "LOCK" );

   hb_retl( pLockInfo.fResult );
}

HARBOUR HB_LUPDATE( void )
{
   if( !pCurrArea )
      hb_itemPutDS( &hb_stack.Return, "" );
   else
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_LASTUPDATE, &hb_stack.Return );
}

HARBOUR HB_NETERR( void )
{
   if( ISLOG( 1 ) )
      bNetError = hb_parl( 1 );

   hb_retl( bNetError );
}

HARBOUR HB_RDDLIST( void )
{
   USHORT uiType;
   PHB_ITEM pName;
   LPRDDNODE pRddNode;

   hb_rddCheck();
   hb_arrayNew( &hb_stack.Return, 0 );
   pName = hb_itemNew( NULL );
   pRddNode = pRddList;
   uiType = hb_parni( 1 );       /* 0 all types of RDD's */
   while( pRddNode )
   {
      if( ( uiType == 0 ) || ( pRddNode->uiType == uiType ) )
         hb_arrayAdd( &hb_stack.Return, hb_itemPutC( pName, pRddNode->szName ) );
      pRddNode = pRddNode->pNext;
   }
   hb_itemRelease( pName );
}

HARBOUR HB_RDDNAME( void )
{
   char * pBuffer;

   if( pCurrArea )
   {
      pBuffer = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
      pBuffer[ 0 ] = '\0';
      SELF_SYSNAME( ( AREAP ) pCurrArea->pArea, ( BYTE * ) pBuffer );
      hb_retc( pBuffer );
      hb_xfree( pBuffer );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "RDDNAME" );
      hb_retc( "" );
   }
}

HARBOUR HB_RDDREGISTER( void )
{
   char * szDriver;
   USHORT uiLen;

   hb_rddCheck();
   szDriver = hb_parc( 1 );
   if( ( uiLen = strlen( szDriver ) ) > 0 )
   {
      hb_strUpper( szDriver, uiLen ); /* TOFIX: Direct access to hb_parc() buffer ! */
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
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( szDefDriver );
   szNewDriver = hb_parc( 1 );
   if( ( uiLen = strlen( szNewDriver ) ) > 0 )
   {
      hb_strUpper( szNewDriver, uiLen ); /* TOFIX: Direct access to hb_parc() buffer ! */

      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1015, NULL, "RDDSETDEFAULT" );
         return;
      }

      szDefDriver = ( char * ) hb_xrealloc( szDefDriver, uiLen + 1 );
      strcpy( szDefDriver, szNewDriver );
   }
}

void hb_rddShutDown( void )
{
   LPRDDNODE pRddNode;

   hb_rddCloseAll();
   if( szDefDriver )
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
   ULONG ulRecCount = 0;

   if( pCurrArea )
      SELF_RECCOUNT( ( AREAP ) pCurrArea->pArea, &ulRecCount );
   hb_retnl( ulRecCount );
}

HARBOUR HB_RECNO( void )
{
   PHB_ITEM pRecNo;

   pRecNo = hb_itemPutNL( NULL, 0 );
   if( pCurrArea )
      SELF_RECNO( ( AREAP ) pCurrArea->pArea, pRecNo );
   hb_itemReturn( pRecNo );
   hb_itemRelease( pRecNo );
}

HARBOUR HB_RECSIZE( void )
{
   PHB_ITEM pRecSize;

   if( !pCurrArea )
      hb_retni( 0 );
   else
   {
      pRecSize = hb_itemNew( NULL );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_GETRECSIZE, pRecSize );
      hb_itemReturn( pRecSize );
      hb_itemRelease( pRecSize );
   }
}

HARBOUR HB_RLOCK( void )
{
   DBLOCKINFO pLockInfo;
   PHB_ITEM pRecNo;

   pLockInfo.fResult = FALSE;
   if( pCurrArea )
   {
      pRecNo = hb_itemPutNL( NULL, 0 );
      SELF_RECNO( ( AREAP ) pCurrArea->pArea, pRecNo );
      pLockInfo.itmRecID = pRecNo->item.asLong.value;
      pLockInfo.uiMethod = REC_LOCK;
      SELF_LOCK( ( AREAP ) pCurrArea->pArea, &pLockInfo );
      hb_itemRelease( pRecNo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "RLOCK" );

   hb_retl( pLockInfo.fResult );
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
   hb_retl( pCurrArea != NULL );
}

/* NOTE: Same as dbSetDriver() and rddSetDefault(), but doesn't
         throw any error if the driver doesn't exist, this is 
         required in the RDDSYS INIT function, since it's not guaranteed
         that the RDD is already registered at that point. */

HARBOUR HB___RDDSETDEFAULT( void )
{
   char * szNewDriver;
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( szDefDriver );
   szNewDriver = hb_parc( 1 );
   if( ( uiLen = strlen( szNewDriver ) ) > 0 )
   {
      hb_strUpper( szNewDriver, uiLen ); /* TOFIX: Direct access to hb_parc() buffer ! */
      szDefDriver = ( char * ) hb_xrealloc( szDefDriver, uiLen + 1 );
      strcpy( szDefDriver, szNewDriver );
   }
}
