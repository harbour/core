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
HARBOUR HB_DBCLEARFILTER( void );
HARBOUR HB_DBCLOSEALL( void );
HARBOUR HB_DBCLOSEAREA( void );
HARBOUR HB_DBCOMMIT( void );
HARBOUR HB_DBCOMMITALL( void );
HARBOUR HB___DBCONTINUE( void );
HARBOUR HB_DBCREATE( void );
HARBOUR HB_DBDELETE( void );
HARBOUR HB_DBF( void );
HARBOUR HB_DBFILTER( void );
HARBOUR HB_DBGOBOTTOM( void );
HARBOUR HB_DBGOTO( void );
HARBOUR HB_DBGOTOP( void );
HARBOUR HB___DBLOCATE( void );
HARBOUR HB_DBRECALL( void );
HARBOUR HB_DBRLOCK( void );
HARBOUR HB_DBRLOCKLIST( void );
HARBOUR HB_DBRUNLOCK( void );
HARBOUR HB_DBSELECTAREA( void );
HARBOUR HB_DBSETDRIVER( void );
HARBOUR HB_DBSETFILTER( void );
HARBOUR HB___DBSETFOUND( void );
HARBOUR HB___DBSETLOCATE( void );
HARBOUR HB_DBSKIP( void );
HARBOUR HB_DBSTRUCT( void );
HARBOUR HB_DBTABLEEXT( void );
HARBOUR HB_DBUNLOCK( void );
HARBOUR HB_DBUNLOCKALL( void );
HARBOUR HB_DBUSEAREA( void );
HARBOUR HB___DBZAP( void );
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

static ERRCODE defAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;
   ULONG ulLen;

   HB_TRACE(HB_TR_DEBUG, ("defAddField(%p, %p)", pArea, pFieldInfo));

   /* Validate the name of field */
   ulLen = strlen( ( char * ) pFieldInfo->atomName );
   hb_strLTrim( ( char * ) pFieldInfo->atomName, &ulLen );
   if( !ulLen )
      return FAILURE;

   pField = pArea->lpFields + pArea->uiFieldCount;
   if( pArea->uiFieldCount > 0 )
   {
      ( ( LPFIELD ) ( pField - 1 ) )->lpfNext = pField;
      if( ( ( LPFIELD ) ( pField - 1 ) )->uiType == 'C' )
         pField->uiOffset = ( ( LPFIELD ) ( pField - 1 ) )->uiOffset +
                            ( ( LPFIELD ) ( pField - 1 ) )->uiLen +
                            ( ( USHORT ) ( ( LPFIELD ) ( pField - 1 ) )->uiDec << 8 );
      else
         pField->uiOffset = ( ( LPFIELD ) ( pField - 1 ) )->uiOffset +
                            ( ( LPFIELD ) ( pField - 1 ) )->uiLen;
   }
   else
      pField->uiOffset = 1;
   pField->sym = ( void * ) hb_dynsymGet( ( char * ) pFieldInfo->atomName );
   pField->uiType = pFieldInfo->uiType;
   pField->uiTypeExtended = pFieldInfo->typeExtended;
   pField->uiLen = pFieldInfo->uiLen;
   pField->uiDec = pFieldInfo->uiDec;
   pField->uiArea = pArea->uiArea;
   pArea->uiFieldCount++;
   return SUCCESS;
}

static ERRCODE defAlias( AREAP pArea, BYTE * szAlias )
{
   HB_TRACE(HB_TR_DEBUG, ("defAlias(%p, %p)", pArea, szAlias));

   strncpy( ( char * ) szAlias,
            ( ( PHB_DYNS ) pArea->atomAlias )->pSymbol->szName,
            HARBOUR_MAX_RDD_ALIAS_LENGTH );
   return SUCCESS;
}

static ERRCODE defBof( AREAP pArea, BOOL * pBof )
{
   HB_TRACE(HB_TR_DEBUG, ("defBof(%p, %p)", pArea, pBof));

   * pBof = pArea->fBof;
   return SUCCESS;
}

static ERRCODE defClearFilter( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("defClearFilter(%p)", pArea));

   if( pArea->dbfi.fFilter )
   {
      hb_itemRelease( pArea->dbfi.itmCobExpr );
      hb_itemRelease( pArea->dbfi.abFilterText );
      pArea->dbfi.fFilter = FALSE;
   }
   return SUCCESS;
}

static ERRCODE defClearLocate( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("defClearLocate(%p)", pArea));

   if( pArea->dbsi.itmCobFor )
   {
      hb_itemRelease( pArea->dbsi.itmCobFor );
      pArea->dbsi.itmCobFor = NULL;
   }
   if( pArea->dbsi.lpstrFor )
   {
      hb_itemRelease( pArea->dbsi.lpstrFor );
      pArea->dbsi.lpstrFor = NULL;
   }
   if( pArea->dbsi.itmCobWhile )
   {
      hb_itemRelease( pArea->dbsi.itmCobWhile );
      pArea->dbsi.itmCobWhile = NULL;
   }
   if( pArea->dbsi.lpstrWhile )
   {
      hb_itemRelease( pArea->dbsi.lpstrWhile );
      pArea->dbsi.lpstrWhile = NULL;
   }
   if( pArea->dbsi.lNext )
   {
      hb_itemRelease( pArea->dbsi.lNext );
      pArea->dbsi.lNext = NULL;
   }
   if( pArea->dbsi.itmRecID )
   {
      hb_itemRelease( pArea->dbsi.itmRecID );
      pArea->dbsi.itmRecID = NULL;
   }
   if( pArea->dbsi.fRest )
   {
      hb_itemRelease( pArea->dbsi.fRest );
      pArea->dbsi.fRest = NULL;
   }
   return SUCCESS;
}

static ERRCODE defClose( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("defClose(%p)", pArea));

   SELF_CLEARFILTER( pArea );
   SELF_CLEARLOCATE( pArea );
   ( ( PHB_DYNS ) pArea->atomAlias )->hArea = 0;
   return SUCCESS;
}

static ERRCODE defCreateFields( AREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiCount, uiItems;
   PHB_ITEM pFieldDesc;
   DBFIELDINFO pFieldInfo;
   long lLong;

   HB_TRACE(HB_TR_DEBUG, ("defCreateFields(%p, %p)", pArea, pStruct));

   uiItems = hb_arrayLen( pStruct );
   SELF_SETFIELDEXTENT( pArea, uiItems );
   pFieldInfo.typeExtended = 0;
   for( uiCount = 0; uiCount < uiItems; uiCount++ )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiCount + 1 );
      pFieldInfo.uiType = toupper( hb_arrayGetCPtr( pFieldDesc, 2 )[ 0 ] );
      pFieldInfo.atomName = ( BYTE * ) hb_arrayGetCPtr( pFieldDesc, 1 );
      lLong = hb_arrayGetNL( pFieldDesc, 3 );
      if( lLong < 0 )
         lLong = 0;
      pFieldInfo.uiLen = ( USHORT ) lLong;
      lLong = hb_arrayGetNL( pFieldDesc, 4 );
      if( lLong < 0 )
         lLong = 0;
      pFieldInfo.uiDec = ( USHORT ) lLong;
      if( SELF_ADDFIELD( pArea, &pFieldInfo ) == FAILURE )
         return FAILURE;
   }
   return SUCCESS;
}

static ERRCODE defEof( AREAP pArea, BOOL * pEof )
{
   HB_TRACE(HB_TR_DEBUG, ("defEof(%p, %p)", pArea, pEof));

   * pEof = pArea->fEof;
   return SUCCESS;
}

static ERRCODE defError( AREAP pArea, PHB_ITEM pError )
{
   char * szRddName;

   HB_TRACE(HB_TR_DEBUG, ("defError(%p, %p)", pArea, pError));

   szRddName = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
   SELF_SYSNAME( pArea, ( BYTE * ) szRddName );
   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, szRddName );
   hb_xfree( szRddName );
   return hb_errLaunch( pError );
}

static ERRCODE defFieldCount( AREAP pArea, USHORT * uiFields )
{
   HB_TRACE(HB_TR_DEBUG, ("defFieldCount(%p, %p)", pArea, uiFields));

   * uiFields = pArea->uiFieldCount;
   return SUCCESS;
}

static ERRCODE defFieldInfo( AREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem )
{
   LPFIELD pField;
   char szType[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("defFieldInfo(%p, %hu, %hu, %p)", pArea, uiIndex, uiType, pItem));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   switch( uiType )
   {
      case DBS_NAME:
         hb_itemPutC( pItem, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName );
         break;

      case DBS_TYPE:
         szType[ 0 ] = pField->uiType;
         szType[ 1 ] = '\0';
         hb_itemPutC( pItem, szType );
         break;

      case DBS_LEN:
         hb_itemPutNL( pItem, pField->uiLen );
         break;

      case DBS_DEC:
         hb_itemPutNL( pItem, pField->uiDec );
         break;

      default:
         return FAILURE;

   }
   return SUCCESS;
}


static ERRCODE defFieldName( AREAP pArea, USHORT uiIndex, void * szName )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("defFieldName(%p, %hu, %p)", pArea, uiIndex, szName));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   strncpy( ( char * ) szName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName,
            HARBOUR_MAX_RDD_FIELDNAME_LENGTH );
   return SUCCESS;
}

static ERRCODE defFilterText( AREAP pArea, PHB_ITEM pFilter )
{
   HB_TRACE(HB_TR_DEBUG, ("defFilterText(%p, %p)", pArea, pFilter));

   if( pArea->dbfi.fFilter )
      hb_itemCopy( pFilter, pArea->dbfi.abFilterText );
   return SUCCESS;
}

static ERRCODE defFound( AREAP pArea, BOOL * pFound )
{
   HB_TRACE(HB_TR_DEBUG, ("defFound(%p, %p)", pArea, pFound));

   * pFound = pArea->fFound;
   return SUCCESS;
}

static ERRCODE defGetRec( AREAP pArea, BYTE ** pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("defGetRec(%p, %p)", pArea, pBuffer));

   * pBuffer = pArea->lpExtendInfo->bRecord;
   return SUCCESS;
}

static ERRCODE defNewArea( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("defNewArea(%p)", pArea));

   pArea->lpFileInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
   memset( pArea->lpFileInfo, 0, sizeof( FILEINFO ) );
   pArea->lpFileInfo->hFile = FS_ERROR;
   pArea->lpExtendInfo = ( LPDBEXTENDINFO ) hb_xgrab( sizeof( DBEXTENDINFO ) );
   memset( pArea->lpExtendInfo, 0, sizeof( DBEXTENDINFO ) );
   return SUCCESS;
}

static ERRCODE defOpen( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("defOpen(%p, %p)", pArea, pOpenInfo));

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

static ERRCODE defRelease( AREAP pArea )
{
   LPFILEINFO pFileInfo;

   HB_TRACE(HB_TR_DEBUG, ("defRelease(%p)", pArea));

   if( pArea->lpFields )
   {
      hb_xfree( pArea->lpFields );
      pArea->uiFieldCount = 0;
   }

   while( pArea->lpFileInfo )
   {
      pFileInfo = pArea->lpFileInfo;
      pArea->lpFileInfo = pArea->lpFileInfo->pNext;
      if( pFileInfo->szFileName )
         hb_xfree( pFileInfo->szFileName );
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

static ERRCODE defSetFieldExtent( AREAP pArea, USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("defSetFieldExtent(%p, %hu)", pArea, uiFieldExtent));

   pArea->uiFieldExtent = uiFieldExtent;
   pArea->lpFields = ( LPFIELD ) hb_xgrab( uiFieldExtent * sizeof( FIELD ) );
   memset( pArea->lpFields, 0, uiFieldExtent * sizeof( FIELD ) );
   return SUCCESS;
}

static ERRCODE defSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("defSetFilter(%p, %p)", pArea, pFilterInfo));

   if( pArea->dbfi.fFilter )
   {
      hb_itemCopy( pArea->dbfi.itmCobExpr, pFilterInfo->itmCobExpr );
      hb_itemCopy( pArea->dbfi.abFilterText, pFilterInfo->abFilterText );
   }
   else
   {
      pArea->dbfi.itmCobExpr = hb_itemNew( NULL );
      hb_itemCopy( pArea->dbfi.itmCobExpr, pFilterInfo->itmCobExpr );
      pArea->dbfi.abFilterText = hb_itemNew( NULL );
      hb_itemCopy( pArea->dbfi.abFilterText, pFilterInfo->abFilterText );
      pArea->dbfi.fFilter = TRUE;
   }
   return SUCCESS;
}

static ERRCODE defSetLocate( AREAP pArea, LPDBSCOPEINFO pScopeInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("defSetLocate(%p, %p)", pArea, pScopeInfo));

   if( pArea->dbsi.itmCobFor )
      hb_itemRelease( pArea->dbsi.itmCobFor );
   pArea->dbsi.itmCobFor = pScopeInfo->itmCobFor;
   if( pArea->dbsi.itmCobWhile )
      hb_itemRelease( pArea->dbsi.itmCobWhile );
   pArea->dbsi.itmCobWhile = pScopeInfo->itmCobWhile;
   if( pArea->dbsi.lNext )
      hb_itemRelease( pArea->dbsi.lNext );
   pArea->dbsi.lNext = pScopeInfo->lNext;
   if( pArea->dbsi.itmRecID )
      hb_itemRelease( pArea->dbsi.itmRecID );
   pArea->dbsi.itmRecID = pScopeInfo->itmRecID;
   if( pArea->dbsi.fRest )
      hb_itemRelease( pArea->dbsi.fRest );
   pArea->dbsi.fRest = pScopeInfo->fRest;
   return SUCCESS;
}

static ERRCODE defSkip( AREAP pArea, LONG lToSkip )
{
   BOOL bExit;

   HB_TRACE(HB_TR_DEBUG, ("defSkip(%p, %ld)", pArea, lToSkip));

   if( pArea->dbfi.fFilter || hb_set.HB_SET_DELETED )
   {
      if( lToSkip > 0 )
      {
         while( lToSkip > 0 )
         {
            SELF_SKIPRAW( pArea, 1 );
            SELF_SKIPFILTER( pArea, 1 );

            SELF_EOF( pArea, &bExit );
            if( bExit )
               return SUCCESS;

            lToSkip--;
         }
      }
      else if( lToSkip < 0 )
      {
         while( lToSkip < 0 )
         {
            SELF_SKIPRAW( pArea, -1 );
            SELF_SKIPFILTER( pArea, -1 );

            SELF_BOF( pArea, &bExit );
            if( bExit )
               return SELF_SKIPFILTER( pArea, 1 );

            lToSkip++;
         }
      }
      else
      {
         SELF_SKIPRAW( pArea, 0 );
         SELF_SKIPFILTER( pArea, 1 );

         SELF_EOF( pArea, &bExit );
         if( bExit )
            return SUCCESS;
      }
   }
   return SELF_SKIPRAW( pArea, lToSkip );
}

static ERRCODE defSkipFilter( AREAP pArea, LONG lUpDown )
{
   BOOL bExit, bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("defSkipFilter(%p, %ld)", pArea, lUpDown));

   if( lUpDown > 0 )
   {
      while( 1 )
      {
         SELF_EOF( pArea, &bExit );
         if( bExit )
            return SUCCESS;

         /* SET DELETED */
         if( hb_set.HB_SET_DELETED )
         {
            SELF_DELETED( pArea, &bDeleted );
            if( bDeleted )
            {
               SELF_SKIPRAW( pArea, 1 );
               continue;
            }
         }

         /* SET FILTER TO */
         if( pArea->dbfi.fFilter )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pArea->dbfi.itmCobExpr );
            hb_vmDo( 0 );
            if( IS_LOGICAL( &hb_stack.Return ) &&
                !hb_stack.Return.item.asLogical.value )
            {
               SELF_SKIPRAW( pArea, 1 );
               continue;
            }
         }
         return SUCCESS;
      }
   }
   else if( lUpDown < 0 )
   {
      while( 1 )
      {
         SELF_BOF( pArea, &bExit );
         if( bExit )
            return SELF_SKIPFILTER( pArea, 1 );

         /* SET DELETED */
         if( hb_set.HB_SET_DELETED )
         {
            SELF_DELETED( pArea, &bDeleted );
            if( bDeleted )
            {
               SELF_SKIPRAW( pArea, -1 );
               continue;
            }
         }

         /* SET FILTER TO */
         if( pArea->dbfi.fFilter )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pArea->dbfi.itmCobExpr );
            hb_vmDo( 0 );
            if( IS_LOGICAL( &hb_stack.Return ) &&
                !hb_stack.Return.item.asLogical.value )
            {
               SELF_SKIPRAW( pArea, 1 );
               continue;
            }
         }
         return SUCCESS;
      }
   }
   return SUCCESS;
}

static ERRCODE defSkipRaw( AREAP pArea, LONG lToSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("defSkipRaw(%p, %ld)", pArea, lToSkip));

   return SELF_GOTO( pArea, pArea->lpExtendInfo->lRecNo + lToSkip );
}

static ERRCODE defStructSize( AREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("defStrucSize(%p, %p)", pArea, uiSize));

   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( uiSize );

   return SUCCESS;
}

static ERRCODE defSysName( AREAP pArea, BYTE * pBuffer )
{
   USHORT uiCount;
   LPRDDNODE pRddNode;

   HB_TRACE(HB_TR_DEBUG, ("defSysName(%p, %p)", pArea, pBuffer));

   pRddNode = pRddList;
   for( uiCount = 0; uiCount < pArea->rddID; uiCount++ )
      pRddNode = pRddNode->pNext;
   strncpy( ( char * ) pBuffer, pRddNode->szName, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   return SUCCESS;
}

static ERRCODE defUnSupported( AREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("defUnSupported(%p)", pArea));

   HB_SYMBOL_UNUSED( pArea );

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );
   SELF_ERROR( pArea, pError );
   hb_errRelease( pError );
   return FAILURE;
}

static RDDFUNCS defTable = { defBof,
                             defEof,
                             defFound,
                             defUnSupported,
                             ( DBENTRYP_UL ) defUnSupported,
                             ( DBENTRYP_I ) defUnSupported,
                             defUnSupported,
                             defSkip,
                             defSkipFilter,
                             defSkipRaw,
                             defAddField,
                             ( DBENTRYP_B ) defUnSupported,
                             defCreateFields,
                             defUnSupported,
                             ( DBENTRYP_BP ) defUnSupported,
                             defFieldCount,
                             ( DBENTRYP_VF ) defUnSupported,
                             defFieldInfo,
                             defFieldName,
                             defUnSupported,
                             defGetRec,
                             ( DBENTRYP_SI ) defUnSupported,
                             ( DBENTRYP_SVL ) defUnSupported,
                             defUnSupported,
                             defUnSupported,
                             ( DBENTRYP_P ) defUnSupported,
                             ( DBENTRYP_SI ) defUnSupported,
                             defUnSupported,
                             ( DBENTRYP_ULP ) defUnSupported,
                             ( DBENTRYP_ISI ) defUnSupported,
                             ( DBENTRYP_I ) defUnSupported,
                             defSetFieldExtent,
                             defAlias,
                             defClose,
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_SI ) defUnSupported,
                             defNewArea,
                             defOpen,
                             defRelease,
                             defStructSize,
                             defSysName,
                             defUnSupported,
                             defClearFilter,
                             defClearLocate,
                             defFilterText,
                             defSetFilter,
                             defSetLocate,
                             defError,
                             ( DBENTRYP_VSP ) defUnSupported,
                             ( DBENTRYP_VL ) defUnSupported,
                             ( DBENTRYP_UL ) defUnSupported,
                             defUnSupported,
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_SVP ) defUnSupported,
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_SVP ) defUnSupported,
                             defUnSupported,
                             defUnSupported,
                             ( DBENTRYP_SVP ) defUnSupported
                           };


static void hb_rddCheck( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddCheck()"));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAll()"));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFindNode(%s, %p)", szDriver, uiIndex));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_rddRegister(%s, %hu)", szDriver, uiType));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelect(%s)", szAlias));

   pSymAlias = hb_dynsymFindName( szAlias );
   if( pSymAlias && pSymAlias->hArea )
      return pSymAlias->hArea;
   else
      return 0;
}

static void hb_rddSelectFirstAvailable( void )
{
   LPAREANODE pAreaNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectFirstAvailable()"));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInherit(%p, %p, %p, %s)", pTable, pSubTable, pSuperTable, szDrvName));

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
      hb_strncpyUpper( szSuperName, (char *)szDrvName, uiCount );
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
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaNumber()"));

   return uiCurrArea;
}

ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
   LPAREANODE pAreaNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaNumber(%d)", iArea));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaSymbol(%p)", pSymAlias));

   if( pSymAlias->pDynSym->hArea )
      bResult = hb_rddSelectWorkAreaNumber( pSymAlias->pDynSym->hArea );
   else
   {
      char * szName = pSymAlias->pDynSym->pSymbol->szName;

      if( strlen( szName ) == 1 && toupper( szName[ 0 ] ) >= 'A' && toupper( szName[ 0 ] ) <= 'K' )
         bResult = hb_rddSelectWorkAreaNumber( toupper( szName[ 0 ] ) - 'A' + 1 );
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
   }
   return bResult;
}

ERRCODE hb_rddSelectWorkAreaAlias( char * szName )
{
   ERRCODE bResult;
   ULONG ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaAlias(%s)", szName));

   ulLen = strlen( szName );
   if( ulLen >= 1 && toupper( szName[ 0 ] ) > '0' && toupper( szName[ 0 ] ) <= '9' )
      bResult = hb_rddSelectWorkAreaNumber( atoi( szName ) );
   else if( ulLen == 1 && toupper( szName[ 0 ] ) >= 'A' && toupper( szName[ 0 ] ) <= 'K' )
      bResult = hb_rddSelectWorkAreaNumber( toupper( szName[ 0 ] ) - 'A' + 1 );
   else
   {
      PHB_DYNS pSymArea;

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
   }

   return bResult;
}

ERRCODE hb_rddGetFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE bSuccess;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetFieldValue(%p, %p)", pItem, pFieldSymbol));

   bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );
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
   ERRCODE bSuccess;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddPutFieldValue(%p, %p)", pItem, pFieldSymbol));

   bSuccess = hb_rddFieldPut( pItem, pFieldSymbol );
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

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", pItem, pFieldSymbol));

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

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", pItem, pFieldSymbol));

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

void hb_rddShutDown( void )
{
   LPRDDNODE pRddNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddShutDown()"));

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
      if( ISLOG( 1 ) )
         bUnLockAll = hb_parl( 1 );
      bNetError = ( SELF_APPEND( ( AREAP ) pCurrArea->pArea, bUnLockAll ) == FAILURE );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBAPPEND" );
}

HARBOUR HB_DBCLEARFILTER( void )
{
   if( pCurrArea )
      SELF_CLEARFILTER( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCLEARFILTER" );
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

   if( pWorkAreas == pCurrArea )
   {
      pWorkAreas = pCurrArea->pNext;
      if( pWorkAreas )
         pWorkAreas->pPrev = NULL;
   }
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

HARBOUR HB___DBCONTINUE()
{
   BOOL bEof;

   if( !pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCONTINUE" );
      return;
   }

   if( !( ( AREAP ) pCurrArea->pArea )->dbsi.itmCobFor )
      return;
   ( ( AREAP ) pCurrArea->pArea )->fFound = FALSE;
   SELF_SKIP( ( AREAP ) pCurrArea->pArea, 1 );
   SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
   if( bEof )
      return;
   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( ( ( AREAP ) pCurrArea->pArea )->dbsi.itmCobFor );
   hb_vmDo( 0 );
   ( ( AREAP ) pCurrArea->pArea )->fFound = hb_itemGetL( &hb_stack.Return );
   while( !bEof && !( ( AREAP ) pCurrArea->pArea )->fFound )
   {
      SELF_SKIP( ( AREAP ) pCurrArea->pArea, 1 );
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( ( ( AREAP ) pCurrArea->pArea )->dbsi.itmCobFor );
      hb_vmDo( 0 );
      ( ( AREAP ) pCurrArea->pArea )->fFound = hb_itemGetL( &hb_stack.Return );
   }
}

HARBOUR HB_DBCREATE( void )
{
   char * szDriver, * szFileName;
   USHORT uiSize, uiLen, uiRddID;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pStruct, pFieldDesc, pFileExt;
   char cDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];
   BOOL bError = FALSE;

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2 , IT_ARRAY );
   uiLen = hb_arrayLen( pStruct );
   if( ( strlen( szFileName ) == 0 ) || !pStruct || uiLen == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
      return;
   }

   for( uiSize = 0; uiSize < uiLen; uiSize++ )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize + 1 );
      if( hb_arrayLen( pFieldDesc ) != 4 )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
         return;
      }

      /* Validate items type, name, size and decimals of field */
      if( hb_arrayGetType( pFieldDesc, 1 ) != IT_STRING ||
          hb_arrayGetType( pFieldDesc, 2 ) != IT_STRING ||
          hb_arrayGetType( pFieldDesc, 3 ) != IT_INTEGER ||
          hb_arrayGetType( pFieldDesc, 4 ) != IT_INTEGER )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
         return;
      }
   }

   hb_rddCheck();
   uiLen = hb_parclen( 3 );
   if( uiLen > 0 )
   {
      hb_strncpyUpper( cDriverBuffer, hb_parc( 3 ), uiLen );
      szDriver = cDriverBuffer;
   }
   else
      szDriver = szDefDriver;

   uiRddID = 0;
   pRddNode = hb_rddFindNode( szDriver, &uiRddID ) ;
   if( !pRddNode )
   {
      hb_errRT_DBCMD( EG_ARG, 1015, NULL, "DBCREATE" );
      return;
   }

   if( !ISLOG( 4 ) )
      hb_rddSelectFirstAvailable();
   else
   {
      if( hb_parl( 4 ) )
         hb_rddSelectFirstAvailable();
      else if( pCurrArea )  /* If current WorkArea is in use then close it */
      {
         SELF_CLOSE( ( AREAP ) pCurrArea->pArea );
         SELF_RELEASE( ( AREAP ) pCurrArea->pArea );

         if( pWorkAreas == pCurrArea )
         {
            pWorkAreas = pCurrArea->pNext;
            if( pWorkAreas )
               pWorkAreas->pPrev = NULL;
         }
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
   }

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
   if( SELF_CREATEFIELDS( ( AREAP ) pCurrArea->pArea, pStruct ) == FAILURE )
   {
      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
      hb_xfree( pCurrArea->pArea );
      hb_xfree( pCurrArea );
      pCurrArea = NULL;
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
      return;
   }

   pFileName = hb_fsFNameSplit( szFileName );
   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   strcpy( szFileName, hb_parc( 1 ) );
   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
      strcat( szFileName, pFileExt->item.asString.value );
      hb_itemRelease( pFileExt );
   }
   hb_xfree( pFileName );
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) hb_parc( 5 );
   pInfo.uiArea = uiCurrArea;

   ( ( AREAP ) pCurrArea->pArea )->uiArea = uiCurrArea;

   /* Insert the new WorkArea node */

   if( !pWorkAreas )
      pWorkAreas = pCurrArea;  /* The new WorkArea node is the first */
   else
   {
      pAreaNode = pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea > uiCurrArea )
         {
            /* Insert the new WorkArea node */
            pCurrArea->pPrev = pAreaNode->pPrev;
            pCurrArea->pNext = pAreaNode;
            pAreaNode->pPrev = pCurrArea;
            if( pCurrArea->pPrev )
               pCurrArea->pPrev->pNext = pCurrArea;
            break;
         }
         if( pAreaNode->pNext )
            pAreaNode = pAreaNode->pNext;
         else
         {
            /* Append the new WorkArea node */
            pAreaNode->pNext = pCurrArea;
            pCurrArea->pPrev = pAreaNode;
            break;
         }
      }
   }

   ( ( AREAP ) pCurrArea->pArea )->lpFileInfo->szFileName = szFileName;
   ( ( AREAP ) pCurrArea->pArea )->atomAlias = hb_dynsymGet( ( char * ) pInfo.atomAlias );
   if( ( ( PHB_DYNS ) ( ( AREAP ) pCurrArea->pArea )->atomAlias )->hArea )
   {
      hb_errRT_DBCMD( EG_DUPALIAS, 1011, NULL, ( char * ) pInfo.atomAlias );
      bError = TRUE;
   }

   if( !bError )
      bError = ( SELF_CREATE( ( AREAP ) pCurrArea->pArea, &pInfo ) == FAILURE );

   if( !bError )
      ( ( PHB_DYNS ) ( ( AREAP ) pCurrArea->pArea )->atomAlias )->hArea = pInfo.uiArea;

   if( !bError && ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->fHasMemo )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_MEMOEXT, pFileExt );
      pFileName = hb_fsFNameSplit( ( char * ) pInfo.abName );
      szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
      szFileName[ 0 ] = '\0';
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      strcat( szFileName, pFileExt->item.asString.value );
      pInfo.abName = ( BYTE * ) szFileName;
      hb_xfree( pFileName );
      hb_itemRelease( pFileExt );
      ( ( AREAP ) pCurrArea->pArea )->lpFileInfo->pNext =
                              ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
      memset( ( ( AREAP ) pCurrArea->pArea )->lpFileInfo->pNext, 0,
              sizeof( FILEINFO ) );
      ( ( AREAP ) pCurrArea->pArea )->lpFileInfo->pNext->hFile = FS_ERROR;
      ( ( AREAP ) pCurrArea->pArea )->lpFileInfo->pNext->szFileName = szFileName;
      bError = ( SELF_CREATEMEMFILE( ( AREAP ) pCurrArea->pArea, &pInfo ) == FAILURE );
   }

   ( ( PHB_DYNS ) ( ( AREAP ) pCurrArea->pArea )->atomAlias )->hArea = 0;
   SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
   if( !ISLOG( 4 ) || bError )
   {
      if( pWorkAreas == pCurrArea )
      {
         pWorkAreas = pCurrArea->pNext;
         if( pWorkAreas )
            pWorkAreas->pPrev = NULL;
      }
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
   else
   {
      SELF_NEW( ( AREAP ) pCurrArea->pArea );
      szFileName = hb_parc( 1 );
      pFileName = hb_fsFNameSplit( szFileName );
      szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
      strcpy( szFileName, hb_parc( 1 ) );
      if( !pFileName->szExtension )
      {
         pFileExt = hb_itemPutC( NULL, "" );
         SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
         strcat( szFileName, pFileExt->item.asString.value );
          hb_itemRelease( pFileExt );
      }
      hb_xfree( pFileName );
      pInfo.abName = ( BYTE * ) szFileName;
      pInfo.fShared = !hb_set.HB_SET_EXCLUSIVE;
      pInfo.fReadonly = FALSE;
      ( ( AREAP ) pCurrArea->pArea )->uiArea = uiCurrArea;
      ( ( AREAP ) pCurrArea->pArea )->lpFileInfo->szFileName = szFileName;
      if( SELF_OPEN( ( AREAP ) pCurrArea->pArea, &pInfo ) == FAILURE )
      {
         SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
         hb_xfree( pCurrArea->pArea );
         hb_xfree( pCurrArea );
         pCurrArea = NULL;
      }
   }
}

HARBOUR HB_DBDELETE( void )
{
   if( pCurrArea )
      SELF_DELETE( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBDELETE" );
}

HARBOUR HB_DBFILTER( void )
{
   PHB_ITEM pFilter;

   if( pCurrArea )
   {
      pFilter = hb_itemPutC( NULL, "" );
      SELF_FILTERTEXT( ( AREAP ) pCurrArea->pArea, pFilter );
      hb_retc( pFilter->item.asString.value );
      hb_itemRelease( pFilter );
   }
   else
      hb_retc( "" );
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
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOTO" );
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

HARBOUR HB___DBLOCATE()
{
   PHB_ITEM pFor, pFor2, pWhile, pNext, pRecord, pRest;
   DBSCOPEINFO pScopeInfo;
   ULONG lNext;
   BOOL bEof, bFor, bWhile;

   if( !pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSETFILTER" );
      return;
   }

   memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
   pFor2 = hb_param( 1, IT_BLOCK );
   pWhile = hb_param( 2, IT_BLOCK );
   pNext = hb_param( 3, IT_NUMERIC );
   pRecord = hb_param( 4, IT_NUMERIC );
   pRest = hb_param( 5, IT_LOGICAL );
   if( !pWhile )
   {
      pWhile = hb_itemPutL( NULL, TRUE );
      pScopeInfo.itmCobWhile = pWhile;
   }
   else
   {
      pRest = hb_itemPutL( NULL, TRUE );
      pScopeInfo.fRest = pRest;
   }
   if( !pFor2 )
      pFor = hb_itemPutL( NULL, TRUE );
   else
   {
      pFor = hb_itemNew( NULL );
      hb_itemCopy( pFor, pFor2 );
   }
   if( !pRest )
   {
      pRest = hb_itemPutL( NULL, FALSE );
      pScopeInfo.fRest = pRest;
   }
   pScopeInfo.itmCobFor = pFor;
   SELF_SETLOCATE( ( AREAP ) pCurrArea->pArea, &pScopeInfo );
   ( ( AREAP ) pCurrArea->pArea )->fFound = FALSE;
   if( pRecord )
   {
      SELF_GOTOID( ( AREAP ) pCurrArea->pArea, pRecord );
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
      if( bEof )
         return;
      if( hb_itemType( pWhile ) == IT_BLOCK )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pWhile );
         hb_vmDo( 0 );
         bWhile = hb_itemGetL( &hb_stack.Return );
      }
      else
         bWhile = hb_itemGetL( pWhile );
      if( hb_itemType( pFor ) == IT_BLOCK )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pFor );
         hb_vmDo( 0 );
         bFor = hb_itemGetL( &hb_stack.Return );
         ( ( AREAP ) pCurrArea->pArea )->fFound = ( bWhile && bFor );
      }
      else
         ( ( AREAP ) pCurrArea->pArea )->fFound = ( bWhile && hb_itemGetL( pFor ) );
   }
   else if( pNext )
   {
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
      lNext = hb_parnl( 3 );
      if( bEof || lNext <= 0 )
         return;
      if( hb_itemType( pWhile ) == IT_BLOCK )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pWhile );
         hb_vmDo( 0 );
         bWhile = hb_itemGetL( &hb_stack.Return );
      }
      else
         bWhile = hb_itemGetL( pWhile );
      if( hb_itemType( pFor ) == IT_BLOCK )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pFor );
         hb_vmDo( 0 );
         bFor = hb_itemGetL( &hb_stack.Return );
      }
      else
         bFor = hb_itemGetL( pFor );
      while( !bEof && lNext-- > 0 && bWhile && !bFor )
      {
         SELF_SKIP( ( AREAP ) pCurrArea->pArea, 1 );
         SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
         if( hb_itemType( pWhile ) == IT_BLOCK )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pWhile );
            hb_vmDo( 0 );
            bWhile = hb_itemGetL( &hb_stack.Return );
         }
         else
            bWhile = hb_itemGetL( pWhile );
         if( hb_itemType( pFor ) == IT_BLOCK )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pFor );
            hb_vmDo( 0 );
            bFor = hb_itemGetL( &hb_stack.Return );
         }
         else
            bFor = hb_itemGetL( pFor );
      }
      ( ( AREAP ) pCurrArea->pArea )->fFound = bFor;
   }
   else if( hb_itemGetL( pRest ) )
   {
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
      if( bEof )
         return;
      if( hb_itemType( pWhile ) == IT_BLOCK )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pWhile );
         hb_vmDo( 0 );
         bWhile = hb_itemGetL( &hb_stack.Return );
      }
      else
         bWhile = hb_itemGetL( pWhile );
      if( hb_itemType( pFor ) == IT_BLOCK )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pFor );
         hb_vmDo( 0 );
         bFor = hb_itemGetL( &hb_stack.Return );
      }
      else
         bFor = hb_itemGetL( pFor );
      while( !bEof && bWhile && !bFor )
      {
         SELF_SKIP( ( AREAP ) pCurrArea->pArea, 1 );
         SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
         if( hb_itemType( pWhile ) == IT_BLOCK )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pWhile );
            hb_vmDo( 0 );
            bWhile = hb_itemGetL( &hb_stack.Return );
         }
         else
            bWhile = hb_itemGetL( pWhile );
         if( hb_itemType( pFor ) == IT_BLOCK )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pFor );
            hb_vmDo( 0 );
            bFor = hb_itemGetL( &hb_stack.Return );
         }
         else
            bFor = hb_itemGetL( pFor );
      }
      ( ( AREAP ) pCurrArea->pArea )->fFound = bFor;
   }
   else
   {
      SELF_GOTOP( ( AREAP ) pCurrArea->pArea );
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
      if( bEof )
         return;
      if( hb_itemType( pFor ) == IT_BLOCK )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pFor );
         hb_vmDo( 0 );
         bFor = hb_itemGetL( &hb_stack.Return );
      }
      else
         bFor = hb_itemGetL( pFor );
      while( !bEof && !bFor )
      {
         SELF_SKIP( ( AREAP ) pCurrArea->pArea, 1 );
         SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
         if( hb_itemType( pFor ) == IT_BLOCK )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pFor );
            hb_vmDo( 0 );
            bFor = hb_itemGetL( &hb_stack.Return );
         }
         else
            bFor = hb_itemGetL( pFor );
      }
      ( ( AREAP ) pCurrArea->pArea )->fFound = bFor;
   }
}

HARBOUR HB___DBSETLOCATE( void )
{
   PHB_ITEM pLocate, pFor;
   DBSCOPEINFO pScopeInfo;

   if( pCurrArea )
   {
      pLocate = hb_param( 1, IT_BLOCK );
      if( pLocate )
      {
         pFor = hb_itemNew( NULL );
         hb_itemCopy( pFor, pLocate );
         memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
         pScopeInfo.itmCobFor = pFor;
         SELF_SETLOCATE( ( AREAP ) pCurrArea->pArea, &pScopeInfo );
      }
   }
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
      ULONG ulLen;

      szAlias = hb_parc( 1 );

      ulLen = strlen( szAlias );

      if( ulLen >= 1 && szAlias[ 0 ] >= '0' && szAlias[ 0 ] <= '9' )
         uiNewArea = atoi( szAlias );
      else if( ulLen == 1 && toupper( szAlias[ 0 ] ) >= 'A' && toupper( szAlias[ 0 ] ) <= 'K' )
         uiNewArea = toupper( szAlias[ 0 ] ) - 'A' + 1;
      else
      {
         if( ( uiNewArea = hb_rddSelect( szAlias ) ) == 0 )
         {
            hb_errRT_BASE( EG_NOALIAS, 1002, NULL, szAlias );
            return;
         }
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

HARBOUR HB___DBSETFOUND( void )
{
   PHB_ITEM pFound;

   if( pCurrArea )
   {
      pFound = hb_param( 1, IT_LOGICAL );
      if( pFound )
         ( ( AREAP ) pCurrArea->pArea )->fFound = hb_itemGetL( pFound );
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

HARBOUR HB_DBSETFILTER( void )
{
   PHB_ITEM pBlock, pText;
   DBFILTERINFO pFilterInfo;

   if( pCurrArea )
   {
      pBlock = hb_param( 1, IT_BLOCK );
      if( pBlock )
      {
         pText = hb_param( 2, IT_STRING );
         pFilterInfo.itmCobExpr = pBlock;
         if( pText )
            pFilterInfo.abFilterText = pText;
         else
            pFilterInfo.abFilterText = hb_itemPutC( NULL, "" );
         SELF_SETFILTER( ( AREAP ) pCurrArea->pArea, &pFilterInfo );
         if( !pText )
            hb_itemRelease( pFilterInfo.abFilterText );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSETFILTER" );
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
   char * szDriver, * szFileName;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;
   USHORT uiSize, uiRddID, uiLen;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pFileExt;
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1];

   bNetError = FALSE;

   if( hb_parl( 1 ) )
      hb_rddSelectFirstAvailable();
   else if( pCurrArea )  /* If current WorkArea is in use then close it */
   {
      SELF_CLOSE( ( AREAP ) pCurrArea->pArea );
      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );

      if( pWorkAreas == pCurrArea )
      {
         pWorkAreas = pCurrArea->pNext;
         if( pWorkAreas )
            pWorkAreas->pPrev = NULL;
      }
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
   uiLen = hb_parclen( 2 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;
      hb_strncpyUpper( szDriverBuffer, hb_parc( 2 ), uiLen );
      szDriver = szDriverBuffer;
   }
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
   strncpy( szAlias, hb_parc( 4 ), HARBOUR_MAX_RDD_ALIAS_LENGTH );
   if( strlen( szAlias ) == 0 )
      strncpy( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );

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
   hb_xfree( pFileName );
   pInfo.uiArea = uiCurrArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;

   ( ( AREAP ) pCurrArea->pArea )->uiArea = uiCurrArea;

   /* Insert the new WorkArea node */

   if( !pWorkAreas )
      pWorkAreas = pCurrArea;  /* The new WorkArea node is the first */
   else
   {
      pAreaNode = pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea > uiCurrArea )
         {
            /* Insert the new WorkArea node */
            pCurrArea->pPrev = pAreaNode->pPrev;
            pCurrArea->pNext = pAreaNode;
            pAreaNode->pPrev = pCurrArea;
            if( pCurrArea->pPrev )
               pCurrArea->pPrev->pNext = pCurrArea;
            break;
         }
         if( pAreaNode->pNext )
            pAreaNode = pAreaNode->pNext;
         else
         {
            /* Append the new WorkArea node */
            pAreaNode->pNext = pCurrArea;
            pCurrArea->pPrev = pAreaNode;
            break;
         }
      }
   }

   ( ( AREAP ) pCurrArea->pArea )->lpFileInfo->szFileName = szFileName;
   if( SELF_OPEN( ( AREAP ) pCurrArea->pArea, &pInfo ) == FAILURE )
   {
      SELF_RELEASE( ( AREAP ) pCurrArea->pArea );

      if( pWorkAreas == pCurrArea )
      {
         pWorkAreas = pCurrArea->pNext;
         if( pWorkAreas )
            pWorkAreas->pPrev = NULL;
      }
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
      return;
   }
}

HARBOUR HB___DBZAP( void )
{
   if( pCurrArea )
      SELF_ZAP( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "__DBZAP" );
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
   LPFIELD pField;

   if( pCurrArea )
   {
      char szName[ HARBOUR_MAX_RDD_FIELDNAME_LENGTH ];

      hb_strncpyUpper( szName, hb_parc( 1 ), hb_parclen( 1 ) );
      uiCount = 0;
      pField = ( ( AREAP ) pCurrArea->pArea )->lpFields;
      while( pField )
      {
         ++uiCount;
         if( strcmp( szName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName ) == 0 )
         {
            hb_retni( uiCount );
            return;
         }
         pField = pField->lpfNext;
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
   char szDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];
   USHORT uiLen;

   hb_rddCheck();
   uiLen = hb_parclen( 1 );
   if( uiLen > 0 )
   {
      hb_strncpyUpper( szDriver, hb_parc( 1 ), uiLen );
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
   char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( szDefDriver );

   uiLen = hb_parclen( 1 );
   if( uiLen > 0 )
   {
      hb_strncpyUpper( szNewDriver, hb_parc( 1 ), uiLen );

      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1015, NULL, "RDDSETDEFAULT" );
         return;
      }

      szDefDriver = ( char * ) hb_xrealloc( szDefDriver, uiLen + 1 );
      strcpy( szDefDriver, szNewDriver );
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
   ULONG ulLen;

   szAlias = hb_parc( 1 );
   ulLen = strlen( szAlias );

   if( ulLen == 1 && toupper( szAlias[ 0 ] ) >= 'A' && toupper( szAlias[ 0 ] ) <= 'K' )
      hb_retni( toupper( szAlias[ 0 ] ) - 'A' + 1 );
   else if( ulLen > 0 )
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
   char cDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];

   hb_rddCheck();
   hb_retc( szDefDriver );
   szNewDriver = hb_parc( 1 );
   if( ( uiLen = strlen( szNewDriver ) ) > 0 )
   {
      hb_strncpyUpper( cDriverBuffer, szNewDriver, uiLen );
      szDefDriver = ( char * ) hb_xrealloc( szDefDriver, uiLen + 1 );
      strcpy( szDefDriver, cDriverBuffer );
   }
}

