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
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
 *    DB* () documentation
 *    ORD*() documentation
 *    RDD*() documentation
 * See doc/license.txt for licensing terms.
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
HARBOUR HB_DBEVAL( void );
HARBOUR HB_DBF( void );
HARBOUR HB_DBFILTER( void );
HARBOUR HB_DBGOBOTTOM( void );
HARBOUR HB_DBGOTO( void );
HARBOUR HB_DBGOTOP( void );
HARBOUR HB___DBLOCATE( void );
HARBOUR HB___DBPACK( void );
HARBOUR HB_DBRECALL( void );
HARBOUR HB_DBRLOCK( void );
HARBOUR HB_DBRLOCKLIST( void );
HARBOUR HB_DBRUNLOCK( void );
HARBOUR HB_DBSEEK( void );
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
HARBOUR HB_INDEXORD( void );
HARBOUR HB_LASTREC( void );
HARBOUR HB_LOCK( void );
HARBOUR HB_LUPDATE( void );
HARBOUR HB_NETERR( void );
HARBOUR HB_ORDBAGEXT( void );
HARBOUR HB_ORDBAGNAME( void );
HARBOUR HB_ORDCONDSET( void );
HARBOUR HB_ORDCREATE( void );
HARBOUR HB_ORDDESTROY( void );
HARBOUR HB_ORDFOR( void );
HARBOUR HB_ORDKEY( void );
HARBOUR HB_ORDLISTADD( void );
HARBOUR HB_ORDLISTCLEAR( void );
HARBOUR HB_ORDLISTREBUILD( void );
HARBOUR HB_ORDNAME( void );
HARBOUR HB_ORDNUMBER( void );
HARBOUR HB_ORDSETFOCUS( void );
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

static ERRCODE defCompile( AREAP pArea, BYTE * szExpr )
{
   HB_MACRO_PTR pMacro;

   HB_TRACE(HB_TR_DEBUG, ("defCompile(%p, %p)", pArea, szExpr));

   pMacro = hb_macroCompile( ( char * ) szExpr );
   if( pMacro )
   {
      pArea->valResult = hb_itemPutPtr( pArea->valResult, ( void * ) pMacro );
      return SUCCESS;
   }
   else
      return FAILURE;
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

static ERRCODE defEval( AREAP pArea, LPDBEVALINFO pEvalInfo )
{
   BOOL bEof, bFor, bWhile;
   ULONG ulNext;

   HB_TRACE(HB_TR_DEBUG, ("defEval(%p, %p)", pArea, pEvalInfo));

   if( pEvalInfo->dbsci.itmRecID )
   {
      SELF_GOTOID( pArea, pEvalInfo->dbsci.itmRecID );
      SELF_EOF( pArea, &bEof );
      if( !bEof )
      {
         if( pEvalInfo->dbsci.itmCobWhile )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pEvalInfo->dbsci.itmCobWhile );
            hb_vmDo( 0 );
            bWhile = hb_itemGetL( &hb_stack.Return );
         }
         else
            bWhile = TRUE;

         if( pEvalInfo->dbsci.itmCobFor )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pEvalInfo->dbsci.itmCobFor );
            hb_vmDo( 0 );
            bFor = hb_itemGetL( &hb_stack.Return );
         }
         else
            bFor = TRUE;

         if( bWhile && bFor )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pEvalInfo->itmBlock );
            hb_vmDo( 0 );
         }
      }
      return SUCCESS;
   }
      
   if( !pEvalInfo->dbsci.fRest || !hb_itemGetL( pEvalInfo->dbsci.fRest ) )
      SELF_GOTOP( pArea );

   if( pEvalInfo->dbsci.lNext )
      ulNext = hb_itemGetNL( pEvalInfo->dbsci.lNext );

   SELF_EOF( pArea, &bEof );
   while( !bEof )
   {
      if( pEvalInfo->dbsci.lNext && ulNext-- < 1 )
         break;

      if( pEvalInfo->dbsci.itmCobWhile )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pEvalInfo->dbsci.itmCobWhile );
         hb_vmDo( 0 );
         bWhile = hb_itemGetL( &hb_stack.Return );
         if( !bWhile )
            break;
      }
      else
         bWhile = TRUE;

      if( pEvalInfo->dbsci.itmCobFor )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pEvalInfo->dbsci.itmCobFor );
         hb_vmDo( 0 );
         bFor = hb_itemGetL( &hb_stack.Return );
      }
      else
         bFor = TRUE;

      if( bFor && bWhile )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pEvalInfo->itmBlock );
         hb_vmDo( 0 );
      }
      SELF_SKIP( pArea, 1 );
      SELF_EOF( pArea, &bEof );
   }

   return SUCCESS;
}

static ERRCODE defEvalBlock( AREAP pArea, PHB_ITEM pBlock )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("defEvalBlock(%p, %p)", pArea, pBlock));

   if( !pBlock && !IS_BLOCK( pBlock ) )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_NOMETHOD );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_NOMETHOD ) );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   hb_vmPushSymbol( &hb_symEval ); 
   hb_vmPush( pBlock );
   hb_vmDo( 0 ); 
   if( !pArea->valResult )
      pArea->valResult = hb_itemNew( NULL );
   hb_itemCopy( pArea->valResult, &hb_stack.Return );

   return SUCCESS;
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

static ERRCODE defNewArea( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("defNewArea(%p)", pArea));

   pArea->lpDataInfo = ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
   memset( pArea->lpDataInfo, 0, sizeof( FILEINFO ) );
   pArea->lpDataInfo->hFile = FS_ERROR;
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

static ERRCODE defOrderCondition( AREAP pArea, LPDBORDERCONDINFO pOrderInfo )
{
   if( pArea->lpdbOrdCondInfo )
   {
      if( pArea->lpdbOrdCondInfo->abFor )
         hb_xfree( pArea->lpdbOrdCondInfo->abFor );
      if( pArea->lpdbOrdCondInfo->itmCobFor )
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmCobFor );
      if( pArea->lpdbOrdCondInfo->itmCobWhile )
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmCobWhile );
      if( pArea->lpdbOrdCondInfo->itmCobEval )
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmCobEval );
      hb_xfree( pArea->lpdbOrdCondInfo );
   }
   pArea->lpdbOrdCondInfo = pOrderInfo;

   return SUCCESS;
}

static ERRCODE defRelease( AREAP pArea )
{
   LPFILEINFO pFileInfo;

   HB_TRACE(HB_TR_DEBUG, ("defRelease(%p)", pArea));

   SELF_ORDSETCOND( pArea, NULL );

   if( pArea->valResult )
      hb_itemRelease( pArea->valResult );

   if( pArea->lpFields )
   {
      hb_xfree( pArea->lpFields );
      pArea->uiFieldCount = 0;
   }

   while( pArea->lpDataInfo )
   {
      pFileInfo = pArea->lpDataInfo;
      pArea->lpDataInfo = pArea->lpDataInfo->pNext;
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

   return SELF_GOTO( pArea, pArea->lpExtendInfo->ulRecNo + lToSkip );
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
                             ( DBENTRYP_BIB ) defUnSupported,
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
                             ( DBENTRYP_PP ) defUnSupported,
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
                             defEval,
                             defUnSupported,
                             defUnSupported,
                             ( DBENTRYP_OI ) defUnSupported,
                             defUnSupported,
                             ( DBENTRYP_OI ) defUnSupported,
                             defUnSupported,
                             defOrderCondition,
                             ( DBENTRYP_VOC ) defUnSupported,
                             ( DBENTRYP_OI ) defUnSupported,
                             ( DBENTRYP_OII ) defUnSupported,
                             defClearFilter,
                             defClearLocate,
                             defFilterText,
                             defSetFilter,
                             defSetLocate,
                             defCompile,
                             defError,
                             defEvalBlock,
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
      hb_strncpyUpper( szSuperName, ( char * ) szDrvName, uiCount );
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
/*  $DOC$
 *  $FUNCNAME$
 *     AFIELDS()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Fills referenced arrays with database field information
 *  $SYNTAX$
 *      AFields(<aNames>[,<aTypes>][,<aLen>][,<aDecs>]) --> <nFields>
 *  $ARGUMENTS$
 *      <aNames>  Array of field names
 *      <aTypes>  Array of field names
 *      <aLens>  Array of field names
 *      <aDecs>  Array of field names
 *  $RETURNS$
 *      <nFields> Number od fields in a database or work area
 *  $DESCRIPTION$
 *        This function will fill a series of arrays with field
 *      names,field types,field lenghts, and number of field
 *      decimal positions for the currently selected or designed
 *      database. Each array parallels the different descriptors
 *      of a file's structure.The first array will consist of the
 *      names of the fields in the current work area.All other arrays
 *      are optional and will be filled with the corrensponding data.
 *      This function will return zero if no parameters are specified 
 *      or if no database is avaliable in the current work area.Otherwise,
 *      the number of fields or the lenght of the shortest array argument,
 *      witchever is smaller, will be returned.
 *  $EXAMPLES$
 *      FUNCTION Main()
 *         LOCAL aNames:={},aTypes:={},aLens:={},aDecs:={},nFields:=0
 *
 *         USE Test
 *
 *         dbGoTop()
 *         nFields:=aFields(aNames,aTypes,aLens,aDecs)
 *         
 *         ? "Number of fields", nFields
 *
 *         RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      AFIELDS() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
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
/*  $DOC$
 *  $FUNCNAME$
 *     ALIAS()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Returns the alias name of a work area
 *  $SYNTAX$
 *      Alias([<nWorkArea>]) --> <cWorkArea>
 *  $ARGUMENTS$
 *      <nWorkArea> Number of a work area
 *  $RETURNS$
 *      <cWorkArea> Name of alias
 *  $DESCRIPTION$
 *        This function returns the alias of the work area
 *      indicated by <nWorkArea>. If <nWorkArea> is not
 *      provided, the alias of the current work area is
 *      returned.This function does not differ from the Clipper
 *      function DBF(), with is a strictly  a compatibility function
 *  $EXAMPLES$
 *      FUNCTION Main()
 *         
 *         USE Test
 *
 *         select 0
 *         qOut( IF(Alias()=="","No Name",Alias()))
 *         Test->(qOut(Alias())
 *         qOut(Alias(1))
 *
 *         RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ALIAS() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

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

/*  $DOC$
 *  $FUNCNAME$
 *      DBEVAL()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Performs a code block operation on the current DATA BASE
 *  $SYNTAX$
 *      DBEVAL( <bBlock>,
 *      [<bFor>], [<bWhile>],
 *      [<nNext>], [<nRecord>],
 *      [<lRest>] ) --> NIL
 *  $ARGUMENTS$
 *      <bBlock> Operation that is to be performed
 *      <bFor> Code block for the For condition
 *      <bWhile> Code block for the WHILE condition
 *      <nNext> Number of NEXT records  to process
 *      <nRecord> Record number to work on exactly
 *      <lRest> Toggle to rewind record pointer
 *  $RETURNS$
 *      NIL
 *  $DESCRIPTION$
 *        Performs a code block operation on the current DATA BASE
 *  $EXAMPLES$
 *      FUNCTION Main()
 *         LOCAL nCount
 *
 *         USE Test
 *
 *         dbGoto( 4 )
 *         ? RecNo()
 *         COUNT TO nCount
 *         ? RecNo(), nCount
 *         COUNT TO nCount NEXT 10
 *         ? RecNo(), nCount
 *
 *         RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      S
 *  $COMPLIANCE$
 *      DBEVAL is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

HARBOUR HB_DBEVAL( void )
{
   if( pCurrArea )
   {
      DBEVALINFO pEvalInfo;

      pEvalInfo.itmBlock = hb_param( 1, IT_BLOCK );
      if( !pEvalInfo.itmBlock )
      {
         hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmCobFor = hb_param( 2, IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobFor )
      {
         if( !ISNIL( 2 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.itmCobWhile = hb_param( 3, IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobWhile )
      {
         if( !ISNIL( 3 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.lNext = hb_param( 4, IT_NUMERIC );
      if( !pEvalInfo.dbsci.lNext )
      {
         if( !ISNIL( 4 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.itmRecID = hb_param( 5, IT_NUMERIC );
      if( !pEvalInfo.dbsci.itmRecID )
      {
         if( !ISNIL( 5 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.fRest = hb_param( 6, IT_LOGICAL );
      if( !pEvalInfo.dbsci.fRest )
      {
         if( !ISNIL( 6 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      SELF_DBEVAL( ( AREAP ) pCurrArea->pArea, &pEvalInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBEVAL" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBF()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Returns the alias name of a work area
 *  $SYNTAX$
 *      Dbf() --> <cWorkArea>
 *  $ARGUMENTS$
 *      
 *  $RETURNS$
 *      <cWorkArea> Name of alias
 *  $DESCRIPTION$
 *        This function returns the same alias name of
 *      the currently selected work area.
 *      This function is the same as Alias()
 *  $EXAMPLES$
 *      FUNCTION Main()
 *         
 *         USE Test
 *
 *         select 0
 *         qOut( IF(DBF()=="","No Name",DBF()))
 *         Test->(qOut(DBF())
 *         qOut(Alias(1))
 *
 *         RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ALIAS() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     BOF()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Test for the beggining-of-file condition
 *  $SYNTAX$
 *      BOF() --> <lBegin>
 *  $ARGUMENTS$
 * 
 *  $RETURNS$
 *       <lBegin> Logical true (.T.) or false (.F.)
 *  $DESCRIPTION$
 *        This function determines if the beggining of the file
 *      marker has been reached. If so, the function will return
 *      a logical true (.T.); otherwise, a logical false(.F.) will
 *      be returned.
 *        By default, BOF() will apply to the currently selected
 *      database unless the function is preceded by an alias
 *  $EXAMPLES$
 *      FUNCTION Main()
 *         
 *         USE Test
 *
 *         qOut( BOF(),Recno())
 *         qOut(Test->(BOF()),Test->(Recno()))
 *         skip -1
 *         qOut( BOF(),Recno())
 *         qOut(Test->(BOF()),Test->(Recno()))
 *         dbGoBottom()
 *         qOut( EOF(),Recno())
 *         qOut(Test->(BOF()),Test->(Recno()),Test->(BOF()))
 *         skip
 *         Test->(dbGoBottom(),LastRec())
 *         qOut( EOF(),Recno())
 *         Test->(qOut(BOF(),Recno(),BOF()))
 *
 *         RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      BOF() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *    EOF(),FOUND(),LASTREC()
 *  $END$
 */

HARBOUR HB_BOF( void )
{
   BOOL bBof = TRUE;

   if( pCurrArea )
      SELF_BOF( ( AREAP ) pCurrArea->pArea, &bBof );
   hb_retl( bBof );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBAPPEND()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Appends a new record to a database file.
 *  $SYNTAX$
 *      DbAppend(<<lLock>]) --> NIL
 *  $ARGUMENTS$
 *      <lLock> Toggle to release record locks
 *  $RETURNS$
 *  $DESCRIPTION$
 *        This function add a new record to the end of the database
 *     in the selected or aliased work area. All fields in that
 *     database will be given empty data values - character fields
 *     will be filled with blank spaces,date fields with CTOD('//'),
 *     numeric fields with 0,logical fields with .F., and memo fields
 *     with NULL bytes.The header of the database is not updated until
 *     the record is flushed from the buffer and the contents are
 *     written to the disk.
 *        Under a networking enviroment, DBAPPEND() performs an addi-
 *     tional operation: It attrmps to lock the newly added record. If
 *     the database file is currently locked or if a locking assignment
 *     if made to LASTREC()+1,NETERR() will return a logical true (.T.)
 *     immediately after the DBAPPEND() function. This function does
 *     not unlock the locked records.
 *        If <lLock> is passed a logical true (.T.) value, it will 
 *     release the record locks, which allows the application to main-
 *     tain multiple record locks during an appending operation. The
 *     default for this parameter is a logical false (.F.).
 *  $EXAMPLES$
 *      FUNCTION Main()
 *         
 *         USE Test
 *         local cName="HARBOUR",nId=10
 *         Test->(DbAppend())
 *         Replace Test->Name wit cName,Id with nId
 *         Use
 *         RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      DBAPPEND() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBCLEARFILTER()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Clears the current filter condiction in a work area
 *  $SYNTAX$
 *      DbClearFilTer() -> NIL
 *  $ARGUMENTS$
 * 
 *  $RETURNS$
 *
 *  $DESCRIPTION$
 *        This function clears any active filter condiction
 *      for the current or selected work area.
 *  $EXAMPLES$
 *      Function Main()
 *
 *       Use Test
 *
 *       Set Filter to Left(Test->Name,2) == "An"
 *
 *       Dbedit()
 *
 *       Test->(DbClearFilter())
 *
 *       USE
 *
 *       Return Nil
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      DBCLEARFILTER() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

HARBOUR HB_DBCLEARFILTER( void )
{
   if( pCurrArea )
      SELF_CLEARFILTER( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCLEARFILTER" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBCLOSEALL()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Close all open files in all work areas.
 *  $SYNTAX$
 *      DbCloseAll() -> NIL
 *  $ARGUMENTS$
 * 
 *  $RETURNS$
 *      DBCLOSEALL always return NIL
 *  $DESCRIPTION$
 *        This function close all open databases and all associated
 *      indexes.In addition, it closes all format files and moves
 *      the work area pointer to the first position
 *  $EXAMPLES$
 *      Function Main()
 *
 *       Use Test New
 *
 *       DbEdit()
 *
 *       Use Test1 New
 *
 *       DbEdit()
 *
 *       DbCloseAll()
 *
 *       USE
 *
 *       Return Nil
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      DBCLOSEALL() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

HARBOUR HB_DBCLOSEALL( void )
{
   hb_rddCloseAll();
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBCLOSEAREA()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Close a database file in a work area.
 *  $SYNTAX$
 *      DbCloseArea() -> NIL
 *  $ARGUMENTS$
 * 
 *  $RETURNS$
 *
 *  $DESCRIPTION$
 *        This function  will close any database open in the selected
 *      or aliased work area.
 *  $EXAMPLES$
 *      Function Main()
 *
 *       Use Test
 *
 *       Dbedit()
 *
 *       Test->(DbCloseArea())
 *
 *       USE
 *
 *       Return Nil
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      DBCLOSEAREA() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBCOMMIT()
 *  $CATEGORY$
 *     DATA BASE
 *  $ONELINER$
 *     Flush pending updates
 *  $SYNTAX$
 *     DBCOMMIT() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBCOMMIT() always returns NIL.
 *  $DESCRIPTION$
 *     DBCOMMIT() causes all updates to the current work area to be written to
 *   disk.  All updated database and index buffers are written to DOS and a
 *   DOS COMMIT request is issued for the database (.dbf) file and any index
 *   files associated with the work area.
 *
 *   DBCOMMIT() performs the same function as the standard COMMIT command
 *   except that it operates only on the current work area.  For more
 *   information, refer to the COMMIT command.
 * 
 *  Notes
 *
 *      Network environment: DBCOMMIT() makes database updates visible
 *      to other processes.  To insure data integrity, issue DBCOMMIT()
 *      before an UNLOCK operation.  For more information, refer to the
 *      Network Programming chapter in the Programming and Utilities guide.
 *
 *     DBCOMMIT() uses DOS interrupt 21h function 68h to perform the
 *      solid-disk write.  It is up to the network operating system to
 *      properly implement this request.  Check with the network vendor to
 *      see if this is supported.
 *     
 *  $EXAMPLES$
 *      In this example, COMMIT is used to force a write to disk after
 *      a series of memory variables are assigned to field variables:
 *
 *      USE Sales EXCLUSIVE NEW
 *      MEMVAR->Name := Sales->Name
 *      MEMVAR->Amount := Sales->Amount
 *      //
 *      @ 10, 10 GET MEMVAR->Name
 *      @ 11, 10 GET MEMVAR->Amount
 *      READ
 *      //
 *      IF UPDATED()
 *         APPEND BLANK
 *         REPLACE Sales->Name WITH MEMVAR->Name
 *         REPLACE Sales->Amount WITH MEMVAR->Amount
 *         Sales->( DBCOMMIT() )
 *      ENDIF
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *
 *  $SEEALSO$
 *     DBCLOSEALL(),DBCOMMITALL(),DBUNLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBCOMMIT( void )
{
   if( pCurrArea )
      SELF_FLUSH( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCOMMIT" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBCOMMITALL()
 *  $CATEGORY$
 *     DATA BASE
 *  $ONELINER$
 *     Flush pending updates in all work areas
 *  $SYNTAX$
 *     DBCOMMIT() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBCOMMIT() always returns NIL.
 *  $DESCRIPTION$
 *   DBCOMMITALL() causes all pending updates to all work areas to be written
 *   to disk.  It is equivalent to calling DBCOMMIT() for every occupied work
 *   area.
 *   For more information, refer to DBCOMMIT().
 * Notes
 *     DBCOMMITALL() uses DOS interrupt 21h function 68h to perform
 *      the solid-disk write.  It is up to the network operating system to
 *      properly implement this request.  Check with the network vendor to
 *      see if this is supported.
 *     
 *  $EXAMPLES$

 *     The following example writes all pending updates to disk:

 *      cLast := "Winston"
 *      //
 *      DBUSEAREA( .T., "DBFNTX", "Sales", "Sales", .T. )
 *      DBSETINDEX( "SALEFNAM" )
 *      DBSETINDEX( "SALELNAM" )
 *      //
 *      DBUSEAREA( .T., "DBFNTX", "Colls", "Colls", .T. )
 *      DBSETINDEX( "COLLFNAM" )
 *      DBSETINDEX( "COLLLNAM" )

 *      DBSELECTAREA( "Sales" )      // select "Sales" work area

 *      IF ( Sales->(DBSEEK(cLast)) )
 *         IF Sales->( DELETED() ) .AND. Sales( RLOCK() )
 *            Sales->( DBRECALL() )
 *            ? "Deleted record has been recalled."
 *         ENDIF
 *      ELSE
 *         ? "Not found"
 *      ENDIF
 *      //
 *      // processing done, write updates to disk and close
 *      DBCOMMITALL()
 *      DBCLOSEALL()
 *      QUIT
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *

 *  $SEEALSO$
 *     DBCLOSEALL(),DBCOMMIT(),DBUNLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

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

/*  $DOC$
 *  $FUNCNAME$
 *     __DBCONTINUE
 *  $CATEGORY$
 *     DATA BASE
 *  $ONELINER$
 *     Resume a pending LOCATE
 *  $SYNTAX$
 *     __DbCONTINUE()   -> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     __DbCONTINUE()  Always return nil
 *  $DESCRIPTION$
 *   __DBCONTINUE is a database command that searches from the current record
 *   position for the next record meeting the most recent LOCATE condition
 *   executed in the current work area.  It terminates when a match is found
 *   or end of file is encountered.  If __DBCONTINUE is successful, the matching
 *   record becomes the current record and FOUND() returns true (.T.); if
 *   unsuccessful, FOUND() returns false (.F.).

 *   Each work area may have an active LOCATE condition.  In CA-Clipper, a
 *   LOCATE condition remains pending until a new LOCATE condition is
 *   specified.  No other commands release the condition.

 Notes

 *     Scope and WHILE condition: Note that the scope and WHILE
 *      condition of the initial LOCATE are ignored; only the FOR condition
 *      is used with CONTINUE.  If you are using a LOCATE with a WHILE
 *      condition and want to continue the search for a matching record, use
 *      SKIP and then repeat the original LOCATE statement adding REST as the
 *      scope.
 *     
 *  $EXAMPLES$
 *     This example scans records in Sales.dbf for a particular
 *      salesman and displays a running total sales amounts:

 *      LOCAL nRunTotal := 0
 *      USE Sales NEW
 *      LOCATE FOR Sales->Salesman = "1002"
 *      DO WHILE FOUND()
 *         ? Sales->Salesname, nRunTotal += Sales->Amount
 *         __DBCONTINUE()
 *      ENDDO

 *     This example demonstrates how to continue if the pending
 *      LOCATE scope contains a WHILE condition:

 *      LOCAL nRunTotal := 0
 *      USE Sales INDEX Salesman NEW
 *      SEEK "1002"
 *      LOCATE REST WHILE Sales->Salesman = "1002";
 *            FOR Sales->Amount > 5000
 *      DO WHILE FOUND()
 *         ? Sales->Salesname, nRunTotal += Sales->Amount
 *         SKIP
 *         LOCATE REST WHILE Sales->Salesman = "1002";
 *            FOR Sales->Amount > 5000
 *      ENDDO
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *

 *  $SEEALSO$
 *     EOF(),FOUND()
 *  $INCLUDE$
 *     
 *  $END$
 */


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

/*  $DOC$
 *  $FUNCNAME$
 *     DBCREATE()
 *  $CATEGORY$
 *     DATA BASE
 *  $ONELINER$
 *      Create a database file from a database structure array
 *  $SYNTAX$
 *     DBCREATE(<cDatabase>, <aStruct>,[<cDriver>]) --> NIL
 *     
 *  $ARGUMENTS$
 *     <cDatabase> is the name of the new database file, with an optional
 *     drive and directory, specified as a character string.  If specified
 *     without an extension (.dbf) is assumed.

 *     <aStruct> is an array that contains the structure of <cDatabase> as
 *     a series of subarrays, one per field.  Each subarray contains the
 *     definition of each field's attributes and has the following structure:

 *     Field Definition Subarray
 *     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 *     Position     Metasymbol     Dbstruct.ch
 *     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 *     1            cName          DBS_NAME
 *     2            cType          DBS_TYPE
 *     3            nLength        DBS_LEN
 *     4            nDecimals      DBS_DEC
 *     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

 *     <cDriver> specifies the replaceable database driver (RDD) to use to
 *     process the current work area.  <cDriver> is name of the RDD specified
 *     as a character expression.  If you specify <cDriver> as a literal value,
 *     you must enclose it in quotes.
 *     
 *  $RETURNS$
 *     DBCREATE() always returns NIL.
 *  $DESCRIPTION$
       DBCREATE() is a database function that creates a database file from an
 *   array containing the structure of the file.  You may create the array
 *   programmatically or by using DBSTRUCT().  DBCREATE() is similar to the
 *   CREATE FROM command which creates a new database file structure from a
 *   structure extended file.  Use CREATE or COPY STRUCTURE EXTENDED commands
 *   to create a structure extended file.

 *   Before using DBCREATE(), you must first create the <aStruct> array and
 *   fill it with the field definition arrays according to the structure in
 *   Field Definition Subarray table (above).  There are some specific rules
 *   for creating a field definition array, including:

 *     Specify all field attributes with a value of the proper data
 *      type for the attribute.  The decimals attribute must be
 *      specified--even for non-numeric fields.  If the field does not have a
 *      decimals attribute, specify zero.

 *     Specify the type attribute using the first letter of the data
 *      type as a minimum.  Use longer and more descriptive terms for
 *      readability.  For example, both "C" and "Character" can be specified
 *      as the type attribute for character fields.

 *     In CA-Clipper, character fields contain up to 64,000
 *      characters.  Unlike the CREATE FROM command, DBCREATE() does not use
 *      the decimals attribute to specify the high-order part of the field
 *      length.  Specify the field length directly, regardless of its
 *      magnitude.

 *   To make references to the various elements of the field definition
 *   subarray more readable, the header file called Dbstruct.ch is supplied
 *   which contains the #defines to assign a name to the array position for
 *   each field attribute.  It is located in \CLIPPER5\INCLUDE.
 *     
 *  $EXAMPLES$
 *     This example creates an empty array and then adds field
 *      definition subarrays using the AADD() function before creating
 *      People.dbf.  You might use this technique to add field definitions to
 *      your structure array dynamically:

 *      aDbf := {}
 *      AADD(aDbf, { "Name", "C", 25, 0 })
 *      AADD(aDbf, { "Address", "C", 1024, 0 })
 *      AADD(aDbf, { "Phone", "N", 13, 0 })
 *      //
 *      DBCREATE("People", aDbf)

 *     This example performs the same types of actions but declares
 *      the structure array as a two-dimensional array, and then uses
 *      subscript addressing to specify the field definitions.  It will be
 *      created using the DBFMDX RDD:

 *      #include "Dbstruct.ch"
 *      //
 *      LOCAL aDbf[1][4]
 *      aDbf[1][ DBS_NAME ] := "Name"
 *      aDbf[1][ DBS_TYPE ] := "Character"
 *      aDbf[1][ DBS_LEN ]  := 25
 *      aDbf[1][ DBS_DEC ]  := 0
 *      //
 *      DBCREATE("Name", aDbf, "DBFMDX")
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     
 *  $SEEALSO$
 *     AFIELDS(),DBSTRUCT()
 *  $INCLUDE$
 *     "Dbstruct.ch"
 *  $END$
 */

HARBOUR HB_DBCREATE( void )
{
   char * szDriver, * szFileName;
   USHORT uiSize, uiLen, uiRddID;
   ULONG ulRecCount;
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
      if( hb_arrayLen( pFieldDesc ) < 4 )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
         return;
      }

      /* Validate items type, name, size and decimals of field */
      if( !( hb_arrayGetType( pFieldDesc, 1 ) & IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 2 ) & IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 3 ) & IT_NUMERIC ) ||
          !( hb_arrayGetType( pFieldDesc, 4 ) & IT_NUMERIC ) )
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
            else
               pWorkAreas = pCurrArea;
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

   ( ( AREAP ) pCurrArea->pArea )->lpDataInfo->szFileName = szFileName;
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
      if( pFileName->szDrive )
         strcat( szFileName, pFileName->szDrive );
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      strcat( szFileName, pFileExt->item.asString.value );
      pInfo.abName = ( BYTE * ) szFileName;
      hb_xfree( pFileName );
      hb_itemRelease( pFileExt );
      ( ( AREAP ) pCurrArea->pArea )->lpDataInfo->pNext =
                              ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
      memset( ( ( AREAP ) pCurrArea->pArea )->lpDataInfo->pNext, 0,
              sizeof( FILEINFO ) );
      ( ( AREAP ) pCurrArea->pArea )->lpDataInfo->pNext->hFile = FS_ERROR;
      ( ( AREAP ) pCurrArea->pArea )->lpDataInfo->pNext->szFileName = szFileName;
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
      ( ( AREAP ) pCurrArea->pArea )->lpDataInfo->szFileName = szFileName;
      if( SELF_OPEN( ( AREAP ) pCurrArea->pArea, &pInfo ) == FAILURE )
      {
         SELF_RELEASE( ( AREAP ) pCurrArea->pArea );
         hb_xfree( pCurrArea->pArea );
         hb_xfree( pCurrArea );
         pCurrArea = NULL;
      }
      SELF_RECCOUNT( ( AREAP ) pCurrArea->pArea, &ulRecCount );
      ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->ulRecCount = ulRecCount;
   }
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBDELETE()
 *  $CATEGORY$
 *     DATA BASE
 *  $ONELINER$
 *     Mark a record for deletion
 *  $SYNTAX$
 *     DBDELETE() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBDELETE() always returns NIL.
 *  $DESCRIPTION$
       DBDELETE() marks the current record as deleted.  Records marked for
     deletion can be filtered using SET DELETED or removed from the file
     using the PACK command.

     DBDELETE() performs the same function as the standard DELETE command
     with a scope of the current record.  For more information, refer to the
     DELETE command.

 Notes

       Logical records: If the global _SET_DELETED status is true
        (.T.), deleted records are not logically visible.  That is, database
        operations which operate on logical records will not consider records
        marked for deletion.  Note, however, that if _SET_DELETED is true
        (.T.) when the current record is marked for deletion, the record
        remains visible until it is no longer the current record.

       Network environment: For a shared database on a network,
        DBDELETE() requires the current record to be locked.  For more
        information, refer to the Network Programming chapter of the
        Programming and Utilities guide.
 *     
 *  $EXAMPLES$
       The following example deletes a record after a successful
        record lock:

        cLast := "Winston"
        DBUSEAREA( .T., "DBFNTX", "Sales", "Sales", .T. )
        DBSETINDEX( "LASTNAME" )
        //
        IF ( Sales->(DBSEEK(cLast)) )
           IF Sales->( RLOCK() )
              Sales->( DBDELETE() )
              ? "Record deleted: ", Sales( DELETED() )
           ELSE
              ? "Unable to lock record..."
           ENDIF
        ELSE
           ? "Not found"
        ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     
 *  $SEEALSO$
 *    DBRECALL() 
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBDELETE( void )
{
   if( pCurrArea )
      SELF_DELETE( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBDELETE" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBFILTER()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the current filter expression as a character string
 *  $SYNTAX$
 *     DBFILTER() --> cFilter
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBFILTER() returns the filter condition defined in the current work area
 *   as a character string.  If no FILTER has been SET, DBFILTER() returns a
 *   null string ("").
 *     
 *  $DESCRIPTION$
 *     DBFILTER() is a database function used to save and reexecute an active
 *   filter by returning the filter expression as a character string that can
 *   be later recompiled and executed using the macro operator (&).  This
 *   function operates like the DBRELATION() and DBRSELECT() functions which
 *   save and reexecute the linking expression of a relation within a work
 *   area.
 *
 *   Since each work area can have an active filter, DBFILTER() can return
 *   the filter expression of any work area.  This is done by referring to
 *   DBFILTER() within an aliased expression as demonstrated below.
 *
 *   Notes
 *
 *     Declared variables: A character string returned by DBFILTER()
 *      may not operate correctly when recompiled and executed using the
 *      macro operator (&) if the original filter expression contained
 *      references to local or static variables, or otherwise depended on
 *      compile-time declarations.
 *     
 *  $EXAMPLES$
 *     This example opens two database files, sets two filters, then
 *      displays the filter expressions for both work areas:
 *
 *      USE Customer INDEX Customer NEW
 *      SET FILTER TO Last = "Smith"
 *      USE Invoices INDEX Invoices NEW
 *      SET FILTER TO CustId = "Smi001"
 *      SELECT Customer
 *      //
 *      ? DBFILTER()                  // Result: Last = "Smith"
 *      ? Invoices->(DBFILTER())      // Result: Custid = "Smi001"
 *
 *     This user-defined function, CreateQry(), uses DBFILTER() to
 *      create a memory file containing the current filter expression in the
 *      private variable cFilter:
 *
 *      FUNCTION CreateQry( cQryName )
 *
 *         PRIVATE cFilter := DBFILTER()
 *         SAVE ALL LIKE cFilter TO (cQryName + ".qwy")
 *         RETURN NIL
 *
 *     You can later RESTORE a query file with this user-defined
 *      function, SetFilter():
 *
 *      FUNCTION SetFilter()
 *      PARAMETER cQryName
 *         RESTORE FROM &cQryName..qwy ADDITIVE
 *         SET FILTER TO &cFilter.
 *         RETURN NIL
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      DBRELATION(),DBRSELECT()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBGOBOTTOM()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Move to the last logical record
 *  $SYNTAX$
 *     DBGOBOTTOM() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBGOBOTTOM() always returns NIL.
 *  $DESCRIPTION$
 *     DBGOBOTTOM() moves to last logical record in the current work area.
 *
 *     DBGOBOTTOM() performs the same function as the standard GO BOTTOM
 *   command.  For more information, refer to the GO command.
 *
 *  Notes
 *
 *     Logical records: DBGOBOTTOM() operates on logical records.  If
 *      there is an active index, DBGOBOTTOM() moves to the last record in
 *      indexed order.  If a filter is set, only records which meet the
 *      filter condition are considered.
 *
 *     Controlling order: If more than one index is active in the
 *      work area, the operation is performed using the controlling order as
 *      set by DBSETORDER() or the SET ORDER command.  For more information,
 *      refer to the SET ORDER command.
 *
 *     Network environment: For a shared file on a network, moving to
 *      a different record may cause updates to the current record to become
 *      visible to other processes. 
 *    
 *  $EXAMPLES$
 *     The following example uses DBGOBOTTOM() to position the record
 *      pointer on the last logical record:
 *
 *      cLast := "Winston"
 *      DBUSEAREA( .T., "DBFNTX", "Sales", "Sales", .T. )
 *      DBSETINDEX( "LASTNAME" )
 *      //
 *      Sales->( DBGOBOTTOM() )
 *      IF ( Sales->Last == "Winston" )
 *         IF RLOCK()
 *            Sales->( DBDELETE() )
 *            ? "Record deleted: ", Sales( DELETED() )
 *         ELSE
 *            ? "Unable to lock record..."
 *         ENDIF
 *      END
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     BOF(),EOF(),DBSKIP(),DBSEEK(),DBGOTOP()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBGOBOTTOM( void )
{
   if( pCurrArea )
      SELF_GOBOTTOM( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOBOTTOM" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBGOTO()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Move to the record having the specified record number
 *  $SYNTAX$
 *     DBGOTO(<nRecordNumber>) --> NIL
 *  $ARGUMENTS$
 *     <nRecordNumber> is a numeric value that specifies the record number
 *   of the desired record.
 *     
 *  $RETURNS$
 *     DBGOTO() always returns NIL.
 *  $DESCRIPTION$
 *     DBGOTO() moves to the record whose record number is equal to
 *   <nRecordNumber>.  If no such record exists, the work area is positioned
 *   to LASTREC() + 1 and both EOF() and BOF() return true (.T.).
 *
 *   DBGOTO() performs the same function as the standard GO command.  For
 *   more information, refer to the GO command.
 *
 *   Notes
 *
 *     Logical records: DBGOTO() does not respect logical visibility.
 *      That is, if the specified record exists, it will become the current
 *      record regardless of any index or filter condition.
 *
 *     Network environment: For a shared file on a network, moving to
 *      a different record may cause updates to the current record to become
 *      visible to other processes. 
 *     
 *  $EXAMPLES$
 *
 *     The following example uses DBGOTO() to iteratively process
 *      every fourth record:
 *
 *      DBUSEAREA( .T., "DBFNTX", "Sales", "Sales", .T. )
 *      //
 *      // toggle every fourth record
 *      DO WHILE !EOF()
 *         DBGOTO( RECNO() + 4 )
 *         Sales->Group := "Bear"
 *      ENDDO
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     BOF(),EOF(),DBGOTOP(),DBGOBOTTOM(),DBSEEK(),DBSKIP()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBGOTOP()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Move to the first logical record
 *  $SYNTAX$
 *     DBGOTOP() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBGOTOP() always returns NIL.
 *  $DESCRIPTION$
 *     DBGOTOP() moves to last logical record in the current work area.
 *
 *     DBGOTOP() performs the same function as the standard GO TOP
 *   command.  For more information, refer to the GO command.
 *
 *  Notes
 *
 *     Logical records: DBGOTOP() operates on logical records.  If
 *      there is an active index, DBGOTOP() moves to the last record in
 *      indexed order.  If a filter is set, only records which meet the
 *      filter condition are considered.
 *
 *     Controlling order: If more than one index is active in the
 *      work area, the operation is performed using the controlling order as
 *      set by DBSETORDER() or the SET ORDER command.  For more information,
 *      refer to the SET ORDER command.
 *
 *     Network environment: For a shared file on a network, moving to
 *      a different record may cause updates to the current record to become
 *      visible to other processes. 
 *    
 *  $EXAMPLES$
 *
 *     This example demonstrates the typical use of DBGOTOP():
 *
 *      DBGOTOP()
 *      WHILE ( !EOF() )
 *         ? FIELD->Name
 *         DBSKIP()
 *      END
 *
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     BOF(),EOF(),DBSKIP(),DBSEEK(),DBGOBOTTOM()
 *  $INCLUDE$
 *     
 *  $END$
 */

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

HARBOUR HB___DBPACK( void )
{
   if( pCurrArea )
   {
      /* Additional feature: __dbPack( [<bBlock>, [<nEvery>] )
         Code Block to execute for every record. */
      ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->itmEval = hb_param( 1, IT_BLOCK );
      ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->ulEvery = hb_parnl( 2 );
      if( !( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->ulEvery )
         ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->ulEvery = 1;
      SELF_PACK( ( AREAP ) pCurrArea->pArea );
      ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->itmEval = NULL;
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "__DBPACK" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBRECALL()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Reinstate a record marked for deletion
 *  $SYNTAX$
 *     DBRECALL() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBRECALL() always returns NIL.
 *  $DESCRIPTION$
 *     DBRECALL() causes the current record to be reinstated if it is marked
 *   for deletion.
 *
 *     DBRECALL() performs the same function as the RECALL command.  For more
 *   information, refer to the DELETE and RECALL commands.
 *
 *     Notes
 *
 *     Logical records: Reinstating a deleted record affects the
 *      record's logical visibility if the global _SET_DELETED status is true
 *      (.T.).  For more information, refer to the DBDELETE() function and
 *      the DELETE and RECALL commands.
 *
 *     Network environment: For a shared database on a network,
 *      DBRECALL() requires the current record to be locked. 
 *     
 *  $EXAMPLES$
 *
 *     The following example recalls a record if it is deleted and
 *      attempts to lock the record if successful:
 *
 *      cLast := "Winston"
 *      DBUSEAREA( .T., "DBFNTX", "Sales", "Sales", .T. )
 *      DBSETINDEX( "LASTNAME" )
 *      //
 *      IF ( Sales->(DBSEEK(cLast)) )
 *         IF Sales->( DELETED() )
 *
 *            IF Sales( RLOCK() )
 *               Sales( DBRECALL() )
 *               ? "Record recalled"
 *            ELSE
 *               ? "Unable to lock record..."
 *            ENDIF
 *         ENDIF
 *      ELSE
 *         ? "Not found"
 *      ENDIF
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBDELETE()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBRECALL( void )
{
   if( pCurrArea )
      SELF_RECALL( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRECALL" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBRLOCK()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Lock the record at the current or specified identity
 *  $SYNTAX$
 *     DBRLOCK([<xIdentity>]) --> lSuccess
 *  $ARGUMENTS$
 *     <xIdentity> is a unique value guaranteed by the structure of the
 *   data file to reference a specific item in a data source (database).  In
 *   a (.dbf) <xIdentity> is the record number.  In other data formats,
 *   <xIdentity> is the unique primary key value.
 *     
 *  $RETURNS$
 *
 *     DBRLOCK() returns lSuccess, a logical data type that is true (.T.) if
 *   successful, false (.F.) if unsuccessful.
 *
 *  $DESCRIPTION$
 *     DBRLOCK() is a database function that locks the record identified by the
 *   value <xIdentity>.  In Xbase, <xIdentity> is the record number.
 *
 *     If you do not specify <xIdentity>, all record locks are released and the
 *   current record is locked.  If you specify <xIdentity>, DBRLOCK()
 *   attempts to lock it and, if successful, adds it to the locked record
 *   list.
 *     
 *  $EXAMPLES$
 *     This example shows two different methods for locking multiple
 *      records:
 *
 *      FUNCTION dbRLockRange( nLo, nHi )
 *
 *         LOCAL nRec
 *         FOR nRec := nLo TO nHi
 *            IF ! DBRLOCK( nRec )
 *               DBRUNLOCK()         // Failed - unlock everything
 *            ENDIF
 *         NEXT
 *         RETURN DBRLOCKLIST()      // Return array of actual locks
 *
 *      FUNCTION dbRLockArray( aList )
 *
 *         LOCAL nElement, nLen, lRet
 *         lRet := .T.
 *         nLen := LEN( aList )
 *         FOR nElement := 1 TO nLen
 *            IF ! DBRLOCK( aList[ nElement ] )
 *               DBRUNLOCK()         // Failed - unlock everything
 *               lRet := .F.
 *            ENDIF
 *         NEXT
 *         RETURN DBRLOCKLIST()
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBUNLOCK(),DBUNLOCKALL(),FLOCK(),RLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBRLOCKLIST()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return an array of the current Lock List
 *  $SYNTAX$
 *     DBRLOCKLIST() --> aRecordLocks
 *  $ARGUMENTS$
 *
 *  $RETURNS$
 *   Returns an array of the locked records in the current or aliased work
 *   area.    
 *  $DESCRIPTION$
 *     DBRLOCKLIST() is a database function that returns a one-dimensional
 *   array that contains the identities of record locks active in the
 *   selected work area.
 *     
 *  $EXAMPLES$
 *   PROCEDURE PrintCurLocks()
 *
 *      LOCAL aList
 *      LOCAL nSize
 *      LOCAL nCount
 *
 *      aList := DBRLOCKLIST()
 *      nSize := LEN( aList )
 *
 *      ? "Currently locked records: "
 *      FOR nCount := 1 TO nSize
 *         ?? aList[ nCount ]
 *         ?? SPACE( 1 )
 *      NEXT
 *      ?
 *
 *      RETURN
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     RLOCK(),DBRLOCK(),DBRUNLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBRUNLOCK()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Release all or specified record locks
 *  $SYNTAX$
 *     DBRUNLOCK([<xIdentity>]) --> NIL
 *  $ARGUMENTS$
 *     <xIdentity> is a unique value guaranteed by the structure of the
 *   data file to reference a specific item in a data source (database).  In
 *   a (.dbf) <xIdentity> is the record number.  In other data formats,
 *   <xIdentity> is the unique primary key value.   
 *  $RETURNS$
 *     DBRUNLOCK() always returns NIL.
 *  $DESCRIPTION$
 *     DBRUNLOCK() is a database function that releases the lock on <xIdentity>
 *   and removes it from the Lock List.  If <xIdentity> is not specified, all
 *   record locks are released.
 *     
 *  $EXAMPLES$
 *     PROCEDURE dbRUnlockRange( nLo, nHi )
 *
 *      LOCAL nCounter
 *
 *      // Unlock the records in the range from nLo to nHi
 *      FOR nCounter := nLo TO nHi
 *         DBRUNLOCK( nCounter )
 *      NEXT
 *
 *      RETURN
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      RLOCK(),DBRLOCK(),DBRLOCKLIST()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBRUNLOCK( void )
{
   if( pCurrArea )
      SELF_UNLOCK( ( AREAP ) pCurrArea->pArea, hb_parnl( 1 ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRUNLOCK" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBSEEK()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Move to the record having the specified key value
 *  $SYNTAX$
 *     DBSEEK(<expKey>, [<lSoftSeek>],[<lFindLast>]) --> lFound
 *  $ARGUMENTS$
 *     <expKey> is a value of any type that specifies the key value
 *   associated with the desired record.
 *
 *     <lSoftSeek> is an optional logical value that specifies whether a
 *   soft seek is to be performed.  This determines how the work area is
 *   positioned if the specified key value is not found (see below).  If
 *   <lSoftSeek> is omitted, the current global _SET_SOFTSEEK setting is
 *   used.
 *     <lFindLast> is an optional logical value that set the current
 *   record position to the last record if successful
 *  $RETURNS$
 *     DBSEEK() returns true (.T.) if the specified key value was found;
 *   otherwise, it returns false (.F.).
 *  $DESCRIPTION$
 *     DBSEEK() moves to the first logical record whose key value is equal to
 *   <expKey>.  If such a record is found, it becomes the current record and
 *   DBSEEK() returns true (.T.).  Otherwise, DBSEEK() returns false (.F.)
 *   and the positioning of the work area is as follows: for a normal (not
 *   soft) seek, the work area is positioned to LASTREC() + 1 and EOF()
 *   returns true (.T.); for a soft seek, the work area is positioned to the
 *   first record whose key value is greater than the specified key value.
 *   If no such record exists, the work area is positioned to LASTREC() + 1
 *   and EOF() returns true (.T.).
 *
 *     For a work area with no active indexes, DBSEEK() has no effect.
 *
 *     DBSEEK() performs the same function as the standard SEEK command.  For
 *   more information, refer to the SEEK command.
 *
 *    Notes
 *
 *     Logical records: DBSEEK() operates on logical records.
 *      Records are considered in indexed order.  If a filter is set, only
 *      records which meet the filter condition are considered.
 *
 *     Controlling order: If the work area has more than one active
 *      index, the operation is performed using the controlling order as set
 *      by DBSETORDER() or the SET ORDER command.  For more information,
 *      refer to the SET ORDER command.
 *
 *     Network environment: For a shared file on a network, moving to
 *      a different record may cause updates to the current record to become
 *      visible to other processes.
 *
 *     
 *  $EXAMPLES$
 *     In this example, DBSEEK() moves the pointer to the record in
 *      the database, Employee, in which the value in FIELD “cName” matches
 *      the entered value of cName:
 *
 *      ACCEPT "Employee name: " TO cName
 *      IF ( Employee->(DBSEEK(cName)) )
 *         Employee->(ViewRecord())
 *      ELSE
 *         ? "Not found"
 *      END
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *     DBSEEK() is  Compatible with CA-Clipper 5.3
 *  $SEEALSO$
 *     DBGOBOTTOM(),DBGOTOP(),DBSKIP(),EOF(),BOF(),FOUND()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBSEEK( void )
{
   PHB_ITEM pKey;
   BOOL bSoftSeek, bFindLast;

   if( pCurrArea )
   {
      if( !ISNIL( 1 ) )
      {
         pKey = hb_param( 1, IT_ANY );
         bSoftSeek = ISLOG( 2 ) ? hb_parl( 2 ) : hb_set.HB_SET_SOFTSEEK;
         bFindLast = ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;
         if( SELF_SEEK( ( AREAP ) pCurrArea->pArea, bSoftSeek, pKey, bFindLast ) == SUCCESS )
         {
            hb_retl( ( ( AREAP ) pCurrArea->pArea )->fFound );
            return;
         }
      }
      else
         hb_errRT_DBCMD( EG_ARG, 1001, NULL, "DBSEEK" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSEEK" );
   hb_retl( FALSE );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBSELECTAREA()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Change the current work area
 *  $SYNTAX$
 *     DBSELECTAREA(<nArea> | <cAlias>) --> NIL
 *  $ARGUMENTS$
 *     <nArea> is a numeric value between zero and 250, inclusive, that
 *   specifies the work area being selected.
 *
 *     <cAlias> is a character value that specifies the alias of a
 *   currently occupied work area being selected.
 *     
 *  $RETURNS$
 *     DBSELECTAREA() always returns NIL.
 *  $DESCRIPTION$
 *
 *     DBSELECTAREA() causes the specified work area to become the current work
 *   area.  All subsequent database operations will apply to this work area
 *   unless another work area is explicitly specified for an operation.
 *   DBSELECTAREA() performs the same function as the standard SELECT
 *   command.  For more information, refer to the SELECT command.
 *
 *   Notes
 *
 *     Selecting zero: Selecting work area zero causes the lowest
 *      numbered unoccupied work area to become the current work area.
 *
 *     Aliased expressions: The alias operator (->) can temporarily
 *      select a work area while an expression is evaluated and automatically
 *      restore the previously selected work area afterward.  For more
 *      information, refer to the alias operator (->).
 *
 *  $EXAMPLES$
 *     The following example selects a work area via the alias name:
 *
 *      cLast := "Winston"
 *      DBUSEAREA( .T., "DBFNTX", "Sales", "Sales", .T. )
 *      DBSETINDEX( "SALEFNAM" )
 *      DBSETINDEX( "SALELNAM" )
 *      //
 *      DBUSEAREA( .T., "DBFNTX", "Colls", "Colls", .T. )
 *      DBSETINDEX( "COLLFNAM" )
 *      DBSETINDEX( "COLLLNAM" )
 *      //
 *      DBSELECTAREA( "Sales" )      // select "Sales" work area
 *      //
 *      IF ( Sales->(DBSEEK(cLast)) )
 *         IF Sales->( DELETED() ) .AND. Sales->( RLOCK() )
 *            Sales->( DBRECALL() )
 *            ? "Deleted record has been recalled."
 *         ENDIF
 *      ELSE
 *         ? "Not found"
 *      ENDIF
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBUSEAREA(),SELECT()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBSETDRIVER()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the default database driver and optionally set a new driver
 *  $SYNTAX$
 *     DBSETDRIVER([<cDriver>]) --> cCurrentDriver
 *  $ARGUMENTS$
 *     <cDriver> is an optional character value that specifies the name of
 *   the database driver that should be used to activate and manage new work
 *   areas when no driver is explicitly specified.   
 *  $RETURNS$
 *     DBSETDRIVER() returns the name of the current default driver.
 *  $DESCRIPTION$
 *     DBSETDRIVER() sets the database driver to be used when activating new
 *   work areas without specifying a driver.  If the specified driver is not
 *   available to the application, the call has no effect.  DBSETDRIVER()
 *   returns the name of the current default driver, if any.
 *     
 *  $EXAMPLES$
 *     This example makes the "DBFNDX" driver the default driver.  If
 *      the driver is unavailable, a message is issued:
 *      DBSETDRIVER("DBFNDX")
 *      IF ( DBSETDRIVER() <> "DBFNDX" )
 *    *    ? "DBFNDX driver not available"
 *      ENDIF
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBUSEAREA()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBSKIP()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Move relative to the current record
 *  $SYNTAX$
 *     DBSKIP([<nRecords>]) --> NIL     
 *  $ARGUMENTS$
 *     <nRecords> is the number of logical records to move, relative to the
 *   current record.  A positive value means to skip forward, and a negative
 *   value means to skip backward.  If <nRecords> is omitted, a value of 1 is
 *   assumed.     
 *  $RETURNS$
 *     DBSKIP() always returns NIL.
 *  $DESCRIPTION$
 *     DBSKIP() moves either forward or backward relative to the current
 *   record.  Attempting to skip forward beyond the last record positions the
 *   work area to LASTREC() + 1 and EOF() returns true (.T.).  Attempting to
 *   skip backward beyond the first record positions the work area to the
 *   first record and BOF() returns true (.T.).
 *     DBSKIP() performs the same function as the standard SKIP command.  For
 *   more information, refer to the SKIP command.
 *
 *    Notes
 *
 *     Logical records: DBSKIP() operates on logical records.  If
 *      there is an active index, records are considered in indexed order.
 *      If a filter is set, only records which meet the filter condition are
 *      considered.
 *
 *     Controlling order: If the work area has more than one active
 *      index, the skip operation is performed using the controlling order as
 *      set by DBSETORDER() or the SET ORDER command.  For more information,
 *      refer to the SET ORDER command.
 *
 *     Network environment: For a shared file on a network, moving to
 *      a different record may cause updates to the current record to become
 *      visible to other processes.
 *
 *  $EXAMPLES$
 *     This example demonstrates a typical use of the DBSKIP()
 *      function:
 *
 *      DBGOTOP()
 *      DO WHILE ( !EOF() )
 *       ? FIELD->Name
 *        DBSKIP()
 *      ENDDO
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *    BOF(),DBGOBOTTOM(),DBGOTOP(),DBSEEK(),EOF()    
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBSETFILTER()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Set a filter condition
 *  $SYNTAX$
 *     DBSETFILTER(<bCondition>, [<cCondition>]) --> NIL
 *  $ARGUMENTS$
 *     <bCondition> is a code block that expresses the filter condition in
 *   executable form.

 *     <cCondition> is an optional character value that expresses the
 *   filter condition in textual form.  If <cCondition> is omitted, the
 *   DBSETFILTER() function will return an empty string for the work area.
 *     
 *  $RETURNS$
 *     DBSETFILTER() always returns NIL.
 *  $DESCRIPTION$
 *     DBSETFILTER() sets a logical filter condition for the current work area.
 *   When a filter is set, records which do not meet the filter condition are
 *   not logically visible.  That is, database operations which act on
 *   logical records will not consider these records.

 *     The filter expression supplied to DBSETFILTER() evaluates to true (.T.)
 *   if the current record meets the filter condition; otherwise, it should
 *   evaluate to false (.F.).

 *     The filter expression may be a code block (<bCondition>) or both a code
 *   block and equivalent text (<cCondition>).  If both versions are
 *   supplied, they must express the same condition.  If the text version is
 *   omitted, DBFILTER() will return an empty string for the work area.

 *     DBSETFILTER() performs the same function as the standard SET FILTER
 *   command.  For more information, refer to the SET FILTER command.
 *
 *     Notes
 *
 *     Logical records: DBSETFILTER() affects the logical visibility
 *      of records (see above).
 *
 *     Side effects: Setting a filter condition is only guaranteed to
 *      restrict visibility of certain records as described above.  The
 *      filter expression is not necessarily evaluated at any particular
 *      time, by any particular means, or on any particular record or series
 *      of records.  If the filter expression relies on information external
 *      to the database file or work area, the effect is unpredictable.  If
 *      the filter expression changes the state of the work area (e.g., by
 *      moving to a different record or changing the contents of a record),
 *      the effect is unpredictable.
 *
 *     Evaluation context: When the filter expression is evaluated,
 *      the associated work area is automatically selected as the current
 *      work area before the evaluation; the previously selected work area is
 *      automatically restored afterward.

 *  $EXAMPLES$
 *     This example limits data access to records in which the Age
 *      field value is less than 40:
 *
 *      USE Employee NEW
 *      DBSETFILTER( {|| Age < 40}, "Age < 40" )
 *      DBGOTOP()
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBFILTER(),DBCLEARFILTER()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBSTRUCT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Create an array containing the structure of a database file
 *  $SYNTAX$
 *     DBSTRUCT() --> aStruct
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBSTRUCT() returns the structure of the current database file in an
 *   array whose length is equal to the number of fields in the database
 *   file.  Each element of the array is a subarray containing information
 *   for one field.  The subarrays have the following format:

 *     DBSTRUCT() Return Array
 *     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 *     Position *   Metasymbol *   Dbstruct.ch
 *     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 *     1 *    *     cName *    *   DBS_NAME
 *     2 *    *     cType *    *   DBS_TYPE
 *     3 *    *     nLength *      DBS_LEN
 *     4 *    *     nDecimals *    DBS_DEC
 *     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

 *     If there is no database file in USE in the current work area, DBSTRUCT()
 *   returns an empty array ({}).
 *     
 *  $DESCRIPTION$
 *     DBSTRUCT() is a database function that operates like COPY STRUCTURE
 *   EXTENDED by creating an array of structure information rather than a
 *   database file of structure information.  There is another function,
 *   DBCREATE(), that can create a database file from the structure array.

 *    By default, DBSTRUCT() operates on the currently selected work area.  It
 *   will operate on an unselected work area if you specify it as part of an
 *   aliased expression as shown below.

 *     Note, a header file, Dbstruct.ch, located in \HARBOUR\INCLUDE contains
 *   a series of manifest constants for each field attribute.
 *     
 *  $EXAMPLES$
 *     This example opens two database files then creates an array
 *      containing the database structure using DBSTRUCT() within an aliased
 *      expression.  The field names are then listed using AEVAL():

 *      #include "Dbstruct.ch"
 *      //
 *      LOCAL aStruct
 *      USE Customer NEW
 *      USE Invoices NEW
 *      //
 *      aStruct := Customer->(DBSTRUCT())
 *      AEVAL( aStruct, {|aField| QOUT(aField[DBS_NAME])} )
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     AFIELDS()
 *  $INCLUDE$
 *     DbStruct.ch
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBUNLOCK()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Release all locks for the current work area
 *  $SYNTAX$
 *     DBUNLOCK() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBUNLOCK() always returns NIL.
 *  $DESCRIPTION$
 *     DBUNLOCK() releases any record or file locks obtained by the current
 *   process for the current work area.  DBUNLOCK() is only meaningful on a
 *   shared database in a network environment.

 *     DBUNLOCK() performs the same function as the standard UNLOCK command.
 *   For more information, refer to the UNLOCK command.

 *   Notes

 *     Network environment: Releasing locks may cause updates to the
 *      database to become visible to other processes.  
 *     
 *  $EXAMPLES$
 *     The following example illustrates a basic use of the
 *      DBUNLOCK() function:

 *      cLast := "Winston"
 *      USE Sales SHARED NEW VIA "DBFNTX"
 *      DBSETINDEX( "LASTNAME" )
 *      //
 *      IF ( Sales->(DBSEEK(cLast)) )
 *         IF Sales->( RLOCK() )
 *            Sales->( DBDELETE() )
 *            ? "Record deleted: ", Sales( DELETED() )
 *            Sales->( DBUNLOCK() )
 *         ELSE
 *            ? "Unable to lock record..."
 *         ENDIF
 *      ELSE
 *         ? "Not found"
 *      ENDIF
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      DBUNLOCKALL(),FLOCK(),RLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBUNLOCK( void )
{
   if( pCurrArea )
      SELF_RAWLOCK( ( AREAP ) pCurrArea->pArea, FILE_UNLOCK, 0 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBUNLOCK" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DBUNLOCKALL()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Release all locks for all work areas
 *  $SYNTAX$
 *     DBUNLOCKALL() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DBUNLOCKALL() always returns NIL.
 *  $DESCRIPTION$
 *     DBUNLOCKALL() releases any record or file locks obtained by the current
 *   process for any work area.  DBUNLOCKALL() is only meaningful on a shared
 *   database in a network environment.  It is equivalent to calling
 *   DBUNLOCK() on every occupied work area.

 *     DBUNLOCKALL() performs the same function as the UNLOCK ALL command.  For
 *   more information, refer to the UNLOCK ALL command.
 *     
 *  $EXAMPLES$
 *     The following example marks a record for deletion if an
 *      RLOCK() attempt is successful, then clears all locks in all work
 *      areas:

 *      cLast := "Winston"
 *      USE Sales SHARED NEW VIA "DBFNTX"
 *      DBSETINDEX( "SALEFNAM" )
 *      DBSETINDEX( "SALELNAM" )
 *      //
 *      USE Colls SHARED NEW VIA "DBFNTX"
 *      DBSETINDEX( "COLLFNAM" )
 *      DBSETINDEX( "COLLLNAM" )
 *      //
 *      DBSELECTAREA( "Sales" ) *   // select "Sales" work area
 *      //
 *      IF ( Colls->(DBSEEK(cLast)) )
 *         IF Colls->( DELETED() )
 *            ? "Record deleted: ", Colls->( DELETED() )
 *            IF Colls->( RLOCK() )
 *               Colls->( DBRECALL() )
 *               ? "Record recalled..."
 *            ENDIF
 *         ENDIF
 *      ELSE
 *         ? "Not found"
 *         DBUNLOCKALL()            // remove all locks in
 *      ENDIF                       // all work areas
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBUNLOCK(),FLOCK(),RLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     DBUSEAREA()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Use a database file in a work area
 *  $SYNTAX$
 *     DBUSEAREA( [<lNewArea>], [<cDriver>], <cName>, [<xcAlias>],
 *      [<lShared>], [<lReadonly>]) --> NIL     
 *  $ARGUMENTS$
 *     <lNewArea> is an optional logical value.  A value of true (.T.)
 *   selects the lowest numbered unoccupied work area as the current work
 *   area before the use operation.  If <lNewArea> is false (.F.) or omitted,
 *   the current work area is used; if the work area is occupied, it is
 *   closed first.

 *     <cDriver> is an optional character value.  If present, it specifies
 *   the name of the database driver which will service the work area.  If
 *   <cDriver> is omitted, the current default driver is used (see note
 *   below).

 *     <cName> specifies the name of the database (.dbf) file to be opened.

 *     <xcAlias> is an optional character value.  If present, it specifies
 *   the alias to be associated with the work area.  The alias must
 *   constitute a valid HARBOUR identifier.  A valid <xcAlias> may be any
 *   legal identifier (i.e., it must begin with an alphabetic character and
 *   may contain numeric or alphabetic characters and the underscore).
 *   Within a single application, HARBOUR will not accept duplicate
 *   aliases.  If <xcAlias> is omitted, a default alias is constructed from
 *     <cName>.

 *     <lShared> is an optional logical value.  If present, it specifies
 *   whether the database (.dbf) file should be accessible to other processes
 *   on a network.  A value of true (.T.) specifies that other processes
 *   should be allowed access; a value of false (.F.) specifies that the
 *   current process is to have exclusive access.  If <lShared> is omitted,
 *   the current global _SET_EXCLUSIVE setting determines whether shared
 *   access is allowed.

 *     <lReadonly> is an optional logical value that specifies whether
 *   updates to the work area are prohibited.  A value of true (.T.)
 *   prohibits updates; a value of false (.F.) permits updates.  A value of
 *   true (.T.) also permits read-only access to the specified database
 *   (.dbf) file.  If <lReadonly> is omitted, the default value is false
 *   (.F.).     
 *  $RETURNS$
 *     DBUSEAREA() always returns NIL.
 *  $DESCRIPTION$
 *     DBUSEAREA() associates the specified database (.dbf) file with the
 *   current work area.

 *     DBUSEAREA() performs the same function as the standard USE command.  For
 *   more information, refer to the USE command.

 *     Notes

 *     Current driver: If no driver is specified in the call to
 *      DBUSEAREA() the default driver is used.  If more than one driver is
 *      available to the application, the default driver is the driver
 *      specified in the most recent call to DBSETDRIVER().  If DBSETDRIVER()
 *      has not been called, the name of the default driver is undetermined.
 *     
 *  $EXAMPLES$
 *     This example is a typical use of the DBUSEAREA() function:

 *      DBUSEAREA(.T., "DBFNDX", "Employees")
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 * DBCLOSEAREA(),DBSETDRIVER(),SELECT(),SET()
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DBUSEAREA( void )
{
   char * szDriver, * szFileName;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;
   USHORT uiSize, uiRddID, uiLen;
   ULONG ulLen;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pFileExt;
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

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
   ulLen = strlen( szAlias );
   if( ulLen == 0 )
      strncpy( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );
   else if( ulLen == 1 )
   {
      /* Alias with a single letter. Only are valid 'L' and > 'M' */
      if( toupper( szAlias[ 0 ] ) < 'N' && toupper( szAlias[ 0 ] ) != 'L' )
      {
         hb_xfree( pFileName );
         hb_errRT_DBCMD( EG_DUPALIAS, 1011, NULL, "DBUSEAREA" );
         return;
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
            else
               pWorkAreas = pCurrArea;
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

   ( ( AREAP ) pCurrArea->pArea )->lpDataInfo->szFileName = szFileName;
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
   SELF_RECCOUNT( ( AREAP ) pCurrArea->pArea, &ulLen );
   ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->ulRecCount = ulLen;
}
/*  $DOC$
 *  $FUNCNAME$
 *     __DBZAP()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Remove all records from the current database file
 *  $SYNTAX$
 *     __DbZap()  -> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     __DbZap()   will always return nil
 *  $DESCRIPTION$
 *     __DbZap*( is a database command that permanently removes all records from
 *   files open in the current work area.  This includes the current database
 *   file, index files, and associated memo file.  Disk space previously
 *   occupied by the ZAPped files is released to the operating system.
 *   __DbZap() performs the same operation as DELETE ALL followed by PACK but is
 *   almost  instantaneous.
 *
 *   To ZAP in a network environment, the current database file must be USEd
 *   EXCLUSIVEly. 
 *     
 *  $EXAMPLES$
 *     This example demonstrates a typical ZAP operation in a network
 *      environment:
 *
 *      USE Sales EXCLUSIVE NEW
 *      IF !NETERR()
 *         SET INDEX TO Sales, Branch, Salesman
 *         __dbZAP()
 *         CLOSE Sales
 *      ELSE
 *         ? "Zap operation failed"
 *         BREAK
 *      ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */
/*  $DOC$
 *  $COMMANDNAME$
 *     ZAP
 *  $CATEGORY$
 *     Command
 *  $ONELINER$
 *     Remove all records from the current database file
 *  $SYNTAX$
 *     ZAP
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *    
 *  $DESCRIPTION$
 *     ZAP is a database command that permanently removes all records from
 *   files open in the current work area.  This includes the current database
 *   file, index files, and associated memo file.  Disk space previously
 *   occupied by the ZAPped files is released to the operating system.  ZAP
 *   performs the same operation as DELETE ALL followed by PACK but is almost
 *   instantaneous.
 *
 *   To ZAP in a network environment, the current database file must be USEd
 *   EXCLUSIVEly. 
 *     
 *  $EXAMPLES$
 *     This example demonstrates a typical ZAP operation in a network
 *      environment:
 *
 *      USE Sales EXCLUSIVE NEW
 *      IF !NETERR()
 *         SET INDEX TO Sales, Branch, Salesman
 *         ZAP
 *         CLOSE Sales
 *      ELSE
 *         ? "Zap operation failed"
 *         BREAK
 *      ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB___DBZAP( void )
{
   if( pCurrArea )
      SELF_ZAP( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "__DBZAP" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     DELETED()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the deleted status of the current record
 *  $SYNTAX$
 *     DELETED() --> lDeleted
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     DELETED() returns true (.T.) if the current record is marked for
 *   deletion; otherwise, it returns false (.F.).  If there is no database
 *   file in USE in the current work area, DELETED() returns false (.F.).
     
 *  $DESCRIPTION$
 *     DELETED() is a database function that determines if the current record
 *   in the active work area is marked for deletion.  Since each work area
 *   with an open database file can have a current record, each work area has
 *   its own DELETED() value.

 *     By default, DELETED() operates on the currently selected work area.  It
 *   will operate on an unselected work area if you specify it as part of an
 *   aliased expression (see example below).

 *     In applications, DELETED() is generally used to query the deleted status
 *   as a part of record processing conditions, or to display the deleted
 *   status as a part of screens and reports.
     
 *  $EXAMPLES$
 *     This example uses DELETED() in the current and in an
 *      unselected work area:

 *      USE Customer NEW

 *      USE Sales NEW
 *      ? DELETED() // Result: .F.
 *      DELETE
 *      ? DELETED()  // Result: .T.
 *      ? Customer->(DELETED())       // Result: .F.

 *     This example uses DELETED() to display a record's deleted
 *      status in screens and reports:

 *      @ 1, 65 SAY IF(DELETED(), "Inactive", "Active")
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_DELETED( void )
{
   BOOL bDeleted = FALSE;

   if( pCurrArea )
      SELF_DELETED( ( AREAP ) pCurrArea->pArea, &bDeleted );
   hb_retl( bDeleted );
}

/*  $DOC$
 *  $FUNCNAME$
 *     EOF()
 *  $CATEGORY$
 *      DATA BASE
 *  $ONELINER$
 *      Determine when end of file is encountered
 *  $SYNTAX$
 *      EOF() --> <lEnd>
 *  $ARGUMENTS$
 * 
 *  $RETURNS$
 * 
 *   EOF() returns true (.T.) when an attempt is made to move the record
 *   pointer beyond the last logical record in a database file; otherwise, it
 *   returns false (.F.).  If there is no database file open in the current
 *   work area, EOF() returns false (.F.).  If the current database file
 *   contains no records, EOF() returns true (.T.).

 *  $DESCRIPTION$

 *     EOF() is a database function used to test for an end of file boundary
 *   condition when the record pointer is moving forward through a database
 *   file.  Any command that can move the record pointer can set EOF().

 *     The most typical application is as a part of the <lCondition> argument
 *   of a DO WHILE construct that sequentially processes records in a
 *   database file.  Here <lCondition> would include a test for .NOT. EOF(),
 *   forcing the DO WHILE loop to terminate when EOF() returns true (.T.).

 *     EOF() and FOUND() are often used interchangeably to test whether a SEEK,
 *   FIND, or LOCATE command failed.  With these commands, however, FOUND()
 *   is preferred.

 *     When EOF() returns true (.T.), the record pointer is positioned at
 *   LASTREC() + 1 regardless of whether there is an active SET FILTER or SET
 *   DELETED is ON.  Further attempts to move the record pointer forward
 *   return the same result without error.  Once EOF() is set to true (.T.),
 *   it retains its value until there is another attempt to move the record
 *   pointer.

 *     By default, EOF() operates on the currently selected work area.  It can
 *   be made to operate on an unselected work area by specifying it within an
 *   aliased expression (see example below).

 *  $EXAMPLES$
 *     This example demonstrates EOF() by deliberately moving the
 *      record pointer beyond the last record:

 *      USE Sales
 *      GO BOTTOM
 *      ? EOF()            // Result: .F.
 *      SKIP
 *      ? EOF()            // Result: .T.

 *     This example uses aliased expressions to query the value of
 *      EOF() in unselected work areas:

 *      USE Sales NEW
 *      USE Customer NEW
 *      ? Sales->(EOF())
 *      ? Customer->(EOF())

 *     This example illustrates how EOF() can be used as part of a
 *      condition for sequential database file operations:

 *      USE Sales INDEX CustNum NEW
 *      DO WHILE !EOF()
 *         nOldCust := Sales->CustNum
 *         nTotalAmount := 0
 *         DO WHILE nOldCust = Sales->CustNum .AND. (!EOF())
 *            ? Sales->CustNum, Sales->Description, ;
 *                  Sales->SaleAmount
 *            nTotalAmount += Sales->SaleAmount
 *            SKIP
 *         ENDDO
 *         ? "Total amount: ", nTotalAmount
 *      ENDDO
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      EOF() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *    BOF(),FOUND(),LASTREC()
 *  $END$
 */

HARBOUR HB_EOF( void )
{
   BOOL bEof = TRUE;

   if( pCurrArea )
      SELF_EOF( ( AREAP ) pCurrArea->pArea, &bEof );
   hb_retl( bEof );
}
/*  $DOC$
 *  $FUNCNAME$
 *     FCOUNT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the number of fields in the current (.dbf) file
 *  $SYNTAX$
 *     FCOUNT() --> nFields
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     FCOUNT() returns the number of fields in the database file in the
 *   current work area as an integer numeric value.  If there is no database
 *   file open, FCOUNT() returns zero.   
 *  $DESCRIPTION$
 *     FCOUNT() is a database function.  It is useful in applications
 *   containing data-independent programs that can operate on any database
 *   file.  These include generalized import/export and reporting programs.
 *   Typically, you use FCOUNT() to establish the upper limit of a FOR...NEXT
 *   or DO WHILE loop that processes a single field at a time.

 *     By default, FCOUNT() operates on the currently selected work area.     
 *  $EXAMPLES$
 *     This example illustrates FCOUNT(), returning the number of
 *      fields in the current and an unselected work area:

 *      USE Sales NEW
 *      USE Customer NEW
 *      ? FCOUNT()                     // Result: 5
 *      ? Sales->(FCOUNT())            // Result: 8

 *     This example uses FCOUNT() to DECLARE an array with field
 *      information:

 *      LOCAL aFields := ARRAY(FCOUNT())
 *      AFIELDS(aFields)

 *     This example uses FCOUNT() as the upper boundary of a FOR loop
 *      that processes the list of current work area fields:

 *      LOCAL nField
 *      USE Sales NEW
 *      FOR nField := 1 TO FCOUNT()
 *         ? FIELD(nField)
 *      NEXT
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      FIELDNAME(),TYPE()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_FCOUNT( void )
{
   USHORT uiFields = 0;

   if( pCurrArea )
      SELF_FIELDCOUNT( ( AREAP ) pCurrArea->pArea, &uiFields );
   hb_retni( uiFields );
}
/*  $DOC$
 *  $FUNCNAME$
 *     FIELDGET()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Retrieve the value of a field variable
 *  $SYNTAX$
 *     FIELDGET(<nField>) --> ValueField
 *  $ARGUMENTS$
 *      <nField> is the ordinal position of the field in the record
 *   structure for the current work area.
 *  $RETURNS$
 *     FIELDGET() returns the value of the specified field.  If <nField> does
 *   not correspond to the position of any field in the current database
 *   file, FIELDGET() returns NIL.     
 *  $DESCRIPTION$
 *     FIELDGET() is a database function that retrieves the value of a field
 *   using its position within the database file structure rather than its
 *   field name.  Within generic database service functions this allows,
 *   among other things, the retrieval of field values without use of the
 *   macro operator.     
 *  $EXAMPLES$
 *     This example compares FIELDGET() to functionally equivalent
 *      code that uses the macro operator to retrieve the value of a field:

 *      LOCAL nField := 1, FName, FVal
 *      USE Customer NEW
 *      //
 *      // Using macro operator
 *      FName := FIELD( nField )        // Get field name
 *      FVal := &FName        // Get field value
 *      // Using FIELDGET()
 *      FVal := FIELDGET( nField )      // Get field value
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     FIELDPUT()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     FIELDNAME()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return a field name from the current (.dbf) file
 *  $SYNTAX$
 *     FIELDNAME/FIELD(<nPosition>) --> cFieldName
 *  $ARGUMENTS$
 *     <nPosition> is the position of a field in the database file
 *   structure.
 *  $RETURNS$
 *     FIELDNAME() returns the name of the specified field as a character
 *   string.  If <nPosition> does not correspond to an existing field in the
 *   current database file or if no database file is open in the current work
 *   area, FIELDNAME() returns a null string ("").     
 *  $DESCRIPTION$
 *     FIELDNAME() is a database function that returns a field name using an
 *   index to the position of the field name in the database structure.  Use
 *   it in data-independent applications where the field name is unknown.  If
 *   information for more than one field is required, use AFIELDS() to create
 *   an array of field information or COPY STRUCTURE EXTENDED to create a
 *   database of field information.

 *     If you need additional database file structure information, use TYPE()
 *   and LEN().  To obtain the number of decimal places for a numeric field,
 *   use the following expression:

 *   LEN(SUBSTR(STR(<idField>), RAT(".", ;
 *          STR(<idField>)) + 1))

 *     By default, FIELDNAME() operates on the currently selected work area as
 *   shown in the example below.
 *     
 *  $EXAMPLES$
 *     These examples illustrate FIELDNAME() used with several other
 *      functions:

 *      USE Sales
 *      ? FIELDNAME(1)       // Result: BRANCH
 *      ? FCOUNT()                // Result: 5
 *      ? LEN(FIELDNAME(0))        // Result: 0
 *      ? LEN(FIELDNAME(40))       // Result: 0

 *     This example uses FIELDNAME() to list the name and type of
 *      each field in Customer.dbf:

 *      USE Customer NEW
 *      FOR nField := 1 TO FCOUNT()
 *         ? PADR(FIELDNAME(nField), 10),;
 *          VALTYPE(&(FIELDNAME(nField)))
 *      NEXT

 *     This example accesses fields in unselected work areas using
 *      aliased expressions:

 *      USE Sales NEW
 *      USE Customer NEW
 *      USE Invoices NEW
 *      //
 *      ? Sales->(FIELDNAME(1))            // Result: SALENUM
 *      ? Customer->(FIELDNAME(1))         // Result: CUSTNUM
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *    DBSTRUCT()  FCOUNT()  LEN()  misc.ngo:VALTYPE()
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     FIELDPOS()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the position of a field in a work area
 *  $SYNTAX$
 *     FIELDPOS(<cFieldName>) --> nFieldPos
 *  $ARGUMENTS$
 *     <cFieldName> is the name of a field in the current or specified work
 *   area.
 *  $RETURNS$
 *     FIELDPOS() returns the position of the specified field within the list
 *   of fields associated with the current or specified work area.  If the
 *   current work area has no field with the specified name, FIELDPOS()
 *   returns zero.
 *  $DESCRIPTION$
 *     FIELDPOS() is a database function that is the inverse of the FIELDNAME()
 *   function.  FIELDPOS() is most often used with the FIELDPUT() and
 *   FIELDGET() functions.

 *     FIELDPOS() return the names of fields in any unselected work area by
 *   referring to the function using an aliased expression.  See the example
 *   below.
 *  $EXAMPLES$
 *     This example demonstrates a typical specification of the
 *      FIELDPOS() function:

 *      USE Customer NEW
 *      ? FIELDPOS("Name") *    *    *    *    // Result: 1
 *      ? FIELDGET(FIELDPOS("Name")) *    *    // Result: Kate

 *     This example uses FIELDPOS() to return the position of a
 *      specified field in a unselected work area:

 *      USE Customer NEW
 *      USE Invoices NEW
 *      ? Customer->(FIELDPOS("Name")) *       // Result: 1
 *      ? Customer->(FIELDGET(FIELDPOS("Name")))  // Result: Kate
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     FIELDGET(),FIELDPUT()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     FIELDPUT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Set the value of a field variable
 *  $SYNTAX$
 *     FIELDPUT(<nField>, <expAssign>) --> ValueAssigned
 *  $ARGUMENTS$
 *    <nField> is the ordinal position of the field in the current
 *   database file.

 *     <expAssign> is the value to assign to the given field.  The data
 *   type of this expression must match the data type of the designated field
 *   variable.
 *  $RETURNS$    
 *     FIELDPUT() returns the value assigned to the designated field.  If
 *   <nField> does not correspond to the position of any field in the current
 *   database file, FIELDPUT() returns NIL.
 *  $DESCRIPTION$
 *     FIELDPUT() is a database function that assigns <expAssign> to the field
 *   at ordinal position <nField> in the current work area.  This function
 *   allows you to set the value of a field using its position within the
 *   database file structure rather than its field name.  Within generic
 *   database service functions this allows, among other things, the setting
 *   of field values without use of the macro operator.
 *  $EXAMPLES$
 *     This example compares FIELDPUT() to functionally equivalent
 *      code that uses the macro operator to set the value of a field:

 *      // Using macro operator
 *      FName := FIELD(nField)         // Get field name
 *      FIELD->&FName := FVal          // Set field value
 *      // Using FIELDPUT()
 *      FIELDPUT(nField, FVal)         // Set field value
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     FIELDGET()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     FLOCK()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Lock an open and shared database file
 *  $SYNTAX$
 *     FLOCK() --> lSuccess
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     FLOCK() returns true (.T.) if an attempt to lock a database file in USE
 *   in the current work area succeeds; otherwise, it returns false (.F.).
 *   For more information on file locking, refer to the Network Programming
 *   chapter in the Programming and Utilities guide.
 *     
 *  $DESCRIPTION$
 *     FLOCK() is a database function used in network environments to lock an
 *   open and shared database file, preventing other users from updating the
 *   file until the lock is released.  Records in the locked file are
 *   accessible for read-only operations.
 *
 *     FLOCK() is related to USE...EXCLUSIVE and RLOCK().  USE...EXCLUSIVE
 *   opens a database file so that no other user can open the same file at
 *   the same time and is the most restrictive locking mechanism in
 *   HARBOUR.  RLOCK() is the least restrictive and attempts to place an
 *   update lock on a shared record, precluding other users from updating the
 *   current record.  FLOCK() falls in the middle.
 *
 *    FLOCK() is used for operations that access the entire database file.
 *   Typically, these are commands that update the file with a scope or a
 *   condition such as DELETE or REPLACE ALL.  The following is a list of
 *   such commands:
 *
 *   Commands that require an FLOCK()
 *   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 *   Command                       Mode
 *   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 *   APPEND FROM                   FLOCK() or USE...EXCLUSIVE
 *   DELETE (multiple records)     FLOCK() or USE...EXCLUSIVE
 *   RECALL (multiple records)     FLOCK() or USE...EXCLUSIVE
 *   REPLACE (multiple records)    FLOCK() or USE...EXCLUSIVE
 *   UPDATE ON                     FLOCK() or USE...EXCLUSIVE
 *   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 *
 *     For each invocation of FLOCK(), there is one attempt to lock the
 *   database file, and the result is returned as a logical value.  A file
 *   lock fails if another user currently has a file or record lock for the
 *   same database file or EXCLUSIVE USE of the database file.  If FLOCK() is
 *   successful, the file lock remains in place until you UNLOCK, CLOSE the
 *   DATABASE, or RLOCK().
 *
 *     By default, FLOCK() operates on the currently selected work area as
 *   shown in the example below.
 *
 *     Notes
 *
 *     SET RELATION: HARBOUR does not automatically lock all work
 *      areas in the relation chain when you lock the current work area, and
 *      an UNLOCK has no effect on related work areas.
 *  $EXAMPLES$
 *     This example uses FLOCK() for a batch update of prices in
 *      Inventory.dbf:
 *
 *      USE Inventory NEW
 *      IF FLOCK()
 *         REPLACE ALL Inventory->Price WITH ;
 *               Inventory->Price * 1.1
 *      ELSE
 *         ? "File not available"
 *      ENDIF
 *
 *     This example uses an aliased expression to attempt a file lock
 *      in an unselected work area:
 *
 *      USE Sales NEW
 *      USE Customer NEW
 *      //
 *      IF !Sales->(FLOCK())
 *         ? "Sales is in use by another"
 *      ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     RLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     FOUND()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Determine if the previous search operation succeeded
 *  $SYNTAX$
 *     FOUND() --> lSuccess
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     FOUND() returns true (.T.) if the last search command was successful;
 *   otherwise, it returns false (.F.).
 *     
 *  $DESCRIPTION$     
 *     FOUND() is a database function that determines whether a search
 *   operation (i.e., FIND, LOCATE, CONTINUE, SEEK, or SET RELATION)
 *   succeeded.  When any of these commands are executed, FOUND() is set to
 *   true (.T.) if there is a match; otherwise, it is set to false (.F.).
 *
 *     If the search command is LOCATE or CONTINUE, a match is the next record
 *   meeting the scope and condition.  If the search command is FIND, SEEK or
 *   SET RELATION, a match is the first key in the controlling index that
 *   equals the search argument.  If the key value equals the search
 *   argument, FOUND() is true (.T.); otherwise, it is false (.F.).
 *
 *     The value of FOUND() is retained until another record movement command
 *   is executed.  Unless the command is another search command, FOUND() is
 *   automatically set to false (.F.).
 *
 *     Each work area has a FOUND() value.  This means that if one work area
 *   has a RELATION set to a child work area, querying FOUND() in the child
 *   returns true (.T.) if there is a match.
 *
 *     By default, FOUND() operates on the currently selected work area.  It
 *   can be made to operate on an unselected work area by specifying it
 *   within an aliased expression (see example below).
 *
 *    FOUND() will return false (.F.) if there is no database open in the
 *   current work area.
 *  $EXAMPLES$
 *     This example illustrates the behavior of FOUND() after a
 *      record movement command:
 *
 *      USE Sales INDEX Sales
 *      ? INDEXKEY(0)            // Result: SALESMAN
 *      SEEK "1000"
 *      ? FOUND()                // Result: .F.
 *      SEEK "100"
 *      ? FOUND()                // Result: .T.
 *      SKIP
 *      ? FOUND()                // Result: .F.
 *
 *     This example tests a FOUND() value in an unselected work area
 *      using an aliased expression:
 *
 *      USE Sales INDEX Sales NEW
 *      USE Customer INDEX Customer NEW
 *      SET RELATION TO CustNum INTO Sales
 *      //
 *      SEEK "Smith"
 *      ? FOUND(), Sales->(FOUND())
 *
 *     This code fragment processes all Customer records with the key
 *      value "Smith" using FOUND() to determine when the key value changes:
 *
 *      USE Customer INDEX Customer NEW
 *      SEEK "Smith"
 *      DO WHILE FOUND()
 *         .
 *         . <statements>
 *         .
 *         SKIP
 *         LOCATE REST WHILE Name == "Smith"
 *      ENDDO
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     EOF()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_FOUND( void )
{
   BOOL bFound = FALSE;

   if( pCurrArea )
      SELF_FOUND( ( AREAP ) pCurrArea->pArea, &bFound );
   hb_retl( bFound );
}
/*  $DOC$
 *  $FUNCNAME$
 *     HEADER()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the current database file header length
 *  $SYNTAX$
 *     HEADER() --> nBytes
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
       HEADER() returns the number of bytes in the header of the current
     database file as an integer numeric value.  If no database file is in
     use, HEADER() returns a zero (0).    
 *  $DESCRIPTION$      
       HEADER() is a database function that is used with LASTREC(), RECSIZE(),
     and DISKSPACE() to create procedures for backing up files.

       By default, HEADER() operates on the currently selected work area.  It
     will operate on an unselected work area if you specify it as part of an
     aliased expression (see example below).
 *  $EXAMPLES$
       This example determines the header size of the Sales.dbf:

        USE Sales NEW
        ? HEADER()            // Result: 258

       This example defines a pseudofunction, DbfSize(), that uses
        HEADER() with RECSIZE() and LASTREC() to calculate the size of the
        current database file in bytes:

        #define DbfSize()   ((RECSIZE() * LASTREC()) + ;
              HEADER() + 1)

        Later you can use DbfSize() as you would any function:

        USE Sales NEW
        USE Customer NEW
        ? DbfSize()
        ? Sales->(DbfSize())
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DISKSPACE(),LASTREC(),RECSIZE()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     INDEXORD()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the order position of the controlling index
 *  $SYNTAX$
 *     INDEXORD() --> nOrder
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
       INDEXORD() returns an integer numeric value.  The value returned is
     equal to the position of the controlling index in the list of open
     indexes for the current work area.  A value of zero indicates that there
     is no controlling index and records are being accessed in natural order.
     If no database file is open, INDEXORD() will also return a zero.
 *  $DESCRIPTION$      
       INDEXORD() is a database function that determines the position of the
     controlling index in the list of index files opened by the last
     USE...INDEX or SET INDEX TO in the current work area.  It is often
     useful to save the last controlling index so it can be restored later.

       By default, INDEXORD() operates on the currently selected work area.  It
     will operate on an unselected work area if you specify it as part of an
     aliased expression (see example below).
 *  $EXAMPLES$ 
       This example uses INDEXORD() to save the current order.  After
        changing to a new order, it uses the saved value to restore the
        original order:

        USE Customer INDEX Name, Serial NEW
        nOrder := INDEXORD()                  // Result: 1
        SET ORDER TO 2
        ? INDEXORD()                          // Result: 2
        SET ORDER TO nOrder
        ? INDEXORD()                          // Result: 1

       This example uses an aliased expression to determine the order
        number of the controlling index in an unselected work area:

        USE Sales INDEX Salesman, CustNum NEW
        USE Customer INDEX Name, Serial NEW
        ? Sales->(INDEXORD())                 // Result: 1
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     INDEXKEY()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_INDEXORD( void )
{
   DBORDERINFO pInfo;

   if( pCurrArea )
   {
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( ( AREAP ) pCurrArea->pArea, DBOI_NUMBER, &pInfo );
      hb_retni( hb_itemGetNI( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_retni( 0 );
}
/*  $DOC$
 *  $FUNCNAME$
 *     LASTREC()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Determine the number of records in the current (.dbf) file
 *  $SYNTAX$
 *          LASTREC() | RECCOUNT()* --> nRecords
 *  $ARGUMENTS$
 *     
 *  $RETURNS$     
       LASTREC() returns the number of physical records in the current database
     file as an integer numeric value.  Filtering commands such as SET FILTER
     or SET DELETED have no effect on the return value.  LASTREC() returns
     zero if there is no database file in USE in the current work area.
 *  $DESCRIPTION$     
       LASTREC() is a database function that determines the number of physical
     records in the current database file.  LASTREC() is identical to
     RECCOUNT() which is supplied as a compatibility function.

       By default, LASTREC() operates on the currently selected work area.  It
     will operate on an unselected work area if you specify it as part of an
     aliased expression (see example below).
 *  $EXAMPLES$
       This example illustrates the relationship between LASTREC(),
        RECCOUNT(), and COUNT:

        USE Sales NEW
        ? LASTREC(), RECCOUNT()          // Result: 84 84
        //
        SET FILTER TO Salesman = "1001"
        COUNT TO nRecords
        ? nRecords, LASTREC()            // Result: 14 84

       This example uses an aliased expression to access the number
        of records in a open database file in an unselected work area:

        USE Sales NEW
        USE Customer NEW
        ? LASTREC(), Sales->(LASTREC())
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     EOF()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     LUPDATE()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the last modification date of a (.dbf) file
 *  $SYNTAX$
 *     LUPDATE() --> dModification
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
       LUPDATE() returns the date of last change to the open database file in
     the current work area.  If there is no database file in USE, LUPDATE()
     returns a blank date.
 *     
 *  $DESCRIPTION$     
       LUPDATE() is a database function that determines the date the database
     file in the current work area was last modified and CLOSEd.  By default,
     LUPDATE() operates on the currently selected work area.  It will operate
     on an unselected work area if you specify it as part of an aliased
     expression as shown in the example below.
 *  $EXAMPLES$
       This example demonstrates that the modification date of
        database file is not changed until the database file is closed:

        ? DATE()                  // Result: 09/01/90
        USE Sales NEW
        ? LUPDATE()               // Result: 08/31/90
        //
        APPEND BLANK
        ? LUPDATE()               // Result: 08/31/90
        CLOSE DATABASES
        //
        USE Sales NEW
        ? LUPDATE()               // Result: 09/01/90

       This example uses an aliased expression to access LUPDATE()
        for a database file open in an unselected work area:

        USE Sales NEW
        USE Customer NEW
        ? LUPDATE(), Sales->(LUPDATE())
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *  FIELDNAME(),LASTREC(),RECSIZE()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_LUPDATE( void )
{
   if( !pCurrArea )
      hb_itemPutDS( &hb_stack.Return, "" );
   else
      SELF_INFO( ( AREAP ) pCurrArea->pArea, DBI_LASTUPDATE, &hb_stack.Return );
}
/*  $DOC$
 *  $FUNCNAME$
 *     NETERR()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Determine if a network command has failed
 *  $SYNTAX$
 *     NETERR([<lNewError>]) --> lError
 *  $ARGUMENTS$
       <lNewError> if specified sets the value returned by NETERR() to the
     specified status.  <lNewError> can be either true (.T.) or false (.F.).
     Setting NETERR() to a specified value allows the runtime error handler
     to control the way certain file errors are handled.  For more
     information, refer to Errorsys.prg.
 *     
 *  $RETURNS$     
       NETERR() returns true (.T.) if a USE or APPEND BLANK fails.  The initial
     value of NETERR() is false (.F.).  If the current process is not running
     under a network operating system, NETERR() always returns false (.F.).
 *  $DESCRIPTION$   
       NETERR() is a network function.  It is a global flag set by USE,
     USE...EXCLUSIVE, and APPEND BLANK in a network environment.  It is used
     to test whether any of these commands have failed by returning true
     (.T.) in the following situations:

     NETERR() Causes
     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
     Command             Cause
     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
     USE                 USE EXCLUSIVE by another process
     USE...EXCLUSIVE     USE EXCLUSIVE or USE by another process
     APPEND BLANK        FLOCK() or RLOCK() of LASTREC() + 1 by another user
     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

       NETERR() is generally applied in a program by testing it following a USE
     or APPEND BLANK command.  If it returns false (.F.), you can perform the
     next operation.  If the command is USE, you can open index files.  If it
     is APPEND BLANK, you can assign values to the new record with a REPLACE
     or @...GET command.  Otherwise, you must handle the error by either
     retrying the USE or APPEND BLANK, or terminating the current operation
     with a BREAK or RETURN.
 *  $EXAMPLES$
       This example demonstrates typical usage of NETERR().  If the
        USE succeeds, the index files are opened and processing continues.
        If the USE fails, a message displays and control returns to the
        nearest BEGIN SEQUENCE construct:

        USE Customer SHARED NEW
        IF !NETERR()
           SET INDEX TO CustNum, CustOrders, CustZip
        ELSE
           ? "File is in use by another"
           BREAK
        ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     FLOCK(),RLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_NETERR( void )
{
   if( ISLOG( 1 ) )
      bNetError = hb_parl( 1 );

   hb_retl( bNetError );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDBAGEXT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the default Order Bag RDD extension
 *  $SYNTAX$
 *     ORDBAGEXT() --> cBagExt
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     ORDBAGEXT() returns a character expression.
 *  $DESCRIPTION$
       ORDBAGEXT() is an Order management function that returns a character
     expression that is the default Order Bag extension of the current or
     aliased work area.  cBagExt is determined by the RDD active in the
     current work area.

       ORDBAGEXT() supersedes the INDEXEXT() and is not recommended.
 *     
 *  $EXAMPLES$
       USE sample VIA "DBFNTX"
       ? ORDBAGEXT()      //  Returns .ntx
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      INDEXEXT(),ORDBAGNAME()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDBAGEXT( void )
{
   LPRDDNODE pRddNode;
   AREAP pTempArea;
   USHORT uiSize, uiRddID;
   DBORDERINFO pInfo;

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
         pInfo.itmResult = hb_itemPutC( NULL, "" );
         SELF_ORDINFO( pTempArea, DBOI_BAGEXT, &pInfo );
         hb_retc( pInfo.itmResult->item.asString.value );
         hb_itemRelease( pInfo.itmResult );
         SELF_RELEASE( ( AREAP ) pTempArea );
      }
      hb_xfree( pTempArea );
   }
   else
   {
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pCurrArea->pArea, DBOI_BAGEXT, &pInfo );
      hb_retc( pInfo.itmResult->item.asString.value );
      hb_itemRelease( pInfo.itmResult );
   }
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDBAGNAME()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the Order Bag name of a specific Order
 *  $SYNTAX$
 *     ORDBAGNAME(<nOrder> | <cOrderName>) --> cOrderBagName
 *  $ARGUMENTS$
       <nOrder> is an integer that identifies the position in the Order
     List of the target Order whose Order Bag name is sought.

       <cOrderName> is a character string that represents the name of the
     target Order whose Order Bag name is sought.
 *     
 *  $RETURNS$    
       ORDBAGNAME() returns a character string, the Order Bag name of the
     specific Order.
 *  $DESCRIPTION$     
       ORDBAGNAME() is an Order management function that lets you access the
     name of the Order Bag in which <cOrderName> resides.  You may identify
     the Order as a character string or with an integer that represents its
     position in the Order List.  In case of duplicate names, ORDBAGNAME()
     only recognizes the first matching name.
 *  $EXAMPLES$
       The following example uses ORDBAGNAME() with the default
        DBFNTX driver:

        USE Customer VIA "DBFNTX" NEW
        SET INDEX TO CuAcct, CuName, CuZip
        ORDBAGNAME( 2 )               // Returns: CuName
        ORDBAGNAME( 1 )               // Returns: CuAcct
        ORDBAGNAME( 3 )               // Returns: CuZip


       In this example, Customer.cdx contains three orders named
        CuAcct, CuName, CuZip:

        USE Customer VIA "DBFCDX" NEW
        SET INDEX TO Customer
        ORDBAGNAME( "CuAcct" )        // Returns: Customer
        ORDBAGNAME( "CuName" )        // Returns: Customer
        ORDBAGNAME( "CuZip" )         // Returns: Customer
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ORDBAGEXT()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDBAGNAME( void )
{
   DBORDERINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, IT_NUMERIC );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDBAGNAME" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pCurrArea->pArea, DBOI_BAGNAME, &pOrderInfo );
      hb_retc( pOrderInfo.itmResult->item.asString.value );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDBAGNAME" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDCONDSET()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Set the Condition and scope for an order
 *  $SYNTAX$
 *     ORDCONSET([<cForCondition>],
          [<bForCondition>],
          [<lAll>],
          [<bWhileCondition>],
          [<bEval>],
          [<nInterval>],
          [<nStart>],
          [<nNext>],
          [<nRecord>],
          [<lRest>],
          [<lDescend>],
          [<lAdditive>],
          [<lCurrent>],
          [<lCustom>],
          [<lNoOptimize>])
 *  $ARGUMENTS$
 *     <cForCondition> is a string that specifies the FOR condition for the
     order.
       <bForCondition> is a code block that defines a FOR condition that
     each record within the scope must meet in order to be processed. If
     a record does not meet the specified condition,it is ignored and the
     next  record is processed.Duplicate keys values are not added to the
     index file when a FOR condition is Used.
 *  $RETURNS$
 *     
 *  $DESCRIPTION$
 *     
 *  $EXAMPLES$
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *     ORDCONDSET() is CA-CLIPPER 5.3 Compilant
 *  $SEEALSO$
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDCONDSET( void )
{
   LPDBORDERCONDINFO pOrderCondInfo;
   char * szFor;
   ULONG ulLen;
   PHB_ITEM pItem;

   if( pCurrArea )
   {
      pOrderCondInfo = ( LPDBORDERCONDINFO ) hb_xgrab( sizeof( DBORDERCONDINFO ) );
      szFor = hb_parc( 1 );
      ulLen = strlen( szFor );
      if( ulLen )
      {
         pOrderCondInfo->abFor = ( BYTE * ) hb_xgrab( ulLen + 1 );
         strcpy( ( char * ) pOrderCondInfo->abFor, szFor );
      }
      else
         pOrderCondInfo->abFor = NULL;
      pItem = hb_param( 2, IT_BLOCK );
      if( pItem )
      {
         pOrderCondInfo->itmCobFor = hb_itemNew( NULL );
         hb_itemCopy( pOrderCondInfo->itmCobFor, pItem );
      }
      else
         pOrderCondInfo->itmCobFor = NULL;
      if( ISLOG( 3 ) ) 
         pOrderCondInfo->fAll = hb_parl( 3 );
      else
         pOrderCondInfo->fAll = TRUE;
      pItem = hb_param( 4, IT_BLOCK );
      if( pItem )
      {
         pOrderCondInfo->itmCobWhile = hb_itemNew( NULL );
         hb_itemCopy( pOrderCondInfo->itmCobWhile, pItem );
      }
      else
         pOrderCondInfo->itmCobWhile = NULL;
      pItem = hb_param( 5, IT_BLOCK );
      if( pItem )
      {
         pOrderCondInfo->itmCobEval = hb_itemNew( NULL );
         hb_itemCopy( pOrderCondInfo->itmCobEval, pItem );
      }
      else
         pOrderCondInfo->itmCobEval = NULL;
      pOrderCondInfo->lStep = hb_parnl( 6 );
      pOrderCondInfo->lStartRecno = hb_parnl( 7 );
      pOrderCondInfo->lNextCount = hb_parnl( 8 );
      pOrderCondInfo->lRecno = hb_parnl( 9 );
      pOrderCondInfo->fRest = hb_parl( 10 );
      pOrderCondInfo->fDescending = hb_parl( 11 );
      pOrderCondInfo->fAdditive = hb_parl( 12 );
      pOrderCondInfo->fScoped = hb_parl( 13 );
      pOrderCondInfo->fCustom = hb_parl( 14 );
      pOrderCondInfo->fNoOptimize = hb_parl( 15 );
      if( !pOrderCondInfo->itmCobWhile )
         pOrderCondInfo->fRest = TRUE;
      if( pOrderCondInfo->lNextCount || pOrderCondInfo->lRecno || pOrderCondInfo->fRest )
         pOrderCondInfo->fAll = FALSE;
      hb_retl( SELF_ORDSETCOND( ( AREAP ) pCurrArea->pArea, pOrderCondInfo ) == SUCCESS );
   }
   else
      hb_retl( FALSE );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDCREATE()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Create an Order in an Order Bag
 *  $SYNTAX$
       ORDCREATE(<cOrderBagName>,[<cOrderName>],    <cExpKey>,
        [<bExpKey>], [<lUnique>]) --> NIL
 *  $ARGUMENTS$    
       <cOrderBagName> is the name of a disk file containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or extension.  If you do not include the extension as part
     of <cOrderBagName> HARBOUR uses the default extension of the current
     RDD.

       <cOrderName> is the name of the Order to be created.

       Note: Although both <cOrderBagName> and <cOrderName> are both
     optional, at least one of them must be specified.

       <cExpKey> is an expression that returns the key value to place in
     the Order for each record in the current work area.  <cExpKey> can
     represent a character, date, logical, or numeric data type.  The maximum
     length of the index key expression is determined by the database driver.

       <bExpKey> is a code block that evaluates to a key value that is
     placed in the Order for each record in the current work area.  If you do
     not supply <bExpKey>, it is macro-compiled from <cExpKey>.

       <lUnique> is an optional logical value that specifies whether a
     unique Order is to be created.  If <lUnique> is omitted, the current
     global _SET_UNIQUE setting is used.
 *  $RETURNS$
 *     ORDCREATE() always returns NIL.
 *  $DESCRIPTION$     
       ORDCREATE() is an Order management function that creates an Order in the
     current work area.  It works like DBCREATEINDEX() except that it lets
     you create Orders in RDDs that recognize multiple-Order Bags.
     ORDCREATE() supersedes the DBCREATEINDEX() function because of this
     capability, and is the preferred function.

       The active RDD determines the Order capacity of an Order Bag.  The
     default  DBFNTX and the DBFNDX drivers only support single-Order Bags,
     while other RDDs may support multiple-Order Bags (e.g., the DBFCDX and
     DBFMDX drivers).

       In RDDs that support production or structural indexes (e.g., DBFCDX,
     DBFMDX), if you specify a Tag but do not specify an Order Bag, the Tag is
     created and added to the index.  If no production or structural index
     exists, it will be created and the Tag will be added to it.  When using
     RDDs that support multiple Order Bags, you must explicitly SET ORDER (or
     ORDSETFOCUS()) to the desired controlling Order.  If you do not specify
     a controlling Order, the data file will be viewed in natural Order.

       If <cOrderBagName> does not exist, it is created in accordance with the
     RDD in the current or specified work area.

      If <cOrderBagName> exists and the RDD specifies that Order Bags can only
     contain a single Order, <cOrderBagName> is erased and the new Order is
     added to the Order List in the current or specified work area.

       If <cOrderBagName> exists and the RDD specifies that Order Bags can
     contain multiple Tags, <cOrderName> is created if it does not already
     exist, otherwise <cOrderName> is replaced in <cOrderBagName> and the
     Order is added to the Order List in the current or specified work area.
 *  $EXAMPLES$
       The following example demonstrates ORDCREATE() with the DBFNDX
        driver:

        USE Customer VIA "DBFNDX" NEW
        ORDCREATE( "CuAcct",, "Customer->Acct" )


       The following example demonstrates ORDCREATE() with the
        default DBFNTX driver:

        USE Customer VIA "DBFNTX" NEW
        ORDCREATE( "CuAcct", "CuAcct", "Customer->Acct", ;
              {|| Customer->Acct } )

       The following example demonstrates ORDCREATE() with the FoxPro
        driver, DBFCDX:

        USE Customer VIA "DBFCDX" NEW
        ORDCREATE( "Customer", "CuAcct", "Customer->Acct" )

       This example creates the Order "CuAcct" and adds it to the
        production index (Order Bag) "Customer".  The production index , will
        be created if it doesn't exist:

        USE Customer VIA "DBFMDX" NEW
        ORDCREATE( , "CuAcct", "Customer->Acct" )
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBCREATEINDEX()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDCREATE( void )
{
   DBORDERCREATEINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.abBagName = ( BYTE * ) hb_parc( 1 );
      pOrderInfo.atomBagName = ( BYTE * ) hb_parc( 2 );
      pOrderInfo.abExpr = hb_param( 3, IT_STRING );
      if( ( ( strlen( ( char * ) pOrderInfo.abBagName ) == 0 ) &&
            ( strlen( ( char * ) pOrderInfo.atomBagName ) == 0 ) ) ||
          !pOrderInfo.abExpr )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDCREATE" );
         return;
      }
      pOrderInfo.itmCobExpr = hb_param( 4, IT_BLOCK );
      if( ISLOG( 5 ) )
         pOrderInfo.fUnique = hb_parl( 5 );
      else
         pOrderInfo.fUnique = hb_set.HB_SET_UNIQUE;
      SELF_ORDCREATE( ( AREAP ) pCurrArea->pArea, &pOrderInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDCREATE" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDDESTROY()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *      Remove a specified Order from an Order Bag
 *  $SYNTAX$
 *          ORDDESTROY(<cOrderName> [, <cOrderBagName> ]) --> NIL
 *  $ARGUMENTS$
       <cOrderName> is the name of the Order to be removed from the current
     or specified work area.

       <cOrderBagName> is the name of a disk file containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or appropriate extension.  If you do not include the
     extension as part of <cOrderBagName> HARBOUR uses the default
     extension of the current RDD.    
 *  $RETURNS$     
 *     ORDDESTROY() always returns NIL.
 *  $DESCRIPTION$
       ORDDESTROY() is an Order management function that removes a specified
     Order from multiple-Order Bags.

       The active RDD determines the Order capacity of an Order Bag.  The
     default DBFNTX and the DBFNDX drivers only support single-Order Bags,
     while other RDDs may support multiple-Order Bags (e.g., the DBFCDX and
     DBPX drivers).

       Note:  RDD suppliers may define specific behaviors for this command.

       Warning!  ORDDESTROY() is not supported for DBFNDX and DBFNTX.
 *  $EXAMPLES$
       This example demonstrates ORDDESTROY() with the FoxPro driver,
        DBFCDX:

        USE Customer VIA "DBFCDX" NEW
        SET INDEX TO Customer, CustTemp
        ORDDESTROY( "CuAcct", "Customer" )
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ORDCREATE()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDDESTROY( void )
{
   DBORDERINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, IT_STRING );
      SELF_ORDDESTROY( ( AREAP ) pCurrArea->pArea, &pOrderInfo );
   }
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDFOR()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the FOR expression of an Order
 *  $SYNTAX$
       ORDFOR(<cOrderName> | <nOrder>
        [, <cOrderBagName>]) --> cForExp
 *     
 *  $ARGUMENTS$     
       <cOrderName> is the name of the target Order, whose cForExp is
     sought.

       <nOrder> is an integer that identifies the position in the Order
     List of the target Order whose cForExp is sought.

       <cOrderBagName> is the name of an Order Bag containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or appropriate extension.  If you do not include the
     extension as part of <cOrderBagName> HARBOUR uses the default
     extension of the current RDD.
 *  $RETURNS$     
       ORDFOR() returns a character expression, cForExp, that represents the
     FOR condition of the specified Order.  If the Order was not created
     using the FOR clause the return value will be an empty string ("").  If
     the database driver does not support the FOR condition, it may either
     return an empty string ("") or raise an "unsupported function" error,
     depending on the driver.
 *  $DESCRIPTION$     
       ORDFOR() is an Order management function that returns the character
     string, cForExp, that represents the logical FOR condition of the Order,
     <cOrderName> or <nOrder>.
 *  $EXAMPLES$
       This example retrieves the FOR condition from an Order:

        USE Customer NEW
        INDEX ON  Customer->Acct ;
           TO  Customer          ;
           FOR Customer->Acct > "AZZZZZ"

        ORDFOR( "Customer" )      // Returns: Customer->Acct > "AZZZZZ"
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ORDKEY(),ORDCREATE(),ORDNAME(),ORDNUMBER()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDFOR( void )
{
   DBORDERINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDFOR" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pCurrArea->pArea, DBOI_CONDITION, &pOrderInfo );
      hb_retc( pOrderInfo.itmResult->item.asString.value );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDFOR" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDKEY()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the key expression of an Order
 *  $SYNTAX$     
       ORDKEY(<cOrderName> | <nOrder>
        [, <cOrderBagName>]) --> cExpKey
 *  $ARGUMENTS$     
       <cOrderName> is the name of an Order, a logical ordering of a
     database.

       <nOrder> is an integer that identifies the position in the Order
     List of the target Order whose cExpKey is sought.

       <cOrderBagName> is the name of a disk file containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or appropriate extension.  If you do not include the
     extension as part of <cOrderBagName> HARBOUR uses the default
     extension of the current RDD.
 *  $RETURNS$     
 *     Returns a character string, cExpKey.
 *  $DESCRIPTION$
       ORDKEY() is an Order management function that returns a character
     expression, cExpKey, that represents the key expression of the specified
     Order.

       You may specify the Order by name or with a number that represents its
     position in the Order List.  Using the Order name is the preferred
     method.

       The active RDD determines the Order capacity of an Order Bag.  The
     default DBFNTX and the DBFNDX drivers only support single-Order Bags,
     while other RDDs may support multiple-Order Bags (e.g., the DBFCDX and
     DBFMDX drivers).
 *  $EXAMPLES$
       This example retrieves the index expression from an Order:

        USE Customer NEW
        INDEX ON  Customer->Acct  ;
           TO  Customer           ;
           FOR Customer->Acct > "AZZZZZ"

        ORDKEY( "Customer" )      // Returns: Customer->Acct
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ORDFOR(),ORDNAME(),ORDNUMBER()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDKEY( void )
{
   DBORDERINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDKEY" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pCurrArea->pArea, DBOI_EXPRESSION, &pOrderInfo );
      hb_retc( pOrderInfo.itmResult->item.asString.value );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDKEY" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDLISTADD()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Add Orders to the Order List
 *  $SYNTAX$     
       ORDLISTADD(<cOrderBagName>
        [, <cOrderName>]) --> NIL
 *  $ARGUMENTS$     
       <cOrderBagName> is the name of a disk file containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or appropriate extension.  If you do not include the
     extension as part of <cOrderBagName> HARBOUR uses the default
     extension of the current RDD.

       <cOrderName> the name of the specific Order from the Order Bag to be
     added to the Order List of the current work area.  If you do not specify
     <cOrderName>, all orders in the Order Bag are added to the Order List of
     the current work area.
 *  $RETURNS$     
 *     ORDLISTADD() always returns NIL.
 *  $DESCRIPTION$
       ORDLISTADD() is an Order management function that adds the contents of
     an Order Bag , or a single Order in an Order Bag, to the Order List.
     This function lets you extend the Order List without issuing a SET INDEX
     command that, first, clears all the active Orders from the Order List.
   
       Any Orders already associated with the work area continue to be active.
     If the newly opened Order Bag contains the only Order associated with
     the work area, it becomes the controlling Order; otherwise, the
     controlling Order remains unchanged.

       After the new Orders are opened, the work area is positioned to the
     first logical record in the controlling Order.

       ORDLISTADD() is similar to the SET INDEX command or the INDEX clause of
     the USE command, except that it does not clear the Order List prior to
     adding the new order(s).

       ORDLISTADD() supersedes the DBSETINDEX() function.

       The active RDD determines the Order capacity of an Order Bag.  The
     default  DBFNTX and the DBFNDX drivers only support single-Order Bags,
     while other RDDs may support multiple-Order Bags (e.g., the DBFCDX and
     DBPX drivers).  When using RDDs that support multiple Order Bags, you
     must explicitly SET ORDER (or ORDSETFOCUS()) to the desired controlling
     Order.  If you do not specify a controlling Order, the data file will be
     viewed in natural Order.
 *  $EXAMPLES$
       In this example Customer.cdx contains three orders, CuAcct,
        CuName, and CuZip.  ORDLISTADD() opens Customer.cdx but only uses the
        order named CuAcct:

        USE Customer VIA "DBFCDX" NEW
        ORDLISTADD( "Customer", "CuAcct" )
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBSETINDEX()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDLISTADD( void )
{
   DBORDERINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.atomBagName = hb_param( 1, IT_STRING );
      pOrderInfo.itmOrder = hb_param( 2, IT_STRING );
      if( !pOrderInfo.atomBagName )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDLISTADD" );
         return;
      }
      SELF_ORDLSTADD( ( AREAP ) pCurrArea->pArea, &pOrderInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDLISTADD" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDLISTCLEAR()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Clear the current Order List
 *  $SYNTAX$
 *     ORDLISTCLEAR() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     ORDLISTCLEAR() always returns NIL.
 *  $DESCRIPTION$
       ORDLISTCLEAR() is an Order management function that removes all Orders
     from the Order List for the current or aliased work area.  When you are
     done, the Order List is empty.

      This function supersedes the function DBCLEARINDEX().
 *     
 *  $EXAMPLES$
     USE Sales NEW
     SET INDEX TO SaRegion, SaRep, SaCode
     .
     . < statements >
     .
     ORDLISTCLEAR()      // Closes all the current indexes
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *    DBCLEARINDEX()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDLISTCLEAR( void )
{
   if( pCurrArea )
      SELF_ORDLSTCLEAR( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDLISTCLEAR" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDLISTREBUILD()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Rebuild all Orders in the Order List of the current work area
 *  $SYNTAX$
 *     ORDLISTREBUILD() --> NIL
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
 *     ORDLISTREBUILD() always returns NIL.
 *  $DESCRIPTION$
       ORDLISTREBUILD() is an Order management function that rebuilds all the
     orders in the current or aliased Order List.

       To only rebuild a single Order use the function ORDCREATE().

       Unlike ORDCREATE(), this function rebuilds all Orders in the Order List.
     It is equivalent to REINDEX.
 *     
 *  $EXAMPLES$
     USE Customer NEW
     SET INDEX TO CuAcct, CuName, CuZip
     ORDLISTREBUILD()     // Causes CuAcct, CuName, CuZip to
                          // be rebuilt

 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      ORDCREATE()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDLISTREBUILD( void )
{
   if( pCurrArea )
      SELF_ORDLSTREBUILD( ( AREAP ) pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDLISTCLEAR" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDNAME()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the name of an Order in the Order List
 *  $SYNTAX$
 *     ORDNAME(<nOrder>[,<cOrderBagName>])
        --> cOrderName
 *  $ARGUMENTS$     
       <nOrder> is an integer that identifies the position in the Order
     List of the target Order whose database name is sought.

       <cOrderBagName> is the name of a disk file containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or appropriate extension.  If you do not include the
     extension as part of <xcOrderBagName> HARBOUR uses the default
     extension of the current RDD.
 *  $RETURNS$     
       ORDNAME() returns the name of the specified Order in the current Order
     List or the specified Order Bag if opened in the Current Order list.
 *  $DESCRIPTION$     
       ORDNAME() is an Order management function that returns the name of the
     specified Order in the current Order List.

       If <cOrderBagName> is an Order Bag that has been emptied into the
     current Order List, only those Orders in the Order List that correspond
     to <cOrderBagName> Order Bag are searched.

       The active RDD determines the Order capacity of an Order Bag.  The
     default DBFNTX and the DBFNDX drivers only support single-Order Bags,
     while other RDDs may support multiple-Order Bags (e.g., the DBFCDX and
     DBPX drivers).
 *  $EXAMPLES$
       This example retrieves the name of an Order using its position
        in the order list:

        USE Customer NEW
        SET INDEX TO CuAcct, CuName, CuZip
        ORDNAME( 2 )                        // Returns: CuName

       This example retrieves the name of an Order given its position
        within a specific Order Bag in the Order List:

        USE Customer NEW
        SET INDEX TO Temp, Customer
        // Assume Customer contains CuAcct, CuName, CuZip
        ORDNAME( 2, "Customer" )            // Returns: CuName
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ORDFOR(),ORDKEY(),ORDNUMBER()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDNAME( void )
{
   DBORDERINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDNAME" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pCurrArea->pArea, DBOI_NAME, &pOrderInfo );
      hb_retc( pOrderInfo.itmResult->item.asString.value );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDNAME" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDNUMBER()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *      Return the position of an Order in the current Order List
 *  $SYNTAX$
       ORDNUMBER(<cOrderName>
        [, <cOrderBagName>]) --> nOrderNo     
 *  $ARGUMENTS$     
       <cOrderName> the name of the specific Order whose position in the
     Order List is sought.

       <cOrderBagName> is the name of a disk file containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or appropriate extension.  If you do not include the
     extension as part of <cOrderBagName> HARBOUR uses the default
     extension of the current RDD.
 *  $RETURNS$     
       Returns nOrderNo, an integer that represents the position of the
     specified Order in the Order List.
 *  $DESCRIPTION$     
       ORDNUMBER() is an Order management function that lets you determine the
     position in the current Order List of the specified Order.  ORDNUMBER()
     searches the Order List in the current work area and returns the
     position of the first Order that matches <cOrderName>.    If
     <cOrderBagName> is the name of an Order Bag newly emptied into the
     current Order List, only those orders in the Order List that have been
     emptied from <cOrderBagName> are searched.

       If <cOrderName> is not found ORDNUMBER() raises a recoverable runtime
     error.

       The active RDD determines the Order capacity of an Order Bag.  The
     default DBFNTX driver only supports single-Order Bags, while other RDDs
     may support multiple-Order Bags (e.g., the DBFCDX and DBPX drivers).
 *  $EXAMPLES$
     USE Customer VIA "DBFNTX" NEW
     SET INDEX TO CuAcct, CuName, CuZip
     ORDNUMBER( "CuName" )            // Returns: 2
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     INDEXORD()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDNUMBER( void )
{
   DBORDERINFO pOrderInfo;

   if( pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, IT_STRING );
      pOrderInfo.atomBagName = hb_param( 2, IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDNUMBER" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( ( AREAP ) pCurrArea->pArea, DBOI_NUMBER, &pOrderInfo );
      hb_retni( hb_itemGetNI( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDNUMBER" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     ORDSETFOCUS()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Set focus to an Order in an Order List
 *  $SYNTAX$
       ORDSETFOCUS([<cOrderName> | <nOrder>]
        [,<cOrderBagName>]) --> cPrevOrderNameInFocus
 *     
 *  $ARGUMENTS$    
       <cOrderName> is the name of the selected Order, a logical ordering
     of a database.  ORDSETFOCUS() ignores any invalid values of
     <cOrderName>.

       <nOrder> is a number representing the position in the Order List of
     the selected Order.

       <cOrderBagName> is the name of a disk file containing one or more
     Orders.  You may specify <cOrderBagName> as the filename with or without
     the pathname or appropriate extension.  If you do not include the
     extension as part of <cOrderBagName> HARBOUR uses the default
     extension of the current RDD.
 *  $RETURNS$
 *     ORDSETFOCUS() returns the Order Name of the previous controlling Order.
 *  $DESCRIPTION$     
       ORDSETFOCUS() is an Order management function that returns the Order
     Name of the previous controlling Order and optionally sets the focus to
     an new Order.

       If you do not specify <cOrderName> or <nOrder>, the name of the
     currently controlling order is returned and the controlling order
     remains unchanged.

       All Orders in an Order List are properly updated no matter what
     <cOrderName> is the controlling Order.  After a change of controlling
     Orders, the record pointer still points to the same record.

       The active RDD determines the Order capacity of an Order Bag.  The
     default DBFNTX driver only supports single-Order Bags, while other RDDs
     may support multiple-Order Bags (e.g., the DBFCDX and DBPX drivers).

       ORDSETFOCUS() supersedes INDEXORD().
 *  $EXAMPLES$

     USE Customer VIA "DBFNTX" NEW
     SET INDEX TO CuAcct, CuName, CuZip
     ? ORDSETFOCUS( "CuName" )        // Displays: "CuAcct"
     ? ORDSETFOCUS()                  // Displays: "CuName"
 *  $TESTS$
 *
 *  $STATUS$
 *     S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ORDSETFOCUS( void )
{
   DBORDERINFO pInfo;

   if( pCurrArea )
   {
      pInfo.itmOrder = hb_param( 1, IT_STRING );
      if( !pInfo.itmOrder )
         pInfo.itmOrder = hb_param( 1, IT_NUMERIC );
      pInfo.atomBagName = hb_param( 2, IT_STRING );
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDLSTFOCUS( ( AREAP ) pCurrArea->pArea, &pInfo );
      hb_retc( pInfo.itmResult->item.asString.value );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDSETFOCUS" );
}
/*  $DOC$
 *  $FUNCNAME$
 *     RDDLIST()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *      Return an array of the available Replaceable Database Drivers
 *  $SYNTAX$
 *     RDDLIST([<nRDDType>]) --> aRDDList
 *  $ARGUMENTS$    
       <nRDDType> is an integer that represents the type of the RDD you
     wish to list.  The constants RDT_FULL and RDT_TRANSFER represent the two
     types of RDDs currently available.

     RDDType Summary
     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
     Constant       Value     Meaning
     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
     RDT_FULL       1         Full RDD implementation
     RDT_TRANSFER   2         Import/Export only driver
     ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

       RDT_FULL identifies full-featured RDDs that have all the capabilities
     associated with an RDD.

       RDT_TRANSFER identifies RDDs of limited capability.  They can only
     transfer records between files.  You cannot use these limited RDD
     drivers to open a file in a work area.  The SDF and DELIM drivers are
     examples of this type of RDD.  They are only used in the implementation
     of APPEND FROM and COPY TO with SDF or DELIMITED files.
 *  $RETURNS$    
       RDDLIST() returns a one-dimensional array of the RDD names registered
     with the application as <nRDDType>.
 *  $DESCRIPTION$     
       RDDLIST() is an RDD function that returns a one-dimensional array that
     lists the available RDDs.

       If you do not supply <nRDDType>, all available RDDs, regardless of type,
     are returned.
 *  $EXAMPLES$
       In this example RDDLIST() returns an array containing the
        character strings, "DBF", "SDF", "DELIM", "DBFCDX", and "DBFNTX":

        REQUEST DBFCDX

        .
        . < statements >
        .

        aRDDs := RDDLIST()

              // Returns {"DBF", SDF", "DELIM", "DBFCDX", "DBFNTX" }

       In this example, RDDLIST() returns an array containing the
        character strings, "SDF" and "DELIM":

        #include "rddsys.ch"
        .
        . < statements >
        .
        aImpExp := RDDLIST( RDT TRANSFER )

 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     
 *  $INCLUDE$
 *     RDDSYS.CH
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     RDDNAME()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the name of the currently active RDD
 *  $SYNTAX$
 *     RDDNAME() --> cRDDName
 *  $ARGUMENTS$
 *     
 *  $RETURNS$     
 *     Returns a character string, cRDDName, the registered name of the active
 *   RDD in the current or specified work area.
 *  $DESCRIPTION$     
 *     RDDNAME() is an RDD function that returns a character string, cRDDName,
 *   the name of the active RDD in the current or specified work area.
 *
 *     You can specify a work area other than the currently active work area by
 *   aliasing the function.
 *  $EXAMPLES$
 *   USE Customer VIA "DBFNTX" NEW
 *   USE Sales    VIA "DBFCDX" NEW
 *
 *   ? RDDNAME()                          // Returns: DBFCDX
 *   ? Customer->( RDDNAME() )            // Returns: DBFNTX
 *   ? Sales->( RDDNAME() )               // Returns: DBFCDX
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     RDDLIST()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     RDDSETDEFAULT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Set or return the default RDD for the application
 *  $SYNTAX$
 *     RDDSETDEFAULT([<cNewDefaultRDD>])
 *      --> cPreviousDefaultRDD
 *     
 *  $ARGUMENTS$     
 *     <cNewDefaultRDD> is a character string, the name of the RDD that is
 *   to be made the new default RDD in the application.
 *  $RETURNS$     
 *     RDDSETDEFAULT() returns a character string, cPreviousDefaultRDD, the
 *   name of the previous default driver.  The default driver is the driver
 *   that HARBOUR uses if you do not explicitly specify an RDD with the
 *   VIA clause of the USE command.
 *  $DESCRIPTION$     
 *     RDDSETDEFAULT() is an RDD function that sets or returns the name of the
 *   previous default RDD driver and, optionally, sets the current driver to
 *   the new RDD driver specified by cNewDefaultRDD.  If <cNewDefaultDriver>
 *   is not specified, the current default driver name is returned and
 *   continues to be the current default driver.
 *
 *    This function replaces the DBSETDRIVER() function.
 *  $EXAMPLES$
 *   // If the default driver is not DBFNTX, make it the default
 *
 *   IF ( RDDSETDEFAULT() != "DBFNTX" )
 *      cOldRdd := RDDSETDEFAULT( "DBFNTX" )
 *   ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBSETDRIVER()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     RECCOUNT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Determine the number of records in the current (.dbf) file
 *  $SYNTAX$
 *     RECCOUNT()* | LASTREC() --> nRecords
 *  $ARGUMENTS$
 *     
 *  $RETURNS$
       RECCOUNT() returns the number of physical records in the current
     database file as an integer numeric value.  Filtering commands such as
     SET FILTER or SET DELETED have no effect on the return value.
     RECCOUNT() returns zero if there is no database file open in the current
     work area.
 *     
 *  $DESCRIPTION$*     
       RECCOUNT() is a database function that is a synonym for LASTREC().  By
     default, RECCOUNT() operates on the currently selected work area.  It
     will operate on an unselected work area if you specify it as part of an
     aliased expression (see example below).
 *  $EXAMPLES$
       This example illustrates the relationship between COUNT and
        RECCOUNT():

        USE Sales NEW
        ? RECCOUNT()                      // Result: 84
        //
        SET FILTER TO Salesman = "1001"
        COUNT TO nRecords
        ? nRecords                        // Result: 14
        ? RECCOUNT()                      // Result: 84

       This example uses an aliased expression to access the number
        of records in an unselected work area:

        USE Sales NEW
        USE Customer NEW
        ? RECCOUNT(), Sales->(RECCOUNT())
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     EOF(),LASTREC()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_RECCOUNT( void )
{
   ULONG ulRecCount = 0;

   if( pCurrArea )
   {
      SELF_RECCOUNT( ( AREAP ) pCurrArea->pArea, &ulRecCount );
      ( ( AREAP ) pCurrArea->pArea )->lpExtendInfo->ulRecCount = ulRecCount;
   }
   hb_retnl( ulRecCount );
}
/*  $DOC$
 *  $FUNCNAME$
 *     RECNO()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Return the identity at the position of the record pointer
 *  $SYNTAX$
 *     RECNO() --> Identity
 *  $ARGUMENTS$
 *     
 *  $RETURNS$     
       RECNO() returns the identity found at the position of the record
     pointer.
 *  $DESCRIPTION$     
       RECNO() is a database function that returns the identity found at the
     current position of the record pointer.  Identity is a unique value
     guaranteed by the structure of the data file to reference a specific
     record of data file.  The data file need not be a traditional Xbase
     file.  Therefore, unlike earlier versions of HARBOUR, the value
     returned need not be a numeric data type.

       Under all RDDs, RECNO() returns the value at the position of the record
     pointer; the data type and other characteristics of this value are
     determined by the content of the accessed data and the RDD active in the
     current work area.  In an Xbase database this value is the record
     number.
 *  $EXAMPLES$
     USE Sales VIA "DBFNTX"
     .
     . < statements >
     .
     DBGOTOP()
     RECNO()            // Returns 1
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBGOTO()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_RECNO( void )
{
   PHB_ITEM pRecNo;

   pRecNo = hb_itemPutNL( NULL, 0 );
   if( pCurrArea )
      SELF_RECNO( ( AREAP ) pCurrArea->pArea, pRecNo );
   hb_itemReturn( pRecNo );
   hb_itemRelease( pRecNo );
}
/*  $DOC$
 *  $FUNCNAME$
 *     RECSIZE()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Determine the record length of a database (.dbf) file
 *  $SYNTAX$
 *     RECSIZE() --> nBytes
 *  $ARGUMENTS$
 *     
 *  $RETURNS$     
       RECSIZE() returns, as a numeric value, the record length, in bytes, of
     the database file open in the current work area.  RECSIZE() returns zero
     if no database file is open.
 *  $DESCRIPTION$    
       RECSIZE() is a database function that determines the length of a record
     by summing the lengths of each field then adding one for the DELETED()
     status flag.  When this value is multiplied by LASTREC(), the product is
     the amount of space occupied by the file's records.

       RECSIZE() is useful in programs that perform automatic file backup.
     When used in conjunction with DISKSPACE(), the RECSIZE() function can
     assist in ensuring that sufficient free space exists on the disk before a
     file is stored.

       By default, RECSIZE() operates on the currently selected work area.  It
     will operate on an unselected work area if you specify it as part of an
     aliased expression (see example below).
 *  $EXAMPLES$
       The following user-defined function, DbfSize(), uses RECSIZE()
        to calculate the size of the current database file:

        FUNCTION DbfSize
           RETURN ((RECSIZE() * LASTREC()) + HEADER() + 1)

       This example illustrates the use of RECSIZE() to determine the
        record length of database files open in unselected work areas:

        USE Customer NEW
        USE Sales NEW
        //
        ? RECSIZE(), Customer->(RECSIZE())
        ? DbfSize(), Customer->(DbfSize())
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *  DISKSPACE(),FIELDNAME(),HEADER(),LASTREC()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     RLOCK()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Lock the current record in the active work area
 *  $SYNTAX$
 *     RLOCK() --> lSuccess
 *  $ARGUMENTS$
 *     
 *  $RETURNS$     
       RLOCK() returns true (.T.) if the record lock is obtained; otherwise, it
     returns false (.F.).
 *  $DESCRIPTION$     
       RLOCK() is a network function that locks the current record, preventing
     other users from updating the record until the lock is released.
     RLOCK() provides a shared lock, allowing other users read-only access to
     the locked record while allowing only the current user to modify it.  A
     record lock remains until another record is locked, an UNLOCK is
     executed, the current database file is closed, or an FLOCK() is obtained
     on the current database file.

       For each invocation of RLOCK(), there is one attempt to lock the current
     record, and the result is returned as a logical value.  An attempt to
     obtain a record lock fails if another user currently has a file or
     record lock, or EXCLUSIVE USE of the database file.  An attempt to
     RLOCK() in an empty database returns true (.T.).

       By default, RLOCK() operates on the currently selected work area.  It
     will operate on an unselected work area if you specify it as part of an
     aliased expression (see example below).  This feature is useful since
     RLOCK() does not automatically attempt a record lock for related files.

       As a general rule, RLOCK() operates solely on the current record.  This
     includes the following commands:

       @...GET

       DELETE (single record)

       RECALL (single record)

       REPLACE (single record)

     Refer to the Network Programming chapter in the Programming and
     Utilities guide for more information.

    Notes

       SET RELATION: HARBOUR does not automatically lock all
        records in the relation chain when you lock the current work area
        record.  Also, an UNLOCK has no effect on related work areas.
 *  $EXAMPLES$
       This example deletes a record in a network environment, using
        RLOCK():

        USE Customer INDEX CustName SHARED NEW
        SEEK "Smith"
        IF FOUND()
           IF RLOCK()
              DELETE
              ? "Smith deleted"
           ELSE
              ? "Record in use by another"
           ENDIF
        ELSE
           ? "Smith not in Customer file"
        ENDIF
        CLOSE

       This example specifies RLOCK() as an aliased expression to
        lock a record in an unselected work area:

        USE Sales SHARED NEW
        USE Customer SHARED NEW
        //
        IF !Sales->(RLOCK())
           ? "The current Sales record is in use by another"
        ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     FLOCK()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     SELECT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Determine the work area number of a specified alias
 *  $SYNTAX$
 *     SELECT([<cAlias>]) --> nWorkArea
 *  $ARGUMENTS$
 *     <cAlias> is the target work area alias name.
 *  $RETURNS$     
       SELECT() returns the work area of the specified alias as a integer
     numeric value.
 *  $DESCRIPTION$     
       SELECT() is a database function that determines the work area number of
     an alias.  The number returned can range from zero to 250.  If <cAlias>
     is not specified, the current work area number is returned.  If <cAlias>
     is specified and the alias does not exist, SELECT() returns zero.

       Note:  The SELECT() function and SELECT command specified with an
     extended expression argument look somewhat alike.  This shouldn't be a
     problem since the SELECT() function is not very useful on a line by
     itself
 *  $EXAMPLES$
       This example uses SELECT() to determine which work area
        USE...NEW selected:

        USE Sales NEW
        SELECT 1
        ? SELECT("Sales")            // Result: 4

       To reselect the value returned from the SELECT() function, use
        the SELECT command with the syntax, SELECT (<idMemvar>), like this:

        USE Sales NEW
        nWorkArea:= SELECT()
        USE Customer NEW
        SELECT (nWorkArea)
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      ALIAS(),USED()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     USED()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Determine whether a database file is in USE
 *  $SYNTAX$
 *     USED() --> lDbfOpen
 *  $ARGUMENTS$
 *     
 *  $RETURNS$     
       USED() returns true (.T.) if there is a database file in USE; otherwise,
     it returns false (.F.).
 *  $DESCRIPTION$     
       USED() is a database function that determines whether there is a
     database file in USE in a particular work area.  By default, USED()
     operates on the currently selected work area.  It will operate on an
     unselected work area if you specify it as part of an aliased expression.
 *  $EXAMPLES$
       This example determines whether a database file is in USE in
        the current work area:

        USE Customer NEW
        ? USED()               // Result: .T.
        CLOSE
        ? USED()               // Result: .F.
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ALIAS(),SELECT()
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_USED( void )
{
   hb_retl( pCurrArea != NULL );
}

/* NOTE: Same as dbSetDriver() and rddSetDefault(), but doesn't
         throw any error if the driver doesn't exist, this is
         required in the RDDSYS INIT function, since it's not guaranteed
         that the RDD is already registered at that point. */

/*  $DOC$
 *  $FUNCNAME$
 *     __RDDSETDEFAULT()
 *  $CATEGORY$
 *     Data Base
 *  $ONELINER$
 *     Set or return the default RDD for the application
 *  $SYNTAX$
       __RDDSETDEFAULT([<cNewDefaultRDD>])
        --> cPreviousDefaultRDD
 *     
 *  $ARGUMENTS$     
       <cNewDefaultRDD> is a character string, the name of the RDD that is
     to be made the new default RDD in the application.
 *  $RETURNS$     
       __RDDSETDEFAULT() returns a character string, cPreviousDefaultRDD, the
     name of the previous default driver.  The default driver is the driver
     that HARBOUR uses if you do not explicitly specify an RDD with the
     VIA clause of the USE command.
 *  $DESCRIPTION$     
       RDDSETDEFAULT() is an RDD function that sets or returns the name of the
     previous default RDD driver and, optionally, sets the current driver to
     the new RDD driver specified by cNewDefaultRDD.  If <cNewDefaultDriver>
     is not specified, the current default driver name is returned and
     continues to be the current default driver.

      This function replaces the DBSETDRIVER() function.
 *  $EXAMPLES$
     // If the default driver is not DBFNTX, make it the default

     IF ( __RDDSETDEFAULT() != "DBFNTX" )
        cOldRdd := __RDDSETDEFAULT( "DBFNTX" )
     ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     DBSETDRIVER()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
