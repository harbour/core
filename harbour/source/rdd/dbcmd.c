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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapirdd.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbvm.h"

#include "rddsys.ch"
#include "set.ch"

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

extern HB_FUNC( _DBF );
extern HB_FUNC( _SDF );
extern HB_FUNC( _DELIM );
extern HB_FUNC( RDDSYS );

static char * s_szDefDriver = NULL;    /* Default RDD name */
static USHORT s_uiCurrArea = 1;        /* Selectd area */
static LPRDDNODE s_pRddList = NULL;    /* Registered RDD's */
static BOOL s_bNetError = FALSE;       /* Error on Networked environments */

static LPAREANODE s_pWorkAreas = NULL; /* WorkAreas */
static LPAREANODE s_pCurrArea = NULL;  /* Pointer to a selectd and valid area */

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
   SELF_CLEARREL( pArea );
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
   DBFIELDINFO pFieldInfo;
   USHORT uiCount;
   USHORT uiItems;

   HB_TRACE(HB_TR_DEBUG, ("defCreateFields(%p, %p)", pArea, pStruct));

   uiItems = ( USHORT ) hb_arrayLen( pStruct );
   SELF_SETFIELDEXTENT( pArea, uiItems );
   pFieldInfo.typeExtended = 0;
   for( uiCount = 0; uiCount < uiItems; uiCount++ )
   {
      PHB_ITEM pFieldDesc;
      long lLong;

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
   ULONG ulNext = 0;

   HB_TRACE(HB_TR_DEBUG, ("defEval(%p, %p)", pArea, pEvalInfo));

   if( pEvalInfo->dbsci.itmRecID )
   {
      SELF_GOTOID( pArea, pEvalInfo->dbsci.itmRecID );
      SELF_EOF( pArea, &bEof );
      if( !bEof )
      {
         if( pEvalInfo->dbsci.itmCobWhile )
         {
            bWhile = hb_itemGetL( hb_vmEvalBlock( pEvalInfo->dbsci.itmCobWhile ) );
         }
         else
            bWhile = TRUE;

         if( pEvalInfo->dbsci.itmCobFor )
         {
            bFor = hb_itemGetL( hb_vmEvalBlock( pEvalInfo->dbsci.itmCobFor ) );
         }
         else
            bFor = TRUE;

         if( bWhile && bFor )
         {
            hb_vmEvalBlock( pEvalInfo->itmBlock );
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
         bWhile = hb_itemGetL( hb_vmEvalBlock( pEvalInfo->dbsci.itmCobWhile ) );
         if( !bWhile )
            break;
      }
      else
         bWhile = TRUE;

      if( pEvalInfo->dbsci.itmCobFor )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pEvalInfo->dbsci.itmCobFor ) );
      }
      else
         bFor = TRUE;

      if( bFor && bWhile )
      {
         hb_vmEvalBlock( pEvalInfo->itmBlock );
      }
      SELF_SKIP( pArea, 1 );
      SELF_EOF( pArea, &bEof );
   }

   return SUCCESS;
}

static ERRCODE defEvalBlock( AREAP pArea, PHB_ITEM pBlock )
{
   PHB_ITEM pResult;

   HB_TRACE(HB_TR_DEBUG, ("defEvalBlock(%p, %p)", pArea, pBlock));

   if( !pBlock || !HB_IS_BLOCK( pBlock ) )
   {
      PHB_ITEM pError;

      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_NOMETHOD );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_NOMETHOD ) );
      SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }

   pResult = hb_vmEvalBlock( pBlock );
   if( !pArea->valResult )
      pArea->valResult = hb_itemNew( NULL );
   hb_itemCopy( pArea->valResult, pResult );

   return SUCCESS;
}

static ERRCODE defclearRel( AREAP pArea )
{
   LPDBRELINFO  lpdbRelations = pArea->lpdbRelations;
   LPDBRELINFO  lpdbRelPrev;

   HB_TRACE(HB_TR_DEBUG, ("defclearRel(%p)", pArea ));

   if( lpdbRelations )
   {
      do
      {
         ( ( AREAP ) lpdbRelations->lpaChild )->uiParents --;
         lpdbRelPrev = lpdbRelations;
         lpdbRelations = lpdbRelations->lpdbriNext;
         hb_xfree( lpdbRelPrev );
      }
      while( lpdbRelations );
   }
   return SUCCESS;
}

static ERRCODE defsetRel( AREAP pArea, LPDBRELINFO  lpdbRelInf )
{
      LPDBRELINFO  lpdbRelations;

      ( (AREAP) lpdbRelInf->lpaChild )->uiParents ++;
      lpdbRelations = pArea->lpdbRelations;
      if( !lpdbRelations )
      {
         pArea->lpdbRelations = ( LPDBRELINFO ) hb_xgrab( sizeof( DBRELINFO ) );
         lpdbRelations = pArea->lpdbRelations;
      }
      else
      {
         while( !lpdbRelations->lpdbriNext )
            lpdbRelations = lpdbRelations->lpdbriNext;
         lpdbRelations->lpdbriNext = ( LPDBRELINFO ) hb_xgrab( sizeof( DBRELINFO ) );
         lpdbRelations = lpdbRelations->lpdbriNext;
      }
      lpdbRelations->lpaChild = lpdbRelInf->lpaChild;
      lpdbRelations->itmCobExpr = lpdbRelInf->itmCobExpr;
      lpdbRelations->abKey = lpdbRelInf->abKey;
      lpdbRelations->lpdbriNext = lpdbRelInf->lpdbriNext;
      return SUCCESS;
}

static ERRCODE defrelText( AREAP pArea, USHORT relNum, char* cExpr )
{
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
         szType[ 0 ] = ( char ) pField->uiType;
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
   pArea->lpdbRelations = NULL;
   pArea->uiParents = 0;
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
      LPFILEINFO pFileInfo;

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
      while( TRUE )
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
            PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
            if( HB_IS_LOGICAL( pResult ) && !hb_itemGetL( pResult ) )
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
      while( TRUE )
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
            PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
            if( HB_IS_LOGICAL( pResult ) && !hb_itemGetL( pResult ) )
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

   pRddNode = s_pRddList;
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
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_V ) defUnSupported,
                             defclearRel,
                             ( DBENTRYP_V ) defUnSupported,
                             ( DBENTRYP_SVP ) defUnSupported,
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_SVP ) defrelText,
                             ( DBENTRYP_VP ) defsetRel,
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
                             ( DBENTRYP_V ) defUnSupported,
                             ( DBENTRYP_VPLP ) defUnSupported,
                             defFilterText,
                             ( DBENTRYP_SI ) defUnSupported,
                             defSetFilter,
                             defSetLocate,
                             ( DBENTRYP_VP ) defUnSupported,
                             ( DBENTRYP_VPL ) defUnSupported,
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

   if( !s_szDefDriver )
   {
      s_szDefDriver = ( char * ) hb_xgrab( 1 );
      s_szDefDriver[ 0 ] = '\0';

      /* Force link the built-in RDD's */
      HB_FUNCNAME( _DBF )();
      HB_FUNCNAME( _SDF )();
      HB_FUNCNAME( _DELIM )();
      HB_FUNCNAME( RDDSYS )();
   }
}

static void hb_rddCloseAll( void )
{
   int nCycl = 0;
   LPAREANODE s_pArea;
   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAll()"));

   while( nCycl < 2 )
   {
      s_pArea = s_pWorkAreas;
      while( s_pArea )
      {
         s_pCurrArea = s_pArea;
         s_pArea = s_pArea->pNext;
         if( ( !nCycl && ( ( AREAP ) s_pCurrArea->pArea )->lpdbRelations ) ||
             ( nCycl && s_pCurrArea->pArea ) )
         {
            SELF_CLOSE( ( AREAP ) s_pCurrArea->pArea );
            SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );
            hb_xfree( s_pCurrArea->pArea );
            s_pCurrArea->pArea = NULL;
         }
         if( nCycl == 1 )
            hb_xfree( s_pCurrArea );
      }
      nCycl ++;
   }

   s_uiCurrArea = 1;
   s_pCurrArea = NULL;
   s_pWorkAreas = NULL;
}

static LPRDDNODE hb_rddFindNode( char * szDriver, USHORT * uiIndex )
{
   LPRDDNODE pRddNode;
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFindNode(%s, %p)", szDriver, uiIndex));

   uiCount = 0;
   pRddNode = s_pRddList;
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
   hb_vmPushPointer( ( void * ) &uiFunctions );
   hb_vmPushPointer( ( void * ) &pRddNewNode->pTable );
   hb_vmDo( 2 );
   if ( hb_parni( -1 ) != SUCCESS )
   {
      hb_xfree( pRddNewNode );         /* Delete de new RDD node */
      return 3;                        /* Invalid FUNCTABLE */
   }

   if( !s_pRddList )                     /* First RDD node */
      s_pRddList = pRddNewNode;
   else
   {
      pRddNode = s_pRddList;
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
      return ( USHORT ) pSymAlias->hArea;
   else
      return 0;
}

static void hb_rddSelectFirstAvailable( void )
{
   LPAREANODE pAreaNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectFirstAvailable()"));

   s_uiCurrArea = 1;
   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea > s_uiCurrArea )
         break;
      else if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
         s_uiCurrArea++;
      pAreaNode = pAreaNode->pNext;
   }
   s_pCurrArea = NULL;   /* Selected WorkArea must be created */
}

/*
 * pTable - a table in new RDDNODE that will be filled
 * pSubTable - a table with a list of supported functions
 * pSuperTable - a current table in a RDDNODE
 * szDrvName - a driver name that will be inherited
*/
ERRCODE hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, BYTE * szDrvName )
{
   LPRDDNODE pRddNode;
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInherit(%p, %p, %p, %s)", pTable, pSubTable, pSuperTable, szDrvName));

   if( !pTable )
      return FAILURE;

   /* Copy the pSuperTable into pTable */
   if( !szDrvName || ( uiCount = strlen( ( const char * ) szDrvName ) )==0 )
   {
      /* no name for inherited driver - use the default one */
      memcpy( pTable, &defTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &defTable, sizeof( RDDFUNCS ) );
   }
   else
   {
      char * szSuperName;

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

/* closes (if requested) and releases the current area preparing it
 * to be used with a new database
*/
void hb_rddReleaseCurrentArea( BOOL bClose )
{
   if( bClose )
      SELF_CLOSE( ( AREAP ) s_pCurrArea->pArea );
   SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );

   if( s_pWorkAreas == s_pCurrArea )
   {
      s_pWorkAreas = s_pCurrArea->pNext;
      if( s_pWorkAreas )
         s_pWorkAreas->pPrev = NULL;
   }
   else
   {
      if( s_pCurrArea->pPrev )
         s_pCurrArea->pPrev->pNext = s_pCurrArea->pNext;
      if( s_pCurrArea->pNext )
         s_pCurrArea->pNext->pPrev = s_pCurrArea->pPrev;
   }

   hb_xfree( s_pCurrArea->pArea );
   hb_xfree( s_pCurrArea );
   s_pCurrArea = NULL;
}

/* Prepares a new area node
*/
LPAREANODE hb_rddNewAreaNode( LPRDDNODE pRddNode, USHORT uiRddID )
{
   LPAREANODE pCurrArea = ( LPAREANODE ) hb_xgrab( sizeof( AREANODE ) );

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      USHORT uiSize;

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

   return pCurrArea;
}


/*
 *  Function for getting current workarea pointer
 */

void * hb_rddGetCurrentWorkAreaPointer( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaPointer()"));

   return s_pCurrArea->pArea;
}

/*
 * -- FUNCTIONS ACCESSED FROM VIRTUAL MACHINE --
 */

int  hb_rddGetCurrentWorkAreaNumber( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaNumber()"));

   return s_uiCurrArea;
}

ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
   LPAREANODE pAreaNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaNumber(%d)", iArea));

   s_uiCurrArea = iArea;

   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
      {
         s_pCurrArea = pAreaNode; /* Select a valid WorkArea */
         return SUCCESS;
      }
      pAreaNode = pAreaNode->pNext;
   }
   s_pCurrArea = NULL;               /* Selected WorkArea is closed */
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
   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", pItem, pFieldSymbol));

   if( s_pCurrArea )
   {
      LPFIELD pField;
      USHORT uiField;

      uiField = 1;
      pField = ( ( AREAP ) s_pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            SELF_PUTVALUE( ( AREAP ) s_pCurrArea->pArea, uiField, pItem );
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
   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", pItem, pFieldSymbol));

   if( s_pCurrArea )
   {
      LPFIELD pField;
      USHORT uiField;

      uiField = 1;
      pField = ( ( AREAP ) s_pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            SELF_GETVALUE( ( AREAP ) s_pCurrArea->pArea, uiField, pItem );
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
   HB_TRACE(HB_TR_DEBUG, ("hb_rddShutDown()"));

   hb_rddCloseAll();
   if( s_szDefDriver )
      hb_xfree( s_szDefDriver );

   while( s_pRddList )
   {
      LPRDDNODE pRddNode;

      pRddNode = s_pRddList;
      s_pRddList = s_pRddList->pNext;
      hb_xfree( pRddNode );
   }
}

/*
 * -- HARBOUR FUNCTIONS --
 */

HB_FUNC( AFIELDS )
{
   PHB_ITEM pName, pType, pLen, pDec, pItem;
   USHORT uiFields, uiArrayLen = 0, uiCount;

   if( !s_pCurrArea )
   {
      hb_retni( 0 );
      return;
   }

   pName = hb_param( 1, HB_IT_ARRAY );
   pType = hb_param( 2, HB_IT_ARRAY );
   pLen = hb_param( 3, HB_IT_ARRAY );
   pDec = hb_param( 4, HB_IT_ARRAY );
   if( !pName && !pType && !pLen && !pDec )
   {
      hb_retni( 0 );
      return;
   }

   pItem = hb_itemNew( NULL );
   SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields );
   if( pName )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pName );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_NAME, pItem );
         hb_arraySet( pName, uiCount, pItem );
      }
   }
   if( pType )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pType );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_TYPE, pItem );
         hb_arraySet( pType, uiCount, pItem );
      }
   }
   if( pLen )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pLen );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_LEN, pItem );
         hb_arraySet( pLen, uiCount, pItem );
      }
   }
   if( pDec )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pDec );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_DEC, pItem );
         hb_arraySet( pDec, uiCount, pItem );
      }
   }

   hb_itemRelease( pItem );
   hb_retni( uiArrayLen );
}

HB_FUNC( ALIAS )
{
   USHORT uiArea;
   LPAREANODE pAreaNode;
   char * szAlias;

   uiArea = hb_parni( 1 );
   uiArea = uiArea ? uiArea : s_uiCurrArea;
   pAreaNode = s_pWorkAreas;
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

HB_FUNC( DBEVAL )
{
   if( s_pCurrArea )
   {
      DBEVALINFO pEvalInfo;

      pEvalInfo.itmBlock = hb_param( 1, HB_IT_BLOCK );
      if( !pEvalInfo.itmBlock )
      {
         hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmCobFor = hb_param( 2, HB_IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobFor )
      {
         if( !ISNIL( 2 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.itmCobWhile = hb_param( 3, HB_IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobWhile )
      {
         if( !ISNIL( 3 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.lNext = hb_param( 4, HB_IT_NUMERIC );
      if( !pEvalInfo.dbsci.lNext )
      {
         if( !ISNIL( 4 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.itmRecID = hb_param( 5, HB_IT_NUMERIC );
      if( !pEvalInfo.dbsci.itmRecID )
      {
         if( !ISNIL( 5 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      pEvalInfo.dbsci.fRest = hb_param( 6, HB_IT_LOGICAL );
      if( !pEvalInfo.dbsci.fRest )
      {
         if( !ISNIL( 6 ) )
         {
            hb_errRT_DBCMD( EG_ARG, 2019, NULL, "DBEVAL" );
            return;
         }
      }

      SELF_DBEVAL( ( AREAP ) s_pCurrArea->pArea, &pEvalInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBEVAL" );
}

HB_FUNC( DBF )
{
   LPAREANODE pAreaNode = s_pWorkAreas;

   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
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

HB_FUNC( BOF )
{
   BOOL bBof = TRUE;

   if( s_pCurrArea )
      SELF_BOF( ( AREAP ) s_pCurrArea->pArea, &bBof );
   hb_retl( bBof );
}

HB_FUNC( DBAPPEND )
{
   if( s_pCurrArea )
   {
      BOOL bUnLockAll = ISLOG( 1 ) ? hb_parl( 1 ) : TRUE;

      s_bNetError = ( SELF_APPEND( ( AREAP ) s_pCurrArea->pArea, bUnLockAll ) == FAILURE );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBAPPEND" );
}

HB_FUNC( DBCLEARFILTER )
{
   if( s_pCurrArea )
      SELF_CLEARFILTER( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCLEARFILTER" );
}

HB_FUNC( DBCLOSEALL )
{
   hb_rddCloseAll();
}

HB_FUNC( DBCLOSEAREA )
{
   if( !s_pCurrArea )
      return;

   hb_rddReleaseCurrentArea( TRUE );   /* close before releasing */
}

HB_FUNC( DBCOMMIT )
{
   if( s_pCurrArea )
      SELF_FLUSH( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCOMMIT" );
}

HB_FUNC( DBCOMMITALL )
{
   LPAREANODE pAreaNode;

   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      SELF_FLUSH( ( AREAP ) pAreaNode->pArea );
      pAreaNode = pAreaNode->pNext;
   }
}

HB_FUNC( __DBCONTINUE )
{
   BOOL bEof;

   if( !s_pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBCONTINUE" );
      return;
   }

   if( !( ( AREAP ) s_pCurrArea->pArea )->dbsi.itmCobFor )
      return;
   ( ( AREAP ) s_pCurrArea->pArea )->fFound = FALSE;
   SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
   SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
   if( bEof )
      return;

   ( ( AREAP ) s_pCurrArea->pArea )->fFound = hb_itemGetL( hb_vmEvalBlock( ( ( AREAP ) s_pCurrArea->pArea )->dbsi.itmCobFor ) );
   while( !bEof && !( ( AREAP ) s_pCurrArea->pArea )->fFound )
   {
      SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
      SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = hb_itemGetL( hb_vmEvalBlock( ( ( AREAP ) s_pCurrArea->pArea )->dbsi.itmCobFor ) );
   }
}

HB_FUNC( DBCREATE )
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
   pStruct = hb_param( 2 , HB_IT_ARRAY );
   uiLen = ( USHORT ) hb_arrayLen( pStruct );

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
      if( !( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          !( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
         return;
      }
   }

   hb_rddCheck();
   uiLen = ( USHORT ) hb_parclen( 3 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( cDriverBuffer, hb_parc( 3 ), uiLen );
      szDriver = cDriverBuffer;
   }
   else
      szDriver = s_szDefDriver;

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
      else if( s_pCurrArea )  /* If current WorkArea is in use then close it */
      {
         hb_rddReleaseCurrentArea( TRUE );   /* close before releasing */
      }
   }

   /* Create a new WorkArea node */

   s_pCurrArea = hb_rddNewAreaNode( pRddNode, uiRddID );

   if( SELF_CREATEFIELDS( ( AREAP ) s_pCurrArea->pArea, pStruct ) == FAILURE )
   {
      SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );
      hb_xfree( s_pCurrArea->pArea );
      hb_xfree( s_pCurrArea );
      s_pCurrArea = NULL;
      hb_errRT_DBCMD( EG_ARG, 1014, NULL, "DBCREATE" );
      return;
   }

   pFileName = hb_fsFNameSplit( szFileName );
   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   strcpy( szFileName, hb_parc( 1 ) );
   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
      strcat( szFileName, hb_itemGetCPtr( pFileExt ) );
      hb_itemRelease( pFileExt );
   }
   hb_xfree( pFileName );
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) hb_parc( 5 );
   pInfo.uiArea = s_uiCurrArea;

   ( ( AREAP ) s_pCurrArea->pArea )->uiArea = s_uiCurrArea;

   /* Insert the new WorkArea node */

   if( !s_pWorkAreas )
      s_pWorkAreas = s_pCurrArea;  /* The new WorkArea node is the first */
   else
   {
      pAreaNode = s_pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea > s_uiCurrArea )
         {
            /* Insert the new WorkArea node */
            s_pCurrArea->pPrev = pAreaNode->pPrev;
            s_pCurrArea->pNext = pAreaNode;
            pAreaNode->pPrev = s_pCurrArea;
            if( s_pCurrArea->pPrev )
               s_pCurrArea->pPrev->pNext = s_pCurrArea;
            else
               s_pWorkAreas = s_pCurrArea;
            break;
         }
         if( pAreaNode->pNext )
            pAreaNode = pAreaNode->pNext;
         else
         {
            /* Append the new WorkArea node */
            pAreaNode->pNext = s_pCurrArea;
            s_pCurrArea->pPrev = pAreaNode;
            break;
         }
      }
   }

   ( ( AREAP ) s_pCurrArea->pArea )->lpDataInfo->szFileName = szFileName;
   ( ( AREAP ) s_pCurrArea->pArea )->atomAlias = hb_dynsymGet( ( char * ) pInfo.atomAlias );
   if( ( ( PHB_DYNS ) ( ( AREAP ) s_pCurrArea->pArea )->atomAlias )->hArea )
   {
      hb_errRT_DBCMD( EG_DUPALIAS, 1011, NULL, ( char * ) pInfo.atomAlias );
      bError = TRUE;
   }

   if( !bError )
      bError = ( SELF_CREATE( ( AREAP ) s_pCurrArea->pArea, &pInfo ) == FAILURE );

   if( !bError )
      ( ( PHB_DYNS ) ( ( AREAP ) s_pCurrArea->pArea )->atomAlias )->hArea = pInfo.uiArea;

   if( !bError && ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->fHasMemo )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_MEMOEXT, pFileExt );
      pFileName = hb_fsFNameSplit( ( char * ) pInfo.abName );
      szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
      szFileName[ 0 ] = '\0';
      if( pFileName->szDrive )
         strcat( szFileName, pFileName->szDrive );
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      strcat( szFileName, hb_itemGetCPtr( pFileExt ) );
      pInfo.abName = ( BYTE * ) szFileName;
      hb_xfree( pFileName );
      hb_itemRelease( pFileExt );
      ( ( AREAP ) s_pCurrArea->pArea )->lpDataInfo->pNext =
                              ( LPFILEINFO ) hb_xgrab( sizeof( FILEINFO ) );
      memset( ( ( AREAP ) s_pCurrArea->pArea )->lpDataInfo->pNext, 0,
              sizeof( FILEINFO ) );
      ( ( AREAP ) s_pCurrArea->pArea )->lpDataInfo->pNext->hFile = FS_ERROR;
      ( ( AREAP ) s_pCurrArea->pArea )->lpDataInfo->pNext->szFileName = szFileName;
      bError = ( SELF_CREATEMEMFILE( ( AREAP ) s_pCurrArea->pArea, &pInfo ) == FAILURE );
   }

   ( ( PHB_DYNS ) ( ( AREAP ) s_pCurrArea->pArea )->atomAlias )->hArea = 0;
   SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );
   if( !ISLOG( 4 ) || bError )
   {
      if( s_pWorkAreas == s_pCurrArea )
      {
         s_pWorkAreas = s_pCurrArea->pNext;
         if( s_pWorkAreas )
            s_pWorkAreas->pPrev = NULL;
      }
      else
      {
         if( s_pCurrArea->pPrev )
            s_pCurrArea->pPrev->pNext = s_pCurrArea->pNext;
         if( s_pCurrArea->pNext )
            s_pCurrArea->pNext->pPrev = s_pCurrArea->pPrev;
      }

      hb_xfree( s_pCurrArea->pArea );
      hb_xfree( s_pCurrArea );
      s_pCurrArea = NULL;
   }
   else
   {
      SELF_NEW( ( AREAP ) s_pCurrArea->pArea );
      szFileName = hb_parc( 1 );
      pFileName = hb_fsFNameSplit( szFileName );
      szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
      strcpy( szFileName, hb_parc( 1 ) );
      if( !pFileName->szExtension )
      {
         pFileExt = hb_itemPutC( NULL, "" );
         SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
         strcat( szFileName, hb_itemGetCPtr( pFileExt ) );
          hb_itemRelease( pFileExt );
      }
      hb_xfree( pFileName );
      pInfo.abName = ( BYTE * ) szFileName;
      pInfo.fShared = !hb_set.HB_SET_EXCLUSIVE;
      pInfo.fReadonly = FALSE;
      ( ( AREAP ) s_pCurrArea->pArea )->uiArea = s_uiCurrArea;
      ( ( AREAP ) s_pCurrArea->pArea )->lpDataInfo->szFileName = szFileName;
      if( SELF_OPEN( ( AREAP ) s_pCurrArea->pArea, &pInfo ) == FAILURE )
      {
         SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );
         hb_xfree( s_pCurrArea->pArea );
         hb_xfree( s_pCurrArea );
         s_pCurrArea = NULL;
      }
      SELF_RECCOUNT( ( AREAP ) s_pCurrArea->pArea, &ulRecCount );
      ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->ulRecCount = ulRecCount;
   }
}

HB_FUNC( DBDELETE )
{
   if( s_pCurrArea )
      SELF_DELETE( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBDELETE" );
}

HB_FUNC( DBFILTER )
{
   if( s_pCurrArea )
   {
      PHB_ITEM pFilter;

      pFilter = hb_itemPutC( NULL, "" );
      SELF_FILTERTEXT( ( AREAP ) s_pCurrArea->pArea, pFilter );
      hb_retc( hb_itemGetCPtr( pFilter ) );
      hb_itemRelease( pFilter );
   }
   else
      hb_retc( "" );
}

HB_FUNC( DBGOBOTTOM )
{
   if( s_pCurrArea )
      SELF_GOBOTTOM( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOBOTTOM" );
}

HB_FUNC( DBGOTO )
{
   PHB_ITEM pItem;

   if( !s_pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOTO" );
      return;
   }

   pItem = hb_param( 1, HB_IT_ANY );
   if( !pItem )
      hb_errRT_DBCMD( EG_ARG, 1003, NULL, "DBGOTO" );
   else
      SELF_GOTOID( ( AREAP ) s_pCurrArea->pArea, pItem );
}

HB_FUNC( DBGOTOP )
{
   if( s_pCurrArea )
      SELF_GOTOP( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBGOTOP" );
}

HB_FUNC( __DBLOCATE )
{
   PHB_ITEM pFor, pFor2, pWhile, pNext, pRecord, pRest;
   DBSCOPEINFO pScopeInfo;
   ULONG lNext;
   BOOL bEof, bFor, bWhile;

   if( !s_pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSETFILTER" );
      return;
   }

   memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
   pFor2 = hb_param( 1, HB_IT_BLOCK );
   pWhile = hb_param( 2, HB_IT_BLOCK );
   pNext = hb_param( 3, HB_IT_NUMERIC );
   pRecord = hb_param( 4, HB_IT_NUMERIC );
   pRest = hb_param( 5, HB_IT_LOGICAL );
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
   SELF_SETLOCATE( ( AREAP ) s_pCurrArea->pArea, &pScopeInfo );
   ( ( AREAP ) s_pCurrArea->pArea )->fFound = FALSE;
   if( pRecord )
   {
      SELF_GOTOID( ( AREAP ) s_pCurrArea->pArea, pRecord );
      SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
      if( bEof )
         return;
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
      {
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      }
      else
         bWhile = hb_itemGetL( pWhile );
      if( hb_itemType( pFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
         ( ( AREAP ) s_pCurrArea->pArea )->fFound = ( bWhile && bFor );
      }
      else
         ( ( AREAP ) s_pCurrArea->pArea )->fFound = ( bWhile && hb_itemGetL( pFor ) );
   }
   else if( pNext )
   {
      SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
      lNext = hb_parnl( 3 );
      if( bEof || lNext <= 0 )
         return;
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
      {
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      }
      else
         bWhile = hb_itemGetL( pWhile );
      if( hb_itemType( pFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
      }
      else
         bFor = hb_itemGetL( pFor );
      while( !bEof && lNext-- > 0 && bWhile && !bFor )
      {
         SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
         SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
         if( hb_itemType( pWhile ) == HB_IT_BLOCK )
         {
            bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
         }
         else
            bWhile = hb_itemGetL( pWhile );
         if( hb_itemType( pFor ) == HB_IT_BLOCK )
         {
            bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
         }
         else
            bFor = hb_itemGetL( pFor );
      }
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = bFor;
   }
   else if( hb_itemGetL( pRest ) )
   {
      SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
      if( bEof )
         return;
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
      {
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      }
      else
         bWhile = hb_itemGetL( pWhile );
      if( hb_itemType( pFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
      }
      else
         bFor = hb_itemGetL( pFor );
      while( !bEof && bWhile && !bFor )
      {
         SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
         SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
         if( hb_itemType( pWhile ) == HB_IT_BLOCK )
         {
            bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
         }
         else
            bWhile = hb_itemGetL( pWhile );
         if( hb_itemType( pFor ) == HB_IT_BLOCK )
         {
            bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
         }
         else
            bFor = hb_itemGetL( pFor );
      }
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = bFor;
   }
   else
   {
      SELF_GOTOP( ( AREAP ) s_pCurrArea->pArea );
      SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
      if( bEof )
         return;
      if( hb_itemType( pFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
      }
      else
         bFor = hb_itemGetL( pFor );
      while( !bEof && !bFor )
      {
         SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
         SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
         if( hb_itemType( pFor ) == HB_IT_BLOCK )
         {
            bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
         }
         else
            bFor = hb_itemGetL( pFor );
      }
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = bFor;
   }
}

HB_FUNC( __DBSETLOCATE )
{
   if( s_pCurrArea )
   {
      PHB_ITEM pLocate = hb_param( 1, HB_IT_BLOCK );

      if( pLocate )
      {
         PHB_ITEM pFor;
         DBSCOPEINFO pScopeInfo;

         pFor = hb_itemNew( NULL );
         hb_itemCopy( pFor, pLocate );
         memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
         pScopeInfo.itmCobFor = pFor;
         SELF_SETLOCATE( ( AREAP ) s_pCurrArea->pArea, &pScopeInfo );
      }
   }
}

HB_FUNC( __DBPACK )
{
   if( s_pCurrArea )
   {
      /* Additional feature: __dbPack( [<bBlock>, [<nEvery>] )
         Code Block to execute for every record. */
      ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->itmEval = hb_param( 1, HB_IT_BLOCK );
      ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->ulEvery = hb_parnl( 2 );
      if( !( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->ulEvery )
         ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->ulEvery = 1;
      SELF_PACK( ( AREAP ) s_pCurrArea->pArea );
      ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->itmEval = NULL;
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "__DBPACK" );
}

HB_FUNC( DBRECALL )
{
   if( s_pCurrArea )
      SELF_RECALL( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRECALL" );
}

HB_FUNC( DBRLOCK )
{
   DBLOCKINFO pLockInfo;

   pLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      pLockInfo.itmRecID = hb_parnl( 1 );
      pLockInfo.uiMethod = REC_LOCK;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &pLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRLOCK" );

   hb_retl( pLockInfo.fResult );
}

HB_FUNC( DBRLOCKLIST )
{
   PHB_ITEM pList;

   pList = hb_itemArrayNew( 0 );
   if( s_pCurrArea )
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_GETLOCKARRAY, pList );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRLOCKLIST" );

   hb_itemReturn( pList );
   hb_itemRelease( pList );
}

HB_FUNC( DBRUNLOCK )
{
   if( s_pCurrArea )
      SELF_UNLOCK( ( AREAP ) s_pCurrArea->pArea, hb_parnl( 1 ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBRUNLOCK" );
}

HB_FUNC( DBSEEK )
{
   if( s_pCurrArea )
   {
      if( !ISNIL( 1 ) )
      {
         PHB_ITEM pKey;
         BOOL bSoftSeek, bFindLast;

         pKey = hb_param( 1, HB_IT_ANY );
         bSoftSeek = ISLOG( 2 ) ? hb_parl( 2 ) : hb_set.HB_SET_SOFTSEEK;
         bFindLast = ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;
         if( SELF_SEEK( ( AREAP ) s_pCurrArea->pArea, bSoftSeek, pKey, bFindLast ) == SUCCESS )
         {
            hb_retl( ( ( AREAP ) s_pCurrArea->pArea )->fFound );
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

HB_FUNC( DBSELECTAREA )
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
      s_uiCurrArea = uiNewArea;

   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
      {
         s_pCurrArea = pAreaNode; /* Select a valid WorkArea */
         return;
      }
      pAreaNode = pAreaNode->pNext;
   }
   s_pCurrArea = NULL; /* Selected WorkArea is closed */
}

HB_FUNC( __DBSETFOUND )
{
   if( s_pCurrArea )
   {
      PHB_ITEM pFound = hb_param( 1, HB_IT_LOGICAL );

      if( pFound )
         ( ( AREAP ) s_pCurrArea->pArea )->fFound = hb_itemGetL( pFound );
   }
}

HB_FUNC( DBSKIP )
{
   if( s_pCurrArea )
      SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, ISNUM( 1 ) ? hb_parnl( 1 ) : 1 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSKIP" );
}

HB_FUNC( DBSETFILTER )
{
   if( s_pCurrArea )
   {
      PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

      if( pBlock )
      {
         DBFILTERINFO pFilterInfo;
         PHB_ITEM pText = hb_param( 2, HB_IT_STRING );

         pFilterInfo.itmCobExpr = pBlock;
         if( pText )
            pFilterInfo.abFilterText = pText;
         else
            pFilterInfo.abFilterText = hb_itemPutC( NULL, "" );
         SELF_SETFILTER( ( AREAP ) s_pCurrArea->pArea, &pFilterInfo );
         if( !pText )
            hb_itemRelease( pFilterInfo.abFilterText );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSETFILTER" );
}

HB_FUNC( DBSTRUCT )
{
   hb_arrayNew( &hb_stack.Return, 0 );

   if( s_pCurrArea )
   {
      PHB_ITEM pItem, pData;
      USHORT uiFields, uiCount;

      SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields );
      pData = hb_itemNew( NULL );
      pItem = hb_itemNew( NULL );
      for( uiCount = 1; uiCount <= uiFields; uiCount++ )
      {
         hb_arrayNew( pItem, 4 );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_NAME, pData );
         hb_arraySet( pItem, 1, pData );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_TYPE, pData );
         hb_arraySet( pItem, 2, pData );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_LEN, pData );
         hb_arraySet( pItem, 3, pData );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_DEC, pData );
         hb_arraySet( pItem, 4, pData );
         hb_arrayAdd( &hb_stack.Return, pItem );
      }
      hb_itemRelease( pItem );
      hb_itemRelease( pData );
   }
}

HB_FUNC( DBTABLEEXT )
{
   PHB_ITEM pItem;

   if( !s_pCurrArea )
   {
      LPRDDNODE pRddNode;
      AREAP pTempArea;
      USHORT uiSize, uiRddID;

      hb_rddCheck();
      uiRddID = 0;
      pRddNode = hb_rddFindNode( s_szDefDriver, &uiRddID );
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
         hb_retc( hb_itemGetCPtr( pItem ) );
         hb_itemRelease( pItem );
         SELF_RELEASE( ( AREAP ) pTempArea );
      }
      hb_xfree( pTempArea );
   }
   else
   {
      pItem = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_TABLEEXT, pItem );
      hb_retc( hb_itemGetCPtr( pItem ) );
      hb_itemRelease( pItem );
   }
}

HB_FUNC( DBUNLOCK )
{
   if( s_pCurrArea )
      SELF_RAWLOCK( ( AREAP ) s_pCurrArea->pArea, FILE_UNLOCK, 0 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBUNLOCK" );
}

HB_FUNC( DBUNLOCKALL )
{
   LPAREANODE pTempArea;

   pTempArea = s_pWorkAreas;
   while( pTempArea )
   {
      SELF_RAWLOCK( ( AREAP ) pTempArea->pArea, FILE_UNLOCK, 0 );
      pTempArea = pTempArea->pNext;
   }
}

HB_FUNC( DBUSEAREA )
{
   char * szDriver, * szFileName;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;
   USHORT uiRddID, uiLen;
   ULONG ulLen;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pFileExt;
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

   s_bNetError = FALSE;

   if( hb_parl( 1 ) )
      hb_rddSelectFirstAvailable();
   else if( s_pCurrArea )  /* If current WorkArea is in use then close it */
   {
      hb_rddReleaseCurrentArea( TRUE );   /* close before releasing */
   }

   hb_rddCheck();
   uiLen = ( USHORT ) hb_parclen( 2 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szDriverBuffer, hb_parc( 2 ), uiLen );
      szDriver = szDriverBuffer;
   }
   else
      szDriver = s_szDefDriver;

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

   s_pCurrArea = hb_rddNewAreaNode( pRddNode, uiRddID );

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   strcpy( szFileName, hb_parc( 3 ) );
   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
      strcat( szFileName, hb_itemGetCPtr( pFileExt ) );
      hb_itemRelease( pFileExt );
   }
   hb_xfree( pFileName );
   pInfo.uiArea = s_uiCurrArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;

   ( ( AREAP ) s_pCurrArea->pArea )->uiArea = s_uiCurrArea;

   /* Insert the new WorkArea node */

   if( !s_pWorkAreas )
      s_pWorkAreas = s_pCurrArea;  /* The new WorkArea node is the first */
   else
   {
      pAreaNode = s_pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea > s_uiCurrArea )
         {
            /* Insert the new WorkArea node */
            s_pCurrArea->pPrev = pAreaNode->pPrev;
            s_pCurrArea->pNext = pAreaNode;
            pAreaNode->pPrev = s_pCurrArea;
            if( s_pCurrArea->pPrev )
               s_pCurrArea->pPrev->pNext = s_pCurrArea;
            else
               s_pWorkAreas = s_pCurrArea;
            break;
         }
         if( pAreaNode->pNext )
            pAreaNode = pAreaNode->pNext;
         else
         {
            /* Append the new WorkArea node */
            pAreaNode->pNext = s_pCurrArea;
            s_pCurrArea->pPrev = pAreaNode;
            break;
         }
      }
   }

   ( ( AREAP ) s_pCurrArea->pArea )->lpDataInfo->szFileName = szFileName;
   if( SELF_OPEN( ( AREAP ) s_pCurrArea->pArea, &pInfo ) == FAILURE )
   {
      SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );

      if( s_pWorkAreas == s_pCurrArea )
      {
         s_pWorkAreas = s_pCurrArea->pNext;
         if( s_pWorkAreas )
            s_pWorkAreas->pPrev = NULL;
      }
      else
      {
         if( s_pCurrArea->pPrev )
            s_pCurrArea->pPrev->pNext = s_pCurrArea->pNext;
         if( s_pCurrArea->pNext )
            s_pCurrArea->pNext->pPrev = s_pCurrArea->pPrev;
      }

      hb_xfree( s_pCurrArea->pArea );
      hb_xfree( s_pCurrArea );
      s_pCurrArea = NULL;
      return;
   }
   SELF_RECCOUNT( ( AREAP ) s_pCurrArea->pArea, &ulLen );
   ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->ulRecCount = ulLen;
}

HB_FUNC( __DBZAP )
{
   if( s_pCurrArea )
      SELF_ZAP( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "__DBZAP" );
}

HB_FUNC( DELETED )
{
   BOOL bDeleted = FALSE;

   if( s_pCurrArea )
      SELF_DELETED( ( AREAP ) s_pCurrArea->pArea, &bDeleted );
   hb_retl( bDeleted );
}

HB_FUNC( EOF )
{
   BOOL bEof = TRUE;

   if( s_pCurrArea )
      SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
   hb_retl( bEof );
}

HB_FUNC( FCOUNT )
{
   USHORT uiFields = 0;

   if( s_pCurrArea )
      SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields );
   hb_retni( uiFields );
}

HB_FUNC( FIELDGET )
{
   PHB_ITEM pItem;
   USHORT uiField;

   pItem = hb_itemNew( NULL );
   uiField = hb_parni( 1 );

   if( s_pCurrArea && uiField )
      SELF_GETVALUE( ( AREAP ) s_pCurrArea->pArea, uiField, pItem );

   hb_itemReturn( pItem );
   hb_itemRelease( pItem );
}

HB_FUNC( FIELDNAME )
{
   if( s_pCurrArea )
   {
      USHORT uiFields, uiIndex;

      uiIndex = hb_parni( 1 );
      if( SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields ) == SUCCESS )
      {
         if( uiIndex > 0 && uiIndex <= uiFields )
         {
            char * szName;

            szName = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_FIELDNAME_LENGTH + 1 );
            SELF_FIELDNAME( ( AREAP ) s_pCurrArea->pArea, hb_parni( 1 ), szName );
            hb_retc( szName );
            hb_xfree( szName );
            return;
         }
         hb_errRT_DBCMD( EG_ARG, 1009, NULL, "FIELDNAME" );
      }
   }
   hb_retc( "" );
}

HB_FUNC( FIELDPOS )
{
   if( s_pCurrArea )
   {
      USHORT uiCount;
      LPFIELD pField;

      char szName[ HARBOUR_MAX_RDD_FIELDNAME_LENGTH ];

      hb_strncpyUpper( szName, hb_parc( 1 ), hb_parclen( 1 ) );
      uiCount = 0;
      pField = ( ( AREAP ) s_pCurrArea->pArea )->lpFields;
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

HB_FUNC( FIELDPUT )
{
   USHORT uiIndex;

   uiIndex = hb_parni( 1 );
   if( s_pCurrArea && uiIndex )
   {
      PHB_ITEM pItem;

      pItem = hb_param( 2, HB_IT_ANY );
      if( SELF_PUTVALUE( ( AREAP ) s_pCurrArea->pArea, uiIndex, pItem ) == SUCCESS )
      {
         hb_itemReturn( pItem );
         return;
      }
   }
}

HB_FUNC( FLOCK )
{
   DBLOCKINFO pLockInfo;

   pLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      pLockInfo.itmRecID = 0;
      pLockInfo.uiMethod = FILE_LOCK;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &pLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "FLOCK" );

   hb_retl( pLockInfo.fResult );
}

HB_FUNC( FOUND )
{
   BOOL bFound = FALSE;

   if( s_pCurrArea )
      SELF_FOUND( ( AREAP ) s_pCurrArea->pArea, &bFound );
   hb_retl( bFound );
}

HB_FUNC( HEADER )
{
   if( !s_pCurrArea )
      hb_retni( 0 );
   else
   {
      PHB_ITEM pRecSize;

      pRecSize = hb_itemNew( NULL );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_GETHEADERSIZE, pRecSize );
      hb_itemReturn( pRecSize );
      hb_itemRelease( pRecSize );
   }
}

HB_FUNC( INDEXORD )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pInfo;

      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      pInfo.itmOrder = NULL;
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_NUMBER, &pInfo );
      hb_retni( hb_itemGetNI( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_retni( 0 );
}

/* Same as RECCOUNT() */

HB_FUNC( LASTREC )
{
   ULONG ulRecCount = 0;

   if( s_pCurrArea )
   {
      SELF_RECCOUNT( ( AREAP ) s_pCurrArea->pArea, &ulRecCount );
      ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->ulRecCount = ulRecCount;
   }
   hb_retnl( ulRecCount );
}

HB_FUNC( LOCK )
{
   DBLOCKINFO pLockInfo;

   pLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      pLockInfo.itmRecID = 0;
      pLockInfo.uiMethod = FILE_LOCK;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &pLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "LOCK" );

   hb_retl( pLockInfo.fResult );
}

HB_FUNC( LUPDATE )
{
   if( !s_pCurrArea )
      hb_itemPutDS( &hb_stack.Return, "" );
   else
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_LASTUPDATE, &hb_stack.Return );
}

HB_FUNC( NETERR )
{
   if( ISLOG( 1 ) )
      s_bNetError = hb_parl( 1 );

   hb_retl( s_bNetError );
}

HB_FUNC( ORDBAGEXT )
{
   DBORDERINFO pInfo;

   pInfo.itmOrder = NULL;

   if( !s_pCurrArea )
   {
      LPRDDNODE pRddNode;
      AREAP pTempArea;
      USHORT uiSize, uiRddID;

      hb_rddCheck();
      uiRddID = 0;
      pRddNode = hb_rddFindNode( s_szDefDriver, &uiRddID );
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
         hb_retc( hb_itemGetCPtr( pInfo.itmResult ) );
         hb_itemRelease( pInfo.itmResult );
         SELF_RELEASE( ( AREAP ) pTempArea );
      }
      hb_xfree( pTempArea );
   }
   else
   {
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_BAGEXT, &pInfo );
      hb_retc( hb_itemGetCPtr( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
}

HB_FUNC( ORDBAGNAME )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDBAGNAME" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_BAGNAME, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDBAGNAME" );
}

HB_FUNC( ORDCONDSET )
{
   if( s_pCurrArea )
   {
      LPDBORDERCONDINFO pOrderCondInfo;
      char * szFor;
      ULONG ulLen;
      PHB_ITEM pItem;

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
      pItem = hb_param( 2, HB_IT_BLOCK );
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
      pItem = hb_param( 4, HB_IT_BLOCK );
      if( pItem )
      {
         pOrderCondInfo->itmCobWhile = hb_itemNew( NULL );
         hb_itemCopy( pOrderCondInfo->itmCobWhile, pItem );
      }
      else
         pOrderCondInfo->itmCobWhile = NULL;
      pItem = hb_param( 5, HB_IT_BLOCK );
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
      hb_retl( SELF_ORDSETCOND( ( AREAP ) s_pCurrArea->pArea, pOrderCondInfo ) == SUCCESS );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( ORDCREATE )
{
   if( s_pCurrArea )
   {
      DBORDERCREATEINFO pOrderInfo;

      pOrderInfo.abBagName = ( BYTE * ) hb_parc( 1 );
      pOrderInfo.atomBagName = ( BYTE * ) hb_parc( 2 );
      pOrderInfo.abExpr = hb_param( 3, HB_IT_STRING );
      if( ( ( strlen( ( char * ) pOrderInfo.abBagName ) == 0 ) &&
            ( strlen( ( char * ) pOrderInfo.atomBagName ) == 0 ) ) ||
          !pOrderInfo.abExpr )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDCREATE" );
         return;
      }
      pOrderInfo.itmCobExpr = hb_param( 4, HB_IT_BLOCK );
      if( ISLOG( 5 ) )
         pOrderInfo.fUnique = hb_parl( 5 );
      else
         pOrderInfo.fUnique = hb_set.HB_SET_UNIQUE;
      SELF_ORDCREATE( ( AREAP ) s_pCurrArea->pArea, &pOrderInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDCREATE" );
}

HB_FUNC( ORDDESTROY )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      SELF_ORDDESTROY( ( AREAP ) s_pCurrArea->pArea, &pOrderInfo );
   }
}

HB_FUNC( ORDFOR )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDFOR" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_CONDITION, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDFOR" );
}

HB_FUNC( ORDKEY )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDKEY" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_EXPRESSION, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDKEY" );
}

HB_FUNC( ORDLISTADD )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pOrderInfo;

      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      pOrderInfo.itmOrder = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.atomBagName )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDLISTADD" );
         return;
      }
      SELF_ORDLSTADD( ( AREAP ) s_pCurrArea->pArea, &pOrderInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDLISTADD" );
}

HB_FUNC( ORDLISTCLEAR )
{
   if( s_pCurrArea )
      SELF_ORDLSTCLEAR( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDLISTCLEAR" );
}

HB_FUNC( ORDLISTREBUILD )
{
   if( s_pCurrArea )
      SELF_ORDLSTREBUILD( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDLISTCLEAR" );
}

HB_FUNC( ORDNAME )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDNAME" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_NAME, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDNAME" );
}

HB_FUNC( ORDNUMBER )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDNUMBER" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_NUMBER, &pOrderInfo );
      hb_retni( hb_itemGetNI( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDNUMBER" );
}

HB_FUNC( ORDSETFOCUS )
{
   if( s_pCurrArea )
   {
      DBORDERINFO pInfo;

      pInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pInfo.itmOrder )
         pInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDLSTFOCUS( ( AREAP ) s_pCurrArea->pArea, &pInfo );
      hb_retc( hb_itemGetCPtr( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDSETFOCUS" );
}

HB_FUNC( RDDLIST )
{
   USHORT uiType;
   PHB_ITEM pName;
   LPRDDNODE pRddNode;

   hb_rddCheck();
   hb_arrayNew( &hb_stack.Return, 0 );
   pName = hb_itemNew( NULL );
   pRddNode = s_pRddList;
   uiType = hb_parni( 1 );       /* 0 all types of RDD's */
   while( pRddNode )
   {
      if( ( uiType == 0 ) || ( pRddNode->uiType == uiType ) )
         hb_arrayAdd( &hb_stack.Return, hb_itemPutC( pName, pRddNode->szName ) );
      pRddNode = pRddNode->pNext;
   }
   hb_itemRelease( pName );
}

HB_FUNC( RDDNAME )
{
   if( s_pCurrArea )
   {
      char * pBuffer;

      pBuffer = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
      pBuffer[ 0 ] = '\0';
      SELF_SYSNAME( ( AREAP ) s_pCurrArea->pArea, ( BYTE * ) pBuffer );
      hb_retc( pBuffer );
      hb_xfree( pBuffer );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "RDDNAME" );
      hb_retc( "" );
   }
}

HB_FUNC( RDDREGISTER )
{
   USHORT uiLen;

   hb_rddCheck();

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      char szDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];

      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szDriver, hb_parc( 1 ), uiLen );
      /*
       * hb_rddRegister returns:
       *
       * 0: Ok, RDD registered
       * 1: RDD already registerd
       * > 1: error
       */
      if( hb_rddRegister( szDriver, hb_parni( 2 ) ) > 1 )
         hb_errInternal( IE_RDDINVALID, NULL, NULL, NULL );
   }
}

/* Same as LASTREC() */

HB_FUNC( RECCOUNT )
{
   ULONG ulRecCount = 0;

   if( s_pCurrArea )
   {
      SELF_RECCOUNT( ( AREAP ) s_pCurrArea->pArea, &ulRecCount );
      ( ( AREAP ) s_pCurrArea->pArea )->lpExtendInfo->ulRecCount = ulRecCount;
   }
   hb_retnl( ulRecCount );
}

HB_FUNC( RECNO )
{
   PHB_ITEM pRecNo;

   pRecNo = hb_itemPutNL( NULL, 0 );
   if( s_pCurrArea )
      SELF_RECNO( ( AREAP ) s_pCurrArea->pArea, pRecNo );
   hb_itemReturn( pRecNo );
   hb_itemRelease( pRecNo );
}

HB_FUNC( RECSIZE )
{
   if( s_pCurrArea )
   {
      PHB_ITEM pRecSize;

      pRecSize = hb_itemNew( NULL );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_GETRECSIZE, pRecSize );
      hb_itemReturn( pRecSize );
      hb_itemRelease( pRecSize );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( RLOCK )
{
   DBLOCKINFO pLockInfo;

   pLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      PHB_ITEM pRecNo;

      pRecNo = hb_itemPutNL( NULL, 0 );
      SELF_RECNO( ( AREAP ) s_pCurrArea->pArea, pRecNo );
      pLockInfo.itmRecID = hb_itemGetNL( pRecNo );
      pLockInfo.uiMethod = REC_LOCK;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &pLockInfo );
      hb_itemRelease( pRecNo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "RLOCK" );

   hb_retl( pLockInfo.fResult );
}

HB_FUNC( SELECT )
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
      hb_retni( s_uiCurrArea );
}

HB_FUNC( USED )
{
   hb_retl( s_pCurrArea != NULL );
}

/* NOTE: Same as dbSetDriver() and rddSetDefault(), but doesn't
         throw any error if the driver doesn't exist, this is
         required in the RDDSYS INIT function, since it's not guaranteed
         that the RDD is already registered at that point. [vszakats] */

HB_FUNC( __RDDSETDEFAULT )
{
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      s_szDefDriver = ( char * ) hb_xrealloc( s_szDefDriver, uiLen + 1 );
      hb_strncpyUpper( s_szDefDriver, hb_parc( 1 ), uiLen );
   }
}

HB_FUNC( RDDSETDEFAULT )
{
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];

      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szNewDriver, hb_parc( 1 ), uiLen );

      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1015, NULL, "RDDSETDEFAULT" );
         return;
      }

      s_szDefDriver = ( char * ) hb_xrealloc( s_szDefDriver, uiLen + 1 );
      strcpy( s_szDefDriver, szNewDriver );
   }
}

HB_FUNC( DBSETDRIVER )
{
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];

      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szNewDriver, hb_parc( 1 ), uiLen );

      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1015, NULL, "DBSETDRIVER" );
         return;
      }

      s_szDefDriver = ( char * ) hb_xrealloc( s_szDefDriver, uiLen + 1 );
      strcpy( s_szDefDriver, szNewDriver );
   }
}

HB_FUNC( ORDSCOPE )
{
   PHB_ITEM pscopeValue;

   if( s_pCurrArea )
   {
      DBORDSCOPEINFO sInfo;

      if( hb_pcount() == 0 || !(hb_parinfo( 1 ) & HB_IT_NUMERIC) ||
         ( hb_pcount() > 1 && hb_parinfo( 2 ) != HB_IT_STRING ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "ORDSCOPE" );
         return;
      }
      sInfo.nScope = hb_parni( 1 );
      pscopeValue = hb_itemPutC( NULL, "" );
      SELF_SCOPEINFO( ( AREAP ) s_pCurrArea->pArea, sInfo.nScope, pscopeValue );
      hb_retc( hb_itemGetCPtr( pscopeValue ) );
      hb_itemRelease( pscopeValue );

      if( hb_pcount() > 1 )
         sInfo.scopeValue = (BYTE*) hb_parc( 2 );
      else
         sInfo.scopeValue = NULL;
      SELF_SETSCOPE( ( AREAP ) s_pCurrArea->pArea, (LPDBOPENINFO) &sInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "ORDSCOPE" );

}

HB_FUNC( DBCLEARRELATION )
{
   if( s_pCurrArea )
   {
      SELF_CLEARREL( ( AREAP ) s_pCurrArea->pArea );
   }
}

HB_FUNC( DBSETRELATION )
{

   if( s_pCurrArea )
   {
      char* szAlias;
      DBRELINFO  dbRelations;
      LPAREANODE s_pArea = NULL, pAreaNode;
      USHORT uiChildArea;

      if( hb_pcount() < 2 || ( !( hb_parinfo( 1 ) & HB_IT_NUMERIC ) && ( hb_parinfo( 1 ) != HB_IT_STRING ) ) )
      {
         hb_errRT_DBCMD( EG_ARG, 1006, NULL, "DBSETRELATION" );
         return;
      }
      if( hb_parinfo( 1 ) & HB_IT_NUMERIC )
      {
         uiChildArea = hb_parni( 1 );
      }
      else
      {
         szAlias = hb_parc( 1 );
         if( ( uiChildArea = hb_rddSelect( szAlias ) ) == 0 )
         {
            hb_errRT_BASE( EG_NOALIAS, 1002, NULL, szAlias );
            return;
         }
      }

      pAreaNode = s_pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiChildArea )
         {
            s_pArea = pAreaNode; /* Select a valid WorkArea */
            break;
         }
         pAreaNode = pAreaNode->pNext;
      }
      if( !s_pArea )
      {
         hb_errRT_BASE( EG_NOTABLE, 1002, NULL, szAlias );
         return;
      }

      dbRelations.lpaChild = s_pArea->pArea;
      dbRelations.itmCobExpr = hb_param( 2, HB_IT_BLOCK );
      dbRelations.abKey = hb_param( 3, HB_IT_STRING );
      dbRelations.lpdbriNext = NULL;

      SELF_SETREL( ( AREAP ) s_pCurrArea->pArea, (LPDBOPENINFO) &dbRelations );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, 2001, NULL, "DBSETRELATION" );
}