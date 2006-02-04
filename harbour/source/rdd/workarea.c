/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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
 *
 * The following functions are added by
 *       Horacio Roldan <harbour_ar@yahoo.com.ar>
 * hb_waCloseAux()
 *
 */

#include <ctype.h>
#include "hbapi.h"
#include "hbapirdd.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbset.h"
#include "hbrddwrk.h"


/*
 * -- METHODS --
 */

/*
 * Determine logical beginning of file.
 */
ERRCODE hb_waBof( AREAP pArea, BOOL * pBof )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waBof(%p, %p)", pArea, pBof));

   * pBof = pArea->fBof;
   return SUCCESS;
}

/*
 * Determine logical end of file.
 */
ERRCODE hb_waEof( AREAP pArea, BOOL * pEof )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waEof(%p, %p)", pArea, pEof));

   * pEof = pArea->fEof;
   return SUCCESS;
}

/*
 * Determine outcome of the last search operation.
 */
ERRCODE hb_waFound( AREAP pArea, BOOL * pFound )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waFound(%p, %p)", pArea, pFound));

   * pFound = pArea->fFound;
   return SUCCESS;
}

/*
 * Reposition cursor relative to current position.
 */
ERRCODE hb_waSkip( AREAP pArea, LONG lToSkip )
{
   LONG lSkip;

   HB_TRACE(HB_TR_DEBUG, ("hb_waSkip(%p, %ld)", pArea, lToSkip));

   /* Flush record and exit */
   if( lToSkip == 0 )
      return SELF_SKIPRAW( pArea, 0 );

   pArea->fTop = pArea->fBottom = FALSE;

   if( lToSkip > 0 )
      lSkip = 1;
   else
   {
      lSkip = -1;
      lToSkip *= -1;
   }
   while( lToSkip-- > 0 )
   {
      SELF_SKIPRAW( pArea, lSkip );
      SELF_SKIPFILTER( pArea, lSkip );
      if( pArea->fBof || pArea->fEof )
         break;
   }

   /* Update Bof and Eof flags */
   if( lSkip < 0 )
      pArea->fEof = FALSE;
   else /* ( lSkip > 0 ) */
      pArea->fBof = FALSE;

   return SUCCESS;
}

/*
 * Reposition cursor respecting any filter setting.
 */
ERRCODE hb_waSkipFilter( AREAP pArea, LONG lUpDown )
{
   BOOL fBottom, fDeleted;
   PHB_ITEM pResult;
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_waSkipFilter(%p, %ld)", pArea, lUpDown));

   if( !hb_set.HB_SET_DELETED && pArea->dbfi.itmCobExpr == NULL )
      return SUCCESS;

   /* Since lToSkip is passed to SkipRaw, it should never request more than
      a single skip.
      The implied purpose of hb_waSkipFilter is to get off of a "bad" record
      after a skip was performed, NOT to skip lToSkip filtered records.
   */
   lUpDown = ( lUpDown < 0  ? -1 : 1 );

   /* remember if we are here after SLEF_GOTOP() */
   fBottom = pArea->fBottom;

   while ( !pArea->fBof && !pArea->fEof )
   {
      /* SET DELETED */
      if( hb_set.HB_SET_DELETED )
      {
         SELF_DELETED( pArea, &fDeleted );
         if( fDeleted )
         {
            if ( SELF_SKIPRAW( pArea, lUpDown ) != SUCCESS )
               return FAILURE;
            continue;
         }
      }

      /* SET FILTER TO */
      if( pArea->dbfi.itmCobExpr )
      {
         pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
         if( HB_IS_LOGICAL( pResult ) && !hb_itemGetL( pResult ) )
         {
            if ( SELF_SKIPRAW( pArea, lUpDown ) != SUCCESS )
               return FAILURE;
            continue;
         }
      }

      break;
   }

   /*
    * The only one situation when we should repos is backward skipping
    * if we are at BOTTOM position (it's SKIPFILTER called from GOBOTTOM)
    * then GOEOF() if not then GOTOP()
    */
   if( pArea->fBof && lUpDown < 0 )
   {
      if ( fBottom )
      {
         /* GOTO EOF (phantom) record -
            this is the only one place where GOTO is used by xHarbour
            directly and RDD which does not operate on numbers should
            serve this method only as SELF_GOEOF() synonym. If it's a
            problem then we can remove this if and always use SELF_GOTOP()
            but it also means second table scan if all records filtered
            are out of filter so I do not want to do that. I will prefer
            explicit add SELF_GOEOF() method
          */
         uiError = SELF_GOTO( pArea, 0 );
      }
      else
      {
         uiError = SELF_GOTOP( pArea );
         pArea->fBof = TRUE;
      }
   }
   else
   {
      uiError = SUCCESS;
   }

   return uiError;
}

/*
 * Add a field to the WorkArea.
 */
ERRCODE hb_waAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;
   char szFieldName[ HB_SYMBOL_NAME_LEN + 1 ], *szPtr;

   HB_TRACE(HB_TR_DEBUG, ("hb_waAddField(%p, %p)", pArea, pFieldInfo));

   /* Validate the name of field */
   szPtr = ( char * ) pFieldInfo->atomName;
   while( HB_ISSPACE( *szPtr ) )
   {
      ++szPtr;
   }
   hb_strncpyUpperTrim( szFieldName, szPtr, HB_SYMBOL_NAME_LEN );
   if( strlen( szFieldName ) == 0 )
      return FAILURE;

   pField = pArea->lpFields + pArea->uiFieldCount;
   if( pArea->uiFieldCount > 0 )
      ( ( LPFIELD ) ( pField - 1 ) )->lpfNext = pField;
   pField->sym = ( void * ) hb_dynsymGetCase( szFieldName );
   pField->uiType = pFieldInfo->uiType;
   pField->uiTypeExtended = pFieldInfo->uiTypeExtended;
   pField->uiLen = pFieldInfo->uiLen;
   pField->uiDec = pFieldInfo->uiDec;
   pField->uiArea = pArea->uiArea;
   pArea->uiFieldCount ++;
   return SUCCESS;
}

/*
 * Add all fields defined in an array to the WorkArea.
 */
ERRCODE hb_waCreateFields( AREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiItems, uiCount, uiLen, uiDec;
   DBFIELDINFO pFieldInfo;
   PHB_ITEM pFieldDesc;
   int iData;

   HB_TRACE(HB_TR_DEBUG, ("hb_waCreateFields(%p, %p)", pArea, pStruct));

   uiItems = ( USHORT ) hb_arrayLen( pStruct );
   SELF_SETFIELDEXTENT( pArea, uiItems );

   for( uiCount = 0; uiCount < uiItems; uiCount++ )
   {
      pFieldInfo.uiTypeExtended = 0;
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiCount + 1 );
      pFieldInfo.atomName = ( BYTE * ) hb_arrayGetCPtr( pFieldDesc, 1 );
      iData = hb_arrayGetNI( pFieldDesc, 3 );
      if( iData < 0 )
         iData = 0;
      uiLen = pFieldInfo.uiLen = ( USHORT ) iData;
      iData = hb_arrayGetNI( pFieldDesc, 4 );
      if( iData < 0 )
         iData = 0;
      uiDec = ( USHORT ) iData;
      pFieldInfo.uiDec = 0;
      iData = toupper( hb_arrayGetCPtr( pFieldDesc, 2 )[ 0 ] );
      switch( iData )
      {
         case 'C':
            pFieldInfo.uiType = HB_IT_STRING;
            pFieldInfo.uiLen = uiLen;
/* Too many people reported the behavior with code below as a
   Clipper compatibility bug so I commented this code. Druzus.
#ifdef HB_C52_STRICT
            pFieldInfo.uiLen = uiLen;
#else
            pFieldInfo.uiLen = uiLen + uiDec * 256;
#endif
*/
            break;

         case 'L':
            pFieldInfo.uiType = HB_IT_LOGICAL;
            pFieldInfo.uiLen = 1;
            break;

         case 'M':
            pFieldInfo.uiType = HB_IT_MEMO;
            pFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            break;

         case 'V':
            pFieldInfo.uiType = HB_IT_ANY;
            pFieldInfo.uiLen = ( uiLen < 3 || uiLen == 5 ) ? 6 : uiLen;
            break;

         case 'D':
            pFieldInfo.uiType = HB_IT_DATE;
            pFieldInfo.uiLen = ( uiLen == 3 || uiLen == 4 ) ? uiLen : 8;
            break;

         case 'I':
            pFieldInfo.uiType = HB_IT_INTEGER;
            pFieldInfo.uiLen = ( ( uiLen > 0 && uiLen <= 4 ) || uiLen != 8 ) ? uiLen : 4;
            break;

         case '2':
         case '4':
            pFieldInfo.uiType = HB_IT_INTEGER;
            pFieldInfo.uiLen = iData - '0';
            break;

         case 'B':
         case '8':
            pFieldInfo.uiType = HB_IT_DOUBLE;
            pFieldInfo.uiLen = 8;
            pFieldInfo.uiDec = uiDec;
            break;

         case 'N':
         case 'F':
            pFieldInfo.uiType = HB_IT_LONG;
            /* DBASE documentation defines maximum numeric field size as 20
             * but Clipper alows to create longer fileds so I remove this
             * limit, Druzus
             */
            /*
            if( uiLen > 20 )
            */
            if( uiLen > 255 )
               return FAILURE;
            else
               pFieldInfo.uiDec = uiDec;
            break;

         default:
            return FAILURE;
      }
      /* Add field */
      if( SELF_ADDFIELD( pArea, &pFieldInfo ) == FAILURE )
         return FAILURE;
   }
   return SUCCESS;
}

/*
 * Determine the number of fields in the WorkArea.
 */
ERRCODE hb_waFieldCount( AREAP pArea, USHORT * uiFields )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waFieldCount(%p, %p)", pArea, uiFields));

   * uiFields = pArea->uiFieldCount;
   return SUCCESS;
}

/*
 * Retrieve information about a field.
 */
ERRCODE hb_waFieldInfo( AREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_waFieldInfo(%p, %hu, %hu, %p)", pArea, uiIndex, uiType, pItem));

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   switch( uiType )
   {
      case DBS_NAME:
         hb_itemPutC( pItem, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
         break;

      case DBS_TYPE:
         switch( pField->uiType )
         {
            case HB_IT_STRING:
               hb_itemPutC( pItem, "C" );
               break;

            case HB_IT_LOGICAL:
               hb_itemPutC( pItem, "L" );
               break;

            case HB_IT_MEMO:
               hb_itemPutC( pItem, "M" );
               break;

            case HB_IT_ANY:
               hb_itemPutC( pItem, "V" );
               break;

            case HB_IT_DATE:
               hb_itemPutC( pItem, "D" );
               break;

            case HB_IT_LONG:
               hb_itemPutC( pItem, "N" );
               break;

            case HB_IT_INTEGER:
               hb_itemPutC( pItem, "I" );
               break;

            case HB_IT_DOUBLE:
               hb_itemPutC( pItem, "B" );
               break;

            default:
               hb_itemPutC( pItem, "U" );
               break;
         }
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

/*
 * Determine the name associated with a field number.
 */
ERRCODE hb_waFieldName( AREAP pArea, USHORT uiIndex, void * szName )
{
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_waFieldName(%p, %hu, %p)", pArea, uiIndex, szName));

   if( uiIndex > pArea->uiFieldExtent )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   hb_strncpy( ( char * ) szName, hb_dynsymName( ( PHB_DYNS ) pField->sym ),
               pArea->uiMaxFieldNameLength );
   return SUCCESS;
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
ERRCODE hb_waSetFieldExtent( AREAP pArea, USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waSetFieldExtent(%p, %hu)", pArea, uiFieldExtent));

   pArea->uiFieldExtent = uiFieldExtent;

   /* Alloc field array */
   pArea->lpFields = ( LPFIELD ) hb_xgrab( uiFieldExtent * sizeof( FIELD ) );
   memset( pArea->lpFields, 0, uiFieldExtent * sizeof( FIELD ) );

   return SUCCESS;
}

/*
 * Obtain the alias of the WorkArea.
 */
ERRCODE hb_waAlias( AREAP pArea, BYTE * szAlias )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waAlias(%p, %p)", pArea, szAlias));

   hb_strncpy( ( char * ) szAlias,
      pArea->atomAlias && hb_dynsymAreaHandle( ( PHB_DYNS ) pArea->atomAlias )
      ? hb_dynsymName( ( PHB_DYNS ) pArea->atomAlias ) : "",
      HARBOUR_MAX_RDD_ALIAS_LENGTH );

   return SUCCESS;
}

/*
 * Close the table in the WorkArea - helper function
 */
static short hb_waCloseAux( AREAP pArea, int nChildArea )
{
   USHORT uiPrevArea, uiArea;
   LPDBRELINFO lpdbRelation, lpdbRelPrev, lpdbRelTmp;

   uiArea = ( USHORT ) nChildArea;
   if ( pArea->lpdbRelations )
   {
      uiPrevArea = hb_rddGetCurrentWorkAreaNumber();
      lpdbRelation = pArea->lpdbRelations;
      lpdbRelPrev = NULL;
      while ( lpdbRelation ) {
         if ( lpdbRelation->lpaChild->uiArea == uiArea ) {
            /* Clear this relation */
            hb_rddSelectWorkAreaNumber( lpdbRelation->lpaChild->uiArea );
            SELF_CHILDEND( lpdbRelation->lpaChild, lpdbRelation );
            hb_rddSelectWorkAreaNumber( uiPrevArea );
            if( lpdbRelation->itmCobExpr )
            {
               hb_itemRelease( lpdbRelation->itmCobExpr );
            }
            if( lpdbRelation->abKey )
               hb_itemRelease( lpdbRelation->abKey );
            lpdbRelTmp = lpdbRelation;
            if ( lpdbRelPrev )
               lpdbRelPrev->lpdbriNext = lpdbRelation->lpdbriNext;
            else
               pArea->lpdbRelations = lpdbRelation->lpdbriNext;
            lpdbRelation = lpdbRelation->lpdbriNext;
            hb_xfree( lpdbRelTmp );
         }
         else
         {
            lpdbRelPrev  = lpdbRelation;
            lpdbRelation = lpdbRelation->lpdbriNext;
         }
      }
   }
   return 1;
}

/*
 * Close the table in the WorkArea.
 */
ERRCODE hb_waClose( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waClose(%p)", pArea));

   /* Clear items */
   SELF_CLEARFILTER( pArea );
   SELF_CLEARREL( pArea );
   SELF_CLEARLOCATE( pArea );

   if( pArea->uiParents > 0 )
   {
      /* Clear relations that has this area as a child */
      hb_rddIterateWorkAreas ( hb_waCloseAux, pArea->uiArea );
   }

   if( pArea->atomAlias )
      hb_dynsymSetAreaHandle( ( PHB_DYNS ) pArea->atomAlias, 0 );

   return SUCCESS;
}

/*
 * Retrieve information about the current driver.
 */
ERRCODE hb_waInfo( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waInfo(%p, %hu, %p)", pArea, uiIndex, pItem));
   HB_SYMBOL_UNUSED( pArea );

   switch ( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
         hb_itemPutL( pItem, FALSE );
         break;

      /*
       * IMHO better to return FAILURE to notice that it's not supported
       */
      case DBI_GETDELIMITER:
      case DBI_SETDELIMITER:
      case DBI_SEPARATOR:
         hb_itemPutC( pItem, "" );
         return FAILURE;

      case DBI_CHILDCOUNT:
      {
         LPDBRELINFO lpdbRelations = pArea->lpdbRelations;
         USHORT uiCount = 0;
         while( lpdbRelations )
         {
            uiCount++;
            lpdbRelations = lpdbRelations->lpdbriNext;
         }
         hb_itemPutNI( pItem, uiCount );
         break;
      }

      case DBI_BOF:
         hb_itemPutL( pItem, pArea->fBof );
         break;

      case DBI_EOF:
         hb_itemPutL( pItem, pArea->fEof );
         break;

      case DBI_DBFILTER:
         if ( pArea->dbfi.abFilterText )
            hb_itemCopy( pItem, pArea->dbfi.abFilterText );
         else
            hb_itemPutC( pItem, "" );
         break;

      case DBI_FOUND:
         hb_itemPutL( pItem, pArea->fFound );
         break;

      case DBI_FCOUNT:
         hb_itemPutNI( pItem, pArea->uiFieldCount );
         break;

      case DBI_ALIAS:
      {
         char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
         if ( SELF_ALIAS( pArea, ( BYTE * ) szAlias ) != SUCCESS )
         {
            return FAILURE;
         }
         hb_itemPutC( pItem, szAlias );
         break;
      }

      case DBI_RM_SUPPORTED:
         hb_itemPutL( pItem, FALSE );
         break;

      case DBI_DB_VERSION:
         hb_itemPutC( pItem, "" );
         break;

      case DBI_RDD_VERSION:
         hb_itemPutC( pItem, "" );
         break;

      default:
         return FAILURE;
   }
   return SUCCESS;
}

/*
 * Retrieve information about the current order that SELF could not.
 * Called by SELF_ORDINFO if uiIndex is not supported.
 */
#ifdef HB_COMPAT_C53
ERRCODE hb_waOrderInfo( AREAP pArea, USHORT index, LPDBORDERINFO param )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waOrderInfo(%p, %hu, %p)", pArea, index, param));

   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( index );
   HB_SYMBOL_UNUSED( param );

   hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "ORDERINFO" );
   return FAILURE;
}
#endif

/*
 * Clear the WorkArea for use.
 */
ERRCODE hb_waNewArea( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waNewArea(%p)", pArea));

   pArea->valResult = hb_itemNew( NULL );
   pArea->lpdbRelations = NULL;
   pArea->uiParents = 0;
   pArea->uiMaxFieldNameLength = 10;

   return SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 * Like in Clipper it's also mapped as Create() method at WA level
 */
ERRCODE hb_waOpen( AREAP pArea, LPDBOPENINFO pInfo )
{
   if( !pArea->atomAlias && pInfo->atomAlias && pInfo->atomAlias[ 0 ] )
   {
      pArea->atomAlias = hb_rddAllocWorkAreaAlias( ( char * ) pInfo->atomAlias,
                                                   ( int ) pInfo->uiArea );
      if( ! pArea->atomAlias )
      {
         SELF_CLOSE( ( AREAP ) pArea );
         return FAILURE;
      }
   }
   return SUCCESS;
}

ERRCODE hb_waOrderCondition( AREAP pArea, LPDBORDERCONDINFO param )
{
   if( pArea->lpdbOrdCondInfo )
   {
      if( pArea->lpdbOrdCondInfo->abFor )
         hb_xfree( pArea->lpdbOrdCondInfo->abFor );
      if( pArea->lpdbOrdCondInfo->abWhile )
         hb_xfree( pArea->lpdbOrdCondInfo->abWhile );
      if( pArea->lpdbOrdCondInfo->itmCobFor )
      {
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmCobFor );
      }
      if( pArea->lpdbOrdCondInfo->itmCobWhile )
      {
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmCobWhile );
      }
      if( pArea->lpdbOrdCondInfo->itmCobEval )
      {
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmCobEval );
      }
      if( pArea->lpdbOrdCondInfo->itmStartRecID )
      {
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmStartRecID );
      }
      if( pArea->lpdbOrdCondInfo->itmRecID )
      {
         hb_itemRelease( pArea->lpdbOrdCondInfo->itmRecID );
      }
      hb_xfree( pArea->lpdbOrdCondInfo );
   }
   pArea->lpdbOrdCondInfo = param;

   return SUCCESS;
}

/*
 * Release all references to a WorkArea.
 */
ERRCODE hb_waRelease( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waRelease(%p)", pArea));

   /* Free all allocated pointers */
   if( pArea->lpFields )
      hb_xfree( pArea->lpFields );
   if( pArea->valResult )
      hb_itemRelease( pArea->valResult );
   if( pArea->lpdbOrdCondInfo )
      /* intentionally direct call not a method */
      hb_waOrderCondition( pArea,NULL );
   hb_xfree( pArea );
   return SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
ERRCODE hb_waStructSize( AREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( AREA );
   return SUCCESS;
}

/*
 * Obtain the name of replaceable database driver (RDD) subsystem.
 */
ERRCODE hb_waSysName( AREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waSysName(%p, %p)", pArea, pBuffer));
   HB_SYMBOL_UNUSED( pArea );

   pBuffer[ 0 ] = 0;
   return SUCCESS;
}

/*
 * Evaluate code block for each record in WorkArea.
 */
ERRCODE hb_waEval( AREAP pArea, LPDBEVALINFO pEvalInfo )
{
   LONG lNext = 1;
   BOOL fEof;

   HB_TRACE(HB_TR_DEBUG, ("hb_waEval(%p, %p)", pArea, pEvalInfo));

   if( pEvalInfo->dbsci.itmRecID )
   {
      if( SELF_GOTOID( pArea, pEvalInfo->dbsci.itmRecID ) == FAILURE )
         return FAILURE;
   }
   else if( pEvalInfo->dbsci.lNext )
   {
      lNext = hb_itemGetNL( pEvalInfo->dbsci.lNext );
   }
   else if( !pEvalInfo->dbsci.itmCobWhile &&
            !hb_itemGetL( pEvalInfo->dbsci.fRest ) )
   {
      if( SELF_GOTOP( pArea ) == FAILURE )
         return FAILURE;
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   if( !pEvalInfo->dbsci.lNext || lNext > 0 )
   {
      while( TRUE )
      {
         if( SELF_EOF( pArea, &fEof ) == FAILURE )
            return FAILURE;

         if( fEof )
            break;

         if( pEvalInfo->dbsci.itmCobWhile &&
             ! hb_itemGetL( hb_vmEvalBlock( pEvalInfo->dbsci.itmCobWhile ) ) )
            break;

         if( ! pEvalInfo->dbsci.itmCobFor ||
             hb_itemGetL( hb_vmEvalBlock( pEvalInfo->dbsci.itmCobFor ) ) )
            hb_vmEvalBlock( pEvalInfo->itmBlock );

         if( pEvalInfo->dbsci.itmRecID || ( pEvalInfo->dbsci.lNext && --lNext < 1 ) )
            break;

         if( SELF_SKIP( pArea, 1 ) == FAILURE )
            return FAILURE;
      }
   }

   return SUCCESS;
}

/*
 * Locate a record which pass given condition
 */
ERRCODE hb_waLocate( AREAP pArea, BOOL fContinue )
{
   LONG lNext = 1;
   BOOL fEof;

   HB_TRACE(HB_TR_DEBUG, ("hb_waLocate(%p, %d)", pArea, fContinue));

   if( fContinue )
   {
      if( ! pArea->dbsi.itmCobFor )
         return SUCCESS;

      if ( SELF_SKIP( pArea, 1 ) == FAILURE )
         return FAILURE;
   }
   else if( pArea->dbsi.itmRecID )
   {
      if( SELF_GOTOID( pArea, pArea->dbsi.itmRecID ) == FAILURE )
         return FAILURE;
   }
   else if( pArea->dbsi.lNext )
   {
      lNext = hb_itemGetNL( pArea->dbsi.lNext );
   }
   else if( !pArea->dbsi.itmCobWhile &&
            !hb_itemGetL( pArea->dbsi.fRest ) )
   {
      if( SELF_GOTOP( pArea ) == FAILURE )
         return FAILURE;
   }

   pArea->fFound = FALSE;

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   if( !pArea->dbsi.lNext || lNext > 0 )
   {
      while( TRUE )
      {
         if( SELF_EOF( pArea, &fEof ) == FAILURE )
            return FAILURE;

         if( fEof )
            break;

         if( !fContinue && pArea->dbsi.itmCobWhile &&
             ! hb_itemGetL( hb_vmEvalBlock( pArea->dbsi.itmCobWhile ) ) )
            break;

         if( ! pArea->dbsi.itmCobFor ||
             hb_itemGetL( hb_vmEvalBlock( pArea->dbsi.itmCobFor ) ) )
         {
            pArea->fFound = TRUE;
            break;
         }

         if( !fContinue &&
             ( pArea->dbsi.itmRecID || ( pArea->dbsi.lNext && --lNext < 1 ) ) )
            break;

         if( SELF_SKIP( pArea, 1 ) == FAILURE )
            return FAILURE;
      }
   }

   return SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
ERRCODE hb_waTrans( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   LONG lNext = 1;
   BOOL fEof;

   HB_TRACE(HB_TR_DEBUG, ("hb_waTrans(%p, %p)", pArea, pTransInfo));

   if( pTransInfo->dbsci.itmRecID )
   {
      if( SELF_GOTOID( pArea, pTransInfo->dbsci.itmRecID ) == FAILURE )
         return FAILURE;
   }
   else if( pTransInfo->dbsci.lNext )
   {
      lNext = hb_itemGetNL( pTransInfo->dbsci.lNext );
   }
   else if( !pTransInfo->dbsci.itmCobWhile &&
            !hb_itemGetL( pTransInfo->dbsci.fRest ) )
   {
      if( SELF_GOTOP( pArea ) == FAILURE )
         return FAILURE;
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   if( !pTransInfo->dbsci.lNext || lNext > 0 )
   {
      while( TRUE )
      {
         if( SELF_EOF( pArea, &fEof ) == FAILURE )
            return FAILURE;

         if( fEof )
            break;

         if( pTransInfo->dbsci.itmCobWhile &&
             ! hb_itemGetL( hb_vmEvalBlock( pTransInfo->dbsci.itmCobWhile ) ) )
            break;

         if( ! pTransInfo->dbsci.itmCobFor ||
             hb_itemGetL( hb_vmEvalBlock( pTransInfo->dbsci.itmCobFor ) ) )
         {
            if( SELF_TRANSREC( pArea, pTransInfo ) == FAILURE )
               return FAILURE;
         }

         if( pTransInfo->dbsci.itmRecID || ( pTransInfo->dbsci.lNext && --lNext < 1 ) )
            break;

         if( SELF_SKIP( pArea, 1 ) == FAILURE )
            return FAILURE;
      }
   }

   return SUCCESS;
}

/*
 * Copy a record to another WorkArea.
 */
ERRCODE hb_waTransRec( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   BOOL bDeleted;
   BYTE *pRecord;
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_waTransRec(%p, %p)", pArea, pTransInfo));

   /* Record deleted? */
   errCode = SELF_DELETED( ( AREAP ) pArea, &bDeleted );
   if( errCode != SUCCESS )
      return errCode;

   if( pTransInfo->uiFlags & DBTF_MATCH && pTransInfo->uiFlags & DBTF_PUTREC )
   {
      errCode = SELF_GETREC( ( AREAP ) pArea, &pRecord );
      if( errCode != SUCCESS )
         return errCode;

      /* Append a new record */
      errCode = SELF_APPEND( ( AREAP ) pTransInfo->lpaDest, TRUE );
      if( errCode != SUCCESS )
         return errCode;

      /* Copy record */
      errCode = SELF_PUTREC( ( AREAP ) pTransInfo->lpaDest, pRecord );
   }
   else
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      LPDBTRANSITEM pTransItem;
      USHORT uiCount;

      /* Append a new record */
      errCode = SELF_APPEND( ( AREAP ) pTransInfo->lpaDest, TRUE );
      if( errCode != SUCCESS )
         return errCode;

      pTransItem = pTransInfo->lpTransItems;
      for( uiCount = pTransInfo->uiItemCount; uiCount; --uiCount )
      {
         errCode = SELF_GETVALUE( ( AREAP ) pArea,
                                  pTransItem->uiSource, pItem );
         if( errCode != SUCCESS )
            break;
         errCode = SELF_PUTVALUE( ( AREAP ) pTransInfo->lpaDest,
                                  pTransItem->uiDest, pItem );
         if( errCode != SUCCESS )
            break;
         ++pTransItem;
      }
      hb_itemRelease( pItem );
   }

   /* Delete the new record if copy fail */
   if( errCode != SUCCESS )
   {
      SELF_DELETE( ( AREAP ) pTransInfo->lpaDest );
      return errCode;
   }

   /* Delete the new record */
   if( bDeleted )
      return SELF_DELETE( ( AREAP ) pTransInfo->lpaDest );

   return SUCCESS;
}

/*
 * Report end of relation.
 */
ERRCODE hb_waChildEnd( AREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waChildEnd(%p, %p)", pArea, pRelInfo));

   if( pRelInfo->isScoped )
   {
      DBORDERINFO pInfo;
      pInfo.itmOrder = NULL;
      pInfo.atomBagName = NULL;
      pInfo.itmResult = hb_itemNew( NULL );
      pInfo.itmNewVal = NULL;
      SELF_ORDINFO( pArea, DBOI_SCOPETOPCLEAR, &pInfo );
      SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOMCLEAR, &pInfo );
      hb_itemRelease( pInfo.itmResult );
   }

   pArea->uiParents--;
   return SUCCESS;
}

/*
 * Report initialization of a relation.
 */
ERRCODE hb_waChildStart( AREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waChildStart(%p, %p)", pArea, pRelInfo));
   HB_SYMBOL_UNUSED( pRelInfo );

   pArea->uiParents ++;
   return SUCCESS;
}

/*
 * Force relational movement in child WorkAreas.
 */
ERRCODE hb_waSyncChildren( AREAP pArea )
{

   LPDBRELINFO lpdbRelation;
   HB_TRACE(HB_TR_DEBUG, ("hb_waSyncChildren(%p)", pArea));

   lpdbRelation = pArea->lpdbRelations;
   while( lpdbRelation )
   {
      SELF_CHILDSYNC( lpdbRelation->lpaChild, lpdbRelation );
      lpdbRelation = lpdbRelation->lpdbriNext;
   }

   return SUCCESS;
}

/*
 * Clear all relations in the specified WorkArea.
 */
ERRCODE hb_waClearRel( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waClearRel(%p)", pArea ));

   /* Free all relations */
   if( pArea->lpdbRelations )
   {
      int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

      do
      {
         LPDBRELINFO lpdbRelation = pArea->lpdbRelations;

         hb_rddSelectWorkAreaNumber( lpdbRelation->lpaChild->uiArea );
         SELF_CHILDEND( lpdbRelation->lpaChild, lpdbRelation );
         pArea->lpdbRelations = lpdbRelation->lpdbriNext;

         if( lpdbRelation->itmCobExpr )
         {
            hb_itemRelease( lpdbRelation->itmCobExpr );
         }
         if( lpdbRelation->abKey )
         {
            hb_itemRelease( lpdbRelation->abKey );
         }
         hb_xfree( lpdbRelation );
      }
      while( pArea->lpdbRelations );

      hb_rddSelectWorkAreaNumber( iCurrArea );
   }

   return SUCCESS;
}

/*
 * Obtain the workarea number of the specified relation.
 */
ERRCODE hb_waRelArea( AREAP pArea, USHORT uiRelNo, void * pRelArea )
{
   LPDBRELINFO lpdbRelations;
   USHORT uiIndex = 1;
   USHORT* pWA = (USHORT *) pRelArea ;
   /*TODO: Why pRelArea declared as void*? This creates casting hassles.*/

   HB_TRACE(HB_TR_DEBUG, ("hb_waRelArea(%p, %hu, %p)", pArea, uiRelNo, pRelArea));

   *pWA = 0;
   lpdbRelations = pArea->lpdbRelations;
   while( lpdbRelations )
   {
      if ( uiIndex++ == uiRelNo )
      {
         *pWA = lpdbRelations->lpaChild->uiArea;
         break;
      }
      lpdbRelations = lpdbRelations->lpdbriNext;
   }
   return *pWA ? SUCCESS : FAILURE ;
}

/*
 * Evaluate a block against the relation in specified WorkArea.
 */
ERRCODE hb_waRelEval( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PHB_ITEM pResult;
   DBORDERINFO pInfo;
   ERRCODE errCode;
   int iOrder;

   HB_TRACE(HB_TR_DEBUG, ("hb_waRelEval(%p, %p)", pArea, pRelInfo));

   errCode = SELF_EVALBLOCK( pRelInfo->lpaParent, pRelInfo->itmCobExpr );

   if( errCode == SUCCESS )
   {
      /*
       *  Check the current order
       */
      pResult = pRelInfo->lpaParent->valResult;
      pRelInfo->lpaParent->valResult = NULL;
      memset( &pInfo, 0, sizeof( DBORDERINFO ) );
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      errCode = SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );

      if( errCode == SUCCESS )
      {
         iOrder = hb_itemGetNI( pInfo.itmResult );
         if( iOrder != 0 )
         {
            if( pRelInfo->isScoped )
            {
               pInfo.itmNewVal = pResult;
               errCode = SELF_ORDINFO( pArea, DBOI_SCOPETOP, &pInfo );
               if( errCode == SUCCESS )
                  errCode = SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOM, &pInfo );
            }
            if( errCode == SUCCESS )
               errCode = SELF_SEEK( pArea, FALSE, pResult, FALSE );
         }
         else
         {
            /*
             * If current order equals to zero, use GOTOID instead of SEEK
             * Unfortunately it interacts with buggy .prg code which returns
             * non numerical values from relation expression and RDD accepts
             * only numerical record ID. In such case SELF_GOTO() works like
             * SELF_GOEOF() but SELF_GOTOID() reports error. So for Clipper
             * compatibility SELF_GOTO() is used here but if RDD can use
             * non numerical record IDs then this method should be overloaded
             * to use SELF_GOTOID(), [druzus]
             */
            /* errCode = SELF_GOTOID( pArea, pResult ); */
            errCode = SELF_GOTO( pArea, hb_itemGetNL( pResult ) );
         }
      }
      hb_itemRelease( pInfo.itmResult );
      hb_itemRelease( pResult );
   }

   return errCode;
}

/*
 * Obtain the character expression of the specified relation.
 */
ERRCODE hb_waRelText( AREAP pArea, USHORT uiRelNo, void * pExpr )
{
   LPDBRELINFO lpdbRelations;
   USHORT uiIndex = 1;

   HB_TRACE(HB_TR_DEBUG, ("hb_waRelText(%p, %hu, %p)", pArea, uiRelNo, pExpr));

   lpdbRelations = pArea->lpdbRelations;

   while( lpdbRelations )
   {
      if ( uiIndex++ == uiRelNo )
      {
         hb_strncpy( ( char* ) pExpr, hb_itemGetCPtr( lpdbRelations->abKey ),
                     HARBOUR_MAX_RDD_RELTEXT_LENGTH );
         return SUCCESS;
      }
      lpdbRelations = lpdbRelations->lpdbriNext;
   }
   * ( char * ) pExpr = 0;
   return FAILURE;
}

/*
 * Set a relation in the parent file.
 */
ERRCODE hb_waSetRel( AREAP pArea, LPDBRELINFO lpdbRelInf )
{
   LPDBRELINFO lpdbRelations;

   HB_TRACE(HB_TR_DEBUG, ("hb_waSetRel(%p, %p)", pArea, lpdbRelInf));

   lpdbRelations = pArea->lpdbRelations;
   if( ! lpdbRelations )
   {
      pArea->lpdbRelations = ( LPDBRELINFO ) hb_xgrab( sizeof( DBRELINFO ) );
      lpdbRelations = pArea->lpdbRelations;
   }
   else
   {
      while( lpdbRelations->lpdbriNext )
         lpdbRelations = lpdbRelations->lpdbriNext;
      lpdbRelations->lpdbriNext = ( LPDBRELINFO ) hb_xgrab( sizeof( DBRELINFO ) );
      lpdbRelations = lpdbRelations->lpdbriNext;
   }
   lpdbRelations->lpaParent = pArea;
   lpdbRelations->lpaChild = lpdbRelInf->lpaChild;
   lpdbRelations->itmCobExpr = lpdbRelInf->itmCobExpr;
   lpdbRelations->isScoped = lpdbRelInf->isScoped;
   lpdbRelations->isOptimized = lpdbRelInf->isOptimized;
   lpdbRelations->abKey = lpdbRelInf->abKey;
   lpdbRelations->lpdbriNext = lpdbRelInf->lpdbriNext;

   SELF_CHILDSTART( ( AREAP ) lpdbRelInf->lpaChild, lpdbRelations );

   return SUCCESS;
}

/*
 * Clear the active filter expression.
 */
ERRCODE hb_waClearFilter( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waClearFilter(%p)", pArea));

   /* Free all items */
   if( pArea->dbfi.itmCobExpr )
   {
      hb_itemRelease( pArea->dbfi.itmCobExpr );
      pArea->dbfi.itmCobExpr = NULL;
   }
   if( pArea->dbfi.abFilterText )
   {
      hb_itemRelease( pArea->dbfi.abFilterText );
      pArea->dbfi.abFilterText = NULL;
   }
   pArea->dbfi.fFilter = FALSE;

   return SUCCESS;
}

/*
 * Clear the active locate expression.
 */
ERRCODE hb_waClearLocate( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waClearLocate(%p)", pArea));

   /* Free all items */
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

/*
 * Return filter condition of the specified WorkArea.
 */
ERRCODE hb_waFilterText( AREAP pArea, PHB_ITEM pFilter )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waFilterText(%p, %p)", pArea, pFilter));

   if( pArea->dbfi.abFilterText )
      hb_itemCopy( pFilter, pArea->dbfi.abFilterText );

   return SUCCESS;
}

/*
 * Set the filter condition for the specified WorkArea.
 */
ERRCODE hb_waSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waSetFilter(%p, %p)", pArea, pFilterInfo));

   /* Clear the active filter expression */
   SELF_CLEARFILTER( pArea );

   if( pFilterInfo->itmCobExpr )
   {
      pArea->dbfi.itmCobExpr = hb_itemNew( pFilterInfo->itmCobExpr );
   }
   if( pFilterInfo->abFilterText )
   {
      pArea->dbfi.abFilterText = hb_itemNew( pFilterInfo->abFilterText );
   }
   pArea->dbfi.fOptimized = pArea->dbfi.fOptimized;
   pArea->dbfi.fFilter = TRUE;

   return SUCCESS;
}

/*
 * Set the locate scope for the specified WorkArea.
 */
ERRCODE hb_waSetLocate( AREAP pArea, LPDBSCOPEINFO pScopeInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waSetLocate(%p, %p)", pArea, pScopeInfo));

   /* Clear the active locate expression */
   SELF_CLEARLOCATE( pArea );

   if( pScopeInfo->itmCobFor )
      pArea->dbsi.itmCobFor = hb_itemNew( pScopeInfo->itmCobFor );

   if( pScopeInfo->lpstrFor )
      pArea->dbsi.lpstrFor = hb_itemNew( pScopeInfo->lpstrFor );

   if( pScopeInfo->itmCobWhile )
      pArea->dbsi.itmCobWhile = hb_itemNew( pScopeInfo->itmCobWhile );

   if( pScopeInfo->lpstrWhile )
      pArea->dbsi.lpstrWhile = hb_itemNew( pScopeInfo->lpstrWhile );

   if( pScopeInfo->lNext )
      pArea->dbsi.lNext = hb_itemNew( pScopeInfo->lNext );

   if( pScopeInfo->itmRecID )
      pArea->dbsi.itmRecID = hb_itemNew( pScopeInfo->itmRecID );

   if( pScopeInfo->fRest )
      pArea->dbsi.fRest = hb_itemNew( pScopeInfo->fRest );

   pArea->dbsi.fIgnoreFilter     = pScopeInfo->fIgnoreFilter;
   pArea->dbsi.fIncludeDeleted   = pScopeInfo->fIncludeDeleted;
   pArea->dbsi.fLast             = pScopeInfo->fLast;
   pArea->dbsi.fIgnoreDuplicates = pScopeInfo->fIgnoreDuplicates;
   pArea->dbsi.fBackword         = pScopeInfo->fBackword;
   pArea->dbsi.fOptimized        = pScopeInfo->fOptimized;

   return SUCCESS;
}

/*
 * Compile a character expression.
 */
ERRCODE hb_waCompile( AREAP pArea, BYTE * pExpr )
{
   HB_MACRO_PTR pMacro;

   HB_TRACE(HB_TR_DEBUG, ("hb_waCompile(%p, %p)", pArea, pExpr));

   pMacro = hb_macroCompile( ( char * ) pExpr );
   if( pMacro )
   {
      pArea->valResult = hb_itemPutPtr( pArea->valResult, ( void * ) pMacro );
      return SUCCESS;
   }
   else
      return FAILURE;
}

/*
 * Raise a runtime error.
 */
ERRCODE hb_waError( AREAP pArea, PHB_ITEM pError )
{
   char * szRddName;

   HB_TRACE(HB_TR_DEBUG, ("hb_waError(%p, %p)", pArea, pError));

   szRddName = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
   if( pArea && pArea->lprfsHost->sysName )
      SELF_SYSNAME( pArea, ( BYTE * ) szRddName );
   else
      strcpy( szRddName, "???DRIVER" );
   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, szRddName );
   hb_xfree( szRddName );
   return hb_errLaunch( pError );
}

/*
 * Evaluate a code block.
 */
ERRCODE hb_waEvalBlock( AREAP pArea, PHB_ITEM pBlock )
{
   PHB_ITEM pItem;
   int iCurrArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_waEvalBlock(%p, %p)", pArea, pBlock));

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   if ( iCurrArea != pArea->uiArea )
      hb_rddSelectWorkAreaNumber( pArea->uiArea );
   else
      iCurrArea = 0;

   pItem = hb_vmEvalBlockOrMacro( pBlock );
   if( ! pArea->valResult )
      pArea->valResult = hb_itemNew( NULL );
   hb_itemCopy( pArea->valResult, pItem );

   if ( iCurrArea )
      hb_rddSelectWorkAreaNumber( iCurrArea );

   return hb_vmRequestQuery() ? FAILURE : SUCCESS;
}

/*
 * RDD info
 */
extern ERRCODE hb_rddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnection, PHB_ITEM pItem )
{
   BOOL fResult;
   int iResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnection, pItem));

   HB_SYMBOL_UNUSED( pRDD );
   HB_SYMBOL_UNUSED( ulConnection );

   switch( uiIndex )
   {
      case RDDI_ISDBF:
      case RDDI_CANPUTREC:
      case RDDI_LOCAL:
      case RDDI_REMOTE:
      case RDDI_RECORDMAP:
      case RDDI_ENCRYPTION:
      case RDDI_TRIGGERS:
      case RDDI_AUTOLOCK:
      case RDDI_STRUCTORD:
      case RDDI_LARGEFILE:
      case RDDI_MULTITAG:
      case RDDI_SORTRECNO:
      case RDDI_MULTIKEY:
         hb_itemPutL( pItem, FALSE );
         break;

      case RDDI_CONNECTION:
      case RDDI_TABLETYPE:
      case RDDI_MEMOTYPE:
      case RDDI_MEMOVERSION:
         hb_itemPutNI( pItem, 0 );
         break;

      case RDDI_STRICTREAD:
         fResult = hb_set.HB_SET_STRICTREAD;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_set.HB_SET_STRICTREAD = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_OPTIMIZE:
         fResult = hb_set.HB_SET_OPTIMIZE;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_set.HB_SET_OPTIMIZE = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_FORCEOPT:
         fResult = hb_set.HB_SET_FORCEOPT;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_set.HB_SET_FORCEOPT = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOOPEN:
         fResult = hb_set.HB_SET_AUTOPEN;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_set.HB_SET_AUTOPEN = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOORDER:
         fResult = hb_set.HB_SET_AUTORDER;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_set.HB_SET_AUTORDER = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOSHARE:
         fResult = hb_set.HB_SET_AUTOSHARE;
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_set.HB_SET_AUTOSHARE = hb_itemGetL( pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_LOCKSCHEME:
         iResult = hb_set.HB_SET_DBFLOCKSCHEME;
         if( hb_itemType( pItem ) & HB_IT_NUMERIC )
            hb_set.HB_SET_DBFLOCKSCHEME = hb_itemGetNI( pItem );
         hb_itemPutNI( pItem, iResult );
         break;
      case RDDI_MEMOBLOCKSIZE:
         iResult = hb_set.HB_SET_MBLOCKSIZE;
         if( hb_itemType( pItem ) & HB_IT_NUMERIC )
            hb_set.HB_SET_MBLOCKSIZE = hb_itemGetNI( pItem );
         hb_itemPutNI( pItem, iResult );
         break;
      case RDDI_MEMOEXT:
         if( hb_itemType( pItem ) & HB_IT_STRING )
         {
            if( hb_set.HB_SET_MFILEEXT )
            {
               hb_itemPutC( pItem, hb_set.HB_SET_MFILEEXT );
               hb_xfree( hb_set.HB_SET_MFILEEXT );
            }
            else
            {
               hb_itemPutC( pItem, "" );
            }
            hb_set.HB_SET_MFILEEXT = hb_strdup( hb_itemGetCPtr( pItem ) );
            break;
         }
         else if( hb_set.HB_SET_MFILEEXT )
         {
            hb_itemPutC( pItem, hb_set.HB_SET_MFILEEXT );
            break;
         }
      case RDDI_TABLEEXT:
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      case RDDI_DELIMITER:
      case RDDI_SEPARATOR:
         hb_itemPutC( pItem, "" );
         /* no break - return FAILURE */

      default:
         return FAILURE;
   }
   return SUCCESS;
}
