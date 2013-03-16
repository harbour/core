/*
 * Harbour Project source code:
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbthread.h"
#include "hbset.h"

/*
 * -- BASIC RDD METHODS --
 */

/*
 * Determine logical beginning of file.
 */
static HB_ERRCODE hb_waBof( AREAP pArea, HB_BOOL * pBof )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waBof(%p, %p)", pArea, pBof ) );

   *pBof = pArea->fBof;
   return HB_SUCCESS;
}

/*
 * Determine logical end of file.
 */
static HB_ERRCODE hb_waEof( AREAP pArea, HB_BOOL * pEof )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waEof(%p, %p)", pArea, pEof ) );

   *pEof = pArea->fEof;
   return HB_SUCCESS;
}

/*
 * Determine outcome of the last search operation.
 */
static HB_ERRCODE hb_waFound( AREAP pArea, HB_BOOL * pFound )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waFound(%p, %p)", pArea, pFound ) );

   *pFound = pArea->fFound;
   return HB_SUCCESS;
}

/*
 * Reposition cursor relative to current position.
 */
static HB_ERRCODE hb_waSkip( AREAP pArea, HB_LONG lToSkip )
{
   HB_LONG lSkip;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waSkip(%p, %ld)", pArea, lToSkip ) );

   /* Flush record and exit */
   if( lToSkip == 0 )
      return SELF_SKIPRAW( pArea, 0 );

   pArea->fTop = pArea->fBottom = HB_FALSE;

   if( lToSkip > 0 )
      lSkip = 1;
   else
   {
      lSkip = -1;
      lToSkip *= -1;
   }
   while( --lToSkip >= 0 )
   {
      if( SELF_SKIPRAW( pArea, lSkip ) != HB_SUCCESS )
         return HB_FAILURE;
      if( SELF_SKIPFILTER( pArea, lSkip ) != HB_SUCCESS )
         return HB_FAILURE;
      if( pArea->fBof || pArea->fEof )
         break;
   }

   /* Update Bof and Eof flags */
   if( lSkip < 0 )
      pArea->fEof = HB_FALSE;
   else /* ( lSkip > 0 ) */
      pArea->fBof = HB_FALSE;

   return HB_SUCCESS;
}

/*
 * Reposition cursor respecting any filter setting.
 */
static HB_ERRCODE hb_waSkipFilter( AREAP pArea, HB_LONG lUpDown )
{
   HB_BOOL fBottom, fDeleted;
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waSkipFilter(%p, %ld)", pArea, lUpDown ) );

   if( pArea->dbfi.itmCobExpr == NULL && ! hb_setGetDeleted() )
      return HB_SUCCESS;

   /* Since lToSkip is passed to SkipRaw, it should never request more than
      a single skip.
      The implied purpose of hb_waSkipFilter is to get off of a "bad" record
      after a skip was performed, NOT to skip lToSkip filtered records.
    */
   lUpDown = ( lUpDown < 0  ? -1 : 1 );

   /* remember if we are here after SLEF_GOTOP() */
   fBottom = pArea->fBottom;

   while( ! pArea->fBof && ! pArea->fEof )
   {
      /* SET DELETED */
      if( hb_setGetDeleted() )
      {
         if( SELF_DELETED( pArea, &fDeleted ) != HB_SUCCESS )
            return HB_FAILURE;
         if( fDeleted )
         {
            if( SELF_SKIPRAW( pArea, lUpDown ) != HB_SUCCESS )
               return HB_FAILURE;
            continue;
         }
      }

      /* SET FILTER TO */
      if( pArea->dbfi.itmCobExpr )
      {
         if( SELF_EVALBLOCK( pArea, pArea->dbfi.itmCobExpr ) != HB_SUCCESS )
            return HB_FAILURE;

         if( HB_IS_LOGICAL( pArea->valResult ) &&
             ! hb_itemGetL( pArea->valResult ) )
         {
            if( SELF_SKIPRAW( pArea, lUpDown ) != HB_SUCCESS )
               return HB_FAILURE;
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
      if( fBottom )
      {
         /* GOTO EOF (phantom) record -
            this is the only one place where GOTO is used by Harbour
            directly and RDD which does not operate on numbers should
            serve this method only as SELF_GOEOF() synonym. If it's a
            problem then we can remove this if and always use SELF_GOTOP()
            but it also means second table scan if all records filtered
            are out of filter so I do not want to do that. I will prefer
            explicit add SELF_GOEOF() method
          */
         errCode = SELF_GOTO( pArea, 0 );
      }
      else
      {
         errCode = SELF_GOTOP( pArea );
         pArea->fBof = HB_TRUE;
      }
   }
   else
   {
      errCode = HB_SUCCESS;
   }

   return errCode;
}

/*
 * Add a field to the WorkArea.
 */
static HB_ERRCODE hb_waAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   LPFIELD pField;
   char szFieldName[ HB_SYMBOL_NAME_LEN + 1 ];
   const char *szPtr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waAddField(%p, %p)", pArea, pFieldInfo ) );

   /* Validate the name of field */
   szPtr = pFieldInfo->atomName;
   while( HB_ISSPACE( *szPtr ) )
   {
      ++szPtr;
   }
   hb_strncpyUpperTrim( szFieldName, szPtr, sizeof( szFieldName ) - 1 );
   if( szFieldName[ 0 ] == 0 )
      return HB_FAILURE;

   pField = pArea->lpFields + pArea->uiFieldCount;
   if( pArea->uiFieldCount > 0 )
      ( ( LPFIELD ) ( pField - 1 ) )->lpfNext = pField;
   pField->sym = ( void * ) hb_dynsymGetCase( szFieldName );
   pField->uiType = pFieldInfo->uiType;
   pField->uiTypeExtended = pFieldInfo->uiTypeExtended;
   pField->uiLen = pFieldInfo->uiLen;
   pField->uiDec = pFieldInfo->uiDec;
   pField->uiFlags = pFieldInfo->uiFlags;
   pField->uiArea = pArea->uiArea;
   pArea->uiFieldCount++;
   return HB_SUCCESS;
}

/*
 * Add all fields defined in an array to the WorkArea.
 */
static HB_ERRCODE hb_waCreateFields( AREAP pArea, PHB_ITEM pStruct )
{
   HB_USHORT uiItems, uiCount, uiLen, uiDec;
   HB_ERRCODE errCode = HB_SUCCESS;
   DBFIELDINFO pFieldInfo;
   PHB_ITEM pFieldDesc;
   int iData;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waCreateFields(%p, %p)", pArea, pStruct ) );

   uiItems = ( HB_USHORT ) hb_arrayLen( pStruct );
   if( SELF_SETFIELDEXTENT( pArea, uiItems ) != HB_SUCCESS )
      return HB_FAILURE;

   for( uiCount = 0; uiCount < uiItems; uiCount++ )
   {
      pFieldInfo.uiTypeExtended = 0;
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiCount + 1 );
      pFieldInfo.atomName = hb_arrayGetCPtr( pFieldDesc, DBS_NAME );
      iData = hb_arrayGetNI( pFieldDesc, DBS_LEN );
      if( iData < 0 )
         iData = 0;
      uiLen = pFieldInfo.uiLen = ( HB_USHORT ) iData;
      iData = hb_arrayGetNI( pFieldDesc, DBS_DEC );
      if( iData < 0 )
         iData = 0;
      uiDec = ( HB_USHORT ) iData;
      pFieldInfo.uiDec = 0;
#ifdef DBS_FLAG
      pFieldInfo.uiFlags = hb_arrayGetNI( pFieldDesc, DBS_FLAG );
#else
      pFieldInfo.uiFlags = 0;
#endif
      iData = HB_TOUPPER( hb_arrayGetCPtr( pFieldDesc, DBS_TYPE )[ 0 ] );
      switch( iData )
      {
         case 'C':
            pFieldInfo.uiType = HB_FT_STRING;
            pFieldInfo.uiLen = uiLen;
/* Too many people reported the behavior with code below as a
   Clipper compatibility bug so I commented this code. Druzus.
#ifdef HB_CLP_STRICT
            pFieldInfo.uiLen = uiLen;
#else
            pFieldInfo.uiLen = uiLen + uiDec * 256;
#endif
*/
            break;

         case 'L':
            pFieldInfo.uiType = HB_FT_LOGICAL;
            pFieldInfo.uiLen = 1;
            break;

         case 'D':
            pFieldInfo.uiType = HB_FT_DATE;
            pFieldInfo.uiLen = ( uiLen == 3 || uiLen == 4 ) ? uiLen : 8;
            break;

         case 'I':
            pFieldInfo.uiType = HB_FT_INTEGER;
            pFieldInfo.uiLen = ( ( uiLen > 0 && uiLen <= 4 ) || uiLen == 8 ) ? uiLen : 4;
            pFieldInfo.uiDec = uiDec;
            break;

         case 'Y':
            pFieldInfo.uiType = HB_FT_CURRENCY;
            pFieldInfo.uiLen = 8;
            pFieldInfo.uiDec = 4;
            break;

         case 'Z':
            pFieldInfo.uiType = HB_FT_CURDOUBLE;
            pFieldInfo.uiLen = 8;
            pFieldInfo.uiDec = uiDec;
            break;

         case '2':
         case '4':
            pFieldInfo.uiType = HB_FT_INTEGER;
            pFieldInfo.uiLen = ( HB_USHORT ) ( iData - '0' );
            break;

         case 'B':
         case '8':
            pFieldInfo.uiType = HB_FT_DOUBLE;
            pFieldInfo.uiLen = 8;
            pFieldInfo.uiDec = uiDec;
            break;

         case 'N':
            pFieldInfo.uiType = HB_FT_LONG;
            pFieldInfo.uiDec = uiDec;
            /* DBASE documentation defines maximum numeric field size as 20
             * but Clipper alows to create longer fileds so I remove this
             * limit, Druzus
             */
            /*
            if( uiLen > 20 )
            */
            if( uiLen > 255 )
               errCode = HB_FAILURE;
            break;

         case 'F':
            pFieldInfo.uiType = HB_FT_FLOAT;
            pFieldInfo.uiDec = uiDec;
            /* see note above */
            if( uiLen > 255 )
               errCode = HB_FAILURE;
            break;

         case 'T':
            if( uiLen == 8 )
            {
               pFieldInfo.uiType = HB_FT_TIMESTAMP;
               pFieldInfo.uiLen = 8;
            }
            else
            {
               pFieldInfo.uiType = HB_FT_TIME;
               pFieldInfo.uiLen = 4;
            }
            break;

         case '@':
            pFieldInfo.uiType = HB_FT_TIMESTAMP;
            pFieldInfo.uiLen = 8;
            break;

         case '=':
            pFieldInfo.uiType = HB_FT_MODTIME;
            pFieldInfo.uiLen = 8;
            break;

         case '^':
            pFieldInfo.uiType = HB_FT_ROWVER;
            pFieldInfo.uiLen = 8;
            break;

         case '+':
            pFieldInfo.uiType = HB_FT_AUTOINC;
            pFieldInfo.uiLen = 4;
            break;

         case 'Q':
            pFieldInfo.uiType = HB_FT_VARLENGTH;
            pFieldInfo.uiLen = uiLen > 255 ? 255 : ( uiLen == 0 ? 1 : uiLen );
            break;

         case 'M':
            pFieldInfo.uiType = HB_FT_MEMO;
            pFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            break;

         case 'V':
            pFieldInfo.uiType = HB_FT_ANY;
            pFieldInfo.uiLen = ( uiLen < 3 || uiLen == 5 ) ? 6 : uiLen;
            break;

         case 'P':
            pFieldInfo.uiType = HB_FT_IMAGE;
            pFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            break;

         case 'W':
            pFieldInfo.uiType = HB_FT_BLOB;
            pFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            break;

         case 'G':
            pFieldInfo.uiType = HB_FT_OLE;
            pFieldInfo.uiLen = ( uiLen == 4 ) ? 4 : 10;
            break;

         default:
            errCode = HB_FAILURE;
            break;
      }

      if( errCode != HB_SUCCESS )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return errCode;
      }
      /* Add field */
      else if( SELF_ADDFIELD( pArea, &pFieldInfo ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Determine the number of fields in the WorkArea.
 */
static HB_ERRCODE hb_waFieldCount( AREAP pArea, HB_USHORT * uiFields )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waFieldCount(%p, %p)", pArea, uiFields ) );

   * uiFields = pArea->uiFieldCount;
   return HB_SUCCESS;
}

/*
 * Retrieve information about a field.
 */
static HB_ERRCODE hb_waFieldInfo( AREAP pArea, HB_USHORT uiIndex, HB_USHORT uiType, PHB_ITEM pItem )
{
   LPFIELD pField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waFieldInfo(%p, %hu, %hu, %p)", pArea, uiIndex, uiType, pItem ) );

   if( uiIndex > pArea->uiFieldCount )
      return HB_FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   switch( uiType )
   {
      case DBS_NAME:
         hb_itemPutC( pItem, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
         break;

      case DBS_TYPE:
         switch( pField->uiType )
         {
            case HB_FT_STRING:
               hb_itemPutC( pItem, "C" );
               break;

            case HB_FT_LOGICAL:
               hb_itemPutC( pItem, "L" );
               break;

            case HB_FT_DATE:
               hb_itemPutC( pItem, "D" );
               break;

            case HB_FT_LONG:
               hb_itemPutC( pItem, "N" );
               break;

            case HB_FT_INTEGER:
               hb_itemPutC( pItem, "I" );
               break;

            case HB_FT_DOUBLE:
               hb_itemPutC( pItem, "B" );
               break;

            case HB_FT_FLOAT:
               hb_itemPutC( pItem, "F" );
               break;

            case HB_FT_TIME:
               hb_itemPutC( pItem, "T" );
               break;

            case HB_FT_TIMESTAMP:
               hb_itemPutC( pItem, "@" );
               break;

            case HB_FT_MODTIME:
               hb_itemPutC( pItem, "=" );
               break;

            case HB_FT_ROWVER:
               hb_itemPutC( pItem, "^" );
               break;

            case HB_FT_AUTOINC:
               hb_itemPutC( pItem, "+" );
               break;

            case HB_FT_CURRENCY:
               hb_itemPutC( pItem, "Y" );
               break;

            case HB_FT_CURDOUBLE:
               hb_itemPutC( pItem, "Z" );
               break;

            case HB_FT_VARLENGTH:
               hb_itemPutC( pItem, "Q" );
               break;

            case HB_FT_MEMO:
               hb_itemPutC( pItem, "M" );
               break;

            case HB_FT_ANY:
               hb_itemPutC( pItem, "V" );
               break;

            case HB_FT_IMAGE:
               hb_itemPutC( pItem, "P" );
               break;

            case HB_FT_BLOB:
               hb_itemPutC( pItem, "W" );
               break;

            case HB_FT_OLE:
               hb_itemPutC( pItem, "G" );
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

#ifdef DBS_FLAG
      case DBS_FLAG:
         hb_itemPutNL( pItem, pField->uiFlags );
         break;
#endif

      default:
         return HB_FAILURE;

   }
   return HB_SUCCESS;
}

/*
 * Determine the name associated with a field number.
 */
static HB_ERRCODE hb_waFieldName( AREAP pArea, HB_USHORT uiIndex, char * szName )
{
   LPFIELD pField;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waFieldName(%p, %hu, %p)", pArea, uiIndex, szName ) );

   if( uiIndex > pArea->uiFieldExtent )
      return HB_FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   hb_strncpy( szName, hb_dynsymName( ( PHB_DYNS ) pField->sym ),
               pArea->uiMaxFieldNameLength );
   return HB_SUCCESS;
}

/*
 * Establish the extent of the array of fields for a WorkArea.
 */
static HB_ERRCODE hb_waSetFieldExtent( AREAP pArea, HB_USHORT uiFieldExtent )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waSetFieldExtent(%p, %hu)", pArea, uiFieldExtent ) );

   pArea->uiFieldExtent = uiFieldExtent;

   /* Alloc field array */
   if( uiFieldExtent )
   {
      pArea->lpFields = ( LPFIELD ) hb_xgrab( uiFieldExtent * sizeof( FIELD ) );
      memset( pArea->lpFields, 0, uiFieldExtent * sizeof( FIELD ) );
   }

   return HB_SUCCESS;
}

/*
 * Obtain the alias of the WorkArea.
 */
static HB_ERRCODE hb_waAlias( AREAP pArea, char * szAlias )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waAlias(%p, %p)", pArea, szAlias ) );

   hb_strncpy( szAlias,
      pArea->atomAlias && hb_dynsymAreaHandle( ( PHB_DYNS ) pArea->atomAlias )
      ? hb_dynsymName( ( PHB_DYNS ) pArea->atomAlias ) : "",
      HB_RDD_MAX_ALIAS_LEN );

   return HB_SUCCESS;
}

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_waClose( AREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waClose(%p)", pArea ) );

   /* Clear items */
   SELF_CLEARFILTER( pArea );
   SELF_CLEARREL( pArea );
   SELF_CLEARLOCATE( pArea );

   /* Clear relations that has this area as a child */
   hb_rddCloseAllParentRelations( pArea );

   if( pArea->atomAlias )
      hb_dynsymSetAreaHandle( ( PHB_DYNS ) pArea->atomAlias, 0 );

   return HB_SUCCESS;
}

/*
 * Retrieve information about the current driver.
 */
static HB_ERRCODE hb_waInfo( AREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waInfo(%p, %hu, %p)", pArea, uiIndex, pItem ) );

   switch( uiIndex )
   {
      case DBI_ISDBF:
      case DBI_CANPUTREC:
      case DBI_ISFLOCK:
      case DBI_SHARED:
         hb_itemPutL( pItem, HB_FALSE );
         break;

      /*
       * IMHO better to return HB_FAILURE to notice that it's not supported
       */
      case DBI_GETDELIMITER:
      case DBI_SETDELIMITER:
      case DBI_SEPARATOR:
         hb_itemPutC( pItem, NULL );
         return HB_FAILURE;

      case DBI_GETHEADERSIZE:
      case DBI_GETRECSIZE:
      case DBI_LOCKCOUNT:
         hb_itemPutNI( pItem, 0 );
         break;

      case DBI_LASTUPDATE:
         hb_itemPutDL( pItem, 0 );
         break;

      case DBI_GETLOCKARRAY:
         hb_arrayNew( pItem, 0 );
         break;

      case DBI_CHILDCOUNT:
      {
         LPDBRELINFO lpdbRelations = pArea->lpdbRelations;
         HB_USHORT uiCount = 0;
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
         if( pArea->dbfi.abFilterText )
            hb_itemCopy( pItem, pArea->dbfi.abFilterText );
         else
            hb_itemPutC( pItem, NULL );
         break;

      case DBI_FOUND:
         hb_itemPutL( pItem, pArea->fFound );
         break;

      case DBI_FCOUNT:
         hb_itemPutNI( pItem, pArea->uiFieldCount );
         break;

      case DBI_ALIAS:
      {
         char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
         if( SELF_ALIAS( pArea, szAlias ) != HB_SUCCESS )
         {
            return HB_FAILURE;
         }
         hb_itemPutC( pItem, szAlias );
         break;
      }

      case DBI_TABLEEXT:
         hb_itemClear( pItem );
         return SELF_RDDINFO( SELF_RDDNODE( pArea ), RDDI_TABLEEXT, 0, pItem );

      case DBI_SCOPEDRELATION:
      {
         int iRelNo = hb_itemGetNI( pItem );
         HB_BOOL fScoped = HB_FALSE;

         if( iRelNo > 0 )
         {
            LPDBRELINFO lpdbRelations = pArea->lpdbRelations;
            while( lpdbRelations )
            {
               if( --iRelNo == 0 )
               {
                  fScoped = lpdbRelations->isScoped;
                  break;
               }
               lpdbRelations = lpdbRelations->lpdbriNext;
            }
         }
         hb_itemPutL( pItem, fScoped );
         break;
      }
      case DBI_POSITIONED:
      {
         HB_ULONG ulRecCount, ulRecNo;
         if( SELF_RECNO( pArea, &ulRecNo ) != HB_SUCCESS )
            return HB_FAILURE;
         if( ulRecNo == 0 )
            hb_itemPutL( pItem, HB_FALSE );
         else if( SELF_RECCOUNT( pArea, &ulRecCount ) != HB_SUCCESS )
            return HB_FAILURE;
         else
            hb_itemPutL( pItem, ulRecNo != ulRecCount + 1 );
         break;
      }
      case DBI_RM_SUPPORTED:
         hb_itemPutL( pItem, HB_FALSE );
         break;

      case DBI_DB_VERSION:
         hb_itemPutC( pItem, NULL );
         break;

      case DBI_RDD_VERSION:
         hb_itemPutC( pItem, NULL );
         break;

      default:
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Retrieve information about the current order that SELF could not.
 * Called by SELF_ORDINFO if uiIndex is not supported.
 */
static HB_ERRCODE hb_waOrderInfo( AREAP pArea, HB_USHORT index, LPDBORDERINFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waOrderInfo(%p, %hu, %p)", pArea, index, pInfo ) );

   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( index );

   if( pInfo->itmResult )
      hb_itemClear( pInfo->itmResult );

   /* CA-Cl*pper does not generate RT error when default ORDERINFO() method
    * is called
    */
   /* hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME ); */

   return HB_FAILURE;
}

/*
 * Clear the WorkArea for use.
 */
static HB_ERRCODE hb_waNewArea( AREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waNewArea(%p)", pArea ) );

   pArea->valResult = hb_itemNew( NULL );
   pArea->lpdbRelations = NULL;
   pArea->uiParents = 0;
   pArea->uiMaxFieldNameLength = 10;

   return HB_SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 * Like in Clipper it's also mapped as Create() method at WA level
 */
static HB_ERRCODE hb_waOpen( AREAP pArea, LPDBOPENINFO pInfo )
{
   if( ! pArea->atomAlias && pInfo->atomAlias && pInfo->atomAlias[ 0 ] )
   {
      pArea->atomAlias = hb_rddAllocWorkAreaAlias( pInfo->atomAlias,
                                                   ( int ) pInfo->uiArea );
      if( ! pArea->atomAlias )
      {
         SELF_CLOSE( ( AREAP ) pArea );
         return HB_FAILURE;
      }
   }
   return HB_SUCCESS;
}

static HB_ERRCODE hb_waOrderCondition( AREAP pArea, LPDBORDERCONDINFO param )
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

   return HB_SUCCESS;
}

/*
 * Release all references to a WorkArea.
 */
static HB_ERRCODE hb_waRelease( AREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waRelease(%p)", pArea ) );

   /* Free all allocated pointers */
   if( pArea->lpFields )
      hb_xfree( pArea->lpFields );
   if( pArea->valResult )
      hb_itemRelease( pArea->valResult );
   if( pArea->lpdbOrdCondInfo )
      /* intentionally direct call not a method */
      hb_waOrderCondition( pArea, NULL );
   hb_xfree( pArea );
   return HB_SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_waStructSize( AREAP pArea, HB_USHORT * uiSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waStrucSize(%p, %p)", pArea, uiSize ) );
   HB_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( AREA );
   return HB_SUCCESS;
}

/*
 * Obtain the name of replaceable database driver (RDD) subsystem.
 */
static HB_ERRCODE hb_waSysName( AREAP pArea, char * pBuffer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waSysName(%p, %p)", pArea, pBuffer ) );

   hb_strncpy( pBuffer, SELF_RDDNODE( pArea )->szName,
               HB_RDD_MAX_DRIVERNAME_LEN );

   return HB_SUCCESS;
}

/*
 * Evaluate code block for each record in WorkArea.
 */
static HB_ERRCODE hb_waEval( AREAP pArea, LPDBEVALINFO pEvalInfo )
{
   HB_LONG lNext = 1;
   HB_BOOL fEof, fFor;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waEval(%p, %p)", pArea, pEvalInfo ) );

   if( pEvalInfo->dbsci.itmRecID )
   {
      if( SELF_GOTOID( pArea, pEvalInfo->dbsci.itmRecID ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   else if( pEvalInfo->dbsci.lNext )
   {
      lNext = hb_itemGetNL( pEvalInfo->dbsci.lNext );
   }
   else if( ! pEvalInfo->dbsci.itmCobWhile &&
            ! hb_itemGetLX( pEvalInfo->dbsci.fRest ) )
   {
      if( SELF_GOTOP( pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   if( ! pEvalInfo->dbsci.lNext || lNext > 0 )
   {
      for( ;; )
      {
         if( SELF_EOF( pArea, &fEof ) != HB_SUCCESS )
            return HB_FAILURE;

         if( fEof )
            break;

         if( pEvalInfo->dbsci.itmCobWhile )
         {
            if( SELF_EVALBLOCK( pArea, pEvalInfo->dbsci.itmCobWhile ) != HB_SUCCESS )
               return HB_FAILURE;
            if( ! hb_itemGetLX( pArea->valResult ) )
               break;
         }

         if( pEvalInfo->dbsci.itmCobFor )
         {
            if( SELF_EVALBLOCK( pArea, pEvalInfo->dbsci.itmCobFor ) != HB_SUCCESS )
               return HB_FAILURE;
            fFor = hb_itemGetLX( pArea->valResult );
         }
         else
            fFor = HB_TRUE;

         if( fFor )
         {
            if( SELF_EVALBLOCK( pArea, pEvalInfo->itmBlock ) != HB_SUCCESS )
               return HB_FAILURE;
         }

         if( pEvalInfo->dbsci.itmRecID || ( pEvalInfo->dbsci.lNext && --lNext < 1 ) )
            break;

         if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
            return HB_FAILURE;
      }
   }

   return HB_SUCCESS;
}

/*
 * Locate a record which pass given condition
 */
static HB_ERRCODE hb_waLocate( AREAP pArea, HB_BOOL fContinue )
{
   HB_LONG lNext = 1;
   HB_BOOL fEof;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waLocate(%p, %d)", pArea, fContinue ) );

   if( fContinue )
   {
      if( ! pArea->dbsi.itmCobFor )
         return HB_SUCCESS;

      if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   else if( pArea->dbsi.itmRecID )
   {
      if( SELF_GOTOID( pArea, pArea->dbsi.itmRecID ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   else if( pArea->dbsi.lNext )
   {
      lNext = hb_itemGetNL( pArea->dbsi.lNext );
   }
   else if( ! pArea->dbsi.itmCobWhile &&
            ! hb_itemGetLX( pArea->dbsi.fRest ) )
   {
      if( SELF_GOTOP( pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   pArea->fFound = HB_FALSE;

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   if( ! pArea->dbsi.lNext || lNext > 0 )
   {
      for( ;; )
      {
         if( SELF_EOF( pArea, &fEof ) != HB_SUCCESS )
            return HB_FAILURE;

         if( fEof )
            break;

         if( ! fContinue && pArea->dbsi.itmCobWhile )
         {
            if( SELF_EVALBLOCK( pArea, pArea->dbsi.itmCobWhile ) != HB_SUCCESS )
               return HB_FAILURE;
            if( ! hb_itemGetLX( pArea->valResult ) )
               break;
         }

         if( ! pArea->dbsi.itmCobFor )
         {
            pArea->fFound = HB_TRUE;
            break;
         }
         else
         {
            if( SELF_EVALBLOCK( pArea, pArea->dbsi.itmCobFor ) != HB_SUCCESS )
               return HB_FAILURE;

            if( hb_itemGetLX( pArea->valResult ) )
            {
               pArea->fFound = HB_TRUE;
               break;
            }
         }

         if( ! fContinue &&
             ( pArea->dbsi.itmRecID || ( pArea->dbsi.lNext && --lNext < 1 ) ) )
            break;

         if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
            return HB_FAILURE;
      }
   }

   return HB_SUCCESS;
}

/*
 * Copy one or more records from one WorkArea to another.
 */
static HB_ERRCODE hb_waTrans( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_LONG lNext = 1;
   HB_BOOL fEof, fFor;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waTrans(%p, %p)", pArea, pTransInfo ) );

   if( pTransInfo->dbsci.itmRecID )
   {
      if( SELF_GOTOID( pArea, pTransInfo->dbsci.itmRecID ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   else if( pTransInfo->dbsci.lNext )
   {
      lNext = hb_itemGetNL( pTransInfo->dbsci.lNext );
   }
   else if( ! pTransInfo->dbsci.itmCobWhile &&
            ! hb_itemGetLX( pTransInfo->dbsci.fRest ) )
   {
      if( SELF_GOTOP( pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   /* TODO: use SKIPSCOPE() method and fRest parameter */

   if( ! pTransInfo->dbsci.lNext || lNext > 0 )
   {
      for( ;; )
      {
         if( SELF_EOF( pArea, &fEof ) != HB_SUCCESS )
            return HB_FAILURE;

         if( fEof )
            break;

         if( pTransInfo->dbsci.itmCobWhile )
         {
            if( SELF_EVALBLOCK( pArea, pTransInfo->dbsci.itmCobWhile ) != HB_SUCCESS )
               return HB_FAILURE;
            if( ! hb_itemGetLX( pArea->valResult ) )
               break;
         }

         if( pTransInfo->dbsci.itmCobFor )
         {
            if( SELF_EVALBLOCK( pArea, pTransInfo->dbsci.itmCobFor ) != HB_SUCCESS )
               return HB_FAILURE;
            fFor = hb_itemGetLX( pArea->valResult );
         }
         else
            fFor = HB_TRUE;

         if( fFor )
         {
            if( SELF_TRANSREC( pArea, pTransInfo ) != HB_SUCCESS )
               return HB_FAILURE;
         }

         if( pTransInfo->dbsci.itmRecID || ( pTransInfo->dbsci.lNext && --lNext < 1 ) )
            break;

         if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
            return HB_FAILURE;
      }
   }

   return HB_SUCCESS;
}

/*
 * Copy a record to another WorkArea.
 */
static HB_ERRCODE hb_waTransRec( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   HB_BOOL bDeleted;
   HB_BYTE * pRecord;
   HB_ERRCODE errCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waTransRec(%p, %p)", pArea, pTransInfo ) );

   /* Record deleted? */
   errCode = SELF_DELETED( ( AREAP ) pArea, &bDeleted );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( pTransInfo->uiFlags & DBTF_MATCH && pTransInfo->uiFlags & DBTF_PUTREC )
   {
      errCode = SELF_GETREC( ( AREAP ) pArea, &pRecord );
      if( errCode != HB_SUCCESS )
         return errCode;

      /* Append a new record */
      errCode = SELF_APPEND( ( AREAP ) pTransInfo->lpaDest, HB_TRUE );
      if( errCode != HB_SUCCESS )
         return errCode;

      /* Copy record */
      errCode = SELF_PUTREC( ( AREAP ) pTransInfo->lpaDest, pRecord );
   }
   else
   {
      LPDBTRANSITEM pTransItem;
      PHB_ITEM pItem;
      HB_USHORT uiCount;

      /* Append a new record */
      errCode = SELF_APPEND( ( AREAP ) pTransInfo->lpaDest, HB_TRUE );
      if( errCode != HB_SUCCESS )
         return errCode;

      pItem = hb_itemNew( NULL );
      pTransItem = pTransInfo->lpTransItems;
      for( uiCount = pTransInfo->uiItemCount; uiCount; --uiCount )
      {
         errCode = SELF_GETVALUE( ( AREAP ) pArea,
                                  pTransItem->uiSource, pItem );
         if( errCode != HB_SUCCESS )
            break;
         errCode = SELF_PUTVALUE( ( AREAP ) pTransInfo->lpaDest,
                                  pTransItem->uiDest, pItem );
         if( errCode != HB_SUCCESS )
            break;
         ++pTransItem;
      }
      hb_itemRelease( pItem );
   }

   /* Delete the new record if copy fail */
   if( errCode != HB_SUCCESS )
   {
      SELF_DELETE( ( AREAP ) pTransInfo->lpaDest );
      return errCode;
   }

   /* Delete the new record */
   if( bDeleted )
      return SELF_DELETE( ( AREAP ) pTransInfo->lpaDest );

   return HB_SUCCESS;
}

/*
 * Report end of relation.
 */
static HB_ERRCODE hb_waChildEnd( AREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waChildEnd(%p, %p)", pArea, pRelInfo ) );

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
   return HB_SUCCESS;
}

/*
 * Report initialization of a relation.
 */
static HB_ERRCODE hb_waChildStart( AREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waChildStart(%p, %p)", pArea, pRelInfo ) );
   HB_SYMBOL_UNUSED( pRelInfo );

   pArea->uiParents++;
   return HB_SUCCESS;
}

/*
 * Force relational movement in child WorkAreas.
 */
static HB_ERRCODE hb_waSyncChildren( AREAP pArea )
{

   LPDBRELINFO lpdbRelation;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waSyncChildren(%p)", pArea ) );

   lpdbRelation = pArea->lpdbRelations;
   while( lpdbRelation )
   {
      if( SELF_CHILDSYNC( lpdbRelation->lpaChild, lpdbRelation ) != HB_SUCCESS )
         return HB_FAILURE;
      lpdbRelation = lpdbRelation->lpdbriNext;
   }

   return HB_SUCCESS;
}

/*
 * Clear all relations in the specified WorkArea.
 */
static HB_ERRCODE hb_waClearRel( AREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waClearRel(%p)", pArea ) );

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

   return HB_SUCCESS;
}

/*
 * Obtain the workarea number of the specified relation.
 */
static HB_ERRCODE hb_waRelArea( AREAP pArea, HB_USHORT uiRelNo, HB_USHORT * pRelArea )
{
   LPDBRELINFO lpdbRelations;
   HB_USHORT uiIndex = 1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waRelArea(%p, %hu, %p)", pArea, uiRelNo, pRelArea ) );

   *pRelArea = 0;
   lpdbRelations = pArea->lpdbRelations;
   while( lpdbRelations )
   {
      if( uiIndex++ == uiRelNo )
      {
         *pRelArea = lpdbRelations->lpaChild->uiArea;
         break;
      }
      lpdbRelations = lpdbRelations->lpdbriNext;
   }
   return *pRelArea ? HB_SUCCESS : HB_FAILURE;
}

/*
 * Evaluate a block against the relation in specified WorkArea.
 */
static HB_ERRCODE hb_waRelEval( AREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_ERRCODE errCode;
   HB_BOOL fEof;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waRelEval(%p, %p)", pArea, pRelInfo ) );

   errCode = SELF_EOF( pRelInfo->lpaParent, &fEof );
   if( errCode == HB_SUCCESS )
   {
      if( fEof )
         errCode = SELF_GOTO( pArea, 0 );
      else
      {
         errCode = SELF_EVALBLOCK( pRelInfo->lpaParent, pRelInfo->itmCobExpr );
         if( errCode == HB_SUCCESS )
         {
            PHB_ITEM pResult;
            DBORDERINFO pInfo;

            /*
             *  Check the current order
             */
            pResult = pRelInfo->lpaParent->valResult;
            pRelInfo->lpaParent->valResult = NULL;
            memset( &pInfo, 0, sizeof( pInfo ) );
            pInfo.itmResult = hb_itemPutNI( NULL, 0 );
            errCode = SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );

            if( errCode == HB_SUCCESS )
            {
               int iOrder = hb_itemGetNI( pInfo.itmResult );
               if( iOrder != 0 )
               {
                  if( pRelInfo->isScoped )
                  {
                     pInfo.itmNewVal = pResult;
                     errCode = SELF_ORDINFO( pArea, DBOI_SCOPETOP, &pInfo );
                     if( errCode == HB_SUCCESS )
                        errCode = SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOM, &pInfo );
                  }
                  if( errCode == HB_SUCCESS )
                     errCode = SELF_SEEK( pArea, HB_FALSE, pResult, HB_FALSE );
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
                  if( errCode == HB_SUCCESS )
                  {
                     errCode = SELF_EOF( pArea, &fEof );
                     if( errCode == HB_SUCCESS )
                        pArea->fFound = ! fEof;
                  }
               }
            }
            hb_itemRelease( pInfo.itmResult );
            hb_itemRelease( pResult );
         }
      }
   }
   return errCode;
}

/*
 * Obtain the character expression of the specified relation.
 */
static HB_ERRCODE hb_waRelText( AREAP pArea, HB_USHORT uiRelNo, PHB_ITEM pExpr )
{
   LPDBRELINFO lpdbRelations;
   HB_USHORT uiIndex = 1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waRelText(%p, %hu, %p)", pArea, uiRelNo, pExpr ) );

   lpdbRelations = pArea->lpdbRelations;

   while( lpdbRelations )
   {
      if( uiIndex++ == uiRelNo )
      {
         hb_itemCopy( pExpr, lpdbRelations->abKey );
         return HB_SUCCESS;
      }
      lpdbRelations = lpdbRelations->lpdbriNext;
   }

   return HB_FAILURE;
}

/*
 * Set a relation in the parent file.
 */
static HB_ERRCODE hb_waSetRel( AREAP pArea, LPDBRELINFO lpdbRelInf )
{
   LPDBRELINFO lpdbRelations;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waSetRel(%p, %p)", pArea, lpdbRelInf ) );

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

   return SELF_CHILDSTART( ( AREAP ) lpdbRelInf->lpaChild, lpdbRelations );
}

/*
 * Clear the active filter expression.
 */
static HB_ERRCODE hb_waClearFilter( AREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waClearFilter(%p)", pArea ) );

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
   pArea->dbfi.fOptimized = HB_FALSE;
   pArea->dbfi.fFilter = HB_FALSE;

   return HB_SUCCESS;
}

/*
 * Clear the active locate expression.
 */
static HB_ERRCODE hb_waClearLocate( AREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waClearLocate(%p)", pArea ) );

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

   return HB_SUCCESS;
}

/*
 * Return filter condition of the specified WorkArea.
 */
static HB_ERRCODE hb_waFilterText( AREAP pArea, PHB_ITEM pFilter )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waFilterText(%p, %p)", pArea, pFilter ) );

   if( pArea->dbfi.abFilterText )
      hb_itemCopy( pFilter, pArea->dbfi.abFilterText );

   return HB_SUCCESS;
}

/*
 * Set the filter condition for the specified WorkArea.
 */
static HB_ERRCODE hb_waSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waSetFilter(%p, %p)", pArea, pFilterInfo ) );

   /* Clear the active filter expression */
   if( SELF_CLEARFILTER( pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   if( pFilterInfo->itmCobExpr )
   {
      pArea->dbfi.itmCobExpr = hb_itemNew( pFilterInfo->itmCobExpr );
   }
   if( pFilterInfo->abFilterText )
   {
      pArea->dbfi.abFilterText = hb_itemNew( pFilterInfo->abFilterText );
   }
   pArea->dbfi.fOptimized = pFilterInfo->fOptimized;
   pArea->dbfi.fFilter = HB_TRUE;

   return HB_SUCCESS;
}

/*
 * Set the locate scope for the specified WorkArea.
 */
static HB_ERRCODE hb_waSetLocate( AREAP pArea, LPDBSCOPEINFO pScopeInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waSetLocate(%p, %p)", pArea, pScopeInfo ) );

   /* Clear the active locate expression */
   if( SELF_CLEARLOCATE( pArea ) != HB_SUCCESS )
      return HB_FAILURE;

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
   pArea->dbsi.fBackward         = pScopeInfo->fBackward;
   pArea->dbsi.fOptimized        = pScopeInfo->fOptimized;

   return HB_SUCCESS;
}

/*
 * Compile a character expression.
 */
static HB_ERRCODE hb_waCompile( AREAP pArea, const char * pExpr )
{
   PHB_MACRO pMacro;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waCompile(%p, %p)", pArea, pExpr ) );

   pMacro = hb_macroCompile( pExpr );
   if( pMacro )
   {
      pArea->valResult = hb_itemPutPtr( pArea->valResult, ( void * ) pMacro );
      return HB_SUCCESS;
   }
   else
      return HB_FAILURE;
}

/*
 * Raise a runtime error.
 */
static HB_ERRCODE hb_waError( AREAP pArea, PHB_ITEM pError )
{
   char szRddName[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_waError(%p, %p)", pArea, pError ) );

   if( pArea && pArea->lprfsHost->sysName )
      SELF_SYSNAME( pArea, szRddName );
   else
      hb_strncpy( szRddName, "???DRIVER", HB_RDD_MAX_DRIVERNAME_LEN );
   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, szRddName );
   return hb_errLaunch( pError );
}

/*
 * Evaluate a code block.
 */
static HB_ERRCODE hb_waEvalBlock( AREAP pArea, PHB_ITEM pBlock )
{
   PHB_ITEM pItem;
   int iCurrArea, iUsedArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waEvalBlock(%p, %p)", pArea, pBlock ) );

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   iUsedArea = pArea->uiArea;
   if( iCurrArea != iUsedArea )
      hb_rddSelectWorkAreaNumber( iUsedArea );

   pItem = hb_vmEvalBlockOrMacro( pBlock );

   if( ( AREAP ) hb_rddGetWorkAreaPointer( iUsedArea ) != pArea )
      return HB_FAILURE;

   if( ! pArea->valResult )
      pArea->valResult = hb_itemNew( NULL );
   hb_itemCopy( pArea->valResult, pItem );

   hb_rddSelectWorkAreaNumber( iCurrArea );

   return hb_vmRequestQuery() ? HB_FAILURE : HB_SUCCESS;
}

/*
 * RDD info
 */
static HB_ERRCODE hb_waRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnection, PHB_ITEM pItem )
{
   HB_BOOL fResult;
   int iResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnection, pItem ) );

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
      case RDDI_AUTOLOCK:
      case RDDI_STRUCTORD:
      case RDDI_LARGEFILE:
      case RDDI_MULTITAG:
      case RDDI_SORTRECNO:
      case RDDI_MULTIKEY:
      case RDDI_BLOB_SUPPORT:
         hb_itemPutL( pItem, HB_FALSE );
         break;

      case RDDI_CONNECTION:
      case RDDI_TABLETYPE:
      case RDDI_MEMOTYPE:
      case RDDI_MEMOVERSION:
         hb_itemPutNI( pItem, 0 );
         break;

      case RDDI_STRICTREAD:
         fResult = hb_setGetStrictRead();
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_setSetItem( HB_SET_STRICTREAD, pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_OPTIMIZE:
         fResult = hb_setGetOptimize();
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_setSetItem( HB_SET_OPTIMIZE, pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_FORCEOPT:
         fResult = hb_setGetForceOpt();
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_setSetItem( HB_SET_FORCEOPT, pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOOPEN:
         fResult = hb_setGetAutOpen();
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_setSetItem( HB_SET_AUTOPEN, pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOORDER:
         fResult = hb_setGetAutOrder();
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_setSetItem( HB_SET_AUTORDER, pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_AUTOSHARE:
         fResult = hb_setGetAutoShare();
         if( hb_itemType( pItem ) == HB_IT_LOGICAL )
            hb_setSetItem( HB_SET_AUTOSHARE, pItem );
         hb_itemPutL( pItem, fResult );
         break;
      case RDDI_LOCKSCHEME:
         iResult = hb_setGetDBFLockScheme();
         if( hb_itemType( pItem ) & HB_IT_NUMERIC )
            hb_setSetItem( HB_SET_DBFLOCKSCHEME, pItem );
         hb_itemPutNI( pItem, iResult );
         break;
      case RDDI_MEMOBLOCKSIZE:
         iResult = hb_setGetMBlockSize();
         if( hb_itemType( pItem ) & HB_IT_NUMERIC )
            hb_setSetItem( HB_SET_MBLOCKSIZE, pItem );
         hb_itemPutNI( pItem, iResult );
         break;
      case RDDI_MEMOEXT:
      {
         const char * szExt = hb_setGetMFileExt();
         char * szResult = szExt ? hb_strdup( szExt ) : NULL;
         if( hb_itemType( pItem ) & HB_IT_STRING )
         {
            hb_setSetItem( HB_SET_MFILEEXT, pItem );
            if( szResult )
               hb_itemPutCLPtr( pItem, szResult, strlen( szResult ) );
            else
               hb_itemPutC( pItem, NULL );
            break;
         }
         else if( szResult )
         {
            hb_itemPutCLPtr( pItem, szResult, strlen( szResult ) );
            break;
         }
         /* no break - return HB_FAILURE */
      }
      case RDDI_TABLEEXT:
      case RDDI_ORDBAGEXT:
      case RDDI_ORDEREXT:
      case RDDI_ORDSTRUCTEXT:
      case RDDI_DELIMITER:
      case RDDI_SEPARATOR:
      case RDDI_TRIGGER:
      case RDDI_PENDINGTRIGGER:
         hb_itemPutC( pItem, NULL );
         /* no break - return HB_FAILURE */

      default:
         return HB_FAILURE;
   }
   return HB_SUCCESS;
}

/*
 * Raise a runtime error if an method is not defined.
 */
static HB_ERRCODE hb_waUnsupported( AREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waUnsupported(%p)", pArea ) );

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );
   SELF_ERROR( pArea, pError );
   hb_itemRelease( pError );

   return HB_FAILURE;
}

/*
 * Raise a runtime error if an method is not defined.
 */
static HB_ERRCODE hb_waRddUnsupported( LPRDDNODE pRDD )
{
   PHB_ITEM pError;

   HB_TRACE( HB_TR_DEBUG, ( "hb_waRDDUnsupported(%p)", pRDD ) );

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );

   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, pRDD->szName );
   hb_errLaunch( pError );
   hb_itemRelease( pError );

   return HB_FAILURE;
}

#if 0
/*
 * Empty method.
 */
static HB_ERRCODE hb_waNull( AREAP pArea )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_waNull(%p)", pArea ) );

   HB_SYMBOL_UNUSED( pArea );

   return HB_SUCCESS;
}
#endif

/*
 * The default virtual method table for all WorkAreas.
 */
static const RDDFUNCS waTable =
{
   /* Movement and positioning methods */
/* ( DBENTRYP_BP )   */ hb_waBof,               /* Bof        */
/* ( DBENTRYP_BP )   */ hb_waEof,               /* Eof        */
/* ( DBENTRYP_BP )   */ hb_waFound,             /* Found      */
   ( DBENTRYP_V )       hb_waUnsupported,       /* GoBottom   */
   ( DBENTRYP_UL )      hb_waUnsupported,       /* GoTo       */
   ( DBENTRYP_I )       hb_waUnsupported,       /* GoToId     */
   ( DBENTRYP_V )       hb_waUnsupported,       /* GoTop      */
   ( DBENTRYP_BIB )     hb_waUnsupported,       /* Seek       */
/* ( DBENTRYP_L )    */ hb_waSkip,              /* Skip       */
/* ( DBENTRYP_L )    */ hb_waSkipFilter,        /* SkipFilter */
   ( DBENTRYP_L )       hb_waUnsupported,       /* SkipRaw    */

   /* Data management */
/* ( DBENTRYP_VF )   */ hb_waAddField,          /* AddField       */
   ( DBENTRYP_B )       hb_waUnsupported,       /* Append         */
/* ( DBENTRYP_I )    */ hb_waCreateFields,      /* CreateFields   */
   ( DBENTRYP_V )       hb_waUnsupported,       /* DeleteRec      */
   ( DBENTRYP_BP )      hb_waUnsupported,       /* Deleted        */
/* ( DBENTRYP_SP )   */ hb_waFieldCount,        /* FieldCount     */
   ( DBENTRYP_VF )      hb_waUnsupported,       /* FieldDisplay   */
/* ( DBENTRYP_SSI )  */ hb_waFieldInfo,         /* FieldInfo      */
/* ( DBENTRYP_SCP )  */ hb_waFieldName,         /* FieldName      */
   ( DBENTRYP_V )       hb_waUnsupported,       /* Flush          */
   ( DBENTRYP_PP )      hb_waUnsupported,       /* GetRec         */
   ( DBENTRYP_SI )      hb_waUnsupported,       /* GetValue       */
   ( DBENTRYP_SVL )     hb_waUnsupported,       /* GetVarLen      */
   ( DBENTRYP_V )       hb_waUnsupported,       /* GoCold         */
   ( DBENTRYP_V )       hb_waUnsupported,       /* GoHot          */
   ( DBENTRYP_P )       hb_waUnsupported,       /* PutRec         */
   ( DBENTRYP_SI )      hb_waUnsupported,       /* PutValue       */
   ( DBENTRYP_V )       hb_waUnsupported,       /* Recall         */
   ( DBENTRYP_ULP )     hb_waUnsupported,       /* RecCount       */
   ( DBENTRYP_ISI )     hb_waUnsupported,       /* RecInfo        */
   ( DBENTRYP_ULP )     hb_waUnsupported,       /* RecNo          */
   ( DBENTRYP_I )       hb_waUnsupported,       /* RecId          */
/* ( DBENTRYP_S )    */ hb_waSetFieldExtent,    /* SetFieldExtent */

   /* WorkArea/Database management */
/* ( DBENTRYP_CP )   */ hb_waAlias,             /* Alias       */
/* ( DBENTRYP_V )    */ hb_waClose,             /* Close       */
   /* Like in Clipper map CREATE() method at work area level to OPEN() */
/* ( DBENTRYP_VO )   */ hb_waOpen,              /* Create      */
/* ( DBENTRYP_SI )   */ hb_waInfo,              /* Info        */
/* ( DBENTRYP_V )    */ hb_waNewArea,           /* NewArea     */
/* ( DBENTRYP_VO )   */ hb_waOpen,              /* Open        */
/* ( DBENTRYP_V )    */ hb_waRelease,           /* Release     */
/* ( DBENTRYP_SP )   */ hb_waStructSize,        /* StructSize  */
/* ( DBENTRYP_CP )   */ hb_waSysName,           /* SysName     */
/* ( DBENTRYP_VEI )  */ hb_waEval,              /* Eval        */
   ( DBENTRYP_V )       hb_waUnsupported,       /* Pack        */
   ( DBENTRYP_LSP )     hb_waUnsupported,       /* PackRec     */
   ( DBENTRYP_VS )      hb_waUnsupported,       /* Sort        */
/* ( DBENTRYP_VT )   */ hb_waTrans,             /* Trans       */
/* ( DBENTRYP_VT )   */ hb_waTransRec,          /* TransRec    */
   ( DBENTRYP_V )       hb_waUnsupported,       /* Zap         */

   /* Relational Methods */
/* ( DBENTRYP_VR )   */ hb_waChildEnd,          /* ChildEnd      */
/* ( DBENTRYP_VR )   */ hb_waChildStart,        /* ChildStart    */
   ( DBENTRYP_VR )      hb_waUnsupported,       /* ChildSync     */
/* ( DBENTRYP_V )    */ hb_waSyncChildren,      /* SyncChildren  */
/* ( DBENTRYP_V )    */ hb_waClearRel,          /* ClearRel      */
   ( DBENTRYP_V )       hb_waUnsupported,       /* ForceRel      */
/* ( DBENTRYP_SSP )  */ hb_waRelArea,           /* RelArea       */
/* ( DBENTRYP_VR )   */ hb_waRelEval,           /* RelEval       */
/* ( DBENTRYP_SI )   */ hb_waRelText,           /* RelText       */
/* ( DBENTRYP_VR )   */ hb_waSetRel,            /* SetRel        */

   /* Order Management */
   ( DBENTRYP_VOI )     hb_waUnsupported,       /* OrderListAdd      */
   ( DBENTRYP_V )       hb_waUnsupported,       /* OrderListClear    */
   ( DBENTRYP_VOI )     hb_waUnsupported,       /* OrderListDelete   */
   ( DBENTRYP_VOI )     hb_waUnsupported,       /* OrderListFocus    */
   ( DBENTRYP_V )       hb_waUnsupported,       /* OrderListRebuild  */
/* ( DBENTRYP_VOO )  */ hb_waOrderCondition,    /* OrderCondition    */
   ( DBENTRYP_VOC )     hb_waUnsupported,       /* OrderCreate       */
   ( DBENTRYP_VOI )     hb_waUnsupported,       /* OrderDestroy      */
/* ( DBENTRYP_SVOI ) */ hb_waOrderInfo,         /* OrderInfo         */

   /* Filters and Scope Settings */
/* ( DBENTRYP_V )    */ hb_waClearFilter,       /* ClearFilter  */
/* ( DBENTRYP_V )    */ hb_waClearLocate,       /* ClearLocate  */
   ( DBENTRYP_V )       hb_waUnsupported,       /* ClearScope   */
   ( DBENTRYP_VPLP )    hb_waUnsupported,       /* CountScope   */
/* ( DBENTRYP_I )    */ hb_waFilterText,        /* FilterText   */
   ( DBENTRYP_SI )      hb_waUnsupported,       /* ScopeInfo    */
/* ( DBENTRYP_VFI )  */ hb_waSetFilter,         /* SetFilter    */
/* ( DBENTRYP_VLO )  */ hb_waSetLocate,         /* SetLocate    */
   ( DBENTRYP_VOS )     hb_waUnsupported,       /* SetScope     */
   ( DBENTRYP_VPL )     hb_waUnsupported,       /* SkipScope    */
/* ( DBENTRYP_B )    */ hb_waLocate,            /* Locate       */

   /* Miscellaneous */
/* ( DBENTRYP_CC )   */ hb_waCompile,           /* Compile    */
/* ( DBENTRYP_I )    */ hb_waError,             /* Error      */
/* ( DBENTRYP_I )    */ hb_waEvalBlock,         /* EvalBlock  */

   /* Network operations */
   ( DBENTRYP_VSP )     hb_waUnsupported,       /* RawLock  */
   ( DBENTRYP_VL )      hb_waUnsupported,       /* Lock     */
   ( DBENTRYP_I )       hb_waUnsupported,       /* UnLock   */

   /* Memofile functions */
   ( DBENTRYP_V )       hb_waUnsupported,       /* CloseMemFile   */
   ( DBENTRYP_VO )      hb_waUnsupported,       /* CreateMemFile  */
   ( DBENTRYP_SCCS )    hb_waUnsupported,       /* GetValueFile   */
   ( DBENTRYP_VO )      hb_waUnsupported,       /* OpenMemFile    */
   ( DBENTRYP_SCCS )    hb_waUnsupported,       /* PutValueFile   */

   /* Database file header handling */
   ( DBENTRYP_V )       hb_waUnsupported,       /* ReadDBHeader   */
   ( DBENTRYP_V )       hb_waUnsupported,       /* WriteDBHeader  */

   /* non WorkArea functions */
   ( DBENTRYP_R )       NULL,                   /* Init    */
   ( DBENTRYP_R )       NULL,                   /* Exit    */
   ( DBENTRYP_RVVL )    hb_waRddUnsupported,    /* Drop    */
   ( DBENTRYP_RVVL )    hb_waRddUnsupported,    /* Exists  */
   ( DBENTRYP_RVVVL )   hb_waRddUnsupported,    /* Rename  */
/* ( DBENTRYP_RSLV ) */ hb_waRddInfo,           /* RddInfo */

   /* Special and reserved methods */
   ( DBENTRYP_SVP )   hb_waUnsupported          /* WhoCares */
};

#define HB_RDD_POOL_ALLOCSIZE  128
/* common for all threads list of registered RDDs */
static HB_CRITICAL_NEW( s_rddMtx );
static LPRDDNODE * s_RddList    = NULL;   /* Registered RDDs pool */
static HB_USHORT   s_uiRddMax   = 0;      /* Size of RDD pool */
static HB_USHORT   s_uiRddCount = 0;      /* Number of registered RDD */

/*
 * Get RDD node poionter
 */
LPRDDNODE hb_rddGetNode( HB_USHORT uiNode )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddGetNode(%hu)", uiNode ) );

   return uiNode < s_uiRddCount ? s_RddList[ uiNode ] : NULL;
}

PHB_ITEM hb_rddList( HB_USHORT uiType )
{
   HB_USHORT uiCount, uiIndex, uiRdds;
   PHB_ITEM pRddArray;
   LPRDDNODE pNode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddList(%hu)", uiType ) );

   for( uiCount = uiRdds = 0; uiCount < s_uiRddCount; ++uiCount )
   {
      if( uiType == 0 || s_RddList[ uiCount ]->uiType == uiType )
         ++uiRdds;
   }
   pRddArray = hb_itemArrayNew( uiRdds );
   for( uiCount = uiIndex = 0; uiCount < s_uiRddCount && uiIndex < uiRdds; ++uiCount )
   {
      pNode = s_RddList[ uiCount ];
      if( uiType == 0 || pNode->uiType == uiType )
         hb_arraySetC( pRddArray, ++uiIndex, pNode->szName );
   }
   return pRddArray;
}

/*
 * Find a RDD node.
 */
LPRDDNODE hb_rddFindNode( const char * szDriver, HB_USHORT * uiIndex )
{
   HB_USHORT uiCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddFindNode(%s, %p)", szDriver, uiIndex ) );

   for( uiCount = 0; uiCount < s_uiRddCount; uiCount++ )
   {
      LPRDDNODE pNode = s_RddList[ uiCount ];
      if( strcmp( pNode->szName, szDriver ) == 0 ) /* Matched RDD */
      {
         if( uiIndex )
            *uiIndex = uiCount;
         return pNode;
      }
   }
   if( uiIndex )
      *uiIndex = 0;
   return NULL;
}

/*
 * Shutdown the RDD system.
 */
void hb_rddShutDown( void )
{
   HB_USHORT uiCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddShutDown()" ) );

   hb_rddCloseDetachedAreas();

   if( s_uiRddCount > 0 )
   {
      for( uiCount = 0; uiCount < s_uiRddCount; uiCount++ )
      {
         if( s_RddList[ uiCount ]->pTable.exit != NULL )
         {
            SELF_EXIT( s_RddList[ uiCount ] );
         }
         hb_xfree( s_RddList[ uiCount ] );
      }
      hb_xfree( s_RddList );
      s_RddList = NULL;
      s_uiRddMax = s_uiRddCount = 0;
   }
}

/*
 * Register a RDD driver.
 */
int hb_rddRegister( const char * szDriver, HB_USHORT uiType )
{
   LPRDDNODE pRddNewNode;
   PHB_DYNS pGetFuncTable;
   char szGetFuncTable[ HB_RDD_MAX_DRIVERNAME_LEN + 14 ];
   HB_USHORT uiFunctions = 0;
   int iResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddRegister(%s, %hu)", szDriver, uiType ) );

   if( hb_rddFindNode( szDriver, NULL ) )    /* Duplicated RDD */
      return 1;

   hb_snprintf( szGetFuncTable, sizeof( szGetFuncTable ), "%s_GETFUNCTABLE",
                szDriver );
   pGetFuncTable = hb_dynsymFindName( szGetFuncTable );
   if( ! pGetFuncTable )
      return 2;              /* Not valid RDD */

   /* Create a new RDD node */
   pRddNewNode = ( LPRDDNODE ) hb_xgrab( sizeof( RDDNODE ) );
   memset( pRddNewNode, 0, sizeof( RDDNODE ) );

   /* Fill the new RDD node */
   hb_strncpy( pRddNewNode->szName, szDriver, sizeof( pRddNewNode->szName ) - 1 );
   pRddNewNode->uiType = uiType;
   pRddNewNode->rddID = s_uiRddCount;
   pRddNewNode->rddSuperID = ( HB_USHORT ) ( -1 );

   /* Call <szDriver>_GETFUNCTABLE() */
   hb_vmPushDynSym( pGetFuncTable );
   hb_vmPushNil();
   hb_vmPushPointer( ( void * ) &uiFunctions );
   hb_vmPushPointer( ( void * ) &pRddNewNode->pTable );
   hb_vmPushPointer( ( void * ) &pRddNewNode->pSuperTable );
   hb_vmPushInteger( s_uiRddCount );
   hb_vmPushPointer( ( void * ) &pRddNewNode->rddSuperID );
   hb_vmProc( 5 );
   if( hb_parnidef( -1, HB_FAILURE ) != HB_SUCCESS )
      iResult = 3;                        /* Invalid FUNCTABLE */
   else
   {
      hb_threadEnterCriticalSection( &s_rddMtx );
      /* repeat the test to protect against possible registering RDD by
       *  <szDriver>_GETFUNCTABLE()
       */
      if( ! hb_rddFindNode( szDriver, NULL ) )    /* Duplicated RDD */
      {
         if( s_uiRddCount == s_uiRddMax )
         {
            s_uiRddMax += HB_RDD_POOL_ALLOCSIZE;
            s_RddList = ( LPRDDNODE * )
                  hb_xrealloc( s_RddList, sizeof( LPRDDNODE ) * s_uiRddMax );
         }
         s_RddList[ s_uiRddCount ] = pRddNewNode;   /* Add the new RDD node */
         s_uiRddCount++;
         iResult = 0;
      }
      else
         iResult = 1;
      hb_threadLeaveCriticalSection( &s_rddMtx );
   }

   if( iResult != 0 )
      hb_xfree( pRddNewNode );
   else if( pRddNewNode->pTable.init != NULL )
      SELF_INIT( pRddNewNode );

   return iResult;
}

/*
 * pTable - a table in new RDDNODE that will be filled
 * pSubTable - a table with a list of supported functions
 * pSuperTable - a current table in a RDDNODE
 * szDrvName - a driver name that will be inherited
 */
HB_ERRCODE hb_rddInheritEx( RDDFUNCS * pTable, const RDDFUNCS * pSubTable,
                            RDDFUNCS * pSuperTable, const char * szDrvName,
                            HB_USHORT * puiSuperRddId )
{
   LPRDDNODE pRddNode;
   HB_USHORT uiCount;
   DBENTRYP_V * pFunction;
   const DBENTRYP_V * pSubFunction;

   HB_TRACE( HB_TR_DEBUG, ( "hb_rddInheritEx(%p, %p, %p, %s, %p)", pTable, pSubTable, pSuperTable, szDrvName, puiSuperRddId ) );

   if( ! pTable )
   {
      return HB_FAILURE;
   }

   /* Copy the pSuperTable into pTable */
   if( ! szDrvName || ! *szDrvName )
   {
      /* no name for inherited driver - use the default one */
      memcpy( pTable, &waTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &waTable, sizeof( RDDFUNCS ) );
      if( puiSuperRddId )
         *puiSuperRddId = ( HB_USHORT ) -1;
   }
   else
   {
      char szSuperName[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
      hb_strncpyUpper( szSuperName, szDrvName, sizeof( szSuperName ) - 1 );
      pRddNode = hb_rddFindNode( szSuperName, NULL );

      if( ! pRddNode )
         return HB_FAILURE;

      memcpy( pTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
      if( puiSuperRddId )
         *puiSuperRddId = pRddNode->rddID;
   }

   /* Copy the non NULL entries from pSubTable into pTable */
   pFunction = ( DBENTRYP_V * ) pTable;
   pSubFunction = ( const DBENTRYP_V * ) pSubTable;
   for( uiCount = 0; uiCount < RDDFUNCSCOUNT; uiCount++ )
   {
      if( *pSubFunction )
         *pFunction = *pSubFunction;
      pFunction++;
      pSubFunction++;
   }
   return HB_SUCCESS;
}

HB_ERRCODE hb_rddInherit( RDDFUNCS * pTable, const RDDFUNCS * pSubTable,
                          RDDFUNCS * pSuperTable, const char * szDrvName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_rddInherit(%p, %p, %p, %s)", pTable, pSubTable, pSuperTable, szDrvName ) );

   return hb_rddInheritEx( pTable, pSubTable, pSuperTable, szDrvName, NULL );
}

HB_BOOL hb_rddIsDerivedFrom( HB_USHORT uiRddID, HB_USHORT uiSuperRddID )
{
   if( uiRddID == uiSuperRddID )
      return HB_TRUE;

   while( uiRddID < s_uiRddCount )
   {
      uiRddID = s_RddList[ uiRddID ]->rddSuperID;
      if( uiRddID == uiSuperRddID )
         return HB_TRUE;
   }
   return HB_FALSE;
}

/* extend the size of RDD nodes buffer to given value to avoid later
 * RT reallocations. It may be useful in some very seldom cases
 * for MT programs which will register dynamically at runtime
 * more then 128 RDDs.
 */
HB_FUNC( __RDDPREALLOCATE )
{
   HB_LONG lNewSize = hb_parnl( 1 );

   if( lNewSize > ( HB_LONG ) USHRT_MAX )
      lNewSize = USHRT_MAX;
   if( lNewSize > ( HB_LONG ) s_uiRddMax )
   {
      s_uiRddMax += HB_RDD_POOL_ALLOCSIZE;
      s_RddList = ( LPRDDNODE * )
                  hb_xrealloc( s_RddList, sizeof( LPRDDNODE ) * s_uiRddMax );
   }

   hb_retnl( s_uiRddMax );
}

HB_FUNC_EXTERN( RDDSYS );
extern void _hb_rddWorkAreaForceLink( void );
void _hb_rddWorkAreaForceLink( void )
{
   HB_FUNC_EXEC( RDDSYS );
}
