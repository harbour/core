/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * BM (bitmap filter) RDD
 *
 * Copyright 1999-2002 Bruno Cantero <bruno@issnet.net>
 * Copyright 2000-2003 Horacio Roldan <harbour_ar@yahoo.com.ar> (portions)
 * Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
 * Copyright 2006-2009 Miguel Angel Marchuet <miguelangel@marchuet.net>
 * www - http://www.xharbour.org
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
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbstack.h"
#include "hbbmcdx.h"
#include "hbmath.h"
#include "rddsys.ch"
#include "hbregex.h"
#include "hbapicdp.h"

static HB_USHORT s_uiRddId = ( HB_USHORT ) -1;

static LPCDXTAG hb_cdxGetActiveTag( CDXAREAP pArea );

static RDDFUNCS bmSuper;

/*
 * check and avaluate record filter
 */
static HB_BOOL hb_cdxCheckRecordFilter( CDXAREAP pArea, HB_ULONG ulRecNo )
{
   HB_BOOL lResult = HB_FALSE;
   HB_BOOL fDeleted = hb_setGetDeleted();

   if( pArea->dbfarea.area.dbfi.fFilter && pArea->dbfarea.area.dbfi.fOptimized )
   {
      if( BM_GetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, ulRecNo ) )
      {
         if( pArea->dbfarea.ulRecNo != ulRecNo || pArea->dbfarea.lpdbPendingRel )
            SELF_GOTO( ( AREAP ) pArea, ulRecNo );

         if( fDeleted )
            SUPER_DELETED( ( AREAP ) pArea, &lResult );

         if( !lResult && pArea->dbfarea.area.dbfi.itmCobExpr )
         {
            PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfarea.area.dbfi.itmCobExpr );
            lResult = HB_IS_LOGICAL( pResult ) && !hb_itemGetL( pResult );
            if( lResult )
            {
                LPCDXTAG pTag;
                BM_ClrBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, ulRecNo );
                pTag = hb_cdxGetActiveTag( pArea );
                if( pTag && CURKEY_LOGCNT(pTag)  )
                    CURKEY_SETLOGCNT( pTag, pTag->logKeyCount - 1 );
            }
         }
      }
      else
         lResult = HB_TRUE;
   }
   else if( pArea->dbfarea.area.dbfi.itmCobExpr || fDeleted )
   {
      if( pArea->dbfarea.ulRecNo != ulRecNo || pArea->dbfarea.lpdbPendingRel )
         SELF_GOTO( ( AREAP ) pArea, ulRecNo );

      if( fDeleted )
         SELF_DELETED( ( AREAP ) pArea, &lResult );

      if( !lResult && pArea->dbfarea.area.dbfi.itmCobExpr )
      {
         PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfarea.area.dbfi.itmCobExpr );
         lResult = HB_IS_LOGICAL( pResult ) && !hb_itemGetL( pResult );
      }
   }
   return !lResult;
}

#if ! defined( HB_SIXCDX )

static HB_BYTE * hb_cdxPageGetKeyValActual( LPCDXPAGE pPage )
{
    while( pPage->Child )
    {
       pPage = pPage->Child;
    }
    return hb_cdxPageGetKeyVal( pPage, pPage->iCurKey );
}


/*
 * compare two values using Tag conditions (len & type)
 */
static int hb_cdxValCompareWild( HB_BYTE * val1, HB_BYTE * val2, HB_BOOL fExact )
{
   return ( ( fExact ) ? hb_strMatchWildExact( ( const char * ) val2, (const char *) val1 ) : hb_strMatchWild( ( const char * ) val2, ( const char * ) val1 ) ) ? 0: 1;
}

/*
 * seek given Key in the Page or in its children
 */
static int hb_cdxPageSeekKeyWild( LPCDXPAGE pPage, LPCDXKEY pKey, HB_ULONG ulKeyRec, HB_BOOL fExact, HB_BOOL fNext  )
{
    int k;

    k = ( ulKeyRec == CDX_MAX_REC_NUM ) ? -1 : 1;

    if( fNext ?  hb_cdxPageReadNextKey( pPage ) : hb_cdxPageReadTopKey( pPage ) )
    {
        do
        {
            k = hb_cdxValCompareWild( pKey->val, hb_cdxPageGetKeyValActual( pPage ), fExact );
        }
        while( k && hb_cdxPageReadNextKey( pPage ) );

        if( k == 0 )
        {
            while( pPage->Child )
            {
               pPage = pPage->Child;
            }
            if( ulKeyRec == CDX_MAX_REC_NUM )
               k = 1;
            else if( ulKeyRec != CDX_IGNORE_REC_NUM )
            {
               HB_ULONG ulRec = hb_cdxPageGetKeyRec( pPage, pPage->iCurKey );
               if( ulKeyRec > ulRec )
                  k = 1;
               else if( ulKeyRec < ulRec )
                  k = -1;
            }
        }
    }
    return k;
}

/*
 * find pKey in pTag return 0 or TagNO
 */
static HB_ULONG hb_cdxTagKeyFindWild( LPCDXTAG pTag, LPCDXKEY pKey, HB_BOOL fNext )
{
   int K;
   HB_ULONG ulKeyRec = pKey->rec;

   pTag->fRePos = HB_FALSE;
   hb_cdxTagOpen( pTag );

   pTag->TagBOF = pTag->TagEOF = HB_FALSE;
   K = hb_cdxPageSeekKeyWild( pTag->RootPage, pKey, ulKeyRec, HB_FALSE, fNext );
   if( ulKeyRec == CDX_MAX_REC_NUM )
      K = - K;

   if( K > 0 )
   {
      pTag->CurKey->rec = 0;
      pTag->TagEOF = HB_TRUE;
   }
   else
   {
      hb_cdxSetCurKey( pTag->RootPage );
      if( K == 0 )
         return pTag->CurKey->rec;
   }
   return 0;
}


/* hb_cdxSeekWild */
static HB_ERRCODE hb_cdxSeekWild( CDXAREAP pArea, HB_BOOL fSoftSeek, PHB_ITEM pKeyItm, HB_BOOL fFindLast, HB_BOOL fNext, HB_BOOL bAll )
{
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSeekWild(%p, %d, %p, %d, %d)", pArea, fSoftSeek, pKeyItm, fFindLast, fNext));

   if( FAST_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pTag = hb_cdxGetActiveTag( pArea );

   if( ! pTag )
   {
      hb_cdxErrorRT( pArea, EG_NOORDER, 1201, NULL, 0, EF_CANDEFAULT, NULL );
      return HB_FAILURE;
   }
   else
   {
      LPCDXKEY pKey;
      HB_ERRCODE retval = HB_SUCCESS;
      HB_BOOL  fEOF = HB_FALSE, fLast;
      HB_ULONG ulRec;

      if( ! bAll && pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped )
         SELF_FORCEREL( ( AREAP ) pArea );

      pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = HB_FALSE;
      pArea->dbfarea.area.fEof = HB_FALSE;

      if( pTag->UsrUnique )
         fLast = !pTag->UsrAscend;
      else
         fLast = pTag->UsrAscend ? fFindLast : !fFindLast;

      /* TODO: runtime error if valtype(pKeyItm) != pTag->Type */
      pKey = hb_cdxKeyPutItem( NULL, pKeyItm, fLast ? CDX_MAX_REC_NUM : CDX_IGNORE_REC_NUM, pTag, HB_TRUE, HB_FALSE );

      hb_cdxIndexLockRead( pTag->pIndex );
      hb_cdxTagRefreshScope( pTag );
      ulRec = hb_cdxTagKeyFindWild( pTag, pKey, fNext );
      if( ( ulRec == 0 && ! fSoftSeek ) || pTag->TagEOF )
         fEOF = HB_TRUE;
      else /* if( fSoftSeek ) */
      {
         if( ! hb_cdxBottomScope( pTag ) )
            fEOF = HB_TRUE;
         else if( ! hb_cdxTopScope( pTag ) )
         {
            hb_cdxTagGoTop( pTag );
            if( pTag->CurKey->rec == 0 )
               fEOF = HB_TRUE;
         }
      }
      hb_cdxIndexUnLockRead( pTag->pIndex );
      if( !fEOF )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, pTag->CurKey->rec );
         if( retval != HB_FAILURE && pArea->dbfarea.fPositioned )
         {
            retval = SELF_SKIPFILTER( ( AREAP ) pArea, fFindLast ? -1 : 1 );
            if( retval != HB_FAILURE && ulRec && pArea->dbfarea.fPositioned )
            {
               pArea->dbfarea.area.fFound = ( ulRec == pArea->dbfarea.ulRecNo ||
                        hb_cdxValCompareWild( pKey->val, pTag->CurKey->val, HB_FALSE ) == 0 );
               if( ! pArea->dbfarea.area.fFound && ! fSoftSeek )
                  fEOF = HB_TRUE;
            }
         }
      }
      if( retval != HB_FAILURE &&
           ( fEOF || ! hb_cdxTopScope( pTag ) ||
                     ! hb_cdxBottomScope( pTag ) ) )
      {
         retval = SELF_GOTO( ( AREAP ) pArea, 0 );
      }
      pArea->dbfarea.area.fBof = HB_FALSE;
      hb_cdxKeyFree( pKey );
      return retval;
   }
}

HB_FUNC( BM_TURBO )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( BM_DBGETFILTERARRAY )
{
   CDXAREAP pArea = ( CDXAREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pList = hb_itemArrayNew( 0 );

   if( pArea->dbfarea.area.dbfi.fOptimized )
   {
      HB_ULONG ulSize = ( ( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo )->Size + 1 ) >> 5 ) + 1;
      HB_ULONG ulLong, ulByte, ulBytes, ulRecno;
      PHB_ITEM pItem = hb_itemNew( NULL );
      HB_ULONG ulRec, ulRecOld;

      ulRecOld = pArea->dbfarea.ulRecNo;

      for( ulLong = 0; ulLong < ulSize; ulLong++ )
         if( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap[ulLong] )
            for( ulByte = ( ulLong << 2 ), ulBytes = 0; ulBytes < 4; ulByte++, ulBytes++ )
               if( ( ( char * )( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo )->rmap )[ ulByte ] )
                  for( ulRec = ( ulByte << 3 ) + 1, ulRecno = 0; ulRecno < 8; ulRec++, ulRecno++ )
                     if( hb_cdxCheckRecordFilter( pArea, ulRec ) )
                        hb_arrayAddForward( pList, hb_itemPutNL( pItem, ulRec ) );

      SELF_GOTO( ( AREAP ) pArea, ulRecOld );
      hb_itemRelease( pItem );
   }
   hb_itemReturnRelease( pList );
}

HB_FUNC( BM_DBSETFILTERARRAY )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArea )
   {
      if( pArray )
      {
         HB_ULONG ulPos, ulRecCount;
         LPCDXTAG pTag;

         /* Limpiamos el filtro activo */
         if( SELF_CLEARFILTER( pArea ) != HB_SUCCESS )
            return;

         pArea->dbfi.fOptimized = HB_TRUE;
         pArea->dbfi.fFilter = HB_TRUE;

         SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
         pArea->dbfi.lpvCargo = hb_xgrab( sizeof( BM_FILTER ) );
         memset( pArea->dbfi.lpvCargo, 0, sizeof( BM_FILTER ) );

         ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->Size = ulRecCount;
         ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->rmap = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * ( ( ( ulRecCount + 1 ) >> 5 ) + 1) );
         memset( ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->rmap, 0, sizeof( HB_ULONG ) * ( ( ( ulRecCount + 1 ) >> 5 ) + 1 ) );

         for( ulPos = 1; ulPos <= hb_arrayLen( pArray ); ulPos++ )
            BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->rmap, ulRecCount, ( HB_ULONG ) hb_arrayGetNL( pArray, ulPos ) );
         pTag = hb_cdxGetActiveTag( ( CDXAREAP ) pArea );
         if( pTag ) /* Con índice activo */
         {
            pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_RAWCNT );
            CURKEY_SETLOGCNT( pTag, ( hb_arrayLen( pArray ) ) );
         }
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( BM_DBSETFILTERARRAYADD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   HB_ULONG ulPos,ulAdd = 0;

   if( pArea && pArea->dbfi.fOptimized )
   {
      if( pArray )
      {
         LPCDXTAG pTag;

         for( ulPos = 1; ulPos <= hb_arrayLen( pArray ); ulPos++ )
            if( ! BM_GetBit( ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->Size, ( HB_ULONG ) hb_arrayGetNL( pArray, ulPos ) ) )
            {
               BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->Size, ( HB_ULONG ) hb_arrayGetNL( pArray, ulPos ) );
               ulAdd++;
            }
         pTag = hb_cdxGetActiveTag( ( CDXAREAP ) pArea );
         if( pTag ) /* Con indice activo */
            CURKEY_SETLOGCNT( pTag, pTag->logKeyCount + ulAdd );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( BM_DBSETFILTERARRAYDEL )
{
   AREAP pArea = (AREAP) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   HB_ULONG ulPos,ulDel = 0;

   if( pArea && pArea->dbfi.fOptimized )
   {
      if( pArray )
      {
         LPCDXTAG pTag;

         for( ulPos = 1; ulPos <= hb_arrayLen( pArray ); ulPos++ )
         {
            if( BM_GetBit( ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->Size, ( HB_ULONG ) hb_arrayGetNL( pArray, ulPos ) ) )
            {
               BM_ClrBit( ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfi.lpvCargo)->Size, ( HB_ULONG ) hb_arrayGetNL( pArray, ulPos ) );
               ulDel++;
            }
         }
         pTag = hb_cdxGetActiveTag( (CDXAREAP) pArea );
         if( pTag ) /* Con indice activo */
             CURKEY_SETLOGCNT( pTag, pTag->logKeyCount - ulDel );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}


HB_FUNC( BM_DBSEEKWILD )
{
   PHB_ITEM pKey;
   HB_BOOL bAll, bNext, bSoftSeek, bFindLast, fFound;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      if( ! HB_ISNIL( 1 ) )
      {
         pKey = hb_param( 1, HB_IT_ANY );
         bSoftSeek = HB_ISLOG( 2 ) ? ( HB_BOOL ) hb_parl( 2 ) : hb_setGetSoftSeek();
         bFindLast = hb_parl( 3 );
         bNext     = hb_parl( 4 );
         bAll      = hb_parl( 5 );
         if( bAll )
         {
            PHB_ITEM pList = hb_itemArrayNew( 0 );
            SELF_GOTOP( ( AREAP ) pArea );
            if( hb_cdxSeekWild( ( CDXAREAP ) pArea, bSoftSeek, pKey, bFindLast, HB_FALSE, bAll ) == HB_SUCCESS &&
                pArea->fEof == HB_FALSE &&
                SELF_FOUND( pArea, &fFound ) == HB_SUCCESS )
            {
               hb_arrayAddForward( pList, hb_itemPutNL( NULL, ( ( CDXAREAP ) pArea )->dbfarea.ulRecNo ) );
               while( hb_cdxSeekWild( ( CDXAREAP ) pArea, bSoftSeek, pKey, bFindLast, HB_TRUE, bAll ) == HB_SUCCESS &&
                      pArea->fEof == HB_FALSE &&
                      SELF_FOUND( pArea, &fFound ) == HB_SUCCESS )
               {
                  hb_arrayAdd( pList, hb_itemPutNL( NULL, ( ( CDXAREAP ) pArea )->dbfarea.ulRecNo ) );
               }
            }
            hb_itemReturnRelease( pList );
            return;
         }
         else
         {
            if( hb_cdxSeekWild( ( CDXAREAP ) pArea, bSoftSeek, pKey, bFindLast, bNext, bAll ) == HB_SUCCESS )
            {
               if( SELF_FOUND( pArea, &fFound ) == HB_SUCCESS )
               {
                  hb_retl( fFound );
                  return;
               }
            }
         }
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );

   hb_retl( HB_FALSE );
}

#endif


/* ( DBENTRYP_L )     hb_cdxSkipFilter */
/*
 * Reposition cursor respecting any filter setting.
 */
static HB_ERRCODE hb_cdxSkipFilter( CDXAREAP pArea, HB_LONG lUpDown )
{
   HB_BOOL fBottom, fDeleted;
   HB_ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSkipFilter(%p, %ld)", pArea, lUpDown));

   if( ! hb_setGetDeleted() && ! pArea->dbfarea.area.dbfi.fFilter )
      return HB_SUCCESS;

   /* Since lToSkip is passed to SkipRaw, it should never request more than
      a single skip.
      The implied purpose of hb_cdxSkipFilter is to get off of a "bad" record
      after a skip was performed, NOT to skip lToSkip filtered records.
   */
   lUpDown = ( lUpDown < 0  ? -1 : 1 );

   /* remember if we are here after SELF_GOTOP() */
   fBottom = pArea->dbfarea.area.fBottom;

   while( !pArea->dbfarea.area.fBof && !pArea->dbfarea.area.fEof )
   {
      /* SET DELETED */
      if( hb_setGetDeleted() )
      {
         LPCDXTAG pTag = hb_cdxGetActiveTag( pArea );

         if( SELF_DELETED( (AREAP) pArea, &fDeleted ) != HB_SUCCESS )
            return HB_FAILURE;
         if( fDeleted )
         {
            if( SELF_SKIPRAW( (AREAP) pArea, lUpDown ) != HB_SUCCESS )
               return HB_FAILURE;
            else if( pTag )
               pTag->logKeyPos += lUpDown;
            continue;
         }
      }

      /* SET FILTER TO */
      if( pArea->dbfarea.area.dbfi.fFilter )
      {
         if( ! hb_cdxCheckRecordFilter( pArea, pArea->dbfarea.ulRecNo ) )
         {
            if( SELF_SKIPRAW( (AREAP) pArea, lUpDown ) != HB_SUCCESS )
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
   if( pArea->dbfarea.area.fBof && lUpDown < 0 )
   {
      if( fBottom )
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
         uiError = SELF_GOTO( (AREAP) pArea, 0 );
      }
      else
      {
         uiError = SELF_GOTOP( (AREAP) pArea );
         pArea->dbfarea.area.fBof = HB_TRUE;
      }
   }
   else
   {
      uiError = HB_SUCCESS;
   }

   return uiError;
}

/* ( DBENTRYP_B )     hb_cdxAppend */
static HB_ERRCODE hb_cdxAppend( CDXAREAP pArea, HB_BOOL bUnLockAll )
{
    if( SUPER_APPEND( (AREAP) pArea, bUnLockAll ) == HB_SUCCESS )
    {
        if( pArea->dbfarea.area.dbfi.fFilter && pArea->dbfarea.area.dbfi.fOptimized )
        {
            HB_ULONG ulRecCount, bytes;

            SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
            bytes = ( (ulRecCount + 1) >> 5 ) + 1;

            if( ( (ulRecCount) >> 5 ) + 1 < bytes )
            {
                ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap = ( HB_ULONG * ) hb_xrealloc( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, bytes << 2 );
                ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size = ulRecCount;
            }
            pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;
            if( hb_cdxCheckRecordFilter( pArea, ulRecCount ) )
            {
                LPCDXTAG pTag;
                BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, ulRecCount );
                pTag = hb_cdxGetActiveTag( (CDXAREAP) pArea );
                if( pTag && CURKEY_LOGCNT(pTag) ) /* Con índice activo */
                    CURKEY_SETLOGCNT( pTag, (pTag)->logKeyCount + 1 );
            }
            else
                BM_ClrBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, ulRecCount );
            pArea->dbfarea.area.dbfi.fFilter = HB_TRUE;
        }
        return HB_SUCCESS;
    }
    else
        return HB_FAILURE;
}
/* ( DBENTRYP_I )     hb_cdxCreateFields    : NULL */
/* ( DBENTRYP_V )     hb_cdxDeleteRec */
static HB_ERRCODE hb_cdxDeleteRec( CDXAREAP pArea )
{
    if( SUPER_DELETE( (AREAP) pArea ) == HB_SUCCESS )
    {
        if( pArea->dbfarea.area.dbfi.fFilter && pArea->dbfarea.area.dbfi.fOptimized )
        {
            pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;
            if( hb_cdxCheckRecordFilter( pArea, pArea->dbfarea.ulRecNo ) )
            {
                if( ! BM_GetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo ) )
                {
                    LPCDXTAG pTag;
                    BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo );
                    pTag = hb_cdxGetActiveTag( (CDXAREAP) pArea );
                    if( pTag && CURKEY_LOGCNT(pTag)  )
                        CURKEY_SETLOGCNT( pTag, (pTag)->logKeyCount + 1 );
                }
            }
            else
            {
                if( BM_GetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo ) )
                {
                    LPCDXTAG pTag;
                    BM_ClrBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo );
                    pTag = hb_cdxGetActiveTag( (CDXAREAP) pArea );
                    if( pTag && CURKEY_LOGCNT(pTag) )
                        CURKEY_SETLOGCNT( pTag, (pTag)->logKeyCount - 1 );
                }
            }
            pArea->dbfarea.area.dbfi.fFilter = HB_TRUE;
        }
        return HB_SUCCESS;
    }
    else
        return HB_FAILURE;
}

/* ( DBENTRYP_P )     hb_cdxPutRec          : NULL */
static HB_ERRCODE hb_cdxPutRec( CDXAREAP pArea, HB_BYTE * pBuffer )
{
    if( SUPER_PUTREC( (AREAP) pArea, pBuffer ) == HB_SUCCESS )
    {
        if( pArea->dbfarea.area.dbfi.fFilter && pArea->dbfarea.area.dbfi.fOptimized )
        {
            pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;

            if( hb_cdxCheckRecordFilter( pArea, pArea->dbfarea.ulRecNo ) )
            {
                if( ! BM_GetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo ) )
                {
                    LPCDXTAG pTag;
                    BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo );
                    pTag = hb_cdxGetActiveTag( pArea );
                    if( pTag )
                        CURKEY_SETLOGCNT( pTag, pTag->logKeyCount + 1 );
                }
            }
            else
            {
                if( BM_GetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo ) )
                {
                    LPCDXTAG pTag;
                    BM_ClrBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo );
                    pTag = hb_cdxGetActiveTag( pArea );
                    if( pTag )
                        CURKEY_SETLOGCNT( pTag, pTag->logKeyCount - 1 );
                }
            }
            pArea->dbfarea.area.dbfi.fFilter = HB_TRUE;
        }
        return HB_SUCCESS;
    }
    else
        return HB_FAILURE;
}
/* ( DBENTRYP_SI )    hb_cdxPutValue        : NULL */
/* ( DBENTRYP_V )     hb_cdxRecall */
static HB_ERRCODE hb_cdxRecall( CDXAREAP pArea )
{
    if( SUPER_RECALL( (AREAP) pArea ) == HB_SUCCESS )
    {
        if( pArea->dbfarea.area.dbfi.fFilter && pArea->dbfarea.area.dbfi.fOptimized )
        {
            pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;
            if( hb_cdxCheckRecordFilter( pArea, pArea->dbfarea.ulRecNo ) )
                BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo );
            else
                BM_ClrBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size, pArea->dbfarea.ulRecNo );
            pArea->dbfarea.area.dbfi.fFilter = HB_TRUE;
        }
        return HB_SUCCESS;
    }
    else
        return HB_FAILURE;
}


/* ( DBENTRYP_V )     hb_cdxClearFilter */
static HB_ERRCODE hb_cdxClearFilter( CDXAREAP pArea )
{
   HB_ERRCODE errCode = SUPER_CLEARFILTER( ( AREAP ) pArea );
   hb_cdxClearLogPosInfo( pArea );
   /* Limpiamos filtro tipo array */
   if( pArea->dbfarea.area.dbfi.lpvCargo )
   {
        hb_xfree( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap );
        hb_xfree( pArea->dbfarea.area.dbfi.lpvCargo );
        pArea->dbfarea.area.dbfi.lpvCargo = NULL;
   }
   return errCode;
}

/* ( DBENTRYP_V )     hb_cdxClearLocate     : NULL */
/* ( DBENTRYP_V )     hb_cdxClearScope      : NULL */

/* ( DBENTRYP_VPLP )  hb_cdxCountScope */
static HB_ERRCODE hb_cdxCountScope( CDXAREAP pArea, void * pPtr, HB_LONG * plRec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxCountScope(%p, %p, %p)", pArea, pPtr, plRec));

   if( pPtr == NULL )
   {
      LPBM_FILTER pMap = (LPBM_FILTER) pArea->dbfarea.area.dbfi.lpvCargo;
      if( pArea->dbfarea.area.dbfi.fFilter && pMap &&
          !BM_GetBit( pMap->rmap, pMap->Size, ( HB_ULONG ) *plRec ) )
      {
         *plRec = 0;
      }
      return HB_SUCCESS;
   }
   return SUPER_COUNTSCOPE( ( AREAP ) pArea, pPtr, plRec );
}

/* ( DBENTRYP_I )     hb_cdxFilterText      : NULL */
/* ( DBENTRYP_SI )    hb_cdxScopeInfo       : NULL */

/* ( DBENTRYP_VFI )   hb_cdxSetFilter */
static HB_ERRCODE hb_cdxSetFilter( CDXAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
    HB_ULONG ulRecCount = 0, ulLogKeyCount = 0;
    LPCDXTAG pTag;
    PHB_ITEM pResult;

    HB_SYMBOL_UNUSED( ulLogKeyCount );

    hb_cdxClearLogPosInfo( pArea );

    if( SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo ) != HB_SUCCESS )
        return HB_FAILURE;

    pArea->dbfarea.area.dbfi.fOptimized = hb_setGetOptimize();

    if( pArea->dbfarea.area.dbfi.fOptimized )
    {
        pArea->dbfarea.area.dbfi.lpvCargo = hb_xgrab( sizeof( BM_FILTER ) );
        memset( pArea->dbfarea.area.dbfi.lpvCargo, 0, sizeof( BM_FILTER ) );

        pArea->dbfarea.area.dbfi.fFilter = HB_FALSE;

        pTag = hb_cdxGetActiveTag( pArea );
        SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );
        ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->Size = ulRecCount;
        ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap = ( HB_ULONG * ) hb_xgrab( sizeof( HB_ULONG ) * (((ulRecCount+1) >> 5) + 1 ) );
        memset( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, 0, sizeof( HB_ULONG ) * (((ulRecCount+1) >> 5) + 1 ) );

        if( pTag ) /* with active index */
        {
            if( FAST_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
               return HB_FAILURE;

            hb_cdxIndexLockRead( pTag->pIndex );
            hb_cdxTagRefreshScope( pTag );
            hb_cdxTagGoTop( pTag );
            ulLogKeyCount = 0;
            while( !pTag->TagEOF )
            {
               if( pArea->dbfarea.ulRecNo != pTag->CurKey->rec || pArea->dbfarea.lpdbPendingRel )
                  SELF_GOTO( (AREAP) pArea, pTag->CurKey->rec );
               pResult = hb_vmEvalBlock( pArea->dbfarea.area.dbfi.itmCobExpr );
               if( HB_IS_LOGICAL( pResult ) && hb_itemGetL( pResult ) )
               {
                  BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ulRecCount, pTag->CurKey->rec );
                  ulLogKeyCount++;
               }
               hb_cdxTagSkipNext( pTag );
            }
            hb_cdxIndexUnLockRead( pTag->pIndex );
            pTag->curKeyState &= ~( CDX_CURKEY_RAWPOS | CDX_CURKEY_RAWCNT );
            CURKEY_SETLOGCNT( pTag, ulLogKeyCount );
        }
        else
        {
            SELF_GOTOP( ( AREAP ) pArea );
            while( ! pArea->dbfarea.area.fEof )
            {
                pResult = hb_vmEvalBlock( pArea->dbfarea.area.dbfi.itmCobExpr );
                if( HB_IS_LOGICAL( pResult ) && hb_itemGetL( pResult ) )
                    BM_SetBit( ( ( LPBM_FILTER ) pArea->dbfarea.area.dbfi.lpvCargo)->rmap, ulRecCount, pArea->dbfarea.ulRecNo );
                SELF_SKIP( ( AREAP ) pArea, 1 );
            }
        }
        pArea->dbfarea.area.dbfi.fFilter = HB_TRUE;
    }

    return HB_SUCCESS;
}


static const RDDFUNCS cdxTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )    NULL,   /* hb_cdxBof */
   ( DBENTRYP_BP )    NULL,   /* hb_cdxEof */
   ( DBENTRYP_BP )    NULL,   /* hb_cdxFound */
   ( DBENTRYP_V )     NULL,   /* hb_cdxGoBottom */
   ( DBENTRYP_UL )    NULL,   /* hb_cdxGoTo */
   ( DBENTRYP_I )     NULL,   /* hb_cdxGoToId */
   ( DBENTRYP_V )     NULL,   /* hb_cdxGoTop */
   ( DBENTRYP_BIB )   NULL,   /* hb_cdxSeek */
   ( DBENTRYP_L )     NULL,   /* hb_cdxSkip */
   ( DBENTRYP_L )     hb_cdxSkipFilter,
   ( DBENTRYP_L )     NULL,   /* hb_cdxSkipRaw */


   /* Data management */

   ( DBENTRYP_VF )    NULL,   /* hb_cdxAddField */
   ( DBENTRYP_B )     hb_cdxAppend,
   ( DBENTRYP_I )     NULL,   /* hb_cdxCreateFields */
   ( DBENTRYP_V )     hb_cdxDeleteRec,
   ( DBENTRYP_BP )    NULL,   /* hb_cdxDeleted */
   ( DBENTRYP_SP )    NULL,   /* hb_cdxFieldCount */
   ( DBENTRYP_VF )    NULL,   /* hb_cdxFieldDisplay */
   ( DBENTRYP_SSI )   NULL,   /* hb_cdxFieldInfo */
   ( DBENTRYP_SCP )   NULL,   /* hb_cdxFieldName */
   ( DBENTRYP_V )     NULL,   /* hb_cdxFlush */
   ( DBENTRYP_PP )    NULL,   /* hb_cdxGetRec */
   ( DBENTRYP_SI )    NULL,   /* hb_cdxGetValue */
   ( DBENTRYP_SVL )   NULL,   /* hb_cdxGetVarLen */
   ( DBENTRYP_V )     NULL,   /* hb_cdxGoCold */
   ( DBENTRYP_V )     NULL,   /* hb_cdxGoHot */
   ( DBENTRYP_P )     hb_cdxPutRec,
   ( DBENTRYP_SI )    NULL,   /* hb_cdxPutValue */
   ( DBENTRYP_V )     hb_cdxRecall,   /* hb_cdxRecall */
   ( DBENTRYP_ULP )   NULL,   /* hb_cdxRecCount */
   ( DBENTRYP_ISI )   NULL,   /* hb_cdxRecInfo */
   ( DBENTRYP_ULP )   NULL,   /* hb_cdxRecNo */
   ( DBENTRYP_I )     NULL,   /* hb_cdxRecId */
   ( DBENTRYP_S )     NULL,   /* hb_cdxSetFieldExtent */


   /* WorkArea/Database management */

   ( DBENTRYP_CP )    NULL,   /* hb_cdxAlias */
   ( DBENTRYP_V )     NULL,   /* hb_cdxClose */
   ( DBENTRYP_VO )    NULL,   /* hb_cdxCreate */
   ( DBENTRYP_SI )    NULL,   /* hb_cdxInfo */
   ( DBENTRYP_V )     NULL,   /* hb_cdxNewArea */
   ( DBENTRYP_VO )    NULL,   /* hb_cdxOpen */
   ( DBENTRYP_V )     NULL,   /* hb_cdxRelease */
   ( DBENTRYP_SP )    NULL,   /* hb_cdxStructSize */
   ( DBENTRYP_CP )    NULL,   /* hb_cdxSysName */
   ( DBENTRYP_VEI )   NULL,   /* hb_cdxEval */
   ( DBENTRYP_V )     NULL,   /* hb_cdxPack */
   ( DBENTRYP_LSP )   NULL,   /* hb_cdxPackRec */
   ( DBENTRYP_VS )    NULL,   /* hb_cdxSort */
   ( DBENTRYP_VT )    NULL,   /* hb_cdxTrans */
   ( DBENTRYP_VT )    NULL,   /* hb_cdxTransRec */
   ( DBENTRYP_V )     NULL,   /* hb_cdxZap */


   /* Relational Methods */

   ( DBENTRYP_VR )    NULL,   /* hb_cdxChildEnd */
   ( DBENTRYP_VR )    NULL,   /* hb_cdxChildStart */
   ( DBENTRYP_VR )    NULL,   /* hb_cdxChildSync */
   ( DBENTRYP_V )     NULL,   /* hb_cdxSyncChildren */
   ( DBENTRYP_V )     NULL,   /* hb_cdxClearRel */
   ( DBENTRYP_V )     NULL,   /* hb_cdxForceRel */
   ( DBENTRYP_SSP )   NULL,   /* hb_cdxRelArea */
   ( DBENTRYP_VR )    NULL,   /* hb_cdxRelEval */
   ( DBENTRYP_SI )    NULL,   /* hb_cdxRelText */
   ( DBENTRYP_VR )    NULL,   /* hb_cdxSetRel */


   /* Order Management */

   ( DBENTRYP_VOI )   NULL,   /* hb_cdxOrderListAdd */
   ( DBENTRYP_V )     NULL,   /* hb_cdxOrderListClear */
   ( DBENTRYP_VOI )   NULL,   /* hb_cdxOrderListDelete */
   ( DBENTRYP_VOI )   NULL,   /* hb_cdxOrderListFocus */
   ( DBENTRYP_V )     NULL,   /* hb_cdxOrderListRebuild */
   ( DBENTRYP_VOO )   NULL,   /* hb_cdxOrderCondition */
   ( DBENTRYP_VOC )   NULL,   /* hb_cdxOrderCreate */
   ( DBENTRYP_VOI )   NULL,   /* hb_cdxOrderDestroy */
   ( DBENTRYP_SVOI )  NULL,   /* hb_cdxOrderInfo */


   /* Filters and Scope Settings */

   ( DBENTRYP_V )     hb_cdxClearFilter,
   ( DBENTRYP_V )     NULL,   /* hb_cdxClearLocate */
   ( DBENTRYP_V )     NULL,   /* hb_cdxClearScope */
   ( DBENTRYP_VPLP )  hb_cdxCountScope,
   ( DBENTRYP_I )     NULL,   /* hb_cdxFilterText */
   ( DBENTRYP_SI )    NULL,   /* hb_cdxScopeInfo */
   ( DBENTRYP_VFI )   hb_cdxSetFilter,
   ( DBENTRYP_VLO )   NULL,   /* hb_cdxSetLocate */
   ( DBENTRYP_VOS )   NULL,   /* hb_cdxSetScope */
   ( DBENTRYP_VPL )   NULL,   /* hb_cdxSkipScope */
   ( DBENTRYP_B )     NULL,   /* hb_cdxLocate */


   /* Miscellaneous */

   ( DBENTRYP_CC )    NULL,   /* hb_cdxCompile */
   ( DBENTRYP_I )     NULL,   /* hb_cdxError */
   ( DBENTRYP_I )     NULL,   /* hb_cdxEvalBlock */


   /* Network operations */

   ( DBENTRYP_VSP )   NULL,   /* hb_cdxRawLock */
   ( DBENTRYP_VL )    NULL,   /* hb_cdxLock */
   ( DBENTRYP_I )     NULL,   /* hb_cdxUnLock */


   /* Memofile functions */

   ( DBENTRYP_V )     NULL,   /* hb_cdxCloseMemFile */
   ( DBENTRYP_VO )    NULL,   /* hb_cdxCreateMemFile */
   ( DBENTRYP_SCCS )  NULL,   /* hb_cdxGetValueFile */
   ( DBENTRYP_VO )    NULL,   /* hb_cdxOpenMemFile */
   ( DBENTRYP_SCCS )  NULL,   /* hb_cdxPutValueFile */


   /* Database file header handling */

   ( DBENTRYP_V )     NULL,   /* hb_cdxReadDBHeader */
   ( DBENTRYP_V )     NULL,   /* hb_cdxWriteDBHeader */


   /* non WorkArea functions       */

   ( DBENTRYP_R )     NULL,   /* hb_cdxInit */
   ( DBENTRYP_R )     NULL,   /* hb_cdxExit */
   ( DBENTRYP_RVVL )  NULL,   /* hb_cdxDrop */
   ( DBENTRYP_RVVL )  NULL,   /* hb_cdxExists */
   ( DBENTRYP_RVVVL ) NULL,   /* hb_cdxRename */
   ( DBENTRYP_RSLV )  NULL,   /* hb_cdxRddInfo */


   /* Special and reserved methods */

   ( DBENTRYP_SVP )   NULL    /* hb_cdxWhoCares */
};

#if defined( HB_SIXCDX )

HB_FUNC_EXTERN( SIXCDX );

HB_FUNC( BMSIXCDX ) {;}

HB_FUNC( BMSIXCDX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   HB_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   uiRddId = hb_parni( 4 );
   puiSuperRddId = ( HB_USHORT * ) hb_parptr( 5 );

   HB_TRACE(HB_TR_DEBUG, ("BMSIXCDX_GETFUNCTABLE(%p, %p)", puiCount, pTable));

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( puiCount )
         * puiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInheritEx( pTable, &cdxTable, &bmSuper, "DBFFPT", puiSuperRddId );
      if( errCode != HB_SUCCESS )
         errCode = hb_rddInheritEx( pTable, &cdxTable, &bmSuper, "DBFDBT", puiSuperRddId );
      if( errCode != HB_SUCCESS )
         errCode = hb_rddInheritEx( pTable, &cdxTable, &bmSuper, "DBF", puiSuperRddId );
      hb_retni( errCode );
      if( errCode == HB_SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         s_uiRddId = uiRddId;
      }
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_bmsixcdxRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBF",    RDT_FULL ) <= 1 )
   {
      hb_rddRegister( "DBFFPT", RDT_FULL );
      if( hb_rddRegister( "BMSIXCDX", RDT_FULL ) <= 1 )
         return;
   }

   hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );

   /* not executed, only to force linking DBFCDX RDD */
   HB_FUNC_EXEC( SIXCDX );
}

HB_INIT_SYMBOLS_BEGIN( bmsixcdx1__InitSymbols )
{ "BMSIXCDX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMSIXCDX )}, NULL },
{ "BMSIXCDX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMSIXCDX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( bmsixcdx1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_bmsixcdx_rdd_init_ )
   hb_vmAtInit( hb_bmsixcdxRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_bmsixcdx_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup bmsixcdx1__InitSymbols
   #pragma startup _hb_bmsixcdx_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( bmsixcdx1__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_bmsixcdx_rdd_init_ )
   #include "hbiniseg.h"
#endif

#else

HB_FUNC_EXTERN( DBFCDX );

HB_FUNC( BMDBFCDX ) {;}

HB_FUNC( BMDBFCDX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   HB_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   uiRddId = hb_parni( 4 );
   puiSuperRddId = ( HB_USHORT * ) hb_parptr( 5 );

   HB_TRACE(HB_TR_DEBUG, ("BMDBFCDX_GETFUNCTABLE(%p, %p)", puiCount, pTable));

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( puiCount )
         * puiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInheritEx( pTable, &cdxTable, &bmSuper, "DBFFPT", puiSuperRddId );
      if( errCode != HB_SUCCESS )
         errCode = hb_rddInheritEx( pTable, &cdxTable, &bmSuper, "DBFDBT", puiSuperRddId );
      if( errCode != HB_SUCCESS )
         errCode = hb_rddInheritEx( pTable, &cdxTable, &bmSuper, "DBF", puiSuperRddId );
      if( errCode == HB_SUCCESS )
      {
         /*
          * we successfully register our RDD so now we can initialize it
          * You may think that this place is RDD init statement, Druzus
          */
         s_uiRddId = uiRddId;
      }
      hb_retni( errCode );
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_bmdbfcdxRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBF", RDT_FULL ) <= 1 )
   {
      hb_rddRegister( "DBFFPT", RDT_FULL );
      if( hb_rddRegister( "BMDBFCDX", RDT_FULL ) <= 1 )
         return;
   }

   hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );

   /* not executed, only to force linking DBF RDD */
   HB_FUNC_EXEC( DBFCDX );
}

HB_INIT_SYMBOLS_BEGIN( bmdbfcdx1__InitSymbols )
{ "BMDBFCDX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFCDX )}, NULL },
{ "BMDBFCDX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFCDX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( bmdbfcdx1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_bmdbfcdx_rdd_init_ )
   hb_vmAtInit( hb_bmdbfcdxRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_bmdbfcdx_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup bmdbfcdx1__InitSymbols
   #pragma startup _hb_bmdbfcdx_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( bmdbfcdx1__InitSymbols ) \
                              HB_DATASEG_FUNC( _hb_bmdbfcdx_rdd_init_ )
   #include "hbiniseg.h"
#endif

#endif
