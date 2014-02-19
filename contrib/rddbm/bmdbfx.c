/*
 * Harbour Project source code:
 *    Alternative BMDBF* implementation which respects RDD inheritance
 *    scheme and gives similar functionality and PRG functions as modified
 *    by Miguel Angel Marchuet <miguelangel@marchuet.net> DBFCDX with
 *    directly hardcoded bitmap filters.
 *    This code is completely new implementation and does not contain
 *    any code created by Miguel.
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
#include "hbapirdd.h"
#include "hbapierr.h"
#include "hbdbferr.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbinit.h"
#include "rddsys.ch"


/* now this function is RDD independent and can work with any RDD suporting
 * DBOI_SKIPWILD and DBOI_SKIPWILDBACK
 */
HB_FUNC( BM_DBSEEKWILD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      const char * szPattern = hb_parc( 1 );

      if( szPattern )
      {
         HB_BOOL fSoft, fBack, fCont, fAll, fFound, fUnlock;
         DBORDERINFO OrderInfo;
         HB_ERRCODE errCode;
         PHB_ITEM pArray = NULL;
         int iOrder = 0;

         fSoft  = hb_parldef( 2, hb_setGetSoftSeek() );
         fBack  = hb_parl( 3 );
         fCont  = hb_parl( 4 );
         fAll   = hb_parl( 5 );
         fFound = HB_FALSE;

         if( fAll )
         {
            pArray = hb_itemArrayNew( 0 );
            fCont = HB_FALSE;
         }

         memset( &OrderInfo, 0, sizeof( OrderInfo ) );
         OrderInfo.itmResult = hb_itemNew( NULL );

         errCode = SELF_ORDINFO( pArea, DBOI_NUMBER, &OrderInfo );
         if( errCode == HB_SUCCESS )
            iOrder = hb_itemGetNI( OrderInfo.itmResult );

         if( iOrder != 0 )
         {
            OrderInfo.itmNewVal = OrderInfo.itmResult;
            hb_itemPutL( OrderInfo.itmNewVal, HB_TRUE );
            if( SELF_ORDINFO( pArea, DBOI_READLOCK, &OrderInfo ) == HB_SUCCESS )
               fUnlock = hb_itemGetL( OrderInfo.itmResult );
            else
               fUnlock = HB_FALSE;
            OrderInfo.itmNewVal = NULL;

            if( ! fCont )
            {
               const char * szKey;

               if( fBack )
                  errCode = SELF_GOBOTTOM( pArea );
               else
                  errCode = SELF_GOTOP( pArea );

               if( errCode == HB_SUCCESS )
               {
                  errCode = SELF_ORDINFO( pArea, DBOI_KEYVAL, &OrderInfo );
                  if( errCode == HB_SUCCESS )
                  {
                     szKey = hb_itemGetCPtr( OrderInfo.itmResult );
                     fFound = hb_strMatchWild( szKey, szPattern );
                  }
               }
            }

            if( ! fFound && errCode == HB_SUCCESS )
            {
               OrderInfo.itmNewVal = hb_param( 1, HB_IT_STRING );
               errCode = SELF_ORDINFO( pArea, fBack ? DBOI_SKIPWILDBACK :
                                                      DBOI_SKIPWILD, &OrderInfo );
               if( errCode == HB_SUCCESS )
                  fFound = hb_itemGetL( OrderInfo.itmResult );
            }

            if( fAll && errCode == HB_SUCCESS )
            {
               OrderInfo.itmNewVal = hb_param( 1, HB_IT_STRING );
               do
               {
                  errCode = SELF_RECID( pArea, OrderInfo.itmResult );
                  if( errCode != HB_SUCCESS )
                     break;
                  hb_arrayAddForward( pArray, OrderInfo.itmResult );
                  errCode = SELF_ORDINFO( pArea, fBack ? DBOI_SKIPWILDBACK :
                                                         DBOI_SKIPWILD, &OrderInfo );
                  if( errCode == HB_SUCCESS )
                     fFound = hb_itemGetL( OrderInfo.itmResult );
                  else
                     fFound = HB_FALSE;
               }
               while( fFound );
            }
            if( fUnlock )
            {
               OrderInfo.itmNewVal = OrderInfo.itmResult;
               hb_itemPutL( OrderInfo.itmNewVal, HB_FALSE );
               SELF_ORDINFO( pArea, DBOI_READLOCK, &OrderInfo );
            }
         }

         hb_itemRelease( OrderInfo.itmResult );

         if( ! fFound && ! fSoft && errCode == HB_SUCCESS )
            SELF_GOTO( pArea, 0 );

         if( pArray )
            hb_itemReturnRelease( pArray );
         else
            hb_retl( fFound );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( BM_TURBO )
{
   hb_retl( HB_FALSE );
}


typedef struct
{
   HB_U32 maxrec;
   HB_U32 map[ 1 ];
} BM_FILTER, * PBM_FILTER;

#define BM_GETFILTER( p )  ( ( PBM_FILTER ) ( p )->dbfi.lpvCargo )
#define BM_ITEMSIZE( n )   ( ( ( n ) + 31 ) >> 5 )
#define BM_BYTESIZE( n )   ( ( ( ( n ) + 31 ) >> 5 ) * sizeof( HB_U32 ) )

#define BM_SETREC( p, r )  \
   do { if( ( r ) > 0 && ( r ) <= ( p )->maxrec ) \
           ( p )->map[ ( ( r ) - 1 ) >> 5 ] |= ( 1 << ( ( ( r ) - 1 ) & 0x1f ) ); \
   } while( 0 )

#define BM_CLRREC( p, r )  \
   do { if( ( r ) > 0 && ( r ) <= ( p )->maxrec ) \
           ( p )->map[ ( ( r ) - 1 ) >> 5 ] &= ~( 1 << ( ( ( r ) - 1 ) & 0x1f ) ); \
   } while( 0 )

#define BM_GETREC( p, r )  ( ( ( r ) > 0 && ( r ) <= ( p )->maxrec ) && \
                             ( ( p )->map[ ( ( r ) - 1 ) >> 5 ] & ( 1 << ( ( ( r ) - 1 ) & 0x1f ) ) ) != 0 )

#define SUPERTABLE  ( hb_bmGetRdd( pArea->rddID ) )

#define BM_RDD_MAX  8

static HB_USHORT s_uiRdds[ BM_RDD_MAX ];
static int s_iRddCount = 0;

static void hb_bmSetRdd( HB_USHORT uiRddId )
{
   if( s_iRddCount < BM_RDD_MAX )
      s_uiRdds[ s_iRddCount++ ] = uiRddId;
}

static const RDDFUNCS * hb_bmGetRdd( HB_USHORT uiRddId )
{
   int i;

   for( i = 0; i < s_iRddCount; ++i )
   {
      if( hb_rddIsDerivedFrom( uiRddId, s_uiRdds[ i ] ) )
         return &( hb_rddGetNode( s_uiRdds[ i ] )->pSuperTable );
   }
   return NULL;
}

static void hb_bmResetFilterOpt( AREAP pArea )
{
   DBORDERINFO OrderInfo;

   memset( &OrderInfo, 0, sizeof( OrderInfo ) );
   SELF_ORDINFO( pArea, DBOI_RESETPOS, &OrderInfo );
   if( OrderInfo.itmResult )
      hb_itemRelease( OrderInfo.itmResult );
}

static HB_BOOL hb_bmCheckRecordFilter( AREAP pArea, HB_ULONG ulRecNo )
{
   HB_BOOL lResult = HB_FALSE;
   HB_BOOL fDeleted = hb_setGetDeleted();

   if( pArea->dbfi.itmCobExpr || fDeleted )
   {
      HB_ULONG ulRec;

      if( SELF_RECNO( pArea, &ulRec ) == HB_SUCCESS )
      {
         if( ulRec != ulRecNo )
            SELF_GOTO( pArea, ulRecNo );

         if( fDeleted )
            SELF_DELETED( pArea, &lResult );

         if( ! lResult && pArea->dbfi.itmCobExpr )
         {
            PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
            lResult = HB_IS_LOGICAL( pResult ) && ! hb_itemGetL( pResult );
         }
      }
   }
   return ! lResult;
}

static AREAP hb_bmGetCurrentWorkArea( void )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( ! pArea )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
   else if( hb_bmGetRdd( pArea->rddID ) == NULL )
   {
      hb_errRT_DBCMD( EG_UNSUPPORTED, EDBF_UNSUPPORTED, NULL, HB_ERR_FUNCNAME );
      pArea = NULL;
   }

   return pArea;
}

static PHB_ITEM hb_bmGetArrayParam( int iParam )
{
   PHB_ITEM pArray = hb_param( iParam, HB_IT_ARRAY );

   if( ! pArray )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );

   return pArray;
}

static PBM_FILTER hb_bmCreate( AREAP pArea, HB_BOOL fFull )
{
   PBM_FILTER pBM = NULL;
   HB_ULONG ulRecCount;

   if( SELF_RECCOUNT( pArea, &ulRecCount ) == HB_SUCCESS )
   {
      HB_SIZE nSize = sizeof( BM_FILTER ) + BM_BYTESIZE( ulRecCount );
      pBM = ( PBM_FILTER ) memset( hb_xgrab( nSize ), fFull ? 0xFF : 0x00, nSize );
      pBM->maxrec = ( HB_U32 ) ulRecCount;
   }
   return pBM;
}

HB_FUNC( BM_DBGETFILTERARRAY )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea )
   {
      PBM_FILTER pBM = BM_GETFILTER( pArea );
      PHB_ITEM pArray = hb_itemArrayNew( 0 );

      if( pBM && pArea->dbfi.fOptimized )
      {
         HB_ULONG ul, ulItems = BM_ITEMSIZE( pBM->maxrec );
         HB_ULONG ulRecNo;

         if( SELF_RECNO( pArea, &ulRecNo ) == HB_SUCCESS )
         {
            PHB_ITEM pItem = hb_itemNew( NULL );

            for( ul = 0; ul < ulItems; ul++ )
            {
               if( pBM->map[ ul ] )
               {
                  HB_U32 nBits = pBM->map[ ul ];
                  HB_ULONG ulRec = ul << 5;

                  do
                  {
                     ++ulRec;
                     if( nBits & 1 )
                     {
                        if( hb_bmCheckRecordFilter( pArea, ulRec ) )
                           hb_arrayAddForward( pArray, hb_itemPutNL( pItem, ulRec ) );
                     }
                     nBits >>= 1;
                  }
                  while( nBits );
               }
            }
            hb_itemRelease( pItem );

            SELF_GOTO( pArea, ulRecNo );
         }
      }
      hb_itemReturnRelease( pArray );
   }
}

HB_FUNC( BM_DBSETFILTERARRAY )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea )
   {
      PHB_ITEM pArray = hb_bmGetArrayParam( 1 );

      if( pArray )
      {
         if( SELF_CLEARFILTER( pArea ) == HB_SUCCESS )
         {
            PBM_FILTER pBM = hb_bmCreate( pArea, HB_FALSE );

            if( pBM )
            {
               HB_SIZE nPos;

               pArea->dbfi.lpvCargo   = pBM;
               pArea->dbfi.fOptimized = HB_TRUE;
               pArea->dbfi.fFilter    = HB_TRUE;

               for( nPos = hb_arrayLen( pArray ); nPos; nPos-- )
               {
                  HB_ULONG ulRec = ( HB_ULONG ) hb_arrayGetNL( pArray, nPos );
                  BM_SETREC( pBM, ulRec );
               }
            }
         }
      }
   }
}

HB_FUNC( BM_DBSETFILTERARRAYADD )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea && pArea->dbfi.fOptimized )
   {
      PHB_ITEM pArray = hb_bmGetArrayParam( 1 );

      if( pArray )
      {
         PBM_FILTER pBM = BM_GETFILTER( pArea );

         if( pBM )
         {
            HB_SIZE nPos;

            for( nPos = hb_arrayLen( pArray ); nPos; nPos-- )
            {
               HB_ULONG ulRec = ( HB_ULONG ) hb_arrayGetNL( pArray, nPos );
               BM_SETREC( pBM, ulRec );
            }
            hb_bmResetFilterOpt( pArea );
         }
      }
   }
}

HB_FUNC( BM_DBSETFILTERARRAYDEL )
{
   AREAP pArea = hb_bmGetCurrentWorkArea();

   if( pArea && pArea->dbfi.fOptimized )
   {
      PHB_ITEM pArray = hb_bmGetArrayParam( 1 );

      if( pArray )
      {
         PBM_FILTER pBM = BM_GETFILTER( pArea );

         if( pBM )
         {
            HB_SIZE nPos;

            for( nPos = hb_arrayLen( pArray ); nPos; nPos-- )
            {
               HB_ULONG ulRec = ( HB_ULONG ) hb_arrayGetNL( pArray, nPos );
               BM_CLRREC( pBM, ulRec );
            }
            hb_bmResetFilterOpt( pArea );
         }
      }
   }
}


static HB_BOOL hb_bmEvalFilter( AREAP pArea, HB_BOOL fUpdate )
{
   PBM_FILTER pBM = BM_GETFILTER( pArea );
   HB_BOOL fResult = HB_TRUE;
   HB_ULONG ulRecNo = 0;

   if( pBM )
   {
      SELF_RECNO( pArea, &ulRecNo );
      if( ! fUpdate && ! BM_GETREC( pBM, ulRecNo ) )
         return HB_FALSE;
   }

   if( pArea->dbfi.itmCobExpr )
   {
      PHB_ITEM pResult = hb_vmEvalBlock( pArea->dbfi.itmCobExpr );
      fResult = ! HB_IS_LOGICAL( pResult ) || hb_itemGetL( pResult );
   }
   if( fResult && hb_setGetDeleted() )
   {
      SELF_DELETED( pArea, &fResult );
      fResult = ! fResult;
   }

   if( pBM )
   {
      if( ulRecNo > pBM->maxrec && fResult )
      {
         HB_SIZE nSize = sizeof( BM_FILTER ) + BM_BYTESIZE( ulRecNo ),
                 nOldSize = sizeof( BM_FILTER ) + BM_BYTESIZE( pBM->maxrec );
         if( nSize > nOldSize )
         {
            pArea->dbfi.lpvCargo = pBM = ( PBM_FILTER ) hb_xrealloc( pBM, nSize );
            memset( ( HB_BYTE * ) pBM + nOldSize, 0xFF, nSize - nOldSize );
         }
         pBM->maxrec = ( HB_U32 ) ulRecNo;
      }
      if( fResult )
         BM_SETREC( pBM, ulRecNo );
      else
         BM_CLRREC( pBM, ulRecNo );
   }
   return fResult;
}

static HB_ERRCODE hb_bmSkipFilter( AREAP pArea, HB_LONG lUpDown )
{
   HB_BOOL fBottom;
   HB_ERRCODE errCode;

   if( ! hb_setGetDeleted() && pArea->dbfi.itmCobExpr == NULL && ! BM_GETFILTER( pArea ) )
      return HB_SUCCESS;

   lUpDown = ( lUpDown < 0  ? -1 : 1 );
   fBottom = pArea->fBottom;
   while( ! pArea->fBof && ! pArea->fEof && ! hb_bmEvalFilter( pArea, HB_FALSE ) )
   {
      errCode = SELF_SKIPRAW( pArea, lUpDown );
      if( errCode != HB_SUCCESS )
         return errCode;
   }

   if( pArea->fBof && lUpDown < 0 )
   {
      if( fBottom )
         errCode = SELF_GOTO( pArea, 0 );
      else
      {
         errCode = SELF_GOTOP( pArea );
         pArea->fBof = HB_TRUE;
      }
   }
   else
      errCode = HB_SUCCESS;

   return errCode;
}

static HB_ERRCODE hb_bmPutRec( AREAP pArea, const HB_BYTE * pBuffer )
{
   HB_ERRCODE errCode;

   errCode = SUPER_PUTREC( pArea, pBuffer );

   if( pBuffer == NULL && errCode == HB_SUCCESS && BM_GETFILTER( pArea ) )
      hb_bmEvalFilter( pArea, HB_TRUE );

   return errCode;
}

static HB_ERRCODE hb_bmCountScope( AREAP pArea, void * pPtr, HB_LONG * plRec )
{
   if( pPtr == NULL )
   {
      PBM_FILTER pBM = BM_GETFILTER( pArea );

      if( pBM && pArea->dbfi.fFilter && ! BM_GETREC( pBM, ( HB_ULONG ) *plRec ) )
         *plRec = 0;

      return HB_SUCCESS;
   }
   return SUPER_COUNTSCOPE( pArea, pPtr, plRec );
}

static HB_ERRCODE hb_bmClearFilter( AREAP pArea )
{
   HB_ERRCODE errCode = SUPER_CLEARFILTER( pArea );

   if( pArea->dbfi.lpvCargo )
   {
      hb_xfree( pArea->dbfi.lpvCargo );
      pArea->dbfi.lpvCargo = NULL;
   }

   return errCode;
}

static HB_ERRCODE hb_bmSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   HB_ERRCODE errCode;

   errCode = SUPER_SETFILTER( pArea, pFilterInfo );
   if( errCode == HB_SUCCESS )
   {
      if( hb_setGetOptimize() )
      {
         PBM_FILTER pBM = hb_bmCreate( pArea, HB_TRUE );

         if( pBM )
         {
            pArea->dbfi.lpvCargo   = pBM;
            pArea->dbfi.fOptimized = HB_TRUE;
            pArea->dbfi.fFilter    = HB_TRUE;

            if( hb_setGetForceOpt() )
            {
               HB_ULONG ulRecNo, ulRec;

               if( SELF_RECNO( pArea, &ulRecNo ) == HB_SUCCESS )
               {
                  for( ulRec = 1; ulRec <= pBM->maxrec; ulRec++ )
                  {
                     SELF_GOTO( pArea, ulRec );
                     hb_bmEvalFilter( pArea, HB_TRUE );
                  }
                  SELF_GOTO( pArea, ulRecNo );
               }
            }
         }
      }
   }
   return errCode;
}


static const RDDFUNCS bmTable =
{
   /* Movement and positioning methods */
   ( DBENTRYP_BP )    NULL,              /* Bof        */
   ( DBENTRYP_BP )    NULL,              /* Eof        */
   ( DBENTRYP_BP )    NULL,              /* Found      */
   ( DBENTRYP_V )     NULL,              /* GoBottom   */
   ( DBENTRYP_UL )    NULL,              /* GoTo       */
   ( DBENTRYP_I )     NULL,              /* GoToId     */
   ( DBENTRYP_V )     NULL,              /* GoTop      */
   ( DBENTRYP_BIB )   NULL,              /* Seek       */
   ( DBENTRYP_L )     NULL,              /* Skip       */
   ( DBENTRYP_L )     hb_bmSkipFilter,   /* SkipFilter */
   ( DBENTRYP_L )     NULL,              /* SkipRaw    */

   /* Data management */
   ( DBENTRYP_VF )    NULL,              /* AddField       */
   ( DBENTRYP_B )     NULL,              /* Append         */
   ( DBENTRYP_I )     NULL,              /* CreateFields   */
   ( DBENTRYP_V )     NULL,              /* DeleteRec      */
   ( DBENTRYP_BP )    NULL,              /* Deleted        */
   ( DBENTRYP_SP )    NULL,              /* FieldCount     */
   ( DBENTRYP_VF )    NULL,              /* FieldDisplay   */
   ( DBENTRYP_SSI )   NULL,              /* FieldInfo      */
   ( DBENTRYP_SCP )   NULL,              /* FieldName      */
   ( DBENTRYP_V )     NULL,              /* Flush          */
   ( DBENTRYP_PP )    NULL,              /* GetRec         */
   ( DBENTRYP_SI )    NULL,              /* GetValue       */
   ( DBENTRYP_SVL )   NULL,              /* GetVarLen      */
   ( DBENTRYP_V )     NULL,              /* GoCold         */
   ( DBENTRYP_V )     NULL,              /* GoHot          */
   ( DBENTRYP_P )     hb_bmPutRec,       /* PutRec         */
   ( DBENTRYP_SI )    NULL,              /* PutValue       */
   ( DBENTRYP_V )     NULL,              /* Recall         */
   ( DBENTRYP_ULP )   NULL,              /* RecCount       */
   ( DBENTRYP_ISI )   NULL,              /* RecInfo        */
   ( DBENTRYP_ULP )   NULL,              /* RecNo          */
   ( DBENTRYP_I )     NULL,              /* RecId          */
   ( DBENTRYP_S )     NULL,              /* SetFieldExtent */

   /* WorkArea/Database management */
   ( DBENTRYP_CP )    NULL,              /* Alias       */
   ( DBENTRYP_V )     NULL,              /* Close       */
   ( DBENTRYP_VO )    NULL,              /* Create      */
   ( DBENTRYP_SI )    NULL,              /* Info        */
   ( DBENTRYP_V )     NULL,              /* NewArea     */
   ( DBENTRYP_VO )    NULL,              /* Open        */
   ( DBENTRYP_V )     NULL,              /* Release     */
   ( DBENTRYP_SP )    NULL,              /* StructSize  */
   ( DBENTRYP_CP )    NULL,              /* SysName     */
   ( DBENTRYP_VEI )   NULL,              /* Eval        */
   ( DBENTRYP_V )     NULL,              /* Pack        */
   ( DBENTRYP_LSP )   NULL,              /* PackRec     */
   ( DBENTRYP_VS )    NULL,              /* Sort        */
   ( DBENTRYP_VT )    NULL,              /* Trans       */
   ( DBENTRYP_VT )    NULL,              /* TransRec    */
   ( DBENTRYP_V )     NULL,              /* Zap         */

   /* Relational Methods */
   ( DBENTRYP_VR )    NULL,              /* ChildEnd      */
   ( DBENTRYP_VR )    NULL,              /* ChildStart    */
   ( DBENTRYP_VR )    NULL,              /* ChildSync     */
   ( DBENTRYP_V )     NULL,              /* SyncChildren  */
   ( DBENTRYP_V )     NULL,              /* ClearRel      */
   ( DBENTRYP_V )     NULL,              /* ForceRel      */
   ( DBENTRYP_SSP )   NULL,              /* RelArea       */
   ( DBENTRYP_VR )    NULL,              /* RelEval       */
   ( DBENTRYP_SI )    NULL,              /* RelText       */
   ( DBENTRYP_VR )    NULL,              /* SetRel        */

   /* Order Management */
   ( DBENTRYP_VOI )   NULL,              /* OrderListAdd      */
   ( DBENTRYP_V )     NULL,              /* OrderListClear    */
   ( DBENTRYP_VOI )   NULL,              /* OrderListDelete   */
   ( DBENTRYP_VOI )   NULL,              /* OrderListFocus    */
   ( DBENTRYP_V )     NULL,              /* OrderListRebuild  */
   ( DBENTRYP_VOO )   NULL,              /* OrderCondition    */
   ( DBENTRYP_VOC )   NULL,              /* OrderCreate       */
   ( DBENTRYP_VOI )   NULL,              /* OrderDestroy      */
   ( DBENTRYP_SVOI )  NULL,              /* OrderInfo         */

   /* Filters and Scope Settings */
   ( DBENTRYP_V )     hb_bmClearFilter,  /* ClearFilter  */
   ( DBENTRYP_V )     NULL,              /* ClearLocate  */
   ( DBENTRYP_V )     NULL,              /* ClearScope   */
   ( DBENTRYP_VPLP )  hb_bmCountScope,   /* CountScope   */
   ( DBENTRYP_I )     NULL,              /* FilterText   */
   ( DBENTRYP_SI )    NULL,              /* ScopeInfo    */
   ( DBENTRYP_VFI )   hb_bmSetFilter,    /* SetFilter    */
   ( DBENTRYP_VLO )   NULL,              /* SetLocate    */
   ( DBENTRYP_VOS )   NULL,              /* SetScope     */
   ( DBENTRYP_VPL )   NULL,              /* SkipScope    */
   ( DBENTRYP_B )     NULL,              /* Locate       */

   /* Miscellaneous */
   ( DBENTRYP_CC )    NULL,              /* Compile    */
   ( DBENTRYP_I )     NULL,              /* Error      */
   ( DBENTRYP_I )     NULL,              /* EvalBlock  */

   /* Network operations */
   ( DBENTRYP_VSP )   NULL,              /* RawLock  */
   ( DBENTRYP_VL )    NULL,              /* Lock     */
   ( DBENTRYP_I )     NULL,              /* UnLock   */

   /* Memofile functions */
   ( DBENTRYP_V )     NULL,              /* CloseMemFile   */
   ( DBENTRYP_VO )    NULL,              /* CreateMemFile  */
   ( DBENTRYP_SCCS )  NULL,              /* GetValueFile   */
   ( DBENTRYP_VO )    NULL,              /* OpenMemFile    */
   ( DBENTRYP_SCCS )  NULL,              /* PutValueFile   */

   /* Database file header handling */
   ( DBENTRYP_V )     NULL,              /* ReadDBHeader   */
   ( DBENTRYP_V )     NULL,              /* WriteDBHeader  */

   /* non WorkArea functions */
   ( DBENTRYP_R )     NULL,              /* Init    */
   ( DBENTRYP_R )     NULL,              /* Exit    */
   ( DBENTRYP_RVVL )  NULL,              /* Drop    */
   ( DBENTRYP_RVVL )  NULL,              /* Exists  */
   ( DBENTRYP_RVVVL ) NULL,              /* Rename  */
   ( DBENTRYP_RSLV )  NULL,              /* RddInfo */

   /* Special and reserved methods */
   ( DBENTRYP_SVP )   NULL               /* WhoCares */
};


static void hb_bmGetFuncTable( const char * szSuper )
{
   RDDFUNCS * pTable, * pSuperTable;
   HB_USHORT * puiCount, uiRddId, * puiSuperRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );
   pSuperTable = ( RDDFUNCS * ) hb_parptr( 3 );
   uiRddId = ( HB_USHORT ) hb_parni( 4 );
   puiSuperRddId = ( HB_USHORT * ) hb_parptr( 5 );

   HB_TRACE( HB_TR_DEBUG, ( "BM%s_GETFUNCTABLE(%p, %p, %p, %hu, %p)", szSuper, puiCount, pTable, pSuperTable, uiRddId, puiSuperRddId ) );

   if( puiCount && pTable && pSuperTable && puiSuperRddId )
   {
      HB_ERRCODE errCode;

      *puiCount = RDDFUNCSCOUNT;
      errCode = hb_rddInheritEx( pTable, &bmTable, pSuperTable, szSuper, puiSuperRddId );
      if( errCode == HB_SUCCESS )
         hb_bmSetRdd( uiRddId );
      hb_retni( errCode );
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_bmRddInit( void * cargo )
{
   HB_BOOL fError;

   HB_SYMBOL_UNUSED( cargo );

   fError = hb_rddRegister( "DBF", RDT_FULL ) > 1;
   if( ! fError )
   {
      hb_rddRegister( "DBFFPT", RDT_FULL );
      if( hb_rddRegister( "DBFCDX", RDT_FULL ) <= 1 )
      {
         if( hb_rddRegister( "BMDBFCDX", RDT_FULL ) > 1 )
            fError = HB_TRUE;
      }
      if( ! fError && hb_rddRegister( "DBFNTX", RDT_FULL ) <= 1 )
      {
         if( hb_rddRegister( "BMDBFNTX", RDT_FULL ) > 1 )
            fError = HB_TRUE;
      }
      if( ! fError && hb_rddRegister( "DBFNSX", RDT_FULL ) <= 1 )
      {
         if( hb_rddRegister( "BMDBFNSX", RDT_FULL ) > 1 )
            fError = HB_TRUE;
      }
   }

   if( fError )
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

HB_FUNC( _BMDBF ) { ; }
HB_FUNC_STATIC( BMDBFCDX_GETFUNCTABLE ) { hb_bmGetFuncTable( "DBFCDX" ); }
HB_FUNC_STATIC( BMDBFNTX_GETFUNCTABLE ) { hb_bmGetFuncTable( "DBFNTX" ); }
HB_FUNC_STATIC( BMDBFNSX_GETFUNCTABLE ) { hb_bmGetFuncTable( "DBFNSX" ); }

HB_INIT_SYMBOLS_BEGIN( _hb_bm_InitSymbols_ )
{ "BMDBFCDX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFCDX_GETFUNCTABLE )}, NULL },
{ "BMDBFNTX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFNTX_GETFUNCTABLE )}, NULL },
{ "BMDBFNSX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( BMDBFNSX_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( _hb_bm_InitSymbols_ )

HB_CALL_ON_STARTUP_BEGIN( _hb_bm_rdd_init_ )
   hb_vmAtInit( hb_bmRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_bm_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
#  pragma startup _hb_bm_InitSymbols_
#  pragma startup _hb_bm_rdd_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_bm_InitSymbols_ ) \
                              HB_DATASEG_FUNC( _hb_bm_rdd_init_ )
   #include "hbiniseg.h"
#endif
