/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SIX compatible function:
 *          Sx_TagOrder() *
 *          Sx_TagNo()
 *          Sx_Freeze()
 *          Sx_Warm()
 *          Sx_Chill()
 *          Sx_Thermometer()
 *          Sx_ClrScope()
 *          Sx_SetScope()
 *          Sx_IsReindex()
 *          Sx_Step()
 *          Sx_KeysIncluded()
 *          Sx_I_IndexName()
 *          Sx_I_TagName()
 *          Sx_IndexCount()
 *          Sx_IndexName()
 *          Sx_IndexType()
 *          Sx_KeyAdd()
 *          Sx_KeyDrop()
 *          Sx_KeyData()
 *          Sx_KeySkip()
 *          Sx_KeyCount()
 *          Sx_KeyNo()
 *          Sx_KeyGoto()
 *          Sx_SkipUnique()
 *          Sx_SeekLast()
 *          Sx_TagUnique()
 *          Sx_WildSeek()
 *          Sx_ROXLock()
 *          Sx_ROXUnLock()
 *          Sx_IsMyROX()
 *          Sx_IsROXLock()
 *          Sx_SortOption()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapifs.h"
#include "hbapirdd.h"

static HB_BOOL hb_sxOrdParam( LPDBORDERINFO pInfo )
{
   memset( pInfo, 0, sizeof( DBORDERINFO ) );

   if( HB_ISCHAR( 1 ) )
   {
      pInfo->itmOrder = hb_param( 1, HB_IT_STRING );
      pInfo->atomBagName = hb_param( 2, HB_IT_STRING );
   }
   else if( HB_ISNUM( 1 ) )
   {
      pInfo->itmOrder = hb_param( 1, HB_IT_NUMERIC );
      if( ! HB_ISNIL( 2 ) ) /* hb_pcount() > 2 */
      {
         pInfo->atomBagName = hb_param( 2, HB_IT_NUMERIC );
         if( hb_parni( 2 ) <= 0 )
            return HB_FALSE;
      }
   }
   return HB_TRUE;
}

HB_FUNC( SX_TAGORDER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int iOrder = 0;

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemPutNI( NULL, 0 );
         SELF_ORDINFO( pArea, DBOI_NUMBER, &Info );
         iOrder = hb_itemGetNI( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }

   hb_retni( iOrder );
}

/*
 * Sx_TagNo(tag,bag) -> nTagPosInBag
 * returns order position in order bag
 */
HB_FUNC( SX_TAGNO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int iBagOrder = 0, iOrder;

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemPutNI( NULL, 0 );
         if( SELF_ORDINFO( pArea, DBOI_NUMBER, &Info ) == HB_SUCCESS )
         {
            iOrder = hb_itemGetNI( Info.itmResult );
            if( iOrder )
            {
               Info.itmOrder = hb_itemPutNI( NULL, iOrder );
               Info.atomBagName = NULL;
               hb_itemClear( Info.itmResult );
               if( SELF_ORDINFO( pArea, DBOI_FULLPATH, &Info ) == HB_SUCCESS &&
                   hb_itemGetCLen( Info.itmResult ) > 0 )
               {
                  Info.atomBagName = Info.itmResult;
                  Info.itmResult = Info.itmOrder;
                  Info.itmOrder = NULL;
                  hb_itemClear( Info.itmResult );
                  if( SELF_ORDINFO( pArea, DBOI_BAGORDER, &Info ) == HB_SUCCESS )
                     iBagOrder = iOrder - hb_itemGetNI( Info.itmResult ) + 1;
                  Info.itmOrder = Info.atomBagName;
               }
               hb_itemRelease( Info.itmOrder );
            }
         }
         hb_itemRelease( Info.itmResult );
      }
   }

   hb_retni( iBagOrder );
}

HB_FUNC( SX_FREEZE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         HB_BOOL fResult = HB_FALSE;
         Info.itmNewVal = hb_itemPutL( NULL, HB_TRUE );
         Info.itmResult = hb_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_CUSTOM, &Info ) == HB_SUCCESS )
            fResult = HB_IS_LOGICAL( Info.itmResult ) &&
                      hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmNewVal );
         hb_itemRelease( Info.itmResult );
         hb_retl( fResult );
      }
   }
}

HB_FUNC( SX_WARM )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         HB_BOOL fResult = HB_FALSE;
         Info.itmNewVal = hb_itemPutL( NULL, HB_FALSE );
         Info.itmResult = hb_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_CHGONLY, &Info ) == HB_SUCCESS )
            fResult = HB_IS_LOGICAL( Info.itmResult ) &&
                      ! hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmNewVal );
         hb_itemRelease( Info.itmResult );
         hb_retl( fResult );
      }
   }
}

HB_FUNC( SX_CHILL )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         HB_BOOL fResult = HB_FALSE;
         Info.itmNewVal = hb_itemPutL( NULL, HB_TRUE );
         Info.itmResult = hb_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_CHGONLY, &Info ) == HB_SUCCESS )
            fResult = HB_IS_LOGICAL( Info.itmResult ) &&
                      hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmNewVal );
         hb_itemRelease( Info.itmResult );
         hb_retl( fResult );
      }
   }
}

/*
 * 1 - Full Update
 * 2 - Full Update (partial index)
 * 3 - Changes Only
 * 4 - No Update
 * -1 - not table or no order
 */
HB_FUNC( SX_THERMOMETER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int iTemperature = -1, i;

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemPutNI( NULL, 0 );
         SELF_ORDINFO( pArea, DBOI_NUMBER, &Info );
         i = hb_itemGetNI( Info.itmResult );
         if( i )
         {
            static const HB_USHORT s_iStates[] =
                     { DBOI_CUSTOM, DBOI_CHGONLY, DBOI_PARTIAL };
            iTemperature = 4;
            for( i = 0; i < 3; ++i, --iTemperature )
            {
               hb_itemClear( Info.itmResult );
               if( SELF_ORDINFO( pArea, s_iStates[ i ], &Info ) == HB_SUCCESS &&
                   HB_IS_LOGICAL( Info.itmResult ) &&
                   hb_itemGetL( Info.itmResult ) )
                  break;
            }
         }
         hb_itemRelease( Info.itmResult );
      }
   }

   hb_retni( iTemperature );
}

HB_FUNC( SX_CLRSCOPE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         int iScope = hb_parnidef( 1, 2 );
         Info.itmResult = hb_itemNew( NULL );
         if( iScope )
            SELF_ORDINFO( pArea, DBOI_SCOPEBOTTOMCLEAR, &Info );
         if( iScope == 0 || iScope == 2 )
            SELF_ORDINFO( pArea, DBOI_SCOPETOPCLEAR, &Info );
         hb_itemRelease( Info.itmResult );
      }
   }
}

HB_FUNC( SX_SETSCOPE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;

      if( hb_sxOrdParam( &Info ) )
      {
         int iScope = hb_parni( 1 );
         Info.itmResult = hb_itemNew( NULL );
         if( ! HB_ISNIL( 2 ) )
            Info.itmNewVal = hb_param( 2, HB_IT_ANY );
         SELF_ORDINFO( pArea, ( HB_USHORT ) ( iScope ? DBOI_SCOPEBOTTOM : DBOI_SCOPETOP ), &Info );
         hb_itemReturnRelease( Info.itmResult );
      }
   }
}

HB_FUNC( SX_ISREINDEX )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fReindex = HB_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_ISREINDEX, &Info );
      fReindex = hb_itemGetL( Info.itmResult );
      hb_itemRelease( Info.itmResult );
   }

   hb_retl( fReindex );
}

HB_FUNC( SX_STEP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_LONG lStep = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_EVALSTEP, &Info );
      lStep = hb_itemGetNL( Info.itmResult );
      hb_itemRelease( Info.itmResult );
   }

   hb_retnint( lStep );
}

HB_FUNC( SX_KEYSINCLUDED )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG ulKeys = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_KEYSINCLUDED, &Info );
      ulKeys = hb_itemGetNL( Info.itmResult );
      hb_itemRelease( Info.itmResult );
   }

   hb_retnint( ulKeys );
}

HB_FUNC( SX_I_INDEXNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_I_BAGNAME, &Info );
      hb_itemReturnRelease( Info.itmResult );
      return;
   }

   hb_retc_null();
}

HB_FUNC( SX_I_TAGNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_I_TAGNAME, &Info );
      hb_itemReturnRelease( Info.itmResult );
      return;
   }

   hb_retc_null();
}

HB_FUNC( SX_INDEXCOUNT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int iCount = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_BAGCOUNT, &Info );
      iCount = hb_itemGetNI( Info.itmResult );
      hb_itemRelease( Info.itmResult );
   }

   hb_retni( iCount );
}

HB_FUNC( SX_INDEXNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_FULLPATH, &Info );
         hb_itemReturnRelease( Info.itmResult );
      }
      else
         hb_retc_null();
   }
}

HB_FUNC( SX_INDEXTYPE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int iType = DBOI_TYPE_UNDEF;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
         {
            Info.atomBagName = Info.itmOrder;
            Info.itmOrder = NULL;
         }
         Info.itmResult = hb_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_INDEXTYPE, &Info ) == HB_SUCCESS )
            iType = hb_itemGetNI( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }
   hb_retni( iType );
}

HB_FUNC( SX_DESCEND )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_ISDESC, &Info ) == HB_SUCCESS )
         {
            Info.itmNewVal = hb_itemPutL( NULL, ! hb_itemGetL( Info.itmResult ) );
            SELF_ORDINFO( pArea, DBOI_ISDESC, &Info );
            hb_itemRelease( Info.itmNewVal );
         }
         hb_itemRelease( Info.itmResult );
      }
   }
}

HB_FUNC( SX_KEYADD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fResult = HB_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemPutL( NULL, HB_FALSE );
         Info.itmNewVal = hb_param( 3, HB_IT_ANY );
         SELF_ORDINFO( pArea, DBOI_KEYADD, &Info );
         fResult = hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }
   hb_retl( fResult );
}

HB_FUNC( SX_KEYDROP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fResult = HB_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemPutL( NULL, HB_FALSE );
         Info.itmNewVal = hb_param( 3, HB_IT_ANY );
         SELF_ORDINFO( pArea, DBOI_KEYDELETE, &Info );
         fResult = hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }
   hb_retl( fResult );
}

HB_FUNC( SX_KEYDATA )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_KEYVAL, &Info );
         hb_itemReturnRelease( Info.itmResult );
      }
   }
}

HB_FUNC( SX_KEYSKIP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fResult = HB_FALSE, fBEof = HB_FALSE;

   if( pArea )
   {
      if( SELF_SKIPRAW( pArea, hb_parnldef( 1, 1 ) ) == HB_SUCCESS )
      {
         if( SELF_EOF( pArea, &fBEof ) == HB_SUCCESS && ! fBEof )
            fResult = SELF_BOF( pArea, &fBEof ) == HB_SUCCESS && ! fBEof;
      }
   }
   hb_retl( fResult );
}

HB_FUNC( SX_KEYCOUNT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG ulKeys = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &Info );
         ulKeys = hb_itemGetNL( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }

   hb_retnint( ulKeys );
}

HB_FUNC( SX_KEYNO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_ULONG ulKeyNo = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_POSITION, &Info );
         ulKeyNo = hb_itemGetNL( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }

   hb_retnint( ulKeyNo );
}

HB_FUNC( SX_KEYGOTO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fResult = HB_FALSE;

   if( pArea && hb_parnl( 3 ) != 0 )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmNewVal = hb_param( 3, HB_IT_NUMERIC );
         Info.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( pArea, DBOI_POSITION, &Info );
         fResult = hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }

   hb_retl( fResult );
}

HB_FUNC( SX_SKIPUNIQUE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmNewVal = hb_param( 1, HB_IT_ANY );
      Info.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_SKIPUNIQUE, &Info );
      hb_itemRelease( Info.itmResult );
   }
}

HB_FUNC( SX_SEEKLAST )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fFound = HB_FALSE;

   if( pArea && hb_pcount() > 0 )
   {
      PHB_ITEM pKey = hb_param( 1, HB_IT_ANY );
      HB_BOOL bSoftSeek = hb_parl( 2 );

      if( SELF_SEEK( pArea, bSoftSeek, pKey, HB_TRUE ) == HB_SUCCESS )
      {
         if( SELF_FOUND( pArea, &fFound ) != HB_SUCCESS )
            fFound = HB_FALSE;
      }
   }
   hb_retl( fFound );
}

HB_FUNC( SX_TAGUNIQUE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemPutL( NULL, HB_FALSE );
         SELF_ORDINFO( pArea, DBOI_UNIQUE, &Info );
         hb_itemReturnRelease( Info.itmResult );
      }
   }
}

HB_FUNC( SX_WILDSEEK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   const char * szPattern = hb_parc( 1 );
   HB_BOOL fCont = hb_parl( 2 );
   HB_BOOL fFound = HB_FALSE;
   int iOrder = 0;

   if( pArea )
   {
      DBORDERINFO Info;
      memset( &Info, 0, sizeof( Info ) );
      Info.itmResult = hb_itemNew( NULL );

      if( szPattern && szPattern[ 0 ] )
      {
         if( SELF_ORDINFO( pArea, DBOI_NUMBER, &Info ) == HB_SUCCESS )
            iOrder = hb_itemGetNI( Info.itmResult );
      }
      if( iOrder > 0 )
      {
         HB_ERRCODE errCode = HB_SUCCESS;
         if( ! fCont )
         {
            errCode = SELF_GOTOP( pArea );
            if( errCode == HB_SUCCESS )
            {
               errCode = SELF_ORDINFO( pArea, DBOI_KEYVAL, &Info );
               if( errCode == HB_SUCCESS )
               {
                  const char * szKey = hb_itemGetCPtr( Info.itmResult );
                  fFound = hb_strMatchWild( szKey, szPattern );
               }
            }
         }
         if( ! fFound && errCode == HB_SUCCESS )
         {
            Info.itmNewVal = hb_param( 1, HB_IT_STRING );
            if( SELF_ORDINFO( pArea, DBOI_SKIPWILD, &Info ) == HB_SUCCESS )
               fFound = HB_IS_LOGICAL( Info.itmResult ) &&
                        hb_itemGetL( Info.itmResult );
         }
      }
      else
         SELF_GOTO( pArea, 0 );
      hb_itemReturnRelease( Info.itmResult );
   }

   hb_retl( fFound );
}

HB_FUNC( SX_ROXLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fLocked = HB_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmNewVal = hb_itemPutL( NULL, HB_TRUE );
         Info.itmResult = hb_itemPutL( NULL, HB_FALSE );
         if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == HB_SUCCESS )
            fLocked = hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmNewVal );
         hb_itemRelease( Info.itmResult );
      }
   }
   hb_retl( fLocked );
}

HB_FUNC( SX_ROXUNLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmNewVal = hb_itemPutL( NULL, HB_FALSE );
         Info.itmResult = hb_itemPutL( NULL, HB_FALSE );
         SELF_ORDINFO( pArea, DBOI_READLOCK, &Info );
         hb_itemRelease( Info.itmNewVal );
         hb_itemRelease( Info.itmResult );
      }
   }
}

HB_FUNC( SX_ISMYROX )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fLocked = HB_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == HB_SUCCESS )
            fLocked = hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }
   hb_retl( fLocked );
}

HB_FUNC( SX_ISROXLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fLocked = HB_FALSE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == HB_SUCCESS )
            fLocked = hb_itemGetL( Info.itmResult );
         if( ! fLocked )
         {
            Info.itmNewVal = hb_itemPutL( NULL, HB_TRUE );
            if( SELF_ORDINFO( pArea, DBOI_READLOCK, &Info ) == HB_SUCCESS )
               fLocked = hb_itemGetL( Info.itmResult );
            if( fLocked )
            {
               hb_itemPutL( Info.itmNewVal, HB_FALSE );
               SELF_ORDINFO( pArea, DBOI_READLOCK, &Info );
            }
            hb_itemRelease( Info.itmNewVal );
         }
         hb_itemRelease( Info.itmResult );
      }
   }
   hb_retl( fLocked );
}

HB_FUNC( SX_SORTOPTION )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fUseCurrent = HB_TRUE;

   if( pArea )
   {
      DBORDERINFO Info;
      if( hb_sxOrdParam( &Info ) )
      {
         Info.itmResult = hb_itemNew( NULL );
         Info.itmNewVal = hb_param( 1, HB_IT_LOGICAL );
         if( SELF_ORDINFO( pArea, DBOI_USECURRENT, &Info ) == HB_SUCCESS )
            fUseCurrent = hb_itemGetL( Info.itmResult );
         hb_itemRelease( Info.itmResult );
      }
   }
   hb_retl( fUseCurrent );
}
