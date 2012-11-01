/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    xHarbour Hash with Associative Array compatibility functions
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* These functions are only for compatibility with existing
 * xHarbour code. Harbour does not emulate associative arrays by
 * special index but it uses real natural order for them. It means
 * that associative array indexes are equal to regular hash indexes
 * so we do not need any translation between them and Harbour users
 * can use regular hash array functions for associative array indexes,
 * i.e. instead of using HAAGETKEYAT() they can use HB_HKEYAT().
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

static void s_errRT_hashArg( void )
{
   hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static void s_errRT_hashBound( void )
{
   hb_errRT_BASE( EG_BOUND, 1187, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static void s_errRT_hashAA( void )
{
   hb_errRT_BASE( EG_ARG, 1123, "Is not a Hash with Associative Array compatibility", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static HB_BOOL s_isHashAA( PHB_ITEM pHash )
{
   return ( hb_hashGetFlags( pHash ) & HB_HASH_KEEPORDER ) != 0;
}

/* HAAGETKEYAT( <hValue>, <nPos> ) -> <value> */
HB_FUNC( HAAGETKEYAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos  = hb_param( 2, HB_IT_NUMERIC );

   if( ! pHash || ! pPos )
      s_errRT_hashArg();
   else if( ! s_isHashAA( pHash ) )
      s_errRT_hashAA();
   else
   {
      PHB_ITEM pItem = hb_hashGetKeyAt( pHash, hb_itemGetNS( pPos ) );
      if( pItem )
         hb_itemReturn( pItem );
      else
         s_errRT_hashBound();
   }
}

/* HAAGETVALUEAT( <hValue>, <nPos> ) -> <value> */
HB_FUNC( HAAGETVALUEAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos  = hb_param( 2, HB_IT_NUMERIC );

   if( ! pHash || ! pPos )
      s_errRT_hashArg();
   else if( ! s_isHashAA( pHash ) )
      s_errRT_hashAA();
   else
   {
      PHB_ITEM pItem = hb_hashGetValueAt( pHash, hb_itemGetNS( pPos ) );
      if( pItem )
         hb_itemReturn( pItem );
      else
         s_errRT_hashBound();
   }
}

/* HAASETVALUEAT( <hValue>, <nPos>, <value> ) -> NIL */
HB_FUNC( HAASETVALUEAT )
{
   PHB_ITEM pHash  = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos   = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );

   if( ! pHash || ! pPos || ! pValue )
      s_errRT_hashArg();
   else if( ! s_isHashAA( pHash ) )
      s_errRT_hashAA();
   else
   {
      PHB_ITEM pItem = hb_hashGetValueAt( pHash, hb_itemGetNS( pPos ) );
      if( pItem )
         hb_itemCopy( pItem, pValue );
      else
         s_errRT_hashBound();
   }
}

/* HAADELAT( <hValue>, <nPos> ) -> NIL */
HB_FUNC( HAADELAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos  = hb_param( 2, HB_IT_NUMERIC );

   if( ! pHash || ! pPos )
      s_errRT_hashArg();
   else if( ! s_isHashAA( pHash ) )
      s_errRT_hashAA();
   else if( ! hb_hashDelAt( pHash, hb_itemGetNS( pPos ) ) )
      s_errRT_hashBound();
}

/* HAAGETPOS( <hValue>, <xKey> ) -> <nPos> */
HB_FUNC( HAAGETPOS )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey  = hb_param( 2, HB_IT_HASHKEY );

   if( ! pHash || ! pKey )
      s_errRT_hashArg();
   else if( ! s_isHashAA( pHash ) )
      s_errRT_hashAA();
   else
   {
      HB_SIZE nPos;
      hb_hashScan( pHash, pKey, &nPos );
      hb_retns( nPos );
   }
}

/* HAAGETREALPOS( <hValue>, <nPos> ) -> <nRealPos> */
HB_FUNC( HAAGETREALPOS )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
   {
      HB_SIZE nPos = hb_parns( 2 );

      hb_retns( s_isHashAA( pHash ) &&
                nPos > 0 && nPos <= hb_hashLen( pHash ) ? nPos : 0 );
   }
   else
      s_errRT_hashArg();
}

/* HGETVAAPOS( <hValue> ) -> <aOrder> */
HB_FUNC( HGETVAAPOS )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
   {
      HB_SIZE  n, nLen = hb_hashLen( pHash );
      PHB_ITEM pArray = hb_itemArrayNew( nLen );

      for( n = 1; n <= nLen; ++n )
         hb_arraySetNS( pArray, n, n );
      hb_itemReturnRelease( pArray );
   }
   else
      s_errRT_hashArg();
}

/* HSETAACOMPATIBILITY( <hValue>, <lAACompat> ) -> <lDone> */
HB_FUNC( HSETAACOMPATIBILITY )
{
   PHB_ITEM pHash  = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_LOGICAL );

   if( pHash && pValue && hb_hashLen( pHash ) == 0 )
   {
      if( hb_itemGetL( pValue ) )
      {
         hb_hashSetFlags( pHash, HB_HASH_KEEPORDER );
      }
      else
      {
         hb_hashClearFlags( pHash, HB_HASH_KEEPORDER );
         hb_hashSetFlags( pHash, HB_HASH_RESORT );
      }
      hb_retl( HB_TRUE );
   }
   else
      s_errRT_hashArg();
}

/* HGETAACOMPATIBILITY( <hValue> ) -> <lAACompat> */
HB_FUNC( HGETAACOMPATIBILITY )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
      hb_retl( s_isHashAA( pHash ) );
   else
      s_errRT_hashArg();
}
