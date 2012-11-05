/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *    Architecture dependent conversions
 *
 * Copyright 2005 Przemyslaw Czerpak <druzus@acn.waw.pl>
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
#include "hbmath.h"

/*
 * functions hb_put_ieee754() and hb_get_ieee754() stores / retrieve
 * IEEE754 double value making conversion from/to native C double type.
 * They should be used on platforms which does not use IEEE754 double
 * and user needs binary compatibility, f.e. he wants to share CDXs or
 * or DBFs with "B" fields with other station or use common .hrb files
 * functions hb_put_ord_ieee754() and hb_get_ord_ieee754() converts
 * to/from special modified IEEE754 double form used by some index formats
 * like CDX or NSX to create index keys. In this form double numbers can
 * be sorted as 8-bytes character values (f.e. with memcmp())
 */

#define HB_MANTISSA_BITS  52
#define HB_MANTISSA_MASK  ( ( ( HB_U64 ) 1 << HB_MANTISSA_BITS ) - 1 )
#define HB_EXPONENT_BITS  11
#define HB_EXPONENT_MASK  ( ( 1 << HB_EXPONENT_BITS ) - 1 )
#define HB_EXPONENT_ADD   0x3ff

void hb_put_ieee754( HB_BYTE * ptr, double d )
{
   int iExp, iSig;
   double df;

#if defined( HB_LONG_LONG_OFF )
   HB_U32 l1, l2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_put_ieee754(%p, %f)", ptr, d ) );

   iSig = d < 0 ? 1 : 0;
   if( d == 0.0 )
   {
      l1 = l2 = 0;
   }
   else
   {
      df = frexp( iSig ? -d : d, &iExp );
      l1 = ( HB_U32 ) ldexp( df, HB_MANTISSA_BITS + 1 );
      l2 = ( HB_U32 ) ldexp( df, HB_MANTISSA_BITS + 1 - 32 ) &
                         ( ( ( HB_U32 ) 1 << ( HB_MANTISSA_BITS - 32 ) ) - 1 );
      l2 |= ( HB_U32 ) ( ( iExp + HB_EXPONENT_ADD - 1 ) & HB_EXPONENT_MASK ) <<
                       ( HB_MANTISSA_BITS - 32 );
   }
   l2 |= ( HB_U32 ) iSig << ( HB_MANTISSA_BITS + HB_EXPONENT_BITS - 32 );
   HB_PUT_LE_UINT32( ptr, l1 );
   HB_PUT_LE_UINT32( ptr + 4, l2 );
#else
   HB_U64 ll;

   HB_TRACE( HB_TR_DEBUG, ( "hb_put_ieee754(%p, %f)", ptr, d ) );

   iSig = d < 0 ? 1 : 0;
   if( d == 0.0 )
   {
      ll = 0;
   }
   else
   {
      df = frexp( iSig ? -d : d, &iExp );
      ll = ( HB_U64 ) ldexp( df, HB_MANTISSA_BITS + 1 ) & HB_MANTISSA_MASK;
      ll |= ( HB_U64 ) ( ( iExp + HB_EXPONENT_ADD - 1 ) & HB_EXPONENT_MASK ) <<
                       HB_MANTISSA_BITS;
   }
   ll |= ( HB_U64 ) iSig << ( HB_MANTISSA_BITS + HB_EXPONENT_BITS );
   HB_PUT_LE_UINT64( ptr, ll );
#endif
}

double hb_get_ieee754( const HB_BYTE * ptr )
{
   int iExp, iSig;

#if defined( HB_LONG_LONG_OFF )
   HB_U32 l1, l2;
   double d;

   HB_TRACE( HB_TR_DEBUG, ( "hb_get_ieee754(%p)", ptr ) );

   l1 = HB_GET_LE_UINT32( ptr );
   l2 = HB_GET_LE_UINT32( ptr + 4 );
   iSig = ( int ) ( l2 >> ( HB_MANTISSA_BITS + HB_EXPONENT_BITS - 32 ) ) & 1;
   iExp = ( int ) ( ( l2 >> ( HB_MANTISSA_BITS - 32 ) ) & HB_EXPONENT_MASK );
   l2 &= ( ( HB_U32 ) 1 << ( HB_MANTISSA_BITS - 32 ) ) - 1;

   if( ( l1 | l2 | iExp ) != 0 )
      l2 |= ( HB_U32 ) 1 << ( HB_MANTISSA_BITS - 32 );

   d = ldexp( ( double ) l2, 32 ) + ( double ) l1;
   return ldexp( iSig ? -d : d, iExp - HB_MANTISSA_BITS - HB_EXPONENT_ADD );
#else
   HB_U64 ll;

   HB_TRACE( HB_TR_DEBUG, ( "hb_get_ieee754(%p)", ptr ) );

   ll = HB_GET_LE_UINT64( ptr );
   iSig = ( int ) ( ll >> ( HB_MANTISSA_BITS + HB_EXPONENT_BITS ) ) & 1;
   iExp = ( int ) ( ( ll >> HB_MANTISSA_BITS ) & HB_EXPONENT_MASK );
   ll &= HB_MANTISSA_MASK;
   if( ( ll | iExp ) != 0 )
      ll |= ( HB_U64 ) 1 << HB_MANTISSA_BITS;
   /* the casting form HB_U64 to HB_I64 is necessary for some
      compilers which does not support HB_U64 -> double conversion
      It will not change results because there is only up to 53bits
      set in mantissa */
   return ldexp( iSig ? -( double ) ( HB_I64 ) ll : ( double ) ( HB_I64 ) ll,
                 iExp - HB_MANTISSA_BITS - HB_EXPONENT_ADD );
#endif
}

void hb_put_ord_ieee754( HB_BYTE * ptr, double d )
{
   int iExp, iSig;
   double df;
   HB_U32 l1, l2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_put_ord_ieee754(%p, %f)", ptr, d ) );

   iSig = d < 0 ? 1 : 0;
   if( d == 0.0 )
   {
      l1 = l2 = 0;
   }
   else
   {
      df = frexp( iSig ? -d : d, &iExp );
      l1 = ( HB_U32 ) ldexp( df, HB_MANTISSA_BITS + 1 );
      l2 = ( HB_U32 ) ldexp( df, HB_MANTISSA_BITS + 1 - 32 ) &
                         ( ( ( HB_U32 ) 1 << ( HB_MANTISSA_BITS - 32 ) ) - 1 );
      l2 |= ( HB_U32 ) ( ( iExp + HB_EXPONENT_ADD - 1 ) & HB_EXPONENT_MASK ) <<
                       ( HB_MANTISSA_BITS - 32 );
   }
   if( iSig )
   {
      l2 ^= 0x7FFFFFFFL;
      l1 ^= 0xFFFFFFFFL;
   }
   else
   {
      l2 ^= 0x80000000L;
   }
   HB_PUT_BE_UINT32( ptr, l2 );
   HB_PUT_BE_UINT32( ptr + 4, l1 );
}

double hb_get_ord_ieee754( const HB_BYTE * ptr )
{
   int iExp, iSig;
   HB_U32 l1, l2;
   double d;

   HB_TRACE( HB_TR_DEBUG, ( "hb_get_ord_ieee754(%p)", ptr ) );

   l1 = HB_GET_BE_UINT32( ptr + 4 );
   l2 = HB_GET_BE_UINT32( ptr );
   iSig = ( l2 & 0x80000000L ) ? 0 : 1;
   if( iSig )
   {
      l2 ^= 0x7FFFFFFFL;
      l1 ^= 0xFFFFFFFFL;
   }
   iExp = ( ( l2 >> ( HB_MANTISSA_BITS - 32 ) ) & HB_EXPONENT_MASK );
   l2 &= ( ( HB_U32 ) 1 << ( HB_MANTISSA_BITS - 32 ) ) - 1;

   if( ( l1 | l2 | iExp ) != 0 )
      l2 |= ( HB_U32 ) 1 << ( HB_MANTISSA_BITS - 32 );

   d = ldexp( ( double ) l2, 32 ) + ( double ) l1;
   return ldexp( iSig ? -d : d, iExp - HB_MANTISSA_BITS - HB_EXPONENT_ADD );
}

/*
 * I added function hb_get_rev_double() and hb_get_std_double() because
 * some compilers does not like constraction used by in HB_GET_LE_DOUBLE
 * macro => d = { ... }
 */
double hb_get_rev_double( const HB_BYTE * ptr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_get_rev_double(%p)", ptr ) );

   {
#if defined( __GNUC__ )
      return _hb_get_rev_double( ptr );
#else
      union
      {
         double  dbl;
         HB_BYTE buffer[ 8 ];
      } u;

      u.buffer[ 0 ] = ptr[ 7 ];
      u.buffer[ 1 ] = ptr[ 6 ];
      u.buffer[ 2 ] = ptr[ 5 ];
      u.buffer[ 3 ] = ptr[ 4 ];
      u.buffer[ 4 ] = ptr[ 3 ];
      u.buffer[ 5 ] = ptr[ 2 ];
      u.buffer[ 6 ] = ptr[ 1 ];
      u.buffer[ 7 ] = ptr[ 0 ];

      return u.dbl;
#endif
   }
}

double hb_get_std_double( const HB_BYTE * ptr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_get_std_double(%p)", ptr ) );

   {
#if defined( __GNUC__ )
      return _hb_get_std_double( ptr );
#else
      union
      {
         double  dbl;
         HB_BYTE buffer[ 8 ];
      } u;

      u.buffer[ 0 ] = ptr[ 0 ];
      u.buffer[ 1 ] = ptr[ 1 ];
      u.buffer[ 2 ] = ptr[ 2 ];
      u.buffer[ 3 ] = ptr[ 3 ];
      u.buffer[ 4 ] = ptr[ 4 ];
      u.buffer[ 5 ] = ptr[ 5 ];
      u.buffer[ 6 ] = ptr[ 6 ];
      u.buffer[ 7 ] = ptr[ 7 ];

      return u.dbl;
#endif
   }
}

#if defined( HB_LONG_LONG_OFF )

/*
 * The function below are only for platforms which do not support
 * 64 but integer values. So the convert them to/from 'double'
 * values. They are necessary for extracting such number from PCODE,
 * databases or serialization streams in RPC
 */
double hb_get_le_uint64( const HB_BYTE * ptr )
{
   HB_U32 l1, l2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_get_le_uint64(%p)", ptr ) );

   l1 = HB_GET_LE_UINT32( ptr );
   l2 = HB_GET_LE_UINT32( ptr + 4 );
   return ldexp( ( double ) l2, 32 ) + ( double ) l1;
}

double hb_get_le_int64( const HB_BYTE * ptr )
{
   HB_U32 l1;
   HB_I32 l2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_get_le_int64(%p)", ptr ) );

   l1 = HB_GET_LE_UINT32( ptr );
   l2 = HB_GET_LE_INT32( ptr + 4 );
   return ldexp( ( double ) l2, 32 ) + ( double ) l1;
}

void hb_put_le_uint64( const HB_BYTE * ptr, double d )
{
   HB_U32 l1, l2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_put_le_uint64(%p)", ptr ) );

   l1 = ( HB_U32 ) ( d );
   l2 = ( HB_U32 ) ( d / 4294967296.0 );
   HB_PUT_LE_UINT32( ptr, l1 );
   HB_PUT_LE_UINT32( ptr + 4, l2 );
}

#endif
