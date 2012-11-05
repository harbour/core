/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Turbo Boyer-Moore (Crochemore) string search
 *    Based on this code:
 *       http://www-igm.univ-mlv.fr/~lecroq/string/node15.html
 *    Authors:
 *       Christian Charras, Thierry Lecroq
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#define ASIZE  UCHAR_MAX

static void preBmBc( const char * needle, HB_ISIZ m, HB_ISIZ bmBc[] )
{
   HB_ISIZ i;

   for( i = 0; i < ASIZE; ++i )
      bmBc[ i ] = m;
   for( i = 0; i < m - 1; ++i )
      bmBc[ ( HB_UCHAR ) needle[ i ] ] = m - i - 1;
}

static void suffixes( const char * needle, HB_ISIZ m, HB_ISIZ * suff )
{
   HB_ISIZ f, g, i;

   f = 0; /* NOTE: Fix added by me [vszakats] */
   suff[ m - 1 ] = m;
   g = m - 1;
   for( i = m - 2; i >= 0; --i )
   {
      if( i > g && suff[ i + m - 1 - f ] < i - g )
         suff[ i ] = suff[ i + m - 1 - f ];
      else
      {
         if( i < g )
            g = i;
         f = i;
         while( g >= 0 && needle[ g ] == needle[ g + m - 1 - f ] )
            --g;
         suff[ i ] = f - g;
      }
   }
}

static void preBmGs( const char * needle, HB_ISIZ m, HB_ISIZ bmGs[] )
{
   HB_ISIZ i, j;
   HB_ISIZ * suff = ( HB_ISIZ * ) hb_xgrab( m * sizeof( HB_ISIZ ) );

   suffixes( needle, m, suff );

   for( i = 0; i < m; ++i )
      bmGs[ i ] = m;

   j = 0;

   for( i = m - 1; i >= 0; --i )
      if( suff[ i ] == i + 1 )
         for( ; j < m - 1 - i; ++j )
            if( bmGs[ j ] == m )
               bmGs[ j ] = m - 1 - i;

   for( i = 0; i <= m - 2; ++i )
      bmGs[ m - 1 - suff[ i ] ] = m - 1 - i;

   hb_xfree( suff );
}

HB_ISIZ hb_strAtTBM( const char * needle, HB_ISIZ m, const char * haystack, HB_ISIZ n )
{
   HB_ISIZ r = 0;
   HB_ISIZ bcShift, i, j, shift, u, v, turboShift;
   HB_ISIZ bmBc[ ASIZE ];
   HB_ISIZ * bmGs;

   bmGs = ( HB_ISIZ * ) hb_xgrab( m * sizeof( HB_ISIZ ) );

   /* Preprocessing */
   preBmGs( needle, m, bmGs );
   preBmBc( needle, m, bmBc );

   /* Searching */
   j = u = 0;
   shift = m;
   while( j <= n - m )
   {
      i = m - 1;
      while( i >= 0 && needle[ i ] == haystack[ i + j ] )
      {
         --i;
         if( u != 0 && i == m - 1 - shift )
            i -= u;
      }

      if( i < 0 )
      {
         r = j + 1;
         break;
#if 0 /* To continue search */
         shift = bmGs[ 0 ];
         u = m - shift;
#endif
      }
      else
      {
         v = m - 1 - i;
         turboShift = u - v;
         bcShift = bmBc[ ( HB_UCHAR ) haystack[ i + j ] ] - m + 1 + i;
         shift = HB_MAX( turboShift, bcShift );
         shift = HB_MAX( shift, bmGs[ i ] );
         if( shift == bmGs[ i ] )
            u = HB_MIN( m - shift, v );
         else
         {
            if( turboShift < bcShift )
               shift = HB_MAX( shift, u + 1 );
            u = 0;
         }
      }
      j += shift;
   }

   hb_xfree( bmGs );

   return r;
}
