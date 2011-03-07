/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MIN(), MAX() functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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
#include "hbapierr.h"

/* returns the maximum of two date or numerics */
/* NOTE: CA-Cl*pper returns 1-st item when they are equal [druzus] */
HB_FUNC( MAX )
{
   PHB_ITEM p1 = hb_param( 1, HB_IT_ANY );
   PHB_ITEM p2 = hb_param( 2, HB_IT_ANY );

   if( p1 && p2 )
   {
      if( HB_IS_NUMINT( p1 ) && HB_IS_NUMINT( p2 ) )
      {
         HB_MAXINT l1 = hb_itemGetNInt( p1 );
         HB_MAXINT l2 = hb_itemGetNInt( p2 );

         if( l1 >= l2 )
            hb_itemReturn( p1 );
         else
            hb_itemReturn( p2 );
         return;
      }
      else if( HB_IS_NUMERIC( p1 ) && HB_IS_NUMERIC( p2 ) )
      {
         double d1 = hb_itemGetND( p1 );
         double d2 = hb_itemGetND( p2 );

         if( d1 >= d2 )
            hb_itemReturn( p1 );
         else
            hb_itemReturn( p2 );
         return;
      }
      else if( HB_IS_LOGICAL( p1 ) && HB_IS_LOGICAL( p2 ) )
      {
         HB_BOOL b1 = hb_itemGetL( p1 );
         HB_BOOL b2 = hb_itemGetL( p2 );

         hb_retl( b1 >= b2 ? b1 : b2 );
         return;
      }
      else if( HB_IS_DATE( p1 ) && HB_IS_DATE( p2 ) )
      {
         long l1 = hb_itemGetDL( p1 );
         long l2 = hb_itemGetDL( p2 );

         hb_retdl( l1 >= l2 ? l1 : l2 );
         return;
      }
      else if( HB_IS_DATETIME( p1 ) && HB_IS_DATETIME( p2 ) )
      {
         if( HB_IS_DATE( p1 ) && hb_itemGetDL( p1 ) == hb_itemGetDL( p2 ) )
            hb_itemReturn( p1 );
         else if( HB_IS_DATE( p2 ) && hb_itemGetDL( p1 ) == hb_itemGetDL( p2 ) )
            hb_itemReturn( p2 );
         else
            hb_itemReturn( hb_itemGetTD( p1 ) >= hb_itemGetTD( p2 ) ? p1 : p2 );
         return;
      }
   }
   hb_errRT_BASE_SubstR( EG_ARG, 1093, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* returns the minimum of two date or numerics */
/* NOTE: CA-Cl*pper returns 1-st item when they are equal [druzus] */
HB_FUNC( MIN )
{
   PHB_ITEM p1 = hb_param( 1, HB_IT_ANY );
   PHB_ITEM p2 = hb_param( 2, HB_IT_ANY );

   if( p1 && p2 )
   {
      if( HB_IS_NUMINT( p1 ) && HB_IS_NUMINT( p2 ) )
      {
         HB_MAXINT l1 = hb_itemGetNInt( p1 );
         HB_MAXINT l2 = hb_itemGetNInt( p2 );

         if( l1 <= l2 )
            hb_itemReturn( p1 );
         else
            hb_itemReturn( p2 );
         return;
      }
      else if( HB_IS_NUMERIC( p1 ) && HB_IS_NUMERIC( p2 ) )
      {
         double d1 = hb_itemGetND( p1 );
         double d2 = hb_itemGetND( p2 );

         if( d1 <= d2 )
            hb_itemReturn( p1 );
         else
            hb_itemReturn( p2 );
         return;
      }
      else if( HB_IS_LOGICAL( p1 ) && HB_IS_LOGICAL( p2 ) )
      {
         HB_BOOL b1 = hb_itemGetL( p1 );
         HB_BOOL b2 = hb_itemGetL( p2 );

         hb_retl( b1 <= b2 ? b1 : b2 );
         return;
      }
      else if( HB_IS_DATE( p1 ) && HB_IS_DATE( p2 ) )
      {
         long l1 = hb_itemGetDL( p1 );
         long l2 = hb_itemGetDL( p2 );

         hb_retdl( l1 <= l2 ? l1 : l2 );
         return;
      }
      else if( HB_IS_DATETIME( p1 ) && HB_IS_DATETIME( p2 ) )
      {
         if( HB_IS_DATE( p1 ) && hb_itemGetDL( p1 ) == hb_itemGetDL( p2 ) )
            hb_itemReturn( p1 );
         else if( HB_IS_DATE( p2 ) && hb_itemGetDL( p1 ) == hb_itemGetDL( p2 ) )
            hb_itemReturn( p2 );
         else
            hb_itemReturn( hb_itemGetTD( p1 ) <= hb_itemGetTD( p2 ) ? p1 : p2 );
         return;
      }
   }

   hb_errRT_BASE_SubstR( EG_ARG, 1092, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
