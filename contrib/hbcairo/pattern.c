/*
 * Harbour Project source code:
 * Cairo library: pattern
 *
 * Copyright 2013 Oscar Hernandez Suarez <oscar.hernandez.suarez@gmail.com>
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
 */


#include "hbcairo.h"
#include "hbapiitm.h"
#include "hbapierr.h"


/* ============ cairo_pattern_t * support ============ */
static HB_GARBAGE_FUNC( hb_cairo_pattern_destructor )
{
   cairo_pattern_t ** ppPattern = ( cairo_pattern_t ** ) Cargo;

   if( *ppPattern )
   {
      cairo_pattern_destroy( *ppPattern );
      *ppPattern = NULL;
   }
}


static const HB_GC_FUNCS s_gcPatternFuncs =
{
   hb_cairo_pattern_destructor,
   hb_gcDummyMark
};


cairo_pattern_t * hb_cairoPatternItemGet( PHB_ITEM pItem )
{
   cairo_pattern_t ** ppPattern = ( cairo_pattern_t ** ) hb_itemGetPtrGC( pItem, &s_gcPatternFuncs );
   return ppPattern ? *ppPattern : NULL;
}


PHB_ITEM hb_cairoPatternItemPut( PHB_ITEM pItem, cairo_pattern_t * pPattern )
{
   cairo_pattern_t ** ppPattern = ( cairo_pattern_t ** ) hb_gcAllocate( sizeof( cairo_pattern_t * ), &s_gcPatternFuncs );

   *ppPattern = pPattern;
   return hb_itemPutPtrGC( pItem, ppPattern );
}


cairo_pattern_t * hb_cairo_pattern_param( int iParam )
{
   cairo_pattern_t ** ppPattern = ( cairo_pattern_t ** ) hb_parptrGC( &s_gcPatternFuncs, iParam );

   if( ppPattern && *ppPattern )
      return *ppPattern;

   hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}


void hb_cairo_pattern_ret( cairo_pattern_t * pPattern )
{
   hb_cairoPatternItemPut( hb_stackReturnItem(), pPattern );
}


HB_FUNC( CAIRO_PATTERN_DESTROY )
{
   cairo_pattern_t ** ppPattern = ( cairo_pattern_t ** ) hb_parptrGC( &s_gcPatternFuncs, 1 );

   if( ppPattern && *ppPattern )
   {
      cairo_pattern_destroy( *ppPattern );
      *ppPattern = NULL;
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( CAIRO_PATTERN_ADD_COLOR_STOP_RGB )
{
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
      cairo_pattern_add_color_stop_rgb( pPattern, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}


HB_FUNC( CAIRO_PATTERN_ADD_COLOR_STOP_RGBA )
{
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
      cairo_pattern_add_color_stop_rgba( pPattern, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
}


HB_FUNC( CAIRO_PATTERN_GET_COLOR_STOP_COUNT )
{
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE( 1, 4, 0 )
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
   {
      int iCount;

      hb_retni( cairo_pattern_get_color_stop_count( pPattern, &iCount ) );
      hb_storni( iCount, 2 );
   }
   else
      hb_retni( -1 );
#else
   hb_cairo_pattern_param( 1 ); /* Parameter validation */
   hb_retni( -1 );              /* There is no good CAIRO_STATUS_* for this */
#endif
}


HB_FUNC( CAIRO_PATTERN_GET_COLOR_STOP_RGBA )
{
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE( 1, 4, 0 )
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
   {
      double dOffset;
      double dRed;
      double dGreen;
      double dBlue;
      double dAlpha;

      hb_retni( cairo_pattern_get_color_stop_rgba( pPattern, hb_parni( 2 ), &dOffset, &dRed, &dGreen, &dBlue, &dAlpha ) );

      hb_stornd( dOffset, 3 );
      hb_stornd( dRed, 4 );
      hb_stornd( dGreen, 5 );
      hb_stornd( dBlue, 6 );
      hb_stornd( dAlpha, 7 );
   }
   else
      hb_retni( -1 );
#else
   hb_cairo_pattern_param( 1 ); /* Parameter validation */
   hb_retni( -1 );              /* There is no good CAIRO_STATUS_* for this */
#endif
}


HB_FUNC( CAIRO_PATTERN_CREATE_RGB )
{
   hb_cairo_pattern_ret( cairo_pattern_create_rgb( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ) ) );
}


HB_FUNC( CAIRO_PATTERN_CREATE_RGBA )
{
   hb_cairo_pattern_ret( cairo_pattern_create_rgba( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 3 ) ) );
}


HB_FUNC( CAIRO_PATTERN_GET_RGBA )
{
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE( 1, 4, 0 )
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
   {
      double dRed;
      double dGreen;
      double dBlue;
      double dAlpha;

      hb_retni( cairo_pattern_get_rgba( pPattern, &dRed, &dGreen, &dBlue, &dAlpha ) );

      hb_stornd( dRed, 2 );
      hb_stornd( dGreen, 3 );
      hb_stornd( dBlue, 4 );
      hb_stornd( dAlpha, 5 );
   }
   else
      hb_retni( -1 );
#else
   hb_cairo_pattern_param( 1 ); /* Parameter validation */
   hb_retni( -1 );              /* There is no good CAIRO_STATUS_* for this */
#endif
}


HB_FUNC( CAIRO_PATTERN_CREATE_FOR_SURFACE )
{
   hb_cairo_pattern_ret( cairo_pattern_create_for_surface( hb_cairo_surface_param( 1 ) ) );
}


HB_FUNC( CAIRO_PATTERN_GET_SURFACE )
{
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE( 1, 4, 0 )
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
   {
      cairo_surface_t *pSurface;

      hb_retni( cairo_pattern_get_surface( pPattern, &pSurface ) );

      hb_cairoSurfaceStor( pSurface, 2 );
   }
   else
      hb_retni( -1 );
#else
   hb_cairo_pattern_param( 1 ); /* Parameter validation */
   hb_retni( -1 );              /* There is no good CAIRO_STATUS_* for this */
#endif
}


HB_FUNC( CAIRO_PATTERN_CREATE_LINEAR )
{
   hb_cairo_pattern_ret( cairo_pattern_create_linear( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) );
}


HB_FUNC( CAIRO_PATTERN_GET_LINEAR_POINTS )
{
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE( 1, 4, 0 )
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
   {
      double dX0;
      double dY0;
      double dX1;
      double dY1;

      hb_retni( cairo_pattern_get_linear_points( pPattern, &dX0, &dY0, &dX1, &dY1 ) );

      hb_stornd( dX0, 2 );
      hb_stornd( dY0, 3 );
      hb_stornd( dX1, 4 );
      hb_stornd( dY1, 5 );
   }
   else
      hb_retni( -1 );
#else
   hb_cairo_pattern_param( 1 ); /* Parameter validation */
   hb_retni( -1 );              /* There is no good CAIRO_STATUS_* for this */
#endif
}


HB_FUNC( CAIRO_PATTERN_CREATE_RADIAL )
{
   hb_cairo_pattern_ret( cairo_pattern_create_radial( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) );
}


HB_FUNC( CAIRO_PATTERN_GET_RADIAL_CIRCLES )
{
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE( 1, 4, 0 )
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
   {
      double dX0;
      double dY0;
      double dR0;
      double dX1;
      double dY1;
      double dR1;

      hb_retni( cairo_pattern_get_radial_points( pPattern, &dX0, &dY0, &dR0, &dX1, &dY1, &dR1 ) );

      hb_stornd( dX0, 2 );
      hb_stornd( dY0, 3 );
      hb_stornd( dR0, 4 );
      hb_stornd( dX1, 5 );
      hb_stornd( dY1, 6 );
      hb_stornd( dR1, 7 );
   }
   else
      hb_retni( -1 );
#else
   hb_cairo_pattern_param( 1 ); /* Parameter validation */
   hb_retni( -1 );              /* There is no good CAIRO_STATUS_* for this */
#endif
}


HB_FUNC( CAIRO_PATTERN_STATUS )
{
   cairo_pattern_t * pPattern = hb_cairo_pattern_param( 1 );

   if ( pPattern )
   {
      hb_retni( cairo_pattern_status( pPattern ) );
   }
   else
      hb_retni( -1 );
}

