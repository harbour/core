/*
 * Harbour Project source code:
 * Cairo library: drawing context
 *
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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


HB_FUNC( CAIRO_CLIP )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_clip( pCairo  );
}


HB_FUNC( CAIRO_CLIP_EXTENTS )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem = hb_stackReturnItem();
      double   x1, y1, x2, y2;

      cairo_clip_extents( pCairo, &x1, &y1, &x2, &y2 );
      hb_arrayNew( pItem, 4 );
      hb_arraySetND( pItem, 1, x1 );
      hb_arraySetND( pItem, 2, y1 );
      hb_arraySetND( pItem, 3, x2 );
      hb_arraySetND( pItem, 4, y2 );
   }
}


HB_FUNC( CAIRO_CLIP_PRESERVE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_clip_preserve( pCairo  );
}


HB_FUNC( CAIRO_COPY_PAGE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_copy_page( pCairo  );
}


HB_FUNC( CAIRO_CREATE )
{
   cairo_surface_t * pSurface = hb_cairo_surface_param( 1 );

   if( pSurface )
      hb_cairo_ret( cairo_create( pSurface ) );
}


HB_FUNC( CAIRO_FILL )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_fill( pCairo  );
}


HB_FUNC( CAIRO_FILL_PRESERVE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_fill_preserve( pCairo );
}


HB_FUNC( CAIRO_GET_DASH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem;
      int      i, iCount;
      double * pDashes, dOffset;

      iCount  = cairo_get_dash_count( pCairo );
      pDashes = ( double * ) hb_xgrab( iCount * sizeof( double ) );
      cairo_get_dash( pCairo, pDashes, &dOffset  );
      hb_stornd( dOffset, 3 );

      pItem = hb_itemArrayNew( ( HB_SIZE ) iCount );
      for( i = 0; i < iCount; i++ )
         hb_arraySetND( pItem, ( HB_SIZE ) i + 1, pDashes[ i ] );
      hb_xfree( pDashes );
      hb_itemParamStoreForward( 2, pItem );
      hb_itemRelease( pItem );
   }
}



HB_FUNC( CAIRO_GET_LINE_WIDTH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      hb_retnd( cairo_get_line_width( pCairo ) );
}


HB_FUNC( CAIRO_IN_FILL )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      hb_retl( cairo_in_fill( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) ) );
}


HB_FUNC( CAIRO_IN_STROKE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      hb_retl( cairo_in_stroke( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) ) );
}


HB_FUNC( CAIRO_PAINT )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_paint( pCairo );
}


HB_FUNC( CAIRO_PAINT_WITH_ALPHA )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_paint_with_alpha( pCairo, hb_parnd( 2 ) );
}


HB_FUNC( CAIRO_RESET_CLIP )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_reset_clip( pCairo  );
}


HB_FUNC( CAIRO_RESTORE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_restore( pCairo );
}


HB_FUNC( CAIRO_SAVE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_save( pCairo );
}


HB_FUNC( CAIRO_SET_DASH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem;

      if( ( pItem = hb_param( 2, HB_IT_ARRAY ) ) != NULL )
      {
         int      i, iCount = ( int ) hb_arrayLen( pItem );
         double * pDashes = NULL;

         if( iCount )
            pDashes = ( double * ) hb_xgrab( iCount * sizeof( double ) );

         for( i = 0; i < iCount; i++ )
            pDashes[ i ] = hb_arrayGetND( pItem, ( HB_SIZE ) i + 1 );
         cairo_set_dash( pCairo, pDashes, iCount, hb_parnd( 3 ) );

         if( pDashes )
            hb_xfree( pDashes );
      }
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}


HB_FUNC( CAIRO_SET_FILL_RULE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_fill_rule( pCairo, ( cairo_fill_rule_t ) hb_parni( 2 ) );
}


HB_FUNC( CAIRO_SET_LINE_CAP )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_line_cap( pCairo, ( cairo_line_cap_t ) hb_parni( 2 ) );
}


HB_FUNC( CAIRO_SET_LINE_JOIN )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_line_join( pCairo, ( cairo_line_join_t ) hb_parni( 2 ) );
}


HB_FUNC( CAIRO_SET_LINE_WIDTH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_line_width( pCairo, hb_parnd( 2 ) );
}


HB_FUNC( CAIRO_SET_MITER_LIMIT )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_miter_limit( pCairo, hb_parnd( 2 ) );
}


HB_FUNC( CAIRO_SET_OPERATOR )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_operator( pCairo, ( cairo_operator_t ) hb_parni( 2 ) );
}


HB_FUNC( CAIRO_SET_SOURCE_RGB )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_source_rgb( pCairo, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}


HB_FUNC( CAIRO_SET_SOURCE_RGBA )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_source_rgba( pCairo, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}


HB_FUNC( CAIRO_SET_TOLERANCE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_set_tolerance( pCairo, hb_parnd( 2 ) );
}

HB_FUNC( CAIRO_SHOW_PAGE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_show_page( pCairo );
}


HB_FUNC( CAIRO_STROKE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_stroke( pCairo );
}


HB_FUNC( CAIRO_STATUS )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      hb_retni( cairo_status( pCairo ) );
}


HB_FUNC( CAIRO_STROKE_EXTENTS )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem = hb_stackReturnItem();
      double   x1, y1, x2, y2;

      cairo_stroke_extents( pCairo, &x1, &y1, &x2, &y2 );
      hb_arrayNew( pItem, 4 );
      hb_arraySetND( pItem, 1, x1 );
      hb_arraySetND( pItem, 2, y1 );
      hb_arraySetND( pItem, 3, x2 );
      hb_arraySetND( pItem, 4, y2 );
   }
}


HB_FUNC( CAIRO_STROKE_PRESERVE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_stroke_preserve( pCairo );
}
