/*
 * Harbour Project source code:
 * Cairo library: path
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


#include "hbcairo.h"
#include "hbapistr.h"


HB_FUNC( CAIRO_APPEND_PATH )
{
   cairo_t *      pCairo = hb_cairo_param( 1 );
   cairo_path_t * pPath  = hb_cairo_path_param( 2 );

   if( pCairo && pPath )
      cairo_append_path( pCairo, pPath );
}


HB_FUNC( CAIRO_ARC )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_arc( pCairo, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
}


HB_FUNC( CAIRO_ARC_NEGATIVE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_arc_negative( pCairo, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
}


HB_FUNC( CAIRO_CLOSE_PATH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_close_path( pCairo );
}


HB_FUNC( CAIRO_COPY_PATH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      hb_cairo_path_ret( cairo_copy_path( pCairo ) );
}


HB_FUNC( CAIRO_COPY_PATH_FLAT )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      hb_cairo_path_ret( cairo_copy_path_flat( pCairo ) );
}


HB_FUNC( CAIRO_CURVE_TO )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_curve_to( pCairo, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}


HB_FUNC( CAIRO_GET_CURRENT_POINT )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem = hb_stackReturnItem();
      double   x, y;
      hb_arrayNew( pItem, 2 );
      cairo_get_current_point( pCairo, &x, &y );
      hb_arraySetND( pItem, 1, x );
      hb_arraySetND( pItem, 2, y );
   }
}


HB_FUNC( CAIRO_HAS_CURRENT_POINT )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      hb_retl( cairo_has_current_point( pCairo ) );
   }
}


HB_FUNC( CAIRO_LINE_TO )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_line_to( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) );
}


HB_FUNC( CAIRO_MOVE_TO )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_move_to( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) );
}


HB_FUNC( CAIRO_NEW_PATH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_new_path( pCairo );
}


HB_FUNC( CAIRO_PATH_EXTENTS )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem = hb_stackReturnItem();
      double   x1, y1, x2, y2;

      cairo_path_extents( pCairo, &x1, &y1, &x2, &y2 );
      hb_arrayNew( pItem, 4 );
      hb_arraySetND( pItem, 1, x1 );
      hb_arraySetND( pItem, 2, y1 );
      hb_arraySetND( pItem, 3, x2 );
      hb_arraySetND( pItem, 4, y2 );
   }
}


HB_FUNC( CAIRO_RECTANGLE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_rectangle( pCairo, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}


HB_FUNC( CAIRO_REL_CURVE_TO )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_rel_curve_to( pCairo, hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}


HB_FUNC( CAIRO_REL_LINE_TO )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_rel_line_to( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) );
}


HB_FUNC( CAIRO_REL_MOVE_TO )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_rel_move_to( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) );
}


HB_FUNC( CAIRO_TEXT_PATH )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      void * hText;
      cairo_text_path( pCairo, hb_parstr_utf8( 2, &hText, NULL ) );
      hb_strfree( hText );
   }
}
