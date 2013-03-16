/*
 * Harbour Project source code:
 * Cairo library: transformations
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


HB_FUNC( CAIRO_GET_MATRIX )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM       pItem = hb_stackReturnItem();
      cairo_matrix_t m;

      cairo_get_matrix( pCairo, &m );
      hb_arrayNew( pItem, 6 );
      hb_arraySetND( pItem, 1, m.xx );
      hb_arraySetND( pItem, 2, m.yx );
      hb_arraySetND( pItem, 3, m.xy );
      hb_arraySetND( pItem, 4, m.yy );
      hb_arraySetND( pItem, 5, m.x0 );
      hb_arraySetND( pItem, 6, m.y0 );
   }
}


HB_FUNC( CAIRO_IDENTITY_MATRIX )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_identity_matrix( pCairo  );
}


HB_FUNC( CAIRO_ROTATE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_rotate( pCairo, hb_parnd( 2 ) );
}


HB_FUNC( CAIRO_SET_MATRIX )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem;
      if( ( pItem = hb_param( 2, HB_IT_ARRAY ) ) != NULL && hb_arrayLen( pItem ) == 6 )
      {
         cairo_matrix_t m;

         m.xx = hb_arrayGetND( pItem, 1 );
         m.yx = hb_arrayGetND( pItem, 2 );
         m.xy = hb_arrayGetND( pItem, 3 );
         m.yy = hb_arrayGetND( pItem, 4 );
         m.x0 = hb_arrayGetND( pItem, 5 );
         m.y0 = hb_arrayGetND( pItem, 6 );
         cairo_set_matrix( pCairo, &m );
      }
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}


HB_FUNC( CAIRO_SCALE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_scale( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) );
}


HB_FUNC( CAIRO_TRANSFORM )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
   {
      PHB_ITEM pItem;
      if( ( pItem = hb_param( 2, HB_IT_ARRAY ) ) != NULL && hb_arrayLen( pItem ) == 6 )
      {
         cairo_matrix_t m;

         m.xx = hb_arrayGetND( pItem, 1 );
         m.yx = hb_arrayGetND( pItem, 2 );
         m.xy = hb_arrayGetND( pItem, 3 );
         m.yy = hb_arrayGetND( pItem, 4 );
         m.x0 = hb_arrayGetND( pItem, 5 );
         m.y0 = hb_arrayGetND( pItem, 6 );
         cairo_transform( pCairo, &m );
      }
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}


HB_FUNC( CAIRO_TRANSLATE )
{
   cairo_t * pCairo = hb_cairo_param( 1 );

   if( pCairo )
      cairo_translate( pCairo, hb_parnd( 2 ), hb_parnd( 3 ) );
}
