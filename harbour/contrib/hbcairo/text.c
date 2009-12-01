/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Cairo library: text
 *
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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


#include "hbcairo.h"
#include "hbapistr.h"


HB_FUNC( CAIRO_SELECT_FONT_FACE )
{
   void *     hFamily;
   cairo_t *  pCairo = hb_cairo_param( 1 );
   if( pCairo )
   {
      cairo_select_font_face( pCairo, hb_parstr_utf8( 2, &hFamily, NULL ), ( cairo_font_slant_t ) hb_parni( 3 ), ( cairo_font_weight_t ) hb_parni( 4 ) );
      hb_strfree( hFamily );
   }
}


HB_FUNC( CAIRO_SET_FONT_SIZE )
{
   cairo_t *  pCairo = hb_cairo_param( 1 );
   if( pCairo )
      cairo_set_font_size( pCairo, hb_parnd( 2 ) );
}


HB_FUNC( CAIRO_SHOW_TEXT )
{
   void *     hText;
   cairo_t *  pCairo = hb_cairo_param( 1 );
   if( pCairo )
   {
      cairo_show_text( pCairo, hb_parstr_utf8( 2, &hText, NULL ) );
      hb_strfree( hText );
   }
}


HB_FUNC( CAIRO_TEXT_EXTENTS )
{
   void *     hText;
   cairo_t *  pCairo = hb_cairo_param( 1 );
   cairo_text_extents_t te;
   if( pCairo )
   {
      PHB_ITEM  pItem = hb_stackReturnItem();

      cairo_text_extents( pCairo, hb_parstr_utf8( 2, &hText, NULL ), &te );
      hb_strfree( hText );
      hb_arrayNew( pItem, 6 )
      hb_arraySetND( pItem, 1, te.x_bearing );
      hb_arraySetND( pItem, 2, te.y_bearing );
      hb_arraySetND( pItem, 3, te.width     );
      hb_arraySetND( pItem, 4, te.height    );
      hb_arraySetND( pItem, 5, te.x_advance );
      hb_arraySetND( pItem, 6, te.y_advance );
   }
}

