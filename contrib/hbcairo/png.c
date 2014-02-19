/*
 * Harbour Project source code:
 * Cairo library: png
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


HB_FUNC( CAIRO_IMAGE_SURFACE_CREATE_FROM_PNG )
{
#ifdef CAIRO_HAS_PNG_FUNCTIONS
   hb_cairo_surface_ret( cairo_image_surface_create_from_png( hb_parc( 1 ) ) );
#else
   hb_retptr( NULL );
#endif
}


HB_FUNC( CAIRO_SURFACE_WRITE_TO_PNG )
{
#ifdef CAIRO_HAS_PNG_FUNCTIONS
   cairo_surface_t * pSurface = hb_cairo_surface_param( 1 );
   if( pSurface )
      hb_retni( cairo_surface_write_to_png( pSurface, hb_parc( 2 ) ) );
#else
   hb_cairo_surface_param( 1 ); /* Parameter validation */
   hb_retni( -1 );              /* There is no good CAIRO_STATUS_* for this */
#endif
}
