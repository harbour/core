/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Cairo library: drawing context
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


HB_FUNC( CAIRO_CREATE )
{
   hb_cairo_ret( cairo_create( hb_cairo_surface_param( 1 ) ) );
}


HB_FUNC( CAIRO_FILL )
{
   cairo_fill( hb_cairo_param( 1 ) );
}


HB_FUNC( CAIRO_FILL_PRESERVE )
{
   cairo_fill_preserve( hb_cairo_param( 1 ) );
}


HB_FUNC( CAIRO_RESTORE )
{
   cairo_restore( hb_cairo_param( 1 ) );
}


HB_FUNC( CAIRO_SAVE )
{
   cairo_save( hb_cairo_param( 1 ) );
}


HB_FUNC( CAIRO_SET_LINE_CAP )
{
   cairo_set_line_cap( hb_cairo_param( 1 ), hb_parni( 2 ) );
}


HB_FUNC( CAIRO_SET_LINE_WIDTH )
{
   cairo_set_line_width( hb_cairo_param( 1 ), hb_parnd( 2 ) );
}


HB_FUNC( CAIRO_SET_SOURCE_RGB )
{
   cairo_set_source_rgb( hb_cairo_param( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}


HB_FUNC( CAIRO_SET_TOLERANCE )
{
   cairo_set_tolerance( hb_cairo_param( 1 ), hb_parnd( 2 ) );
}

HB_FUNC( CAIRO_SHOW_PAGE )
{
   cairo_show_page( hb_cairo_param( 1 ) );
}


HB_FUNC( CAIRO_STROKE )
{
   cairo_stroke( hb_cairo_param( 1 ) );
}


HB_FUNC( CAIRO_STROKE_PRESERVE )
{
   cairo_stroke_preserve( hb_cairo_param( 1 ) );
}

