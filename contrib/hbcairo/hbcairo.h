/*
 * Harbour Project source code:
 * Cairo library: .c include file
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

#ifndef HB_CAIRO_H_
#define HB_CAIRO_H_

#include "hbapi.h"
#include "hbstack.h"

#include "cairo.h"

HB_EXTERN_BEGIN

extern HB_EXPORT cairo_t *          hb_cairoItemGet( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM           hb_cairoItemPut( PHB_ITEM pItem, cairo_t * pCairo );
extern HB_EXPORT cairo_t *          hb_cairo_param( int iParam );
extern HB_EXPORT void               hb_cairo_ret( cairo_t * );

extern HB_EXPORT cairo_surface_t *  hb_cairoSurfaceItemGet( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM           hb_cairoSurfaceItemPut( PHB_ITEM pItem, cairo_surface_t * pSurface );
extern HB_EXPORT void               hb_cairoSurfaceStor( cairo_surface_t * pSurface, int iParam );
extern HB_EXPORT cairo_surface_t *  hb_cairo_surface_param( int iParam );
extern HB_EXPORT void               hb_cairo_surface_ret( cairo_surface_t * pSurface );

extern HB_EXPORT cairo_path_t *     hb_cairoPathItemGet( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM           hb_cairoPathItemPut( PHB_ITEM pItem, cairo_path_t * pPath );
extern HB_EXPORT cairo_path_t *     hb_cairo_path_param( int iParam );
extern HB_EXPORT void               hb_cairo_path_ret( cairo_path_t * pPath );

extern HB_EXPORT cairo_pattern_t *  hb_cairoPatternItemGet( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM           hb_cairoPatternItemPut( PHB_ITEM pItem, cairo_pattern_t * pPattern );
extern HB_EXPORT cairo_pattern_t *  hb_cairo_pattern_param( int iParam );
extern HB_EXPORT void               hb_cairo_pattern_ret( cairo_pattern_t * pPattern );

HB_EXTERN_END

#endif /* HB_CAIRO_H_ */
