/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Terminal API (undocumented part)
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

#include "hbapigt.h"

void hb_gtWCreate( HB_GT_RECT * rect, HB_GT_WND ** wnd )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( rect );
   HB_SYMBOL_UNUSED( wnd );
}

void hb_gtWDestroy( HB_GT_WND * wnd )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( wnd );
}

BOOL hb_gtWFlash( void )
{
   /* TODO: */

   return FALSE;
}

void hb_gtWApp( HB_GT_WND ** wnd )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( wnd );
}

void hb_gtWCurrent( HB_GT_WND * wnd )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( wnd );
}

void hb_gtWPos( HB_GT_WND * wnd, HB_GT_RECT * rect )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( wnd );
   HB_SYMBOL_UNUSED( rect );
}

BOOL hb_gtWVis( HB_GT_WND * wnd, USHORT uiStatus )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( wnd );
   HB_SYMBOL_UNUSED( uiStatus );

   return FALSE;
}

USHORT hb_gtSLR( HB_GT_SLR * pSLR ) /* System Level Request */
{
   /* Do nothing in Harbour, since the low-level GT API is
      implemented with a different method than in CA-Cl*pper. */

   HB_SYMBOL_UNUSED( pSLR );

   return 1;
}

USHORT hb_gtModalRead( void * dummy )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( dummy );

   return 1;
}

USHORT hb_gtBeginWrite( void )
{
   /* Do nothing in Harbour */

   return 0;
}

USHORT hb_gtEndWrite( void )
{
   /* Do nothing in Harbour */

   return 0;
}

USHORT hb_gtFlushCursor( void )
{
   /* TODO: */

   return 1;
}

USHORT hb_gtSetColor( HB_GT_RGB * color )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( color );

   return 1;
}

USHORT hb_gtGetColor( HB_GT_RGB * color )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( color );

   return 1;
}

USHORT hb_gtSetBorder( HB_GT_RGB * color )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( color );

   return 1;
}

