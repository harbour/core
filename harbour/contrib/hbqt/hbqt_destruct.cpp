/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

static int s_iObjectReleaseMethod = HBQT_RELEASE_WITH_DELETE_LATER;

/*----------------------------------------------------------------------*/

HB_GARBAGE_FUNC( Q_release )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->ph )
      p->func( p );
}

const HB_GC_FUNCS QT_gcFuncs =
{
   Q_release,
   hb_gcDummyMark
};

const HB_GC_FUNCS * hbqt_gcFuncs( void )
{
   return &QT_gcFuncs;
}

void * hbqt_gcpointer( int iParam )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), iParam );

   if( p && p->ph )
      return p->ph;
   else
      return hb_parptr( iParam ); /* TOFIX: This is dangerous. */
}

int hbqt_get_object_release_method()
{
   return s_iObjectReleaseMethod;
}

HB_FUNC( HBQT_SET_RELEASE_METHOD )
{
   hb_retni( s_iObjectReleaseMethod );

   if( HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 && hb_parni( 1 ) <= HBQT_RELEASE_WITH_DELETE_LATER )
      s_iObjectReleaseMethod = hb_parni( 1 );
}

/*----------------------------------------------------------------------*/

#endif                  // #if QT_VERSION >= 0x040500
