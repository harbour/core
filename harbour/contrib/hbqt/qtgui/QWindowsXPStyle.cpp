/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QWindowsXPStyle>


/*
 * QWindowsXPStyle ()
 * ~QWindowsXPStyle ()
 *
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QWindowsXPStyle > pq;
} QGC_POINTER_QWindowsXPStyle;

QT_G_FUNC( release_QWindowsXPStyle )
{
   QGC_POINTER_QWindowsXPStyle * p = ( QGC_POINTER_QWindowsXPStyle * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QWindowsXPStyle              p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QWindowsXPStyle             ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QWindowsXPStyle * ) p->ph )->~QWindowsXPStyle();
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QWindowsXPStyle             Object deleted!" ) );
         #if defined(__debug__)
            just_debug( "  YES release_QWindowsXPStyle             %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QWindowsXPStyle             Object Name Missing!" ) );
         #if defined(__debug__)
            just_debug( "  NO  release_QWindowsXPStyle" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QWindowsXPStyle             Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QWindowsXPStyle" );
      #endif
   }
}

void * gcAllocate_QWindowsXPStyle( void * pObj )
{
   QGC_POINTER_QWindowsXPStyle * p = ( QGC_POINTER_QWindowsXPStyle * ) hb_gcAllocate( sizeof( QGC_POINTER_QWindowsXPStyle ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QWindowsXPStyle;
   new( & p->pq ) QPointer< QWindowsXPStyle >( ( QWindowsXPStyle * ) pObj );
   #if defined(__debug__)
      just_debug( "          new_QWindowsXPStyle             %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QWINDOWSXPSTYLE )
{
   void * pObj = NULL;

#if defined( HB_OS_WIN )
   pObj = ( QWindowsXPStyle* ) new QWindowsXPStyle() ;
#else
   pObj = NULL ;
#endif

   hb_retptrGC( gcAllocate_QWindowsXPStyle( pObj ) );
}

/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
